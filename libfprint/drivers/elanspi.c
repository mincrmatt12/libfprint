/*
 * Elan SPI driver for libfprint
 *
 * Copyright (C) 2020 Matthew Mirvish <matthew@mm12.xyz>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#define FP_COMPONENT "elanspi"

#include "drivers_api.h"
#include "elanspi.h"

#include <linux/spi/spidev.h>
#include <linux/hidraw.h>
#include <sys/ioctl.h>
#include <sys/fcntl.h>
#include <sys/unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <linux/types.h>
#include <errno.h>

GQuark elanspi_init_error_quark(void);
G_DEFINE_QUARK(elanspi-init-error-quark, elanspi_init_error)

struct _FpiDeviceElanSpi {
	FpImageDevice parent;

	/* sensor info */
	unsigned char sensor_width, sensor_height, sensor_ic_version, sensor_id;
	gboolean sensor_otp;
	/* end sensor info */

	/* active SPI status info */
	int spi_fd;
};

G_DECLARE_FINAL_TYPE(FpiDeviceElanSpi, fpi_device_elanspi, FPI, DEVICE_ELANSPI, FpImageDevice);
G_DEFINE_TYPE(FpiDeviceElanSpi, fpi_device_elanspi, FP_TYPE_IMAGE_DEVICE);

struct _ElanSpiUdevData {
	GObject parent;
	
	gint   mode; // unused
	gchar *spi_device;
	gchar *hid_device;
};

G_DECLARE_FINAL_TYPE(ElanSpiUdevData, elanspi_udev_data, ELANSPI, UDEV_DATA, GObject);
G_DEFINE_TYPE(ElanSpiUdevData, elanspi_udev_data, G_TYPE_OBJECT);

static void elanspi_udev_data_finalize(GObject *gobject) {
	ElanSpiUdevData *priv = ELANSPI_UDEV_DATA(gobject);
	
	g_free(priv->hid_device);
	g_free(priv->spi_device);

	G_OBJECT_CLASS(elanspi_udev_data_parent_class)->finalize(gobject);
}

static void elanspi_udev_data_class_init(ElanSpiUdevDataClass *klass) {
	GObjectClass *oklass = G_OBJECT_CLASS(klass);

	oklass->finalize = elanspi_udev_data_finalize;
}

static void elanspi_udev_data_init(ElanSpiUdevData *self) {
	self->spi_device = NULL;
	self->hid_device = NULL;
}

GObject * elanspi_udev_check_acpi_hid(GUdevClient *client, const void* arg) {
	const char *acpi_name = arg;

	g_autofree gchar* hid_device = NULL;
	g_autofree gchar* spi_device = NULL;
	// Try to enumerate HID devices
	{
		g_autoptr(GUdevEnumerator) enumerator = g_udev_enumerator_new(client);
		g_udev_enumerator_add_match_subsystem(enumerator, "hidraw");
		g_autoptr(GList) list_of_devs = g_udev_enumerator_execute(enumerator);

		for (GList* i = list_of_devs; i; i = i->next) {
			// Check if the acpi id is in the syspath
			const gchar* devnode = g_udev_device_get_device_file(i->data);
			if (!devnode) continue;

			int temp_hid = open(devnode, O_RDWR);
			if (temp_hid < 0) continue;

			struct hidraw_devinfo info;
			int res = ioctl(temp_hid, HIDIOCGRAWINFO, &info);
			close(temp_hid);
			if (res < 0) continue;
			if (info.vendor == ELANSPI_TP_VID && info.product == ELANSPI_TP_PID) {
				g_debug("Found matching elan HID %s", devnode);

				hid_device = g_strdup(devnode);
				break;
			}
		}

		g_list_foreach(list_of_devs, (GFunc)g_object_unref, NULL);
	}

	if (!hid_device) return NULL;

	// Try to enumerate for spi devices
	{
		g_autoptr(GUdevEnumerator) enumerator = g_udev_enumerator_new(client);
		g_udev_enumerator_add_match_subsystem(enumerator, "spidev");
		g_autoptr(GList) list_of_devs = g_udev_enumerator_execute(enumerator);

		for (GList* i = list_of_devs; i; i = i->next) {
			// Check if the acpi id is in the syspath
			const gchar* sysfs = g_udev_device_get_sysfs_path(i->data);
			if (!sysfs) continue;
			if (strstr(sysfs, acpi_name)) {

				spi_device = g_strdup(g_udev_device_get_device_file(i->data));
				g_debug("Found matching elan SPI %s", spi_device);
				break;
			}
		}

		g_list_foreach(list_of_devs, (GFunc)g_object_unref, NULL);
	}

	if (!spi_device) return NULL;
	GObject *udev_data_ = g_object_new(elanspi_udev_data_get_type(), NULL);
	ElanSpiUdevData *udev_data = ELANSPI_UDEV_DATA(udev_data_);

	udev_data->hid_device = g_strdup(hid_device);
	udev_data->spi_device = g_strdup(spi_device);
	udev_data->mode = 1; // who knows

	return udev_data_;
}

static void elanspi_do_hwreset(FpiDeviceElanSpi *self, GError **err) {
}

static void elanspi_do_swreset(int fd, GError **err) {
	guint8 cmd = 0x31;
	write(fd, &cmd, 1);
	usleep(4000); // todo: maybe this will be the ssm?
}

/*
 * COMM ROUTINES
 *
 * TODO: error reporting via GError
 */

static void elanspi_spi_duplex(int fd, guint8* rx_buf, guint8* tx_buf, gsize length, GError **err) {
	struct spi_ioc_transfer mesg;
	memset(&mesg, 0, sizeof mesg);
	mesg.len = length;
	mesg.rx_buf = (__u64)rx_buf;
	mesg.tx_buf = (__u64)tx_buf;

	if (ioctl(fd, SPI_IOC_MESSAGE(1), &mesg) < 0) {
		g_set_error(err, elanspi_init_error_quark(), 20, "unable to talk to spi: %s",
				g_strerror(errno));
	}
}

static guint8 elanspi_single_read_cmd(int fd, guint8 cmd_id, GError **err) {
	guint8 cmd[3] = {cmd_id, 0xff, 0xff};
	guint8 resp[3];

	elanspi_spi_duplex(fd, resp, cmd, 3, err);
	return resp[2];
}

static guint8 elanspi_read_status(int fd, GError **err) {
	return elanspi_single_read_cmd(fd, 0x3, err);
}

static guint8 elanspi_read_width(int fd, GError **err) {
	return elanspi_single_read_cmd(fd, 0x9, err) + 1;
}

static guint8 elanspi_read_height(int fd, GError **err) {
	return elanspi_single_read_cmd(fd, 0x8, err) + 1;
}

static guint8 elanspi_read_version(int fd, GError **err) {
	return elanspi_single_read_cmd(fd, 0xa, err);
}

static guint8 elanspi_read_register(int fd, guint8 addr, GError **err) {
	guint8 cmd[2] = {addr | 0x40, 0xff};
	guint8 resp[2];

	elanspi_spi_duplex(fd, resp, cmd, 2, err);
	return resp[1];
}


/* 
 * INIT ROUTINE:
 *
 * I'd make this use SSM, but spidev is synchronous, so save for running everything in a separate
 * thread, i'm not exactly sure if it would really help...
 */

static void elanspi_init(FpiDeviceElanSpi *self, GError **err) {
	g_debug("Beginning init");

	guint8 spistatus = elanspi_read_status(self->spi_fd, err);
	if (*err) return;
	g_debug("SPIStatus = %.2x", spistatus);

	elanspi_do_hwreset(self, err);
	if (*err) return;

	guint8 raw_height = elanspi_read_height(self->spi_fd, err);
	guint8 raw_width = elanspi_read_width(self->spi_fd, err);
	if (*err) return;

	g_debug("Raw sensor dimensions %dx%d", raw_width, raw_height);

	self->sensor_width = raw_width;
	self->sensor_height = raw_height;
	self->sensor_ic_version = 0;

	// Do a hardcoded check:
	// It appears that the format changed with the versions, as some sensors report 1+the correct values for these, indicating that the +1 was added later, hence why they are 
	// labelled ICVersion 0
	//
	// As a result, we check for the three dimensions that have this behaviour first
	if ( ((raw_height == 0xa1) && (raw_width == 0xa1)) ||
	     ((raw_height == 0xd1) && (raw_width == 0x51)) ||
	     ((raw_height == 0xc1) && (raw_width == 0x39)) ) {
		self->sensor_ic_version = 0; // Version 0
		self->sensor_width = raw_width - 1;
		self->sensor_height = raw_height - 1;
	}
	else {
		// If the sensor is exactly 96x96 (0x60 x 0x60), the version is the high bit of register 17
		if (raw_width == 0x60 && raw_height == 0x60) {
			if (-1 < (gint8)(elanspi_read_register(self->spi_fd, 0x17, err))) {
				self->sensor_ic_version = 0;
			}
			else {
				self->sensor_ic_version = 1;
			}

			if (*err) return;
		}
		else {
			if ( ((raw_height != 0xa0) || (raw_width != 0x50)) &&
				 ((raw_height != 0x90) || (raw_width != 0x40)) &&
				 ((raw_height != 0x78) || (raw_width != 0x78)) ) {
				if ( ((raw_height != 0x40) || (raw_width != 0x58)) &&
				     ((raw_height != 0x50) || (raw_width != 0x50)) ) {
					// Old sensor hack??
					self->sensor_width = 0x78;
					self->sensor_height = 0x78;
					self->sensor_ic_version = 0;
				}
				else {
					// Otherwise, read the version 'normally'
					self->sensor_ic_version = elanspi_read_version(self->spi_fd, err);
					if (*err) return;
					if ((self->sensor_ic_version & 0x70) == 0x10) self->sensor_ic_version = 1;
				}
			}
			else {
				self->sensor_ic_version = 1;
			}
		}
	}

	for (const struct elanspi_sensor_entry *entry = elanspi_sensor_table; entry->name; ++entry) {
		if (entry->ic_version == self->sensor_ic_version && entry->width == self->sensor_width &&
			entry->height == self->sensor_height) {
			self->sensor_id = entry->sensor_id;
			self->sensor_otp = entry->is_otp_model;

			g_debug("Found sensor ID %d => [%s] (%d x %d)", self->sensor_id, entry->name, self->sensor_width, self->sensor_height);
			break;
		}
	}

	if (self->sensor_id == 0xff) {
		g_set_error(err, elanspi_init_error_quark(), 10, "Unable to find matching ID for %dx%d (%d)", self->sensor_width, self->sensor_height, self->sensor_ic_version);
		return;
	}

	elanspi_do_swreset(self->spi_fd, err);
	if (*err) return;
}

static void dev_activate(FpImageDevice *dev) {
	FpiDeviceElanSpi *self = FPI_DEVICE_ELANSPI(dev);
	GError *err = NULL;

	G_DEBUG_HERE();
	elanspi_init(self, &err);
	fpi_image_device_activate_complete(dev, err);
	return;
}

static void dev_open(FpImageDevice *dev) {
	FpiDeviceElanSpi *self = FPI_DEVICE_ELANSPI(dev);
	GError *err = NULL;

	G_DEBUG_HERE();

	// Try to open the SPI device from our info

	ElanSpiUdevData *dat;
	if (!ELANSPI_IS_UDEV_DATA(fpi_device_get_udev_data(FP_DEVICE(dev)))) {
		g_error("invalid udev data");
		g_set_error_literal(&err, elanspi_init_error_quark(), 1, "invalid state");
		fpi_image_device_open_complete(dev, err);
		return;
	}
	dat = ELANSPI_UDEV_DATA(fpi_device_get_udev_data(FP_DEVICE(dev)));

	int spi_fd = open(dat->spi_device, O_RDWR);
	int saved_errno = errno;
	if (spi_fd < 0) {
		g_set_error(&err, elanspi_init_error_quark(), 2, "unable to open spi: %s",
				g_strerror(saved_errno));
		fpi_image_device_open_complete(dev, err);
		return;
	}

	self->spi_fd = spi_fd;
	fpi_image_device_open_complete(dev, NULL);
}

static void dev_close(FpImageDevice *dev) {
	FpiDeviceElanSpi *self = FPI_DEVICE_ELANSPI(dev);

	G_DEBUG_HERE();

	// Try to open the SPI device from our info

	if (self->spi_fd >= 0) close(self->spi_fd);
	fpi_image_device_close_complete(dev, NULL);
}

static void fpi_device_elanspi_init(FpiDeviceElanSpi *self) {
	self->spi_fd = -1;
	self->sensor_id = 0xff;
}

static void fpi_device_elanspi_class_init(FpiDeviceElanSpiClass *klass) {
	FpDeviceClass *dev_class = FP_DEVICE_CLASS (klass);
	FpImageDeviceClass *img_class = FP_IMAGE_DEVICE_CLASS (klass);

	dev_class->id = "elanspi";
	dev_class->full_name = "ElanTech Embedded Fingerprint Sensor";
	dev_class->type = FP_DEVICE_TYPE_UDEV;
	dev_class->id_table = elanspi_id_table;
	dev_class->scan_type = FP_SCAN_TYPE_PRESS;

	img_class->bz3_threshold = 10;
	img_class->img_open = dev_open;
	img_class->activate = dev_activate;
	img_class->img_close = dev_close;
}
