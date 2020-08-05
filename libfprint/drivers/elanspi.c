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

struct _FpiDeviceElanSpi {
	FpImageDevice parent;

	/* sensor info */
	unsigned char sensor_width, sensor_height, sensor_ic_version, sensor_id;
	gboolean sensor_otp;
	/* end sensor info */
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
			if (strstr(acpi_name, sysfs)) {
				g_debug("Found matching elan SPI %s", sysfs);

				spi_device = g_strdup(g_udev_device_get_device_file(i->data));
				break;
			}
		}

		g_list_foreach(list_of_devs, (GFunc)g_object_unref, NULL);
	}

	if (!spi_device) return NULL;
	GObject *udev_data_ = g_object_new(elanspi_udev_data_get_type(), NULL);
	ElanSpiUdevData *udev_data = ELANSPI_UDEV_DATA(udev_data_);

	udev_data->hid_device = hid_device;
	udev_data->spi_device = spi_device;
	udev_data->mode = 1; // who knows

	return udev_data_;
}

static void fpi_device_elanspi_init(FpiDeviceElanSpi *self) {
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
}
