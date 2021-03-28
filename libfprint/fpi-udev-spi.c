/*
 * Copyright (C) 2021 Matthew Mirvish <matthew@mm12.xyz>
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

#include "fpi-udev-spi.h"

#ifdef HAVE_UDEV

typedef struct _FpiUdevDataSpiPrivate {
	gchar * spi_path;
} FpiUdevDataSpiPrivate;

G_DEFINE_TYPE_WITH_PRIVATE(FpiUdevDataSpi, fpi_udev_data_spi, G_TYPE_OBJECT);

static void fpi_udev_data_spi_init(FpiUdevDataSpi *data) {}

static void fpi_udev_data_spi_finalize(GObject *data) {
	FpiUdevDataSpiPrivate * p = fpi_udev_data_spi_get_instance_private(FPI_UDEV_DATA_SPI(data));
	g_free(p->spi_path);
	p->spi_path = NULL;
}

static void fpi_udev_data_spi_class_init(FpiUdevDataSpiClass *cls) {
	cls->parent_class.finalize = fpi_udev_data_spi_finalize;
}

const gchar * fpi_udev_data_spi_get_path(FpiUdevDataSpi *data) {
	FpiUdevDataSpiPrivate * p = fpi_udev_data_spi_get_instance_private(data);
	return p->spi_path;
}

FpiUdevDataSpi * fpi_udev_data_spi_new(const gchar * path) {
	FpiUdevDataSpi * new_obj = g_object_new(FPI_TYPE_UDEV_DATA_SPI, NULL);
	FpiUdevDataSpiPrivate * new_obj_priv = fpi_udev_data_spi_get_instance_private(new_obj);
	new_obj_priv->spi_path = g_strdup(path);
	return new_obj;
}

/**
 * fpi_detect_udev_spi:
 *
 * Detects `spidev` devices which are associated with specific ACPI IDs (provided as strings in check_args).
 */
void fpi_detect_udev_spi(GUdevClient *client, size_t device_count, gpointer check_args_in[], GObject *udev_data_out[]) {
	g_autoptr(GUdevEnumerator) enumerator = g_udev_enumerator_new(client);
	g_udev_enumerator_add_match_subsystem(enumerator, "spidev");
	g_autoptr(GList) list_of_devs = g_udev_enumerator_execute(enumerator);

	for (GList* i = list_of_devs; i; i = i->next) {
		// Check if the acpi id is in the syspath
		const gchar* sysfs = g_udev_device_get_sysfs_path(i->data);
		if (!sysfs) continue;
		for (int j = 0; j < device_count; ++j) {
			if (strstr(sysfs, (gchar *)check_args_in[j])) {
				const gchar * spi_path = g_udev_device_get_device_file(i->data);
				if (udev_data_out[j] == NULL) {
					udev_data_out[j] = G_OBJECT(fpi_udev_data_spi_new(spi_path));
					g_debug("Found matching SPI %s for %s", spi_path, (gchar*)check_args_in[j]);
				}
				else {
					g_warning("Found multiple matching SPI %s for %s", spi_path, (gchar*)check_args_in[j]);
				}
				break;
			}
		}
	}

	g_list_foreach(list_of_devs, (GFunc)g_object_unref, NULL);
}

#endif
