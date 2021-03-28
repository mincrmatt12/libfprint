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

#pragma once

#include "fpi-device.h"

#ifdef HAVE_UDEV

/* Basic SPI udev opaque gobject. */

#define FPI_TYPE_UDEV_DATA_SPI (fpi_udev_data_spi_get_type())
G_DECLARE_DERIVABLE_TYPE(FpiUdevDataSpi, fpi_udev_data_spi, FPI, UDEV_DATA_SPI, GObject);

struct _FpiUdevDataSpiClass {
	GObjectClass parent_class;
};

FpiUdevDataSpi * fpi_udev_data_spi_new(const gchar* udev_string);
const gchar *    fpi_udev_data_spi_get_path(FpiUdevDataSpi * data);

/* Basic SPI udev detect_spi implementation */

void fpi_detect_udev_spi(GUdevClient *client, size_t device_count, gpointer check_args_in[], GObject *udev_data_out[]);

#endif
