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

#pragma once

#include <config.h>

#ifndef HAVE_UDEV
#error "elanspi requries udev"
#endif

#include <gudev/gudev.h>

/* TODO: get some way of configuring these constants */

#include <glib.h>

#define ELANSPI_TP_VID 0x04f3
#define ELANSPI_TP_PID 0x3057

/* Sensor ID information copied from the windows driver */

struct elanspi_sensor_entry {
	unsigned char sensor_id, height, width, ic_version;
	gboolean is_otp_model;
	const gchar* name;
};

static const struct elanspi_sensor_entry elanspi_sensor_table[] = {
	{0x0, 0x78, 0x78, 0x0, 0x0, "eFSA120S"},
	{0x1, 0x78, 0x78, 0x1, 0x1, "eFSA120SA"},
	{0x2, 0xA0, 0xA0, 0x0, 0x0, "eFSA160S"},
	{0x3, 0xd0, 0x50, 0x0, 0x0, "eFSA820R"},
	{0x4, 0xC0, 0x38, 0x0, 0x0, "eFSA519R"},
	{0x5, 0x60, 0x60, 0x0, 0x0, "eFSA96S"},
	{0x6, 0x60, 0x60, 0x1, 0x1, "eFSA96SA"},
	{0x7, 0x60, 0x60, 0x2, 0x1, "eFSA96SB"},
	{0x8, 0xa0, 0x50, 0x1, 0x1, "eFSA816RA"},
	{0x9, 0x90, 0x40, 0x1, 0x1, "eFSA614RA"},
	{0xA, 0x90, 0x40, 0x2, 0x1, "eFSA614RB"},
	{0xB, 0x40, 0x58, 0x1, 0x1, "eFSA688RA"},
	{0xC, 0x50, 0x50, 0x1, 0x0, "eFSA80SA"},
	{0,   0,    0,    0,   0,   NULL}
};

struct elanspi_reg_entry {
	unsigned char addr, value;
	/* terminates with 0xFF, 0xFF since register 0x0 is valid */
};

struct elanspi_regtable {
	const struct elanspi_reg_entry *other;
	struct {
		unsigned char sid;
		const struct elanspi_reg_entry *table;
	} entries[];
};

static const struct elanspi_reg_entry elanspi_calibration_table_default[] = {
	{0x05, 0x60},
	{0x06, 0xc0},
	{0x07, 0x80},
	{0x08, 0x04},
	{0x0a, 0x97},
	{0x0b, 0x72},
	{0x0c, 0x69},
	{0x0f, 0x2a},
	{0x11, 0x2a},
	{0x13, 0x27},
	{0x15, 0x67},
	{0x18, 0x04},
	{0x21, 0x20},
	{0x22, 0x36},
	{0x2a, 0x5f},
	{0x2b, 0xc0},
	{0x2e, 0xff},

	{0xff, 0xff}
};

static const struct elanspi_reg_entry elanspi_calibration_table_id567[] = {
	{0x2A, 0x07},
	{0x5,  0x60},
	{0x6,  0xC0},
	{0x7,  0x80},
	{0x8,  0x04},
	{0xA,  0x97},
	{0xB,  0x72},
	{0xC,  0x69},
	{0xF,  0x2A},
	{0x11, 0x2A},
	{0x13, 0x27},
	{0x15, 0x67},
	{0x18, 0x04},
	{0x21, 0x20},
	{0x22, 0x36},
	{0x2A, 0x5F},
	{0x2B, 0xC0},
	{0x2E, 0xFF},

	{0xff, 0xff}
};

static const struct elanspi_reg_entry elanspi_calibration_table_id0[] = {
	{0x5,  0x60},
	{0x6,  0xC0},
	{0x8,  0x04},
	{0xA,  0x97},
	{0xB,  0x72},
	{0xC,  0x69},
	{0xF,  0x2B},
	{0x11, 0x2B},
	{0x13, 0x28},
	{0x15, 0x28},
	{0x18, 0x04},
	{0x21, 0x20},
	{0x2A, 0x4B},

	{0xff, 0xff}
};

static const struct elanspi_regtable elanspi_calibration_table = {
	.other = elanspi_calibration_table_default,
	.entries = {
		{ .sid = 0x0, .table = elanspi_calibration_table_id0 },
		{ .sid = 0x5, .table = elanspi_calibration_table_id567 },
		{ .sid = 0x6, .table = elanspi_calibration_table_id567 },
		{ .sid = 0x7, .table = elanspi_calibration_table_id567 },
		{ .sid = 0x0, .table = NULL }
	}
};

#define ELANSPI_DEV_HIDONLY 1

GObject * elanspi_udev_check_acpi_hid(GUdevClient *client, const void* arg);

/* ACPI ids from the windows driver INF file (ELAN7002 untested) */
static const FpIdEntry elanspi_id_table[] = {
	{.checkhook = elanspi_udev_check_acpi_hid, .checkarg = "ELAN7001", .driver_data = ELANSPI_DEV_HIDONLY},
	{.checkhook = elanspi_udev_check_acpi_hid, .checkarg = "ELAN7002", .driver_data = ELANSPI_DEV_HIDONLY},
	{.checkhook = NULL, .checkarg = NULL, .driver_data = 0}
};

/* constats relating to detecting finger presence */
#define ELANSPI_MAX_REAL_STDDEV 850*850
#define ELANSPI_MIN_EMPTY_STDDEV 2200*2200

#define ELANSPI_MAX_REAL_DISTAVG 200
#define ELANSPI_MIN_EMPTY_DISTAVG 1100

#define ELANSPI_MIN_FRAMES_DEBOUNCE 6
#define ELANSPI_SWIPE_FRAMES_DISCARD 3
#define ELANSPI_MIN_FRAMES_SWIPE (7+ELANSPI_SWIPE_FRAMES_DISCARD)
#define ELANSPI_MAX_FRAMES_SWIPE (25+ELANSPI_SWIPE_FRAMES_DISCARD)

/* calibration constants from the windows driver */
#define ELANSPI_MAX_STAGE1_CALIBRATION_MEAN 1000

#define ELANSPI_MIN_STAGE2_CALBIRATION_MEAN 3000
#define ELANSPI_MAX_STAGE2_CALBIRATION_MEAN 8000

#define ELANSPI_MAX_STAGE2_CALIBRATION_ATTEMPTS 2

/* bg rejection percent max */
#define ELANSPI_BGREJECT_PERCENT_MAX 60

#define ELANSPI_MAX_FRAME_HEIGHT 43

