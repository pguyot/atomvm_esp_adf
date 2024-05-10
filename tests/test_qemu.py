# SPDX-License-Identifier: MIT

from pytest_embedded import Dut
import pytest
import os
import shutil
from esptool import merge_bin
from collections import namedtuple


def create_flash_image(target):
    path = "flash_image.bin"
    if target == "esp32":
        bootloader = 0x1000
    else:
        bootloader = 0
    args = namedtuple(
        "args",
        "chip addr_filename format output fill_flash_size flash_mode flash_size flash_freq target_offset",
    )(
        **{
            "chip": target,
            "addr_filename": [
                (bootloader, open("../../../build/bootloader/bootloader.bin", "rb")),
                (
                    0x8000,
                    open("../../../build/partition_table/partition-table.bin", "rb"),
                ),
                (0x10000, open("../../../build/atomvm-esp32.bin", "rb")),
                (
                    0x1D0000,
                    open("../../../../../../build/libs/esp32boot/esp32boot.avm", "rb"),
                ),
                (0x210000, open("_build/default/lib/tests.avm", "rb")),
            ],
            "format": "raw",
            "output": path,
            "fill_flash_size": "4MB",
            "flash_mode": "dio",
            "flash_size": "4MB",
            "flash_freq": "keep",
            "target_offset": 0,
        }
    )
    merge_bin(args)


def pytest_generate_tests(metafunc):
    option_value = metafunc.config.option.target
    if option_value is None or option_value == "esp32":
        create_flash_image("esp32")
        metafunc.parametrize(
            "embedded_services, qemu_image_path, qemu_extra_args",
            [("qemu", "flash_image.bin", "-nic user,model=open_eth")],
            indirect=True,
        )
    if option_value == "esp32c3":
        create_flash_image("esp32c3")
        metafunc.parametrize(
            "embedded_services, qemu_prog_path, qemu_cli_args, qemu_image_path, qemu_extra_args",
            [
                (
                    "qemu",
                    shutil.which("qemu-system-riscv32"),
                    "-nographic -machine esp32c3",
                    "flash_image.bin",
                    "-nic user,model=open_eth -drive file=../../../test/qemu_esp32c3_efuse.bin,if=none,format=raw,id=efuse -global driver=nvram.esp32c3.efuse,property=drive,value=efuse",
                )
            ],
            indirect=True,
        )


def test_atomvm_esp_adf(dut, redirect):
    dut.expect_unity_test_output(timeout=40)
    assert len(dut.testsuite.testcases) > 0
    assert dut.testsuite.attrs["failures"] == 0
    assert dut.testsuite.attrs["errors"] == 0
