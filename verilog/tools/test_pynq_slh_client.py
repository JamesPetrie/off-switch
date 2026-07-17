#!/usr/bin/env python3
"""Host-side unit test for the PYNQ SLH-DSA client protocol."""

from __future__ import annotations

import importlib.util
from pathlib import Path


REPOSITORY = Path(__file__).resolve().parents[2]
CLIENT_PATH = REPOSITORY / "pynq-z2" / "off_switch_client.py"
SPEC = importlib.util.spec_from_file_location("off_switch_client", CLIENT_PATH)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError(f"cannot import {CLIENT_PATH}")
CLIENT = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CLIENT)


class FakeAxiIp:
    def __init__(self) -> None:
        self.status = (
            (CLIENT.CRYPTO_SLH_DSA << CLIENT.STATUS_MODE_SHIFT)
            | CLIENT.STATUS_SLOT_READY
        )
        self.low = 0
        self.high = 0
        self.count = 0
        self.received = bytearray()

    def read(self, address: int) -> int:
        if address == CLIENT.REG_STATUS:
            return self.status
        if address == CLIENT.REG_SIG_COUNT:
            return self.count
        return 0

    def write(self, address: int, value: int) -> None:
        if address == CLIENT.REG_CONTROL:
            if value & 1:
                self.status &= ~(
                    CLIENT.STATUS_DONE
                    | CLIENT.STATUS_PASSED
                    | CLIENT.STATUS_OVERFLOW
                )
                self.count = 0
                self.received.clear()
        elif address == CLIENT.REG_SIG_LO:
            self.low = value
        elif address == CLIENT.REG_SIG_HI:
            self.high = value
        elif address == CLIENT.REG_SIG_PUSH:
            if not value & 1:
                return
            word = self.low | (self.high << 32)
            self.received.extend(word.to_bytes(8, "little"))
            self.count += 1
            if value & 2:
                self.status |= CLIENT.STATUS_DONE | CLIENT.STATUS_PASSED


def main() -> int:
    nonce = int.from_bytes(bytes(range(32)), "big")
    message = CLIENT.canonical_slh_message(nonce)
    expected_message = (
        CLIENT.OFFSWITCH_DOMAIN
        + CLIENT.OFFSWITCH_DEVICE_ID
        + bytes(range(32))
        + CLIENT.OFFSWITCH_POLICY_EPOCH.to_bytes(8, "big")
    )
    if message != expected_message or len(message) != 72:
        raise RuntimeError("canonical PYNQ message construction mismatch")
    print("PASS [PYNQ client canonical 72-byte message]")

    signature = bytes(index & 0xFF for index in range(CLIENT.SLH_SIGNATURE_BYTES))
    fake_ip = FakeAxiIp()
    if not CLIENT.stream_slh_signature(fake_ip, signature):
        raise RuntimeError("fake FPGA did not report a passing signature")
    if bytes(fake_ip.received) != signature:
        raise RuntimeError("client changed signature byte order")
    if fake_ip.count != CLIENT.SLH_SIGNATURE_WORDS:
        raise RuntimeError("client emitted the wrong signature word count")
    print("PASS [PYNQ client streams 7,856 bytes in exact order]")
    print("All 2 PYNQ SLH client protocol tests passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
