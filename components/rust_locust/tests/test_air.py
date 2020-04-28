import gc
import json
import os
import platform

import psutil
import pytest

from rust_locust.air import AIR

PYPY = platform.python_implementation() == "PyPy"


def test_air_basic():
    air = AIR()
    txn = air.make_assign_txn()
    print(txn)
    txn = air.make_assign_txn()
    print(txn)
