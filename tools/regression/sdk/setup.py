import os

required = ["requests"]


def import_or_install(package):
    try:
        __import__(package)
    except ImportError:
        os.system("python3 -m pip install " + package)


for package in required:
    import_or_install(package)
