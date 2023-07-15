# This program is a part of EASIFEM library.
# See. www.easifem.com
# Copyright (c) 2020-2021, All right reserved, Vikas Sharma, Ph.D.
#

import os
import platform


def installRequest(LIB):
    while True:
        choice = input(f"Do you want to Install {LIB} 'yes' or 'no' [Y/n]: ")
        if choice == " ":
            choice = "no"
        else:
            choice = choice.lower()
        if choice in ["Y", "y", "ye", "yes"]:
            return True
        else:
            return False


def getOption(key, opt):
    while True:
        separator = ", "
        return (
            input(
                f"select option for {key}, possible options are : {separator.join(opt)} : "
            )
            + " "
        )


print("Detecting OS type...")
_os = platform.system()
if _os == "Windows":
    print("ERROR: INSTALLATION on windows is work in progress")
    exit
    # print("Please use Windows Subsystem Linux(WSL) ")
    # print("Installation DONE!!")
else:

    cmake_def = ""
    user_query = False
    cmake_def = ""
    cmake_def += ' -G "Ninja"'  # Unix Makefiles, Ninja, Ninja Multi-Config
    cmake_def += " -D USE_OpenMP:BOOL=ON"  # OFF
    cmake_def += " -D CMAKE_BUILD_TYPE:STRING=Release"  # Debug
    cmake_def += " -D BUILD_SHARED_LIBS:BOOL=ON"
    cmake_def += " -D USE_PLPLOT:BOOL=ON"
    cmake_def += " -D CMAKE_INSTALL_PREFIX:PATH=${EASIFEM_BASE}"
    cmake_def += " -D USE_BLAS95:BOOL=ON"
    cmake_def += " -D USE_LAPACK95:BOOL=ON"
    cmake_def += " -D USE_FFTW:BOOL=ON"
    cmake_def += " -D USE_GTK:BOOL=OFF"
    cmake_def += " -D USE_ARPACK:BOOL=ON"
    cmake_def += " -D USE_SUPERLU:BOOL=ON"
    cmake_def += " -D USE_LIS:BOOL=ON"
    cmake_def += " -D USE_PARPACK:BOOL=OFF"
    cmake_def += " -D USE_METIS:BOOL=OFF"
    cmake_def += " -D USE_Int32:BOOL=ON"
    cmake_def += " -D USE_Real64:BOOL=ON"
    cmake_def += " -D COLOR_DISP:BOOL=OFF"

    print("CMAKE DEF : ", cmake_def)

    _build0 = os.path.join(os.environ["HOME"], "temp")
    build_dir = os.path.join(
        os.environ.get("EASIFEM_BUILD_DIR", _build0), "easifem", "base", "build"
    )
    # build_dir = os.environ["HOME"] + "/temp/easifem-base/build"
    os.makedirs(build_dir, exist_ok=True)
    os.system(f"cmake -S ./ -B {build_dir} {cmake_def}")
    os.system(f"cmake --build {build_dir} --target install")
    print("Installation DONE!!")
