#!/usr/bin/env python3
#!/Users/easifem/anaconda3/envs/easifem/bin/python3

# This program is a part of EASIFEM library.
# See. www.easifem.com
# Copyright (c) 2020-2021, All right reserved, Vikas Sharma, Ph.D.
#

import os
import platform

print("Detecting OS type...")
_os = platform.system()
if _os == "Windows":
    print("ERROR: INSTALLATION on windows is work in progress")
    exit
    # print("Please use Windows Subsystem Linux(WSL) ")
    # print("Installation DONE!!")
else:
    cmake_def = ""
    cmake_def += ' -G "Ninja"'  # Unix Makefiles, Ninja, Ninja Multi-Config
    cmake_def += " -D USE_OPENMP:BOOL=ON"  # OFF
    cmake_def += " -D CMAKE_BUILD_TYPE:STRING=Debug"  # Release
    cmake_def += " -D CMAKE_INSTALL_PREFIX:PATH=${EASIFEM_BASE}"
    cmake_def += " -D CMAKE_EXPORT_COMPILE_COMMANDS:BOOL=ON"
    cmake_def += " -D BUILD_SHARED_LIBS:BOOL=ON"
    cmake_def += " -D USE_PLPLOT:BOOL=ON"
    cmake_def += " -D USE_BLAS95:BOOL=ON"
    cmake_def += " -D USE_LAPACK95:BOOL=ON"
    cmake_def += " -D USE_FFTW:BOOL=ON"
    cmake_def += " -D USE_GTK:BOOL=OFF"
    cmake_def += " -D USE_ARPACK:BOOL=ON"
    cmake_def += " -D USE_SUPERLU:BOOL=ON"
    cmake_def += " -D USE_LIS:BOOL=ON"
    cmake_def += " -D USE_PARPACK:BOOL=OFF"
    cmake_def += " -D USE_METIS:BOOL=OFF"
    cmake_def += " -D USE_LUA:BOOL=ON"
    cmake_def += " -D USE_INT32:BOOL=ON"
    cmake_def += " -D USE_REAL64:BOOL=ON"
    cmake_def += " -D USE_RAYLIB:BOOL=ON"
    cmake_def += " -D USE_COLORDISP:BOOL=OFF"

    print("CMAKE DEF : ", cmake_def)

    _build0 = os.path.join(os.environ["HOME"], "temp")
    build_dir = os.path.join(
        os.environ.get("EASIFEM_BUILD_DIR", _build0), "easifem", "base", "build"
    )
    # build_dir = os.environ["HOME"] + "/temp/easifem-base/build"
    os.makedirs(build_dir, exist_ok=True)
    os.system(f"cmake {cmake_def} -S ./ -B {build_dir}")
    os.system(f"cmake --build {build_dir}")
    print("Build DONE!!")
