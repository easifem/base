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
    if user_query:
        ###################################
        opt = getOption(
            "CMAKE_GENERATOR", ["Unix Makefiles", "Ninja", "Ninja Multi-Config"]
        )
        if opt == " ":
            # opt = '"Unix Makefiles"'
            opt = '"Ninja"'
        cmake_def += " -G " + opt
        ###################################
        opt = getOption("USE_PLPLOT", ["ON", "OFF"])
        if opt == " ":
            opt = "ON"
        cmake_def += " -DUSE_PLPLOT=" + opt
        ###################################
        opt = getOption("USE_BLAS95", ["ON", "OFF"])
        if opt == " ":
            opt = "ON"
        cmake_def += " -DUSE_BLAS95=" + opt
        ###################################
        opt = getOption("USE_LAPACK95", ["ON", "OFF"])
        if opt == " ":
            opt = "ON"
        cmake_def += " -DUSE_LAPACK95=" + opt
        ###################################
        opt = getOption("USE_ARPACK", ["ON", "OFF"])
        if opt == " ":
            opt = "ON"
        cmake_def += " -DUSE_ARPACK=" + opt
        ###################################
        opt = getOption("USE_PARPACK", ["ON", "OFF"])
        if opt == " ":
            opt = "ON"
        cmake_def += " -DUSE_PARPACK=" + opt
        ###################################
        opt = getOption("USE_OpenMP", ["ON", "OFF"])
        if opt == " ":
            opt = "ON"
        cmake_def += " -DUSE_OpenMP=" + opt
        ###################################
        opt = getOption("USE_FFTW", ["ON", "OFF"])
        if opt == " ":
            opt = "ON"
        cmake_def += " -DUSE_FFTW=" + opt
        ###################################
        opt = getOption("USE_METIS", ["ON", "OFF"])
        if opt == " ":
            opt = "ON"
        cmake_def += " -DUSE_METIS=" + opt
        ###################################
        opt = getOption("USE_SUPERLU", ["ON", "OFF"])
        if opt == " ":
            opt = "ON"
        cmake_def += " -DUSE_SUPERLU=" + opt
        ###################################
        opt = getOption("USE_LIS", ["ON", "OFF"])
        if opt == " ":
            opt = "ON"
        cmake_def += " -DUSE_LIS=" + opt
        ###################################
        opt = getOption("USE_GTK4", ["ON", "OFF"])
        if opt == " ":
            opt = "ON"
        cmake_def += " -DUSE_GTK4=" + opt
        ###################################
        opt = getOption("CMAKE_BUILD_TYPE", ["Release", "Debug"])
        if opt == " ":
            opt = "Release"
        cmake_def += " -DCMAKE_BUILD_TYPE=" + opt
        ###################################
        opt = getOption("BUILD_SHARED_LIBS", ["ON", "OFF"])
        if opt == " ":
            opt = "ON"
        cmake_def += " -DBUILD_SHARED_LIBS=" + opt
        ###################################
        opt = getOption("CMAKE_INSTALL_PREFIX", ["${PREFIX}"])
        if opt == " ":
            opt = "${EASIFEM_BASE}"
        cmake_def += " -DCMAKE_INSTALL_PREFIX=" + opt
    else:
        cmake_def = ""
        cmake_def += ' -G "Ninja"'
        cmake_def += " -D USE_OpenMP:BOOL=ON"
        cmake_def += " -D CMAKE_BUILD_TYPE:STRING=Release"
        cmake_def += " -D BUILD_SHARED_LIBS:BOOL=ON"
        cmake_def += " -D USE_PLPLOT:BOOL=ON"
        cmake_def += " -D CMAKE_INSTALL_PREFIX:PATH=${EASIFEM_BASE}"
        cmake_def += " -D USE_BLAS95:BOOL=ON"
        cmake_def += " -D USE_LAPACK95:BOOL=ON"
        cmake_def += " -D USE_FFTW:BOOL=ON"
        cmake_def += " -D USE_GTK:BOOL=ON"
        cmake_def += " -D USE_ARPACK:BOOL=ON"
        cmake_def += " -D USE_SUPERLU:BOOL=ON"
        cmake_def += " -D USE_LIS:BOOL=ON"
        cmake_def += " -D USE_PARPACK:BOOL=OFF"
        cmake_def += " -D USE_METIS:BOOL=OFF"

    cmake_def += " -D USE_Int32=ON -D USE_Real64=ON"

    print("CMAKE DEF : ", cmake_def)
    build_dir = os.environ["HOME"] + "/temp/easifem-base/build"
    os.makedirs(build_dir, exist_ok=True)
    os.system(f"cmake -S ./ -B {build_dir} {cmake_def}")
    os.system(f"cmake --build {build_dir} --target package")
    print("Installation DONE!!")
