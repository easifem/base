# This program is a part of EASIFEM library.
# See. www.easifem.com
# Copyright (c) 2020-2021, All right reserved, Vikas Sharma, Ph.D.
#

import os
import sys
import platform


def installRequest(LIB):
    while True:
        choice = input(f"Do you want to Install {LIB} 'yes' or 'no' [Y/n]: ").lower()
        if choice in ['Y', 'y', 'ye', 'yes']:
          return True
        else:
          return False

def getOption(key, opt):
  while True:
    separator = ', '
    return input( f"select option for {key}, possible options are : {separator.join(opt)} : ") + " "

print("Detecting OS type...")
_os = platform.system()
if _os == 'Windows':
    print("ERROR: INSTALLATION on windows is work in progress")
    exit
    #print("Please use Windows Subsystem Linux(WSL) ")
    #print("Installation DONE!!")
else:
    if(installRequest("OpenBLAS")):
      cwd = os.getcwd()
      os.chdir(os.getenv('EASIFEM_EXTPKGS'))
      os.system(f"git clone --branch develop https://github.com/xianyi/OpenBLAS.git")
      os.chdir(os.getenv('EASIFEM_EXTPKGS') + "/OpenBLAS")
      openblas_def = "-S ./ -B build -DCMAKE_INSTALL_PREFIX=${EASIFEM_EXTPKGS} -DBUILD_WITHOUT_LAPACK=OFF -DBUILD_WITHOUT_CBLAS=ON -DBUILD_SHARED_LIBS=ON -DCMAKE_BUILD_TYPE=Release -DNOFORTRAN=OFF"
      os.system( f"cmake {openblas_def}")
      os.chdir(cwd)

    cmake_def = ""
    opt = getOption("USE_OpenMP", ["ON", "OFF"] )
    cmake_def += "-DUSE_OpenMP=" + opt

    cmake_def += "-DBUILD_TYPE=" + getOption("BUILD_TYPE", ["Release", "Debug"])

    cmake_def += "-DBUILD_SHARED_LIBS=" + getOption("BUILD_SHARED_LIBS", ["ON", "OFF"])

    cmake_def += " -DCMAKE_INSTALL_PREFIX=${EASIFEM_BASE} "

    cmake_def += " -DUSE_Int32=ON -DUSE_Real64=ON "

    print( "CMAKE DEF : ", cmake_def )

    # os.system( f"cmake -S ./ -B ./build {cmake_def}")
    # os.system(f"cmake --build ./build --target install" )
    print("Installation DONE!!")
