# This program is a part of EASIFEM library.
# See. www.easifem.com
# Copyright (c) 2020-2021, All right reserved, Vikas Sharma, Ph.D.
#

import os
import sys
import platform


def installRequest(LIB):
    while True:
        choice = input(f"Do you want to Install {LIB} 'yes' or 'no' [Y/n]: ")
        if(choice == " "):
          choice = "no"
        else:
          choice = choice.lower()
        if choice in ['Y', 'y', 'ye', 'yes']:
          return True
        else:
          return False


def getOption(key, opt):
  while True:
    separator = ', '
    return input(f"select option for {key}, possible options are : {separator.join(opt)} : ") + " "


print("Detecting OS type...")
_os = platform.system()
if _os == 'Windows':
    print("ERROR: INSTALLATION on windows is work in progress")
    exit
    #print("Please use Windows Subsystem Linux(WSL) ")
    #print("Installation DONE!!")
else:

    cmake_def = ""
    cmake_def += " -DUSE_METIS=ON"
    user_query = False
    if user_query:

      opt = getOption("CMAKE_GENERATOR", [
                      "Unix Makefiles", "Ninja", "Ninja Multi-Config"])
      if(opt == " "):
          # opt = '"Unix Makefiles"'
          opt = '"Ninja"'
      cmake_def += " -G " + opt

      opt = getOption("USE_PLPLOT", ["ON", "OFF"])
      if(opt == " "):
          opt = "ON"
      cmake_def += " -DUSE_PLPLOT=" + opt

      opt = getOption("USE_BLAS95", ["ON", "OFF"])
      if(opt == " "):
          opt = "ON"
      cmake_def += " -DUSE_BLAS95=" + opt

      opt = getOption("USE_LAPACK95", ["ON", "OFF"])
      if(opt == " "):
          opt = "ON"
      cmake_def += " -DUSE_LAPACK95=" + opt

      opt = getOption("USE_OpenMP", ["ON", "OFF"])
      if(opt == " "):
          opt = "ON"
      cmake_def += " -DUSE_OpenMP=" + opt

      opt = getOption("CMAKE_BUILD_TYPE", ["Release", "Debug"])
      if(opt == " "):
          opt = "Release"
      cmake_def += " -DCMAKE_BUILD_TYPE=" + opt

      opt = getOption("BUILD_SHARED_LIBS", ["ON", "OFF"])
      if(opt == " "):
          opt = "ON"
      cmake_def += " -DBUILD_SHARED_LIBS=" + opt

      opt = getOption("CMAKE_INSTALL_PREFIX", ["${PREFIX}"])
      if(opt == " "):
          opt = "${EASIFEM_BASE}"
      cmake_def += " -DCMAKE_INSTALL_PREFIX=" + opt
    else:
      cmake_def += ' -G "Ninja" -DUSE_OpenMP:BOOL=ON -DCMAKE_BUILD_TYPE:STRING=Release -DBUILD_SHARED_LIBS:BOOL=ON -DUSE_PLPLOT:BOOL=ON -DCMAKE_INSTALL_PREFIX:PATH=${EASIFEM_BASE} -DUSE_BLAS95:BOOL=ON -DUSE_LAPACK95:BOOL=ON'

    cmake_def += " -DUSE_Int32=ON -DUSE_Real64=ON"

    print("CMAKE DEF : ", cmake_def)
    os.system(f"cmake -S ./ -B ~/temp/easifem-base/build {cmake_def}")
    os.system(f"cmake --build ~/temp/easifem-base/build --target install")
    print("Installation DONE!!")
