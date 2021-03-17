#!/bin/sh
# This is a setup script for installing EASIFEM-base library.
# (c) 2021, Dr Vikas Sharma, all rights reserved
#
#
# Log (dd/mm/yyyy)
# 15/02/2021 this document was created
#
#--------------------------------------------------------------

alias cm="cmake -DCMAKE_INSTALL_PREFIX=${EASIFEM_BASE} -S ./ -B ./build"
alias cmb="cmake --build ./build --target install"
alias cmbt="cmake --build build"
alias cmt="cmake -Bbuild"
alias rt="./build/test"

read -p "path where you want to install easifemBase:: " prefix

export EASIFEM_BASE=${prefix}/.easifem/base/
export EASIFEM_CLASSES=${prefix}/.easifem/classes/
export EASIFEM_EXTPKGS=${prefix}/.easifem/extpkgs/
export EASIFEM_KERNEL=${prefix}/.easifem/kernel/

mkdir -p ${EASIFEM_EXTPKGS}
mkdir -p ${EASIFEM_BASE}

echo "running: brew install gcc"
brew install gcc
# brew install  openmpi
echo "brew install  python3"
brew install  python3
echo "brew install gmsh"
brew install gmsh
echo "brew install  gnuplot"
brew install gnuplot
echo "brew install curl"
brew install curl
echo "brew install openblas"
brew install openblas
echo "brew install lapack"
brew install lapack
# curl https://bootstrap.pypa.io/get-pip.py -o ~/get-pip.py
# python3 ~/get-pip.py
echo "pip3 install numpy"
pip3 install numpy
echo "pip3 pip install scipy"
pip3 pip install scipy
echo "pip3 install matplotlib"
pip3 install matplotlib
echo "pip3 install jupyter"
pip3 install jupyter
echo "pip3 install jupyterlab"
pip3 install jupyterlab
echo "pip3 install plotly"
pip3 install plotly
echo "pip3 install dash"
pip3 install dash
# pip3 install pillow
# pip3 install opencv-python
