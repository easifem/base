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

read -p "Whee do you want to install easifemBase? : " prefix

export EASIFEM_BASE=${prefix}/.easifem/base/
export EASIFEM_CLASSES=${prefix}/.easifem/classes/
export EASIFEM_EXTPKGS=${prefix}/.easifem/extpkgs/
export EASIFEM_KERNEL=${prefix}/.easifem/kernel/

mkdir -p ${EASIFEM_EXTPKGS}
mkdir -p ${EASIFEM_BASE}

echo "running: apt-get update"
apt update
apt upgrade
echo "running: apt-get install -y curl"
apt install -y curl
apt install -y git
apt install -y gcc
apt install -y gfortran
apt install -y python3
apt install -y python3-pip
apt install -y liblapack-dev
apt install -y cmake
apt install -y openblas-dev
apt install -y libopenmpi-dev
# apt install -y gmsh
# apt install -y gnuplot

# create python virtual environment and install requirement there
# put them in requirements.txt
# curl https://bootstrap.pypa.io/get-pip.py -o ~/get-pip.py
# python3 ~/get-pip.py
# echo "pip3 install numpy"
# pip3 install numpy
# echo "pip3 pip install scipy"
# pip3 pip install scipy
# echo "pip3 install matplotlib"
# pip3 install matplotlib
# echo "pip3 install jupyter"
# pip3 install jupyter
# echo "pip3 install jupyterlab"
# pip3 install jupyterlab
# echo "pip3 install plotly"
# pip3 install plotly
# echo "pip3 install dash"
# pip3 install dash
# pip3 install pillow
# pip3 install opencv-python
