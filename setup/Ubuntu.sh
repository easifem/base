#!/bin/sh
# This is a setup script for installing EASIFEM-base library.
# (c) 2021, Dr Vikas Sharma, all rights reserved
#
#
# Log (dd/mm/yyyy)
# 15/02/2021 this document was created
#
#--------------------------------------------------------------

echo "running: apt update"
sudo apt update
echo "running: apt upgrade"
sudo apt upgrade
echo "running: apt install -y curl"
sudo apt install -y curl
echo "running: apt install -y git"
sudo apt install -y git
echo "running: apt install -y gcc-10"
sudo apt install -y gcc-10
echo "running: apt install -y gfortran-10"
sudo apt install -y gfortran-10
echo "running: apt install -y python3"
sudo apt install -y python3
echo "running: apt install -y python3-pip"
sudo apt install -y python3-pip
echo "running: apt install -y liblapack-dev"
sudo apt install -y liblapack-dev
echo "running: apt install -y cmake"
sudo apt install -y cmake
echo "running: apt install -y gmsh"
sudo apt install -y gmsh
echo "running: apt install -y cmake"
sudo apt install -y gnuplot

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

BP=~/.bashrc
if [-f "$BP"]; then
    echo "${BP} found"
else
  touch ${BP}
  echo '#!/bin/sh' >> ${BP}
fi

ERC=~/.easifemrc
if [-f "$ERC"]; then
    echo "${ERC} found"
else
  touch ${ERC}
  echo '#!/bin/sh' >> ${ERC}
fi

read -p "Where do you want to install easifemBase? : " prefix

echo `easifem_prefix=${prefix}` >> ${ERC}
echo `export EASIFEM_BASE=${prefix}/.easifem/base/` >> ${ERC}
echo `export EASIFEM_EXTPKGS=${prefix}/.easifem/extpkgs/` >> ${ERC}
echo `mkdir -p ${EASIFEM_EXTPKGS}` >> ${ERC}
echo `mkdir -p ${EASIFEM_BASE}` >> ${ERC}
# export EASIFEM_CLASSES=${prefix}/.easifem/classes/
# export EASIFEM_KERNEL=${prefix}/.easifem/kernel/
echo `export CC=/usr/bin/gcc-10` >> ${ERC}
echo `export CXX=/usr/bin/g++-10` >> ${ERC}
echo `export CPP=/usr/bin/cpp-10` >> ${ERC}
echo `export LD=/usr/bin/gcc-10` >> ${ERC}
echo `export FC=/usr/bin/gfortran-10` >> ${ERC}
echo `alias c++=/usr/bin/c++-10` >> ${ERC}
echo `alias g++=/usr/bin/g++-10` >> ${ERC}
echo `alias gcc=/usr/bin/gcc-10` >> ${ERC}
echo `alias cpp=/usr/bin/cpp-10` >> ${ERC}
echo `alias ld=/usr/bin/gcc-10` >> ${ERC}
echo `alias cc=/usr/bin/gcc-10` >> ${ERC}
echo `alias gfortran=/usr/bin/gfortran-10` >> ${ERC}

echo `source ~/.easifemrc` >> ${BP}

source ~/.bashrc