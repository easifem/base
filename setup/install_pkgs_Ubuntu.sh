#!/bin/sh
# This is a setup script for installing EASIFEM-base library.
# (c) 2021, Dr Vikas Sharma, all rights reserved
#
#
# Log (dd/mm/yyyy)
# 15/02/2021 this document was created
#
#--------------------------------------------------------------

echo "=========================================="
echo "running: apt update"
sudo apt update
echo "=========================================="
echo "running: apt upgrade"
sudo apt upgrade
echo "=========================================="
echo "running: apt install -y curl"
sudo apt install -y curl
echo "=========================================="
echo "running: apt install -y git"
sudo apt install -y git
echo "=========================================="
echo "running: apt install -y gcc-10"
sudo apt install -y gcc-10
echo "=========================================="
echo "running: apt install -y gfortran-10"
sudo apt install -y gfortran-10
echo "=========================================="
echo "running: apt install -y python3"
sudo apt install -y python3
echo "=========================================="
echo "running: apt install -y python3-pip"
sudo apt install -y python3-pip
echo "=========================================="
echo "running: apt install -y liblapack-dev"
sudo apt install -y liblapack-dev
echo "=========================================="
echo "running: apt install -y cmake"
sudo apt install -y cmake
echo "=========================================="
echo "running: apt install -y gmsh"
sudo apt install -y gmsh
echo "=========================================="
echo "running: apt install -y cmake"
sudo apt install -y gnuplot
echo "=========================================="
