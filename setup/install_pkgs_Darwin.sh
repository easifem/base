#!/bin/sh
# This is a setup script for installing EASIFEM-base library.
# (c) 2021, Dr Vikas Sharma, all rights reserved
#
#
# Log (dd/mm/yyyy)
# 15/02/2021 this document was created
#
#--------------------------------------------------------------

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

echo "=========================================="
echo "brew install curl"
brew install curl
echo "=========================================="
echo "brew install git"
brew install git
echo "=========================================="
echo "brew install gcc"
brew install gcc
echo "=========================================="
echo "brew install gfortran"
brew install gfortran
echo "=========================================="
echo "brew install python3"
brew install python3
echo "=========================================="
echo "brew install lapack"
brew install lapack
echo "=========================================="
echo "brew install cmake"
brew install cmake
echo "=========================================="
echo "brew install gmsh"
brew install gmsh
echo "=========================================="
echo "brew install gnuplot"
brew install gnuplot
echo "=========================================="
