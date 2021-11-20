#!/bin/sh
# This program is a part of EASIFEM library
# # Copyright (C) 2020-2021 Vikas Sharma, Ph.D
# This program is free software: you can redistribute it and/or modify
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>


#sh -c "$(curl -fsSL https://api.cacher.io/raw/41c42c3479fa291be9d8/f7f3a874cad01c19127f/install_pkgs.sh)"

# go to home
cd ${HOME}

# run update and upgrade
echo "[try:] apt update"
sudo apt update
if [ $? -eq 0 ] ; then echo 'apt update [OK!]' ; else echo 'apt update [FAILED!]'; exit ; fi
echo "[try:] apt upgrade"
sudo apt upgrade
if [ $? -eq 0 ] ; then echo 'apt upgrade [OK!]' ; else echo 'apt upgrade [FAILED!]'; exit ; fi
#
#
# Install pkgs here
#
#  sudo apt install -y gmsh
#  sudo apt install -y gnuplot
#
pkg="curl git neovim zsh gfortran gcc g++ python3 python3-pip liblapack-dev libopenblas-dev"
echo "[try:] apt install -y ${pkg}"
sudo apt install -y ${pkg}
if [ $? -eq 0 ] ; then echo "${pkg} install [SUCCESSFUL!]" ; else echo "${pkg} install [FAILED!]"; exit ; fi
#
# Install cmake from pip
#
pip3 install cmake
#
# install spacevim
#
#
#
mkdir -p ${HOME}/.SpaceVim.d
echo "Download init.toml"
url="https://api.cacher.io/raw/5f5dd01fcf10a2d39603/6385a7e389aafd0c99c6/init.toml"
curl -o ${HOME}/.SpaceVim.d/init.toml ${url}
url="https://spacevim.org/install.sh"
#
# oh-my-zsh
#
sudo chsh -s /bin/zsh
url="https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh"
sh -c "$(curl -fsSL ${url})"
