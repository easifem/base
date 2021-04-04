#!/bin/sh
# This is a setup script for installing EASIFEM-base library.
# (c) 2021, Dr Vikas Sharma, all rights reserved
#
#
# Log (dd/mm/yyyy)
# 15/02/2021 this document was created
#
#--------------------------------------------------------------

SHELL_=${SHELL}
if [[ $SHELL_ =~ .*zsh.* ]]; then
  BP=${HOME}/.zshrc
fi
if [[ $SHELL_ =~ .*bash.* ]]; then
  BP=${HOME}/.bashrc
fi
echo $BP

if [ -f "$BP" ]; then
  echo "${BP} found"
else
  touch ${BP}
  echo '#!/bin/sh' >> ${BP}
fi

ERC=${HOME}/.easifemrc
if [ -f "$ERC" ]; then
    echo "${ERC} found, removing it"
    rm -rf ${ERC}
    touch ${ERC}
    echo '#!/bin/sh' >> ${ERC}
else
  touch ${ERC}
  echo '#!/bin/sh' >> ${ERC}
fi

prefix=${HOME}
echo "easifem_prefix=${prefix}" >> ${ERC}

echo 'export EASIFEM_BASE=${prefix}/.easifem/base/' >> ${ERC}
echo "export EASIFEM_EXTPKGS=${prefix}/.easifem/extpkgs/" >> ${ERC}
mkdir -p ${EASIFEM_EXTPKGS}
mkdir -p ${EASIFEM_BASE}
echo "mkdir -p ${EASIFEM_EXTPKGS}" >> ${ERC}
echo "mkdir -p ${EASIFEM_BASE}" >> ${ERC}
# export EASIFEM_CLASSES=${prefix}/.easifem/classes/
# export EASIFEM_KERNEL=${prefix}/.easifem/kernel/
echo "export CC=/usr/local/bin/gcc-10" >> ${ERC}
echo "export CXX=/usr/local/bin/g++-10" >> ${ERC}
echo "export CPP=/usr/local/bin/cpp-10" >> ${ERC}
echo "export LD=/usr/local/bin/gcc-10" >> ${ERC}
echo "export FC=/usr/local/bin/gfortran-10" >> ${ERC}
echo "alias c++=/usr/local/bin/c++-10" >> ${ERC}
echo "alias g++=/usr/local/bin/g++-10" >> ${ERC}
echo "alias gcc=/usr/local/bin/gcc-10" >> ${ERC}
echo "alias cpp=/usr/local/bin/cpp-10" >> ${ERC}
echo "alias ld=/usr/local/bin/gcc-10" >> ${ERC}
echo "alias cc=/usr/local/bin/gcc-10" >> ${ERC}
echo "alias gfortran=/usr/local/bin/gfortran-10" >> ${ERC}
echo 'source ~/.easifemrc' >> ${BP}
source ${BP}