#!/bin/sh
# This is a setup script for installing EASIFEM-base library.
# (c) 2021, Dr Vikas Sharma, all rights reserved
#
#
# Log (dd/mm/yyyy)
# 15/02/2021 this document was created
#
#--------------------------------------------------------------

read -p "path where you want to install easifemBase:: " prefix
EASIFEM_BASE_ARCH=${prefix}/.easifem/easifem-base
EASIFEM_BASE_INCLUDE=${EASIFEM_BASE_ARCH}/include
EASIFEM_BASE_LIB=${EASIFEM_BASE_ARCH}/lib

if [[ -d "${EASIFEM_BASE_ARCH}" ]]
then
  echo "${EASIFEM_BASE_ARCH}" " directory already exists"
else
  mkdir -p ${EASIFEM_BASE_ARCH}
fi

if [[ -d "${EASIFEM_BASE_INCLUDE}" ]]
then
  echo "${EASIFEM_BASE_INCLUDE}" " directory already exists"
else
  mkdir -p ${EASIFEM_BASE_INCLUDE}
fi

if [[ -d "${EASIFEM_BASE_LIB}" ]]
then
  echo "${EASIFEM_BASE_LIB}" " directory already exists"
else
  mkdir -p ${EASIFEM_BASE_LIB}
fi


# install HDF5 library if not installed