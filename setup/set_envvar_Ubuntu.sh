#!/bin/sh
# This is a setup script for installing EASIFEM-base library.
# (c) 2021, Dr Vikas Sharma, all rights reserved
#
#
# Log (dd/mm/yyyy)
# 15/02/2021 this document was created
#
#--------------------------------------------------------------

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
if [[ $SHELL_ =~ .*zsh.* ]]
then
  BP=${HOME}/.zshrc
fi
if [[ $SHELL_ =~ .*bash.* ]]
then
  BP=${HOME}/.bashrc
fi

if [ -f "${BP}" ]
then
  echo "${BP} found"
else
  touch ${BP}
  echo '#!/bin/sh' >> ${BP}
fi

ERC=${HOME}/.easifemrc
if [ -f "$ERC" ]
then
    echo "${ERC} found, removing it"
    rm -rf ${ERC}
    touch ${ERC}
    echo '#!/bin/sh' >> ${ERC}
else
  touch ${ERC}
  echo '#!/bin/sh' >> ${ERC}
fi

prefix=${HOME}
EASIFEM_BASE=${prefix}/.easifem/base
EASIFEM_EXTPKGS=${prefix}/.easifem/extpkgs
EASIFEM_CLASSES=${prefix}/.easifem/classes
EASIFEM_MATERIALS=${prefix}/.easifem/materials
EASIFEM_KERNELS=${prefix}/.easifem/kernels

echo "export EASIFEM_BASE=${prefix}/.easifem/base" >> ${ERC}
echo "export EASIFEM_EXTPKGS=${prefix}/.easifem/extpkgs" >> ${ERC}
echo "export EASIFEM_CLASSES=${prefix}/.easifem/classes" >> ${ERC}
echo "export EASIFEM_MATERIALS=${prefix}/.easifem/materials" >> ${ERC}
echo "export EASIFEM_KERNELS=${prefix}/.easifem/kernels" >> ${ERC}

echo "mkdir -p ${EASIFEM_EXTPKGS}" >> ${ERC}
echo "mkdir -p ${EASIFEM_BASE}" >> ${ERC}
echo "mkdir -p ${EASIFEM_CLASSES}" >> ${ERC}
echo "mkdir -p ${EASIFEM_KERNELS}" >> ${ERC}

echo "source ${ERC}" >> ${BP}
source ${BP}
