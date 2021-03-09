
## some alias for me
alias o=open
alias o=open -a
alias g=open -a gmsh
alias c=clear
## added by vikas;
export PATH=/usr/local/bin/:$PATH
export PATH=~/bin/:$PATH
export PATH=/Applications/gmsh/4.5.6/Gmsh.app/Contents/MacOS:$PATH
export bp=~/.bash_profile
export br=~/.bash_profile
export zrc=~/.zshrc
export dropbox=~/Dropbox
export EASIFEM_BASE=${HOME}/.easifem/base/
export EASIFEM_CLASSES=${HOME}/.easifem/classes/
export EASIFEM_EXTPKGS=${HOME}/.easifem/extpkgs/
export EASIFEM_KERNEL=${HOME}/.easifem/kernel/
# export CMAKE_BLAS_ROOT
# export CMAKE_LAPACK_ROOT
alias cm="cmake -DCMAKE_INSTALL_PREFIX=${EASIFEM_BASE} -S ./ -B ./build"
alias cmb="cmake --build ./build --target install"
alias cmbt="cmake --build build"
alias cmt="cmake -Bbuild"
alias rt="./build/test"

export PKG_CONFIG_PATH="${EASIFEM_EXTPKGS}/lib/pkgconfig:${PKG_CONFIG_PATH}"

# related to petsc
# export PETSC_DIR=~/opt/petsc
# export PETSC_ARCH="gnu-serial-debug"
# export PETSC_ARCH="gnu-mpich-basic"
# alias petsc_mpi=$PETSC_DIR/$PETSC_ARCH/bin/mpiexec
