# Installation of easifemBase on Linux

## Ubuntu

### System requirements

Then download the requirements by copying following code and paste it in terminal.

```bash
sudo apt-get update && sudo apt-get install -y gfortran gcc libomp-dev curl git \
python3 python3-pip cmake ninja-build \
liblapack-dev libopenblas-dev libhdf5-dev \
libplplot-dev plplot-driver-cairo libboost-all-dev \
gnuplot doxygen libgtk-4-dev
```

### Install easifem CLI

The easiest way and the recommended way to install the components of easifem  is through `easifem` command line interface.

```bash
python3 -m pip install --upgrade easifem
```

### Set environment variables

After downloading the easifem CLI, we need to set three environment variables related to the location of the source files, build files, and installation of the easifem.

You can read about the environment variables [here](./Environment.md)

```bash
easifem setenv --install ~/.easifem/install --build ~/.easifem/build --source ~/.easifem/src
```

### Install External packages

```bash
easifem install extpkgs
```

You can also install individual package by using following:

```bash
easifem install openblas superlu lis metis scotch arpack fftw gtk-fortran lapack95 sparsekit gmsh
```

- The packages will be stored at `EASIFEM_SOURCE_DIR/extpkgs/<pkg-name>`
- The packages will be build at `EASIFEM_BUILD_DIR/extpkgs/<pkg-name>`
- The packages will be installed at `EASIFEM_INSTALL_DIR/extpkgs/<pkg-name>`

### Install easifemBase

```bash
easifem install base
```

### Installation by using CMake

Download the source code:

```bash
git clone https://github.com/vickysharma0812/easifem-base.git
```

or

```bash
git clone git@github.com:vickysharma0812/easifem-base.git
```

or

```bash
gh repo clone vickysharma0812/easifem-base
```

After downloading the source code, enter the source directory, and make a build directory.

```bash
cd easifem-base
mkdir ./build
```

EASIFEM uses CMake build system. You can install the Base library from CMake by using following steps

1. Configuration
2. Build
3. Install

To configure the `Base` library you can define following variables:

| Variable | Type | Options |
| --- | --- | --- |
| USE_OpenMP | BOOL | `ON`, `OFF` |
| CMAKE_BUILD_TYPE | STRING | `Release`, `Debug` |
| BUILD_SHARED_LIBS | BOOL | `ON`, `OFF` |
| USE_PLPLOT | BOOL | `ON`, `OFF`|
| CMAKE_INSTALL_PREFIX | PATH | |
| USE_BLAS95 | BOOL | `ON`, `OFF`|
| USE_LAPACK95 | BOOL | `ON`, `OFF`|
| USE_FFTW | BOOL | `ON`, `OFF`|
| USE_GTK | BOOL | `ON`, `OFF`|
| USE_ARPACK | BOOL | `ON`, `OFF`|
| USE_SUPERLU | BOOL | `ON`, `OFF`|
| USE_LIS | BOOL | `ON`, `OFF`|
| USE_PARPACK | BOOL | `ON`, `OFF`|
| USE_METIS | BOOL | `ON`, `OFF`|
| USE_Int32 | BOOL | `ON`, `OFF`|
| USE_Real64 | BOOL | `ON`, `OFF`|

An example of configuration step is given below:

```bash
export EASIFEM_BASE=${HOME}/.local/easifem/base
cmake -G "Ninja" -S ./ -B ./build \
-D USE_OpenMP:BOOL=ON \
-D CMAKE_BUILD_TYPE:STRING=Release \
-D BUILD_SHARED_LIBS:BOOL=ON \
-D USE_PLPLOT:BOOL=ON \
-D CMAKE_INSTALL_PREFIX:PATH=${EASIFEM_BASE} \
-D USE_BLAS95:BOOL=ON \
-D USE_LAPACK95:BOOL=ON \
-D USE_FFTW:BOOL=ON \
-D USE_GTK:BOOL=ON \
-D USE_ARPACK:BOOL=ON \
-D USE_PARPACK:BOOL=ON \
-D USE_METIS:BOOL=ON \
-D USE_Int32:BOOL=ON \
-D USE_Real64:BOOL=ON
```

After configuration, you can build and install the library by using:

```bash
cmake --build ./build --target --install
```

## Arch Linux

Coming soon.

## Fedora

Coming soon.

## Other Linux Distro

Coming soon.

