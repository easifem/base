# Installation of EASIFEM on Linux Machine

## Setup

Before installation of easifem we should setup a simple working environment for the library. This means that we need to set some environment variables and install some packages. The procedure is given below.

### Install packages


**Step 5** Update linux by using following commands

Run following commands

```bash
sudo apt update
apt list --upgradable
sudo apt upgrade
sudo apt install -y curl
sudo apt install -y git
sudo apt install -y gcc-10
sudo apt install -y gfortran-10
sudo apt install -y python3
sudo apt install -y python3-pip
sudo apt install -y liblapack-dev
sudo apt install -y openmpi-bin libopenmpi-dev
sudo apt install -y cmake
sudo apt install -y gmsh
sudo apt install -y gnuplot
```

### Set environment variables

> `gnuplot` and `gmsh` are relatively big programs. You can ignore their installation if you want.

**Step 1:** Go to home directory by using the following command, and check the existance of `.bashrc`

```bash
cd ~
ls ~/.bashrc
```

If `.bashrc` file does not exists then you will receive an error message something like following.

```bash
ls: cannot access '.bashrc': No such file or directory
```

We can create the `.bashrc` by following commands.

```bash
touch ~/.bashrc
```

**Step 2** Create `~/.easifemrc`

Similarly we will also create the another file `~/.easifemrc`.

```bash
touch ~/.easifemrc
```

**Step 3** Editing `~/.easifemrc`

Now we will add some important lines into `~/.easifemrc` to make our life easier while installing the library.

Open the `~/.easifemrc` in your favorite editor and add following lines.

```bash
#!/bin/sh
easifem_prefix=${HOME}
export EASIFEM_BASE=${easifem_prefix}/.easifem/base/
export EASIFEM_EXTPKGS=${easifem_prefix}/.easifem/extpkgs/
mkdir -p ${EASIFEM_EXTPKGS}
mkdir -p ${EASIFEM_BASE}
```

**Step 4** Specifying alias for latest fortran and C/C++ compilers

`easifemBase` has been tested using `gfortran-10`. It is better to compiler `easifemBase` using latest compilers. Therefore, one needs to define following alias for compiler in `.easifemrc`

```bash
export CC=/usr/bin/gcc-10
export CXX=/usr/bin/g++-10
export CPP=/usr/bin/cpp-10
export LD=/usr/bin/gcc-10
export FC=/usr/bin/gfortran-10
alias c++=/usr/bin/c++-10
alias g++=/usr/bin/g++-10
alias gcc=/usr/bin/gcc-10
alias cpp=/usr/bin/cpp-10
alias ld=/usr/bin/gcc-10
alias cc=/usr/bin/gcc-10
alias gfortran=/usr/bin/gfortran-10
```

- The 1st line specifies the path where easifem package will be installed, it is better to specify home the home directory
- The 2nd and 3rd *should not be changed*, these env-vars are needed in `CMakeLists.txt` file.
- The 4th and the 5th lines create directories, *do not change it*

**Step 5** Edit `.bashrc`

Now that we have added useful commands to our `.easifemrc`, we can bring those changes in our `.bashrc` by adding following lines in `.bashrc`

```bash
#!/bin/sh
source ~/.easifemrc
```

**Step 6** Restart the shell

Now we restart the shell to bring following changes.

```bash
source ~/.bashrc
```

#### CMake

#### Python

#### apt-get