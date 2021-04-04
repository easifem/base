## EASIFEM-BASE
[![GitHub release](https://img.shields.io/github/release/vickysharma0812/easifem-base.svg?style=plastic)](https://github.com/vickysharma0812/easifem-base/releases/latest)

- [EASIFEM-BASE](#easifem-base)
- [Status](#status)
- [Introduction](#introduction)
- [VISION](#vision)
- [EASIFEM-BASE](#easifem-base-1)
- [Installation](#installation)
  - [MacOSX](#macosx)
    - [CMake](#cmake)
    - [Python](#python)
    - [Homebrew](#homebrew)
  - [Linux](#linux)
    - [CMake](#cmake-1)
    - [Python](#python-1)
    - [apt-get](#apt-get)
  - [Windows](#windows)
  - [Run on Cloud](#run-on-cloud)
- [Usage](#usage)
- [Dependencies](#dependencies)
  - [PENF](#penf)
  - [BeFoR64](#befor64)
  - [StringiFor](#stringifor)
  - [FoXy](#foxy)
  - [vtkFortran](#vtkfortran)
  - [H5Fortran](#h5fortran)
  - [Functional-fortran](#functional-fortran)
  - [OGPF](#ogpf)
  - [Sparsekit](#sparsekit)
- [Contributing](#contributing)
- [Credits](#credits)
- [License](#license)

## Status

[![GitHub issues](https://img.shields.io/github/issues/vickysharma0812/easifem-base.png?style=plastic)](https://github.com/vickysharma0812/easifem-base/issues)
[![GitHub stars](https://img.shields.io/github/stars/vickysharma0812/easifem-base.png?style=plastic)](https://github.com/vickysharma0812/easifem-base/stars)
[![GitHub license](https://img.shields.io/github/license/vickysharma0812/easifem-base.png?style=plastic)](https://github.com/vickysharma0812/easifem-base/license)

## Introduction

EASIFEM stands for **E**xpandable **A**nd **S**calable **I**nfrastructure for **F**inite **E**lement **M**ethod. EASIFEM is designed by using [Modern-Fortran](https://fortran-lang.org/) programming language.

- It is a framework for writing for developing Finite Element Programs for solving partial differential equations.
- It is written in Modern Fortran.
- The design of EASIFEM is simple and effective; it employs both object oriented paradigm and multi-dispatch paradigm.
- It will be useful for students, teachers, and researchers working in the area of finite element method.
- The library is still in development state, which means the new features will be added to the library as time-passes without hurting the backward compatibility of EASIFEM.

## VISION

The vision of EASIFEM is to ease the coding part of FEM. This library can be employed for developing FEM code for complex problems including multi-physics and multiphase couplings. In future, EASIFEM, will include interfaces with the available open-source programs and new kernels for solving program related to geotechnical and earthquake engineering, dynamic soil-structure interaction (e.g., wind-turbine and its foundation), large deformation analysis of soils (e.g., rainfall induced slope failure, soil liquefaction, mud-flow, etc.), geo-environmental problems, soil physics, reactive transport, carbon-storage, to name a few.

## EASIFEM-BASE

1. EASIFEM-base contains the fundamental user defined data-types and their methods, which are necessary for implementation of numerical methods such as finite element/volume/difference method, material point method, particle finite element method, random finite element method, among others.
2. EASIFEM-base is the lowest layer on which the entire landscape of EASIFEM resides.
3. All user defined data-types are included in a module called `baseType.f90`
4. Methods of a data-type, let us say XXX, are defined in a module, `XXX_method.f90` . It is noteworthy that this module only contains the interface of the method. Actual implementations are given in the submodules, `XXX_method@SubmoduleName.f90`. Modules are kept in a directory called `/modules` and submodules are kept in `/submodules`

Details of data-types and methods included in EASIFEM-base can be found [here](https://www.notion.so/d1490ecc9fa84e75ac9731c1f8538e67).

## Installation

### MacOSX

#### CMake

#### Python
#### Homebrew

### Linux

#### CMake

#### Python

#### apt-get

### Windows

### Run on Cloud

## Usage

## Dependencies

For some of its functionality EASIFEM depends upon following fortran-libraries, which are brain children of the great fortran developers. Note that some of these libraries have been suitably modified to serve the desire purpose.

### PENF

PENF is developed  by [Mr. Szaghi](https://github.com/szaghi). This library tries to exploit code portability for modern (2003+) Fortran projects. It is a pure Fortran library for achieving portable codes for modern Fortran projects. It  provides many number-to-string and vice-versa facilities. [Read more](https://github.com/szaghi/PENF/tree/master/src)

### BeFoR64

This library is also a fortran project of [Mr. Szaghi](https://github.com/szaghi). It is for *base64* encoding/decoding for modern Fortran projects. [Read more](https://github.com/szaghi/BeFoR64)

### StringiFor

This library is also a fortran project of [Mr. Szaghi](https://github.com/szaghi). This library makes an attempt to define string data type for handling characters in an object oriented way.  [Read more](https://github.com/szaghi/StringiFor)

### FoXy

This is a fortran library which is designed to handle XML files. [Read more](https://github.com/Fortran-FOSS-Programmers/FoXy)

### vtkFortran

This fortran library handles IO with vtk files. [Read more](https://github.com/szaghi/VTKFortran)

### H5Fortran

This fortran project, which is developed by [Michael Hirsch](https://github.com/scivision), can handle IO with hdf5 files. [Read more](https://github.com/geospace-code/h5fortran.git).

### Functional-fortran

This modern fortran project is developed by [Milan Curcic](https://github.com/milancurcic). This library facilitates a set of commonly used tools in functional programming. [Read more](https://github.com/wavebitscientific/functional-fortran)

### OGPF

This is program creates an interface between modern fortran and gnuplot. This is a useful library for visualising fortran data using gnuplot. [Read more](https://github.com/kookma/ogpf)

### Sparsekit

Sparsekit is a legacy fortran code written by the great [Yusef Saad](https://en.wikipedia.org/wiki/Yousef_Saad) for peforming linear algebra with sparse matrices. [Read more](https://www-users.cs.umn.edu/~saad/software/SPARSKIT/)

## Contributing

## Credits

## License
