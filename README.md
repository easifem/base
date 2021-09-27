## EASIFEM-BASE
[![GitHub release](https://img.shields.io/github/release/vickysharma0812/easifem-base.svg?style=plastic)](https://github.com/vickysharma0812/easifem-base/releases/latest)

- [EASIFEM-BASE](#easifem-base)
- [Status](#status)
- [Introduction](#introduction)
- [VISION](#vision)
- [EASIFEM-BASE](#easifem-base-1)
- [Installation](#installation)
  - [Run on Cloud](#run-on-cloud)
- [Usage](#usage)
- [Dependencies](#dependencies)
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
3. All user defined data-types are included in a module called `baseType.F90`
4. Methods of a data-type, let us say XXX, are defined in a module, `XXX_method.F90` . It is noteworthy that this module only contains the interface of the method. Actual implementations are given in the submodules, `XXX_method@SubmoduleName.F90`. Modules are kept in a directory called `/modules` and submodules are kept in `/submodules`

Details of data-types and methods included in EASIFEM-base can be found [here](https://www.notion.so/d1490ecc9fa84e75ac9731c1f8538e67).

## Installation

- [Install easifemBASE on MacOSX](pages/Installation_MacOS.md)
- [Installtion easifemBASE on Linux](pages/Installation_Linux.md)
- [Installtion easifemBASE on Windows](pages/Installation_Windows.md)

### Run on Cloud

## Usage

## Dependencies

[External packages used in easifemBASE](pages/Extpkgs.md)

## Contributing

## Credits

## License

[License](LICENSE)