! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary: Modules related to [[BaseType]] module.
!
!# Introduction
! This module contains the modules related to data types which are defined
! inside the [[BaseType]] module. This module should be compiled before
! compilation of any submodule because almost all the submodules of user
! defined data type methods uses [[BaseMethod]] module. Further, after adding
! a new user defined data type inside [[BaseType]] module, its method should
! be included here.

MODULE BaseMethod

#ifdef USE_SuperLU
USE SuperLUInterface
#endif

#ifdef USE_PLPLOT
USE PLPLOT
#endif

#ifdef USE_OpenMP
USE OMP_LIB
#endif

#ifdef USE_BLAS95
USE F77_BLAS
USE F95_BLAS
#endif

#ifdef USE_LAPACK95
USE F77_LAPACK
USE F95_LAPACK
USE Lapack_Method
#endif

#ifdef USE_ARPACK
USE EASIFEM_ARPACK
#endif

#ifdef USE_FFTW
USE FFTW3
#endif

USE String_Class
USE String_Method
USE PENF, ONLY: endianL, endianB, endian, byte_size, str_ascii, &
  & str_ucs4, str, strz, cton, bstr, bcton, check_endian, digit, &
  & penf_Init, penf_print
USE BeFoR64
USE FACE
USE FPL
USE System_Method
USE CInterface
USE MetisInterface
USE OpenMP_Method
USE GlobalData
USE Hashing32
USE OGPF
USE Test_Method
USE MdEncode_Method
USE DISPMODULE
USE Display_Method
USE ErrorHandling
USE Utility
USE PolynomialUtility
USE BaseType
USE MultiIndices_Method
USE Random_Method
USE BoundingBox_Method
USE IntVector_Method
USE IndexValue_Method
USE KeyValue_Method
USE IterationData_Method
USE Vector3D_Method
USE BLAS1V_Method
USE RealVector_Method
USE DOF_Method
USE Geometry_Method
USE QuadraturePoint_Method
USE FEVariable_Method
USE Elemshapedata_Method
USE RealMatrix_Method
USE FEMatrix_Method
USE FEVector_Method
USE Rank2Tensor_Method
USE VoigtRank2Tensor_Method
USE CSRSparsity_Method
USE CSRMatrix_Method
END MODULE BaseMethod
