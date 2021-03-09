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

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	[[BaseMethod]] module contains the modules related to data types defined inside the [[BaseType]] module.
!
!### Introduction
! This module contains the modules related to data types which are defined inside the [[BaseType]] module. This module should be compiled before compilation of any submodule because almost all the submodules of user defined data type methods uses [[BaseMethod]] module. Further, after adding aa new user defined data type inside [[BaseType]] module, its method should be included here.
!

MODULE BaseMethod
  USE System_Method
#ifdef USE_OpenMP
  USE OMP_LIB
#endif
  USE OpenMP_Method
  USE StringiFor
  USE PENF
  USE FACE
  USE EASIFEM_BLAS
  USE GlobalData
  USE Display_Method
  USE ErrorHandling
  USE Utility
  USE BaseType
  USE Random_Method
  USE AbstractArray_Method
  USE AbstractMatrix_Method
  USE AbstractVector_Method
  USE BoundingBox_Method
  USE IntVector_Method
  USE IndexValue_Method
  USE KeyValue_Method
  USE IterationData_Method
  USE Vector3D_Method
  USE BLAS1V_Method
  USE RealVector_Method
  USE DOF_Method
  USE ReferenceElement_Method
  USE ReferenceLine_Method
  USE ReferenceTriangle_Method
  USE ReferenceQuadrangle_Method
  USE ReferenceTetrahedron_Method
  USE ReferenceHexahedron_Method
  USE ReferencePrism_Method
  USE ReferencePyramid_Method
  USE QuadraturePoint_Method
  USE FEVariable_Method
  USE Elemshapedata_Method
  USE File_Method
  USE RealMatrix_Method
  USE FEMatrix_Method
  ! USE Lapack_Method
  ! USE SparseMatrix_Method
  ! USE VoigtRank2Tensor_Method
  ! USE Rank2Tensor_Method

END MODULE BaseMethod
