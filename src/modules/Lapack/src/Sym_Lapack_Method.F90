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
! date: 20 December 2022
! summary: Lapack methods
!
!# Introduction
!
! This module contains linear algebra packages for fortran matrix (2D array)
! The matrix is symmetric and dense
!
! This module contains following submoduls
! - @LinearSolveMethods TODO
! - @EigenValueMethods TODO
! - @SingularValueMethods TODO
! - @CompRoutineMethods TODO
! - @AuxRoutinesMethods TODO
!

MODULE Sym_Lapack_Method
USE GlobalData
IMPLICIT NONE
PRIVATE

#include "./Sym/LinearSolveMethods.inc"
!#include "./Sym/EigenvalueMethods.inc"
#include "./Sym/LUMethods.inc"
! #include "./Sym/GE_CompRoutine.inc"

END MODULE Sym_Lapack_Method
