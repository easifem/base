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

SUBMODULE(FEMatrix_Method) STConvectiveMatrixMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

#include "./STCM_1.inc"
#include "./STCM_1b.inc"
#include "./STCM_2.inc"
#include "./STCM_3.inc"
#include "./STCM_4.inc"
#include "./STCM_5.inc"
#include "./STCM_6.inc"
#include "./STCM_6b.inc"
#include "./STCM_7.inc"
#include "./STCM_7b.inc"
#include "./STCM_8.inc"
#include "./STCM_8b.inc"
#include "./STConvectiveMatrix_1.inc"
#include "./STConvectiveMatrix_2.inc"

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Mat2_STConvectiveMatrix_1
REAL(DFP), ALLOCATABLE :: mat4(:, :, :, :)
mat4 = STConvectiveMatrix(test=test, trial=trial, term1=term1, &
     & term2=term2, c=c, projectOn=projectOn)
CALL convert(from=mat4, to=ans)
IF (ALLOCATED(mat4)) DEALLOCATE (mat4)
END PROCEDURE Mat2_STConvectiveMatrix_1

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Mat2_STConvectiveMatrix_2
REAL(DFP), ALLOCATABLE :: mat4(:, :, :, :)
mat4 = STConvectiveMatrix(test=test, trial=trial, term1=term1, &
     & term2=term2, rho=rho, c=c, projectOn=projectOn)
CALL convert(from=mat4, to=ans)
IF (ALLOCATED(mat4)) DEALLOCATE (mat4)
END PROCEDURE Mat2_STConvectiveMatrix_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE STConvectiveMatrixMethods
