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

SUBMODULE(FEMatrix_Method) ConvectiveMatrixMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ConvectiveMatrix_1
#include "./ConvectiveMatrix_1.inc"
END PROCEDURE ConvectiveMatrix_1

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ConvectiveMatrix_1b
#include "./ConvectiveMatrix_1.inc"
END PROCEDURE ConvectiveMatrix_1b

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ConvectiveMatrix_2
!! Define internal variables
INTEGER(I4B) :: nips, ips
REAL(DFP), ALLOCATABLE :: realval(:), cbar(:)
!! main
CALL Reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
nips = SIZE(trial%N, 2)
IF (PRESENT(c)) THEN
  CALL Reallocate(cbar, nips)
  CALL GetInterpolation(obj=trial, val=c, interpol=cbar)
  realval = trial%js * trial%ws * trial%thickness * cbar
ELSE
  realval = trial%js * trial%ws * trial%thickness
END IF
!!
IF (term1 .EQ. 1 .AND. term2 .EQ. 0) THEN
  DO ips = 1, nips
    ans = ans + outerprod(a=test%dNdXt(:, dim, ips), &
         & b=trial%N(:, ips)) * realval(ips)
  END DO
ELSE IF (term1 .EQ. 0 .AND. term2 .EQ. 1) THEN
  DO ips = 1, nips
    ans = ans + outerprod(a=test%N(:, ips), &
         & b=trial%dNdXt(:, dim, ips)) * realval(ips)
  END DO
END IF
!! cleanup
IF (ALLOCATED(realval)) DEALLOCATE (realval)
IF (ALLOCATED(cbar)) DEALLOCATE (cbar)
END PROCEDURE ConvectiveMatrix_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConvectiveMatrixMethods
