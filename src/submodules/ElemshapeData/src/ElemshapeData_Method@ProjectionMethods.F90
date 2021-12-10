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

SUBMODULE(ElemshapeData_Method) ProjectionMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       getProjectionOfdNdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetProjectionOfdNdXt_1
!! Define internal variables
INTEGER(I4B) :: ii
!! main
CALL Reallocate(cdNdXt, SIZE(obj%N, 1), SIZE(obj%N, 2))
DO ii = 1, SIZE(obj%N, 2)
  cdNdXt(:, ii) = MATMUL(obj%dNdXt(:, :, ii), Val)
END DO
END PROCEDURE elemsd_GetProjectionOfdNdXt_1

!----------------------------------------------------------------------------
!                                                       getProjectionOfdNdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetProjectionOfdNdXt_2
INTEGER(I4B) :: ii
REAL(DFP), ALLOCATABLE :: cbar(:, :)
!! main
CALL getInterpolation(obj=obj, val=val, interpol=cbar)
CALL Reallocate(cdNdXt, SIZE(obj%N, 1), SIZE(obj%N, 2))
DO ii = 1, SIZE(obj%N, 2)
  cdNdXt(:, ii) = MATMUL(obj%dNdXt(:, :, ii), cbar(:, ii))
END DO
IF (ALLOCATED(cbar)) DEALLOCATE (cbar)
END PROCEDURE elemsd_GetProjectionOfdNdXt_2

!----------------------------------------------------------------------------
!                                                      getProjectionOfdNTdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE getProjectionOfdNTdXt_1
INTEGER(I4B) :: ii
!! main
CALL Reallocate(cdNTdXt, SIZE(obj%N, 1), SIZE(obj%T), SIZE(obj%N, 2))
DO ii = 1, SIZE(obj%N, 2)
  cdNTdXt(:, :, ii) = MATMUL(obj%dNTdXt(:, :, :, ii), Val)
END DO
END PROCEDURE getProjectionOfdNTdXt_1

!----------------------------------------------------------------------------
!                                                      getProjectionOfdNTdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE getProjectionOfdNTdXt_2
INTEGER(I4B) :: ii
REAL(DFP), ALLOCATABLE :: cbar(:, :)
!! main
CALL getInterpolation(obj=obj, val=val, interpol=cbar)
CALL Reallocate(cdNTdXt, SIZE(obj%N, 1), SIZE(obj%T), SIZE(obj%N, 2))
DO ii = 1, SIZE(obj%N, 2)
  cdNTdXt(:, :, ii) = MATMUL(obj%dNTdXt(:, :, :, ii), cbar(:, ii))
END DO
DEALLOCATE (cbar)
END PROCEDURE getProjectionOfdNTdXt_2

END SUBMODULE ProjectionMethods
