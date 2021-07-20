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
! date: 14 July 2021
! summary: This submodule contains the methods for sparse matrix

SUBMODULE( CSRMatrix_Method ) getMethod
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 isSquare
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_isSquare
  IF( obj%csr%nrow .EQ. obj%csr%ncol ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE csrMat_isSquare

!----------------------------------------------------------------------------
!                                                               isRectangle
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_isRectangle
  IF( obj%csr%nrow .EQ. obj%csr%ncol ) THEN
    ans = .FALSE.
  ELSE
    ans = .TRUE.
  END IF
END PROCEDURE csrMat_isRectangle



END SUBMODULE getMethod