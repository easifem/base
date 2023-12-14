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

!! authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This submodule contains the methods for sparse matrix

SUBMODULE(CSRMatrix_GetMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           GetSingleValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSingleValue
ans = obj%A(indx)
END PROCEDURE obj_GetSingleValue

!----------------------------------------------------------------------------
!                                                           GetSeveralValues
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSeveralValue
INTEGER(I4B) :: ii, tsize
tsize = SIZE(indx)
DO ii = 1, tsize; ans(ii) = obj%A(indx(ii)); END DO
END PROCEDURE obj_GetSeveralValue

!----------------------------------------------------------------------------
!                                                             GetStorageFMT
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetStorageFMT
IF (i .EQ. 1) THEN
  ans = obj%csr%idof%storageFMT
ELSE
  ans = obj%csr%jdof%storageFMT
END IF
END PROCEDURE obj_GetStorageFMT

!----------------------------------------------------------------------------
!                                                            GetMatrixProp
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMatrixProp
ans = TRIM(obj%matrixProp)
END PROCEDURE obj_GetMatrixProp

!----------------------------------------------------------------------------
!                                                              GetDOFPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDOFPointer
IF (i .EQ. 1) THEN
  ans => obj%csr%idof
ELSE
  ans => obj%csr%jdof
END IF
END PROCEDURE obj_GetDOFPointer

!----------------------------------------------------------------------------
!                                                                 isSquare
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isSquare
IF (obj%csr%nrow .EQ. obj%csr%ncol) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE obj_isSquare

!----------------------------------------------------------------------------
!                                                               isRectangle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isRectangle
IF (obj%csr%nrow .EQ. obj%csr%ncol) THEN
  ans = .FALSE.
ELSE
  ans = .TRUE.
END IF
END PROCEDURE obj_isRectangle

!----------------------------------------------------------------------------
!                                                               GetColNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetColNumber
ans = GetColNumber(obj%csr, indx)
END PROCEDURE obj_GetColNumber

!----------------------------------------------------------------------------
!                                                               GetColIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetColIndex
ans = GetColIndex(obj%csr, irow)
END PROCEDURE obj_GetColIndex

!----------------------------------------------------------------------------
!                                                              startColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_startColumn
ans = obj%csr.startColumn.irow
END PROCEDURE obj_startColumn

!----------------------------------------------------------------------------
!                                                              endColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_endColumn
ans = obj%csr.endColumn.irow
END PROCEDURE obj_endColumn

END SUBMODULE Methods
