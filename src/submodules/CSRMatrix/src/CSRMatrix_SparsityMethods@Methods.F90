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

SUBMODULE(CSRMatrix_SparsityMethods) Methods
USE CSRSparsity_Method, ONLY: CSRSetSparsity => SetSparsity
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity1
CALL CSRSetSparsity(obj=obj%csr, row=row, col=col)
END PROCEDURE obj_SetSparsity1

!----------------------------------------------------------------------------
!                                                                SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity2
CALL CSRSetSparsity(obj=obj%csr, row=row, col=col)
END PROCEDURE obj_SetSparsity2

!----------------------------------------------------------------------------
!                                                                SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity3
CALL CSRSetSparsity(obj=obj%csr, row=row, col=col, ivar=ivar, jvar=jvar)
END PROCEDURE obj_SetSparsity3

!----------------------------------------------------------------------------
!                                                                SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity4
CALL CSRSetSparsity(obj=obj%csr, row=row, col=col, ivar=ivar, jvar=jvar)
END PROCEDURE obj_SetSparsity4

!----------------------------------------------------------------------------
!                                                                SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity5
REAL(DFP), ALLOCATABLE :: tempA(:)
INTEGER(I4B) :: m

IF (.NOT. obj%csr%isSparsityLock) CALL CSRSetSparsity(obj%csr)

IF (ALLOCATED(obj%A)) THEN
  m = SIZE(obj%A)
  IF (m .EQ. 0) THEN
    CALL Reallocate(obj%A, obj%csr%nnz)
  ELSE IF (m .NE. obj%csr%nnz) THEN
    tempA = obj%A
    CALL Reallocate(obj%A, obj%csr%nnz)
    IF (SIZE(obj%A) .GE. SIZE(tempA)) THEN
      obj%A(1:SIZE(tempA)) = tempA(:)
    ELSE
      obj%A(1:obj%csr%nnz) = tempA(1:obj%csr%nnz)
    END IF
    DEALLOCATE (tempA)
  END IF
ELSE
  CALL Reallocate(obj%A, obj%csr%nnz)
END IF
!> Sort entries according to their column index
! Following method belongs to Sparsekit
CALL CSORT(obj%csr%nrow, obj%A, obj%csr%JA, obj%csr%IA, .TRUE.)

obj%csr%isSorted = .TRUE.
obj%csr%isSparsityLock = .FALSE.
CALL CSRSetSparsity(obj%csr)
END PROCEDURE obj_SetSparsity5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
