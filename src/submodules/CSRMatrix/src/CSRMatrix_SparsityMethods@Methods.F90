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
! date:         22 March 2021
! summary: It contains method for setting values in [[CSRMatrix_]]

SUBMODULE(CSRMatrix_SparsityMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setSparsity1
CALL setSparsity(obj=obj%csr, row=row, col=col)
END PROCEDURE csrMat_setSparsity1

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setSparsity2
CALL setSparsity(obj=obj%csr, row=row, col=col)
END PROCEDURE csrMat_setSparsity2

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setSparsity3
CALL setSparsity(obj=obj%csr, row=row, col=col, ivar=ivar, jvar=jvar)
END PROCEDURE csrMat_setSparsity3

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setSparsity4
CALL setSparsity(obj=obj%csr, row=row, col=col, ivar=ivar, jvar=jvar)
END PROCEDURE csrMat_setSparsity4

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setSparsity_final
REAL(DFP), ALLOCATABLE :: tempA(:)
INTEGER(I4B) :: m
!
IF (.NOT. obj%csr%isSparsityLock) CALL setSparsity(obj%csr)
IF (ALLOCATED(obj%A)) THEN
  m = SIZE(obj%A)
  IF (m .EQ. 0) THEN
    CALL Reallocate(obj%A, obj%csr%nnz)
  ELSE IF (m .NE. obj%csr%nnz) THEN
    tempA = obj%A
    CALL Reallocate(obj%A, obj%csr%nnz)
    IF (SIZE(obj%A) .LT. SIZE(tempA)) THEN
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
CALL CSORT(obj%csr%nrow, obj%A, obj%csr%JA, obj%csr%IA, .TRUE.)
obj%csr%isSorted = .TRUE.
obj%csr%isSparsityLock = .FALSE.
CALL setSparsity(obj%csr)
END PROCEDURE csrMat_setSparsity_final

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
