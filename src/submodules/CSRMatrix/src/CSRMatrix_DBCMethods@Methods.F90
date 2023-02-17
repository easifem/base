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
! date: 14 July 2021
! summary: This submodule contains the methods for sparse matrix

SUBMODULE(CSRMatrix_DBCMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_ApplyDBC
INTEGER(I4B) :: i, ii, nrow
LOGICAL(LGT), ALLOCATABLE :: mask(:)
REAL(DFP), ALLOCATABLE :: diag_entries(:)

ASSOCIATE (IA => obj%csr%IA, JA => obj%csr%JA, A => obj%A)

  nrow = SIZE(obj, 1)
  ALLOCATE (mask(nrow))
  mask = .FALSE.
  mask(dbcPtrs) = .TRUE.

  ! make row zeros

  DO CONCURRENT(i=1:SIZE(dbcPtrs))
    ii = dbcPtrs(i)
    A(IA(ii):IA(ii + 1) - 1) = 0.0_DFP
  END DO

  DO CONCURRENT(i=1:nrow)
    DO ii = IA(i), IA(i + 1) - 1
      IF (mask(JA(ii))) THEN
        A(ii) = 0.0_DFP
      END IF
    END DO
  END DO

  IF (obj%csr%isDiagStored) THEN
    A(obj%csr%idiag(dbcPtrs)) = 1.0_DFP
  ELSE
    CALL GetDiagonal(obj=obj, diag=diag_entries)
    A(obj%csr%idiag(dbcPtrs)) = 1.0_DFP
    DEALLOCATE (diag_entries)
  END IF

  DEALLOCATE (mask)

END ASSOCIATE

END PROCEDURE csrMat_ApplyDBC

END SUBMODULE Methods
