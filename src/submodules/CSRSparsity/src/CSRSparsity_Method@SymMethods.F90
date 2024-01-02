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
! date: 13 Jul 2021
! summary: Input output related methods

SUBMODULE(CSRSparsity_Method) SymMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_GetSymU1(obj, symobj)
  TYPE(CSRSparsity_), INTENT(IN) :: obj
  TYPE(CSRSparsity_), INTENT(INOUT) :: symobj
  !
  INTEGER(I4B) :: nnz_parts(3), ii, jj, rindx, indx, nrow, nnzU, ncol, &
    & nnzD, al, ar, ad
  INTEGER(I4B), ALLOCATABLE :: IA_csr(:), IA_csc(:), JA_csr(:), &
    & JA_csc(:), idiag(:)
  REAL(DFP) :: real_dummy(1)
  !
  nnz_parts = GetNNZ(obj, [""])
  nrow = obj%nrow
  ncol = obj%ncol
  nnzU = nnz_parts(1)
  nnzD = nnz_parts(3)
  !
  CALL Reallocate(JA_csr, nnzU, IA_csr, nrow + 1)
  CALL Reallocate(idiag, nrow)
  !
  indx = 0
  !
  DO ii = 1, nrow
    IA_csr(ii) = indx + 1
    IA_csr(ii + 1) = IA_csr(ii)
    DO rindx = obj%IA(ii), obj%IA(ii + 1) - 1
      jj = obj%JA(rindx)
      IF (ii .LT. jj) THEN
        indx = indx + 1
        IA_csr(ii + 1) = IA_csr(ii + 1) + 1
        JA_csr(indx) = jj
      ELSE IF (ii .EQ. jj) THEN
        idiag(ii) = rindx
      END IF
    END DO
  END DO
  !
  CALL Reallocate(IA_csc, ncol + 1, JA_csc, nnzU)
  !
  CALL csrcsc( &
    & nrow, &
    & 0, &
    & 1, &
    & real_dummy, &
    & JA_csr, &
    & IA_csr, &
    & real_dummy, &
    & JA_csc, &
    & IA_csc)
  !
  symobj%nnz = nnz_parts(1) * 2 + nnz_parts(3)
  symobj%ncol = ncol
  symobj%nrow = nrow
  symobj%isSorted = obj%isSorted
  symobj%isInitiated = obj%isInitiated
  symobj%isSparsityLock = obj%isSparsityLock
  symobj%isDiagStored = .TRUE.
  symobj%idof = obj%idof
  symobj%jdof = obj%jdof
  !
  CALL Reallocate(symobj%IA, nrow + 1, symobj%idiag, nrow)
  CALL Reallocate(symobj%JA, symobj%nnz)
  !
  indx = 0
  !
  DO ii = 1, symobj%nrow
    ar = IA_csr(ii + 1) - IA_csr(ii)
    al = IA_csc(ii + 1) - IA_csr(ii)
    IF (idiag(ii) .NE. 0) THEN
      ad = 1
    ELSE
      ad = 0
    END IF
    !
    symobj%IA(ii) = indx + 1
    symobj%IA(ii + 1) = symobj%IA(ii) + ar + al + ad
    !
    DO rindx = IA_csc(ii), IA_csc(ii + 1) - 1
      indx = indx + 1
      symobj%JA(indx) = JA_csc(rindx)
    END DO
    !
    IF (idiag(ii) .NE. 0) THEN
      indx = indx + 1
      symobj%JA(indx) = obj%JA(idiag(ii))
      symobj%idiag(ii) = indx
    END IF
    !
    DO rindx = IA_csr(ii), IA_csr(ii + 1) - 1
      indx = indx + 1
      symobj%JA(indx) = JA_csr(rindx)
    END DO
    !
  END DO
  !
  ! Clean up
  !
  DEALLOCATE (IA_csr, IA_csc, JA_csr, JA_csc, idiag)
  !
END SUBROUTINE obj_GetSymU1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_GetSymL1(obj, symobj)
  TYPE(CSRSparsity_), INTENT(IN) :: obj
  TYPE(CSRSparsity_), INTENT(INOUT) :: symobj
  !
  INTEGER(I4B) :: nnz_parts(3), ii, jj, rindx, indx, nrow, nnzL, ncol, &
    & nnzD, al, ar, ad
  INTEGER(I4B), ALLOCATABLE :: IA_csr(:), IA_csc(:), JA_csr(:), &
    & JA_csc(:), idiag(:)
  REAL(DFP) :: real_dummy(1)
  !
  nnz_parts = GetNNZ(obj, [""])
  nrow = obj%nrow
  ncol = obj%ncol
  nnzL = nnz_parts(2)
  nnzD = nnz_parts(3)
  !
  CALL Reallocate(JA_csr, nnzL, IA_csr, nrow + 1)
  CALL Reallocate(idiag, nrow)
  !
  indx = 0
  !
  DO ii = 1, nrow
    IA_csr(ii) = indx + 1
    IA_csr(ii + 1) = IA_csr(ii)
    DO rindx = obj%IA(ii), obj%IA(ii + 1) - 1
      jj = obj%JA(rindx)
      IF (ii .GT. jj) THEN
        indx = indx + 1
        IA_csr(ii + 1) = IA_csr(ii + 1) + 1
        JA_csr(indx) = jj
      ELSE IF (ii .EQ. jj) THEN
        idiag(ii) = rindx
      END IF
    END DO
  END DO
  !
  CALL Reallocate(IA_csc, ncol + 1, JA_csc, nnzL)
  !
  CALL csrcsc( &
    & nrow, &
    & 0, &
    & 1, &
    & real_dummy, &
    & JA_csr, &
    & IA_csr, &
    & real_dummy, &
    & JA_csc, &
    & IA_csc)
  !
  symobj%nnz = nnzL * 2 + nnzD
  symobj%ncol = ncol
  symobj%nrow = nrow
  symobj%isSorted = obj%isSorted
  symobj%isInitiated = obj%isInitiated
  symobj%isSparsityLock = obj%isSparsityLock
  symobj%isDiagStored = .TRUE.
  symobj%idof = obj%idof
  symobj%jdof = obj%jdof
  !
  CALL Reallocate(symobj%IA, nrow + 1, symobj%idiag, nrow)
  CALL Reallocate(symobj%JA, symobj%nnz)
  !
  indx = 0
  !
  DO ii = 1, symobj%nrow
    al = IA_csr(ii + 1) - IA_csr(ii)
    ar = IA_csc(ii + 1) - IA_csc(ii)
    IF (idiag(ii) .NE. 0) THEN
      ad = 1
    ELSE
      ad = 0
    END IF
    !
    symobj%IA(ii) = indx + 1
    symobj%IA(ii + 1) = symobj%IA(ii) + ar + al + ad
    !
    DO rindx = IA_csr(ii), IA_csr(ii + 1) - 1
      indx = indx + 1
      symobj%JA(indx) = JA_csr(rindx)
    END DO
    !
    IF (idiag(ii) .NE. 0) THEN
      indx = indx + 1
      symobj%JA(indx) = obj%JA(idiag(ii))
      symobj%idiag(ii) = indx
    END IF
    !
    DO rindx = IA_csc(ii), IA_csc(ii + 1) - 1
      indx = indx + 1
      symobj%JA(indx) = JA_csc(rindx)
    END DO
    !
  END DO
  !
  ! Clean up
  !
  DEALLOCATE (IA_csr, IA_csc, JA_csr, JA_csc, idiag)
  !
END SUBROUTINE obj_GetSymL1

!----------------------------------------------------------------------------
!                                                                    GetSym
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSym1
SELECT CASE (from)
CASE ("U", "u")
  CALL obj_GetSymU1(obj=obj, symobj=symobj)
CASE ("L", "l")
  CALL obj_GetSymL1(obj=obj, symobj=symobj)
CASE default
  CALL Errormsg( &
    & msg="No case found for given from = "//from, &
    & file=__FILE__, &
    & routine="obj_GetSym1()", &
    & line=__LINE__, &
    & unitno=stderr)
END SELECT
END PROCEDURE obj_GetSym1

!----------------------------------------------------------------------------
!                                                                    GetSym
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSym2

END PROCEDURE obj_GetSym2

END SUBMODULE SymMethods
