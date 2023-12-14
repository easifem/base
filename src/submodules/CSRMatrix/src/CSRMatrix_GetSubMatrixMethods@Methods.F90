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

SUBMODULE(CSRMatrix_GetSubMatrixMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             GetSubMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSubMatrix1
LOGICAL(LGT), ALLOCATABLE :: selectCol(:)
INTEGER(I4B) :: nnz, nrow, ncol, submat_nnz, ii, nn, irow, colIndx(2),  &
& icol, jj
REAL(DFP) :: aval
TYPE(String) :: astr

nnz = GetNNZ(obj=obj)
nrow = SIZE(obj, 1)
ncol = SIZE(obj, 2)

CALL Reallocate(selectCol, ncol)

selectCol = .FALSE.

nn = SIZE(cols)
DO ii = 1, nn
  jj = cols(ii)
  IF (jj .GT. ncol) THEN
    astr = "Error cols( "//tostring(ii)//") is greater than "//  &
    & "ncol = "//tostring(ncol)
    CALL ErrorMSG( &
      & astr%chars(), &
      & "CSRMatrix_GetSubMatrixMethods@Methods.F90", &
      & "obj_GetSubMatrix1()", &
      & __LINE__, stderr)
    STOP
  END IF
  selectCol(jj) = .TRUE.
END DO

submat_nnz = 0
DO irow = 1, nrow
  colIndx = GetColIndex(obj=obj, irow=irow)
  DO ii = colIndx(1), colIndx(2)
    icol = GetColNumber(obj, ii)
    IF (selectCol(icol)) submat_nnz = submat_nnz + 1
  END DO
END DO

CALL Reallocate(subIndices, submat_nnz)
CALL Initiate(obj=submat, ncol=ncol, nrow=nrow, nnz=submat_nnz)

submat_nnz = 1
CALL SetIA(obj=submat, irow=1, VALUE=submat_nnz)

DO irow = 1, nrow
  colIndx = GetColIndex(obj=obj, irow=irow)
  jj = 0
  DO ii = colIndx(1), colIndx(2)
    icol = GetColNumber(obj%csr, ii)
    IF (selectCol(icol)) THEN
      CALL SetJA(obj=submat, indx=submat_nnz + jj, VALUE=icol)
      aval = GetSingleValue(obj=obj, indx=ii)
      CALL SetSingleValue(obj=submat, indx=submat_nnz + jj, VALUE=aval)
      subIndices(submat_nnz + jj) = ii
      jj = jj + 1
    END IF
  END DO
  submat_nnz = submat_nnz + jj
  CALL SetIA(obj=submat, irow=irow + 1, VALUE=submat_nnz)
END DO

CALL Display(subIndices, "debug subIndices: ")

IF (ALLOCATED(selectCol)) DEALLOCATE (selectCol)

END PROCEDURE obj_GetSubMatrix1

!----------------------------------------------------------------------------
!                                                             GetSubMatrix1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSubMatrix2
LOGICAL(LGT) :: isok

IF (isFirstCall) THEN
  CALL obj_GetSubMatrix2(obj=obj, cols=cols, submat=submat,  &
    & subIndices=subIndices)
  RETURN
END IF

isok = ALLOCATED(subIndices)

IF (.NOT. isok) THEN
  CALL ErrorMSG( &
    & "subIndices not allocated", &
    & "CSRMatrix_GetSubMatrixMethods@Methods.F90", &
    & "obj_GetSubMatrix2()", &
    & __LINE__, stderr)
  STOP
END IF

isok = ALLOCATED(submat%A)
IF (.NOT. isok) THEN
  CALL ErrorMSG( &
    & "submat%A not allocated", &
    & "CSRMatrix_GetSubMatrixMethods@Methods.F90", &
    & "obj_GetSubMatrix2()", &
    & __LINE__, stderr)
  STOP
END IF

isok = SIZE(submat%A) .EQ. SIZE(subIndices)
IF (.NOT. isok) THEN
  CALL ErrorMSG( &
    & "Size of submat%A not same as size of subIndices.", &
    & "CSRMatrix_GetSubMatrixMethods@Methods.F90", &
    & "obj_GetSubMatrix2()", &
    & __LINE__, stderr)
  STOP
END IF

submat%A = Get(obj=obj, indx=subIndices)

END PROCEDURE obj_GetSubMatrix2

END SUBMODULE Methods
