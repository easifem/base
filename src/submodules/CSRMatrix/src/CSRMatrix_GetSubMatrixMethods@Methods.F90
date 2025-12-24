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
USE Display_Method, ONLY: ToString, Display
USE BaseType, ONLY: math => TypeMathOpt
USE CSRMatrix_Method, ONLY: GetNNZ
USE CSRMatrix_Method, ONLY: CSRMatrix_GetColIndex => GetColIndex
USE CSRMatrix_Method, ONLY: CSRMatrix_GetColNumber => GetColNumber
USE CSRMatrix_Method, ONLY: CSRMatrix_Size => SIZE
USE CSRMatrix_Method, ONLY: CSRMatrix_GetSingleValue => GetSingleValue
USE CSRMatrix_Method, ONLY: CSRMatrix_SetIA => SetIA
USE CSRMatrix_Method, ONLY: CSRMatrix_SetJA => SetJA
USE CSRMatrix_Method, ONLY: CSRMatrix_SetSingleValue => SetSingleValue
USE CSRMatrix_Method, ONLY: CSRMatrix_GetValue => GetValue
USE CSRMatrix_Method, ONLY: CSRMatrix_Initiate => Initiate
USE CSRSparsity_Method, ONLY: CSR_GetColNumber => GetColNumber
USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE

CHARACTER(*), PARAMETER :: modName="CSRMatrix_GetSubMatrixMethods@Methods.F90"

CONTAINS

!----------------------------------------------------------------------------
!                                                            GetSubMatrixNNZ
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSubMatrixNNZ
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetSubMatrixNNZ()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: nnz, nrow, ncol, ii, nn, irow, colIndx(2), &
                icol, jj

nnz = GetNNZ(obj=obj)
nrow = CSRMatrix_Size(obj, 1)
ncol = CSRMatrix_Size(obj, 2)

! CALL Reallocate(selectCol, ncol)

selectCol(1:ncol) = math%no

nn = SIZE(cols)
DO ii = 1, nn
  jj = cols(ii)

#ifdef DEBUG_VER
  isok = jj .LE. ncol
  CALL AssertError1( &
    isok, myName, modName, __LINE__, "Error cols( "//ToString(ii)// &
    ") is greater than ncol = "//ToString(ncol))
#endif

  selectCol(jj) = math%yes
END DO

ans = 0
DO irow = 1, nrow
  colIndx = CSRMatrix_GetColIndex(obj=obj, irow=irow)
  DO ii = colIndx(1), colIndx(2)
    icol = CSRMatrix_GetColNumber(obj, ii)
    IF (selectCol(icol)) ans = ans + 1
  END DO
END DO
END PROCEDURE obj_GetSubMatrixNNZ

!----------------------------------------------------------------------------
!                                                               GetSubMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSubMatrix1
LOGICAL(LGT), ALLOCATABLE :: selectCol(:)
INTEGER(I4B) :: tsize
tsize = CSRMatrix_Size(obj, 2)
CALL Reallocate(selectCol, tsize)
CALL GetSubMatrixNNZ(obj=obj, cols=cols, selectCol=selectCol, ans=tsize)
CALL Reallocate(subIndices, tsize)
CALL GetSubMatrix_( &
  obj=obj, cols=cols, submat=submat, subIndices=subIndices, &
  selectCol=selectCol, tsize=tsize)
IF (ALLOCATED(selectCol)) DEALLOCATE (selectCol)
END PROCEDURE obj_GetSubMatrix1

!----------------------------------------------------------------------------
!                                                             GetSubMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSubMatrix_1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetSubMatrix_1()"
#endif

INTEGER(I4B) :: nnz, nrow, ncol, submat_nnz, ii, irow, colIndx(2), icol, jj
REAL(DFP) :: aval

nnz = GetNNZ(obj=obj)
nrow = CSRMatrix_Size(obj, 1)
ncol = CSRMatrix_Size(obj, 2)

! CALL Reallocate(selectCol, ncol)
CALL GetSubMatrixNNZ(obj=obj, cols=cols, selectCol=selectCol, ans=submat_nnz)

! CALL Reallocate(subIndices, submat_nnz)
CALL CSRMatrix_Initiate(obj=submat, ncol=ncol, nrow=nrow, nnz=submat_nnz)

submat_nnz = 1
CALL CSRMatrix_SetIA(obj=submat, irow=1, VALUE=submat_nnz)

DO irow = 1, nrow
  colIndx = CSRMatrix_GetColIndex(obj=obj, irow=irow)

  jj = 0
  DO ii = colIndx(1), colIndx(2)
    icol = CSR_GetColNumber(obj%csr, ii)

    IF (selectCol(icol)) THEN
      CALL CSRMatrix_SetJA(obj=submat, indx=submat_nnz + jj, VALUE=icol)

      aval = CSRMatrix_GetSingleValue(obj=obj, indx=ii)

      CALL CSRMatrix_SetSingleValue( &
        obj=submat, indx=submat_nnz + jj, VALUE=aval)

      subIndices(submat_nnz + jj) = ii

      jj = jj + 1
    END IF
  END DO

  submat_nnz = submat_nnz + jj
  CALL CSRMatrix_SetIA(obj=submat, irow=irow + 1, VALUE=submat_nnz)

END DO
END PROCEDURE obj_GetSubMatrix_1

!----------------------------------------------------------------------------
!                                                             GetSubMatrix1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSubMatrix2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetSubMatrix2()"
#endif
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
isok = ALLOCATED(submat%A)
CALL AssertError1( &
  isok, myName, modName, __LINE__, "submat%A is not allocated")
#endif

#ifdef DEBUG_VER
isok = SIZE(submat%A) .EQ. SIZE(subIndices)
CALL AssertError1(isok, myName, modName, __LINE__, &
                  "Size of submat%A not same as size of subIndices.")
#endif

CALL CSRMatrix_GetValue(obj=obj, indx=subIndices, ans=submat%A, tsize=tsize)
END PROCEDURE obj_GetSubMatrix2

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
