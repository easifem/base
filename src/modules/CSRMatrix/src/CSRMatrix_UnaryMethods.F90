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

MODULE CSRMatrix_UnaryMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: CSRMatrix_, RealMatrix_
IMPLICIT NONE
PRIVATE

PUBLIC :: Convert
PUBLIC :: ColumnSORT
PUBLIC :: RemoveDuplicates
PUBLIC :: Clean
PUBLIC :: Copy
PUBLIC :: Get
PUBLIC :: DropEntry
PUBLIC :: GetTRANSPOSE
PUBLIC :: GetDiagonal
PUBLIC :: GetLowerTriangle
PUBLIC :: GetUpperTriangle
PUBLIC :: PermuteRow
PUBLIC :: PermuteColumn
PUBLIC :: Permute
PUBLIC :: GetSym
PUBLIC :: ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                              Convert@Unary
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:          This subroutine converts sparsematrix to dense storage
!
!# Introduction
!
! This subroutine converts sparsematrix into a dense storage format
! `A(:), IA(:), JA(:)` denotes CSR format.
! This subroutine can be used for debuggin purpose.

INTERFACE Convert
  MODULE PURE SUBROUTINE obj_Convert1(A, IA, JA, mat)
    REAL(DFP), INTENT(IN) :: A(:)
    INTEGER(I4B), INTENT(IN) :: IA(:)
    INTEGER(I4B), INTENT(IN) :: JA(:)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
  END SUBROUTINE obj_Convert1
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                                              Convert@Unary
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:          This subroutine converts sparsematrix to dense storage
!
!# Introduction
!
! This subroutine converts sparsematrix to dense storage format
! `A(:), IA(:), JA(:)` denotes CSR format.

INTERFACE Convert
  MODULE PURE SUBROUTINE obj_Convert2(To, From)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: To(:, :)
    TYPE(CSRMatrix_), INTENT(IN) :: From
  END SUBROUTINE obj_Convert2
END INTERFACE Convert

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE obj_Convert2
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                              Convert@Unary
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:          This subroutine converts sparsematrix to dense storage
!
!# Introduction
!
! This subroutine converts sparsematrix to dense storage format
! `A(:), IA(:), JA(:)` denotes CSR format.

INTERFACE Convert
  MODULE PURE SUBROUTINE obj_Convert3(To, From)
    TYPE(RealMatrix_), INTENT(INOUT) :: To
    TYPE(CSRMatrix_), INTENT(IN) :: From
  END SUBROUTINE obj_Convert3
END INTERFACE Convert

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE obj_Convert3
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                           ColumnSORT@Unary
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 March 2021
! summary: Sort column of row
!
!# Introduction
!
! - This routine sorts the elements of a matrix (stored in Compressed
! Sparse Row Format) in increasing order of their column indices within
! each row. It uses insertion sort algorithm
!
! - `values`= logical indicating whether or not the real values a(*) must
! also be permuted. IF (.not. values) then the array a is not
! touched by csort and can be a dummy array.
!
! - Default value of `SortValue` is true.

INTERFACE ColumnSORT
  MODULE SUBROUTINE obj_ColumnSORT(obj, isValues)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isValues
  END SUBROUTINE obj_ColumnSORT
END INTERFACE ColumnSORT

!----------------------------------------------------------------------------
!                                                     RemoveDuplicates@Unary
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: Removes duplicate entries from the sparse matrix
!
!# Introduction
!
! This routine calls CLNCSR routine from Sparsekit

INTERFACE RemoveDuplicates
  MODULE SUBROUTINE obj_RemoveDuplicates(obj, isValues)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isValues
  END SUBROUTINE obj_RemoveDuplicates
END INTERFACE RemoveDuplicates

!----------------------------------------------------------------------------
!                                                                Clean@Unary
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: Performs different tasks related to cleaning of sparse matrix
!
!# Introduction
! This routine performs tasks related to the cleaning of sparse matrix.

INTERFACE Clean
  MODULE SUBROUTINE obj_Clean(obj, isValues, ExtraOption)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isValues
    !! If .TRUE. then values will be touched, otherwise they remain
    !! untouched by this subroutine
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ExtraOption
    !! If it is 0, then do nothing
    !! If 1, then remove duplicates and zeros, if any
    !! If 2, then remove duplicates and perform partial ordering
    !! If 3, then remove duplicates, sort entries in increasing order of col
  END SUBROUTINE obj_Clean
END INTERFACE Clean

!----------------------------------------------------------------------------
!                                                                 Copy@Unary
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 March 2021
! summary:         Copy sparse matrix into each other

INTERFACE Copy
  MODULE SUBROUTINE obj_Copy(From, To)
    TYPE(CSRMatrix_), INTENT(IN) :: From
    TYPE(CSRMatrix_), INTENT(INOUT) :: To
  END SUBROUTINE obj_Copy
END INTERFACE Copy

!----------------------------------------------------------------------------
!                                                                 get@Unary
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This returns a sigle value from the matrix

INTERFACE Get
  MODULE FUNCTION obj_Get1(obj, i, j) RESULT(Ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: i, j
    REAL(DFP) :: Ans
  END FUNCTION obj_Get1
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                              Filter@Unary
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 15 July, 2021
! summary: This routine removes any elements whose absolute value is small
!  from an input matrix A and puts the resulting matrix in B.
!
!# Introduction
!
! - `option` = integer. used to determine strategy chosen by caller to drop
!  elements from matrix A.
! - `option` = 1, Elements whose absolute value is less than the drop
! tolerance are removed.
! - `option` = 2, Elements whose absolute value is less than the product of
! the drop tolerance and the Euclidean norm of the row are removed.
! - `option` = 3, Elements whose absolute value is less that the product of
! the drop tolerance and the largest element in the row are removed.
! - `droptol` = real. drop tolerance used for dropping strategy.

INTERFACE DropEntry
  MODULE SUBROUTINE obj_DropEntry(objIn, objOut, droptol, option)
    TYPE(CSRMatrix_), INTENT(IN) :: objIn
    TYPE(CSRMatrix_), INTENT(INOUT) :: objOut
    REAL(DFP), INTENT(IN) :: droptol
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: option
  END SUBROUTINE obj_DropEntry
END INTERFACE DropEntry

!----------------------------------------------------------------------------
!                                                          Transpose@Unary
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: Transpose of the sparse matrix
!
!# Introduction
!
! In-place transposition routine. This subroutine transposes a matrix stored
! in compressed sparse row format. the transposition is done in place in that
! the arrays a,ja,ia c of the transpose are overwritten onto the original
! arrays.

INTERFACE GetTRANSPOSE
  MODULE SUBROUTINE obj_Transpose(obj)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Transpose
END INTERFACE GetTRANSPOSE

!----------------------------------------------------------------------------
!                                                         getDiagonal@Unary
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: Returns the diagonal of sparse matrix
!
!# Introduction
!
! This subroutine returns the diagonal entries of sparse matrix.
!
! - offset: containing the offset of the wanted diagonal the diagonal
! extracted is the one corresponding to the entries `a(i,j)` with `j-i =
! ioff`. thus `ioff = 0` means the main diagonal
! - `diag` : real*8 array of length nrow containing the wanted diagonal. diag
! contains the diagonal (`a(i,j),j-i = ioff`) as defined above.
! - `idiag` = integer array of  length `len`, containing the poisitions in
! the original arrays `a` and `ja` of the diagonal elements collected in
! `diag`. A zero entry in `idiag(i)` means that there was no entry found in
! row i belonging to the diagonal.

INTERFACE GetDiagonal
  MODULE SUBROUTINE obj_getDiagonal1(obj, diag, idiag, offset)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: diag(:)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: idiag(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: offset
  END SUBROUTINE obj_getDiagonal1
END INTERFACE GetDiagonal

!----------------------------------------------------------------------------
!                                                         getDiagonal@Unary
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: Returns the diagonal of sparse matrix
!
!# Introduction
!
! This subroutine returns the diagonal entries of sparse matrix.
!
! - offset: containing the offset of the wanted diagonal the diagonal
! extracted is the one corresponding to the entries `a(i,j)` with `j-i =
! ioff`. thus `ioff = 0` means the main diagonal
! - `diag` : real*8 array of length nrow containing the wanted diagonal. diag
! contains the diagonal (`a(i,j),j-i = ioff`) as defined above.

INTERFACE GetDiagonal
  MODULE SUBROUTINE obj_getDiagonal2(obj, diag, offset)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: diag(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: offset
  END SUBROUTINE obj_getDiagonal2
END INTERFACE GetDiagonal

!----------------------------------------------------------------------------
!                                                    getLowerTriangle@Unary
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: Returns the lower part of the sparse matrix
!
!# Introduction
!
! This subroutine returns the lower part of the sparse matrix.

INTERFACE GetLowerTriangle
  MODULE SUBROUTINE obj_getLowerTriangle(obj, L)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    TYPE(CSRMatrix_), INTENT(INOUT) :: L
  END SUBROUTINE obj_getLowerTriangle
END INTERFACE GetLowerTriangle

!----------------------------------------------------------------------------
!                                                    getUpperTriangle@Unary
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: Returns the Upper part of the sparse matrix
!
!# Introduction
!
! This subroutine returns the Upper part of the sparse matrix.

INTERFACE GetUpperTriangle
  MODULE SUBROUTINE obj_getUpperTriangle(obj, U)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    TYPE(CSRMatrix_), INTENT(INOUT) :: U
  END SUBROUTINE obj_getUpperTriangle
END INTERFACE GetUpperTriangle

!----------------------------------------------------------------------------
!                                                         PermuteRow@Unary
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2021
! summary: Permute the rows of sparse matrix

INTERFACE PermuteRow
  MODULE FUNCTION obj_permuteRow(obj, PERM, isValues) &
    & RESULT(ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: PERM(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isValues
    TYPE(CSRMatrix_) :: ans
  END FUNCTION obj_permuteRow
END INTERFACE PermuteRow

!----------------------------------------------------------------------------
!                                                        PermuteColumn@Unary
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2021
! summary: Permute the columns of sparse matrix

INTERFACE PermuteColumn
  MODULE FUNCTION obj_permuteColumn(obj, PERM, isValues) &
    & RESULT(ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: PERM(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isValues
    TYPE(CSRMatrix_) :: ans
  END FUNCTION obj_permuteColumn
END INTERFACE PermuteColumn

!----------------------------------------------------------------------------
!                                                             Permute@Unary
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2021
! summary: Permute the columns of sparse matrix

INTERFACE Permute
  MODULE FUNCTION obj_permute(obj, rowPERM, colPERM, &
    & isValues, symPERM) RESULT(ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rowPERM(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: colPERM(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isValues
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: symPERM
    TYPE(CSRMatrix_) :: ans
  END FUNCTION obj_permute
END INTERFACE Permute

!----------------------------------------------------------------------------
!                                                                    GetSym
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-28
! summary: Returns symmetric part of csrmatrix in symObj

INTERFACE GetSym
  MODULE SUBROUTINE obj_GetSym1(obj, symObj, from)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    TYPE(CSRMatrix_), INTENT(INOUT) :: symObj
    CHARACTER(1), INTENT(IN) :: from
  END SUBROUTINE obj_GetSym1
END INTERFACE GetSym

!----------------------------------------------------------------------------
!                                                                    GetSym
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-28
! summary: Returns symmetric part of csrmatrix in symObj

INTERFACE GetSym
  MODULE SUBROUTINE obj_GetSym2(obj, from)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    CHARACTER(1), INTENT(IN) :: from
  END SUBROUTINE obj_GetSym2
END INTERFACE GetSym

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END MODULE CSRMatrix_UnaryMethods
