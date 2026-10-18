! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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
! date: 2025-12-30
! summary: Index helpers for column-major (Fortran) arrays.
!
! Description:
!   This module contains the generic function `FortranIndex`, which
!   converts the subscripts of an element of a 2D, 3D or 4D array into
!   the position of that element in the flat (column-major) storage.
!
!   Both the subscripts and the extents are 1-based. The functions do
!   not check that the subscripts lie within the extents.

MODULE IndexUtility
USE GlobalData, ONLY: I4B
IMPLICIT NONE

PRIVATE
PUBLIC :: FortranIndex

!----------------------------------------------------------------------------
!                                                                FortranIndex
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-30
! summary: Get the index of an element of a 2D Fortran array.
!
! Description:
!   Returns the position of the element `(ii, jj)` of an array of shape
!   `(nrow, ncol)` in its flat column-major storage. The first subscript
!   varies fastest. Subscripts and extents are 1-based and no range
!   check is done.
!
! Arguments:
!   ii   : INTEGER(I4B) - row subscript, `1 <= ii <= nrow`
!   jj   : INTEGER(I4B) - column subscript, `1 <= jj <= ncol`
!   nrow : INTEGER(I4B) - number of rows
!   ncol : INTEGER(I4B) - number of columns (not used in the result)
!
! Returns:
!   ans : INTEGER(I4B) - position of `a(ii, jj)` in `RESHAPE(a, [n])`
!
! Formula:
!   $$\mathrm{ans} = ii + (jj - 1)\, nrow$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE IndexUtility, ONLY: FortranIndex
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: nrow = 3_I4B, ncol = 4_I4B
! INTEGER(I4B) :: a(nrow, ncol), b(nrow * ncol)
! INTEGER(I4B) :: ii, jj, n, indx
! LOGICAL(LGT) :: isok
!
! ! fill a with 1, 2, ..., nrow*ncol in column-major order
! n = 0_I4B
! DO jj = 1, ncol
!   DO ii = 1, nrow
!     n = n + 1_I4B
!     a(ii, jj) = n
!   END DO
! END DO
!
! ! flat storage of a
! b = RESHAPE(a, [nrow * ncol])
!
! ! the index is the position of a(ii, jj) in the flat storage
! isok = .TRUE.
! DO jj = 1, ncol
!   DO ii = 1, nrow
!     indx = FortranIndex(ii, jj, nrow, ncol)
!     isok = isok .AND. (b(indx) .EQ. a(ii, jj))
!     isok = isok .AND. (indx .EQ. a(ii, jj))
!   END DO
! END DO
! CALL OK(isok, "index matches the flat storage of a")
!
! ! first and last element
! indx = FortranIndex(1_I4B, 1_I4B, nrow, ncol)
! CALL OK(indx .EQ. 1_I4B, "index of the first element")
! indx = FortranIndex(nrow, ncol, nrow, ncol)
! CALL OK(indx .EQ. nrow * ncol, "index of the last element")
!
! ! a single entry: 2 + (3 - 1) * 3 = 8
! indx = FortranIndex(2_I4B, 3_I4B, nrow, ncol)
! CALL OK(indx .EQ. 8_I4B, "index of a(2, 3)")
!
! ! dimension of size 1
! indx = FortranIndex(1_I4B, 4_I4B, 1_I4B, 5_I4B)
! CALL OK(indx .EQ. 4_I4B, "nrow = 1")
! indx = FortranIndex(4_I4B, 1_I4B, 5_I4B, 1_I4B)
! CALL OK(indx .EQ. 4_I4B, "ncol = 1")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION FortranIndex1(ii, jj, nrow, ncol) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: ii, jj, nrow, ncol
    INTEGER(I4B) :: ans
  END FUNCTION FortranIndex1
END INTERFACE

INTERFACE FortranIndex
  MODULE PROCEDURE FortranIndex1
END INTERFACE FortranIndex

!----------------------------------------------------------------------------
!                                                                FortranIndex
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-30
! summary: Get the index of an element of a 3D Fortran array.
!
! Description:
!   Returns the position of the element `(ii, jj, kk)` of an array of
!   shape `(dim1, dim2, dim3)` in its flat column-major storage. The
!   first subscript varies fastest. Subscripts and extents are 1-based
!   and no range check is done.
!
! Arguments:
!   ii   : INTEGER(I4B) - first subscript, `1 <= ii <= dim1`
!   jj   : INTEGER(I4B) - second subscript, `1 <= jj <= dim2`
!   kk   : INTEGER(I4B) - third subscript, `1 <= kk <= dim3`
!   dim1 : INTEGER(I4B) - extent of the first dimension
!   dim2 : INTEGER(I4B) - extent of the second dimension
!   dim3 : INTEGER(I4B) - extent of the third dimension (not used in the
!          result)
!
! Returns:
!   ans : INTEGER(I4B) - position of `a(ii, jj, kk)` in `RESHAPE(a, [n])`
!
! Formula:
!   $$\mathrm{ans} = ii + (jj - 1)\, dim1
!   + (kk - 1)\, dim1\, dim2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE IndexUtility, ONLY: FortranIndex
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: dim1 = 2_I4B, dim2 = 3_I4B, dim3 = 4_I4B
! INTEGER(I4B) :: a(dim1, dim2, dim3), b(dim1 * dim2 * dim3)
! INTEGER(I4B) :: ii, jj, kk, n, indx
! LOGICAL(LGT) :: isok
!
! ! fill a with 1, 2, ..., dim1*dim2*dim3 in column-major order
! n = 0_I4B
! DO kk = 1, dim3
!   DO jj = 1, dim2
!     DO ii = 1, dim1
!       n = n + 1_I4B
!       a(ii, jj, kk) = n
!     END DO
!   END DO
! END DO
!
! ! flat storage of a
! b = RESHAPE(a, [dim1 * dim2 * dim3])
!
! ! the index is the position of a(ii, jj, kk) in the flat storage
! isok = .TRUE.
! DO kk = 1, dim3
!   DO jj = 1, dim2
!     DO ii = 1, dim1
!       indx = FortranIndex(ii, jj, kk, dim1, dim2, dim3)
!       isok = isok .AND. (b(indx) .EQ. a(ii, jj, kk))
!       isok = isok .AND. (indx .EQ. a(ii, jj, kk))
!     END DO
!   END DO
! END DO
! CALL OK(isok, "index matches the flat storage of a")
!
! ! first and last element
! indx = FortranIndex(1_I4B, 1_I4B, 1_I4B, dim1, dim2, dim3)
! CALL OK(indx .EQ. 1_I4B, "index of the first element")
! indx = FortranIndex(dim1, dim2, dim3, dim1, dim2, dim3)
! CALL OK(indx .EQ. dim1 * dim2 * dim3, "index of the last element")
!
! ! a single entry: 2 + (2 - 1) * 2 + (3 - 1) * 2 * 3 = 16
! indx = FortranIndex(2_I4B, 2_I4B, 3_I4B, dim1, dim2, dim3)
! CALL OK(indx .EQ. 16_I4B, "index of a(2, 2, 3)")
!
! ! dimensions of size 1
! indx = FortranIndex(1_I4B, 1_I4B, 3_I4B, 1_I4B, 1_I4B, 5_I4B)
! CALL OK(indx .EQ. 3_I4B, "dim1 = 1 and dim2 = 1")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION FortranIndex2(ii, jj, kk, dim1, dim2, dim3) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: ii, jj, kk, dim1, dim2, dim3
    INTEGER(I4B) :: ans
  END FUNCTION FortranIndex2
END INTERFACE

INTERFACE FortranIndex
  MODULE PROCEDURE FortranIndex2
END INTERFACE FortranIndex

!----------------------------------------------------------------------------
!                                                               FortranIndex
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-30
! summary: Get the index of an element of a 4D Fortran array.
!
! Description:
!   Returns the position of the element `(ii, jj, kk, ll)` of an array
!   of shape `(dim1, dim2, dim3, dim4)` in its flat column-major
!   storage. The first subscript varies fastest. Subscripts and extents
!   are 1-based and no range check is done.
!
! Arguments:
!   ii   : INTEGER(I4B) - first subscript, `1 <= ii <= dim1`
!   jj   : INTEGER(I4B) - second subscript, `1 <= jj <= dim2`
!   kk   : INTEGER(I4B) - third subscript, `1 <= kk <= dim3`
!   ll   : INTEGER(I4B) - fourth subscript, `1 <= ll <= dim4`
!   dim1 : INTEGER(I4B) - extent of the first dimension
!   dim2 : INTEGER(I4B) - extent of the second dimension
!   dim3 : INTEGER(I4B) - extent of the third dimension
!   dim4 : INTEGER(I4B) - extent of the fourth dimension (not used in
!          the result)
!
! Returns:
!   ans : INTEGER(I4B) - position of `a(ii, jj, kk, ll)` in
!         `RESHAPE(a, [n])`
!
! Formula:
!   $$\mathrm{ans} = ii + (jj - 1)\, dim1
!   + (kk - 1)\, dim1\, dim2
!   + (ll - 1)\, dim1\, dim2\, dim3$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE IndexUtility, ONLY: FortranIndex
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: dim1 = 2_I4B, dim2 = 3_I4B
! INTEGER(I4B), PARAMETER :: dim3 = 4_I4B, dim4 = 2_I4B
! INTEGER(I4B), PARAMETER :: ntotal = dim1 * dim2 * dim3 * dim4
! INTEGER(I4B) :: a(dim1, dim2, dim3, dim4), b(ntotal)
! INTEGER(I4B) :: ii, jj, kk, ll, n, indx
! LOGICAL(LGT) :: isok
!
! ! fill a with 1, 2, ..., ntotal in column-major order
! n = 0_I4B
! DO ll = 1, dim4
!   DO kk = 1, dim3
!     DO jj = 1, dim2
!       DO ii = 1, dim1
!         n = n + 1_I4B
!         a(ii, jj, kk, ll) = n
!       END DO
!     END DO
!   END DO
! END DO
!
! ! flat storage of a
! b = RESHAPE(a, [ntotal])
!
! ! the index is the position of a(ii, jj, kk, ll) in the flat storage
! isok = .TRUE.
! DO ll = 1, dim4
!   DO kk = 1, dim3
!     DO jj = 1, dim2
!       DO ii = 1, dim1
!         indx = FortranIndex(ii, jj, kk, ll, dim1, dim2, dim3, dim4)
!         isok = isok .AND. (b(indx) .EQ. a(ii, jj, kk, ll))
!         isok = isok .AND. (indx .EQ. a(ii, jj, kk, ll))
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "index matches the flat storage of a")
!
! ! first and last element
! indx = FortranIndex(1_I4B, 1_I4B, 1_I4B, 1_I4B, dim1, dim2, dim3, dim4)
! CALL OK(indx .EQ. 1_I4B, "index of the first element")
! indx = FortranIndex(dim1, dim2, dim3, dim4, dim1, dim2, dim3, dim4)
! CALL OK(indx .EQ. ntotal, "index of the last element")
!
! ! a single entry: 1 + (2-1)*2 + (3-1)*2*3 + (2-1)*2*3*4 = 39
! indx = FortranIndex(1_I4B, 2_I4B, 3_I4B, 2_I4B, dim1, dim2, dim3, dim4)
! CALL OK(indx .EQ. 39_I4B, "index of a(1, 2, 3, 2)")
!
! ! dimensions of size 1
! indx = FortranIndex(1_I4B, 1_I4B, 1_I4B, 4_I4B, &
!                     1_I4B, 1_I4B, 1_I4B, 4_I4B)
! CALL OK(indx .EQ. 4_I4B, "dim1 = dim2 = dim3 = 1")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION FortranIndex3(ii, jj, kk, ll, dim1, dim2, dim3, &
                                     dim4) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: ii, jj, kk, ll, dim1, dim2, dim3, dim4
    INTEGER(I4B) :: ans
  END FUNCTION FortranIndex3
END INTERFACE

INTERFACE FortranIndex
  MODULE PROCEDURE FortranIndex3
END INTERFACE FortranIndex

END MODULE IndexUtility
