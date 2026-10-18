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
! date: 2021-03-22
! summary: Outer, Kronecker and vector products of arrays.
!
! Description:
!   This module contains the following generic routines.
!
!   - `OuterProd`: outer (tensor) product of two, three or four real
!     arrays. The rank of the result is the sum of the ranks of the
!     operands, up to 6, and its shape is the shapes of the operands one
!     after another.
!   - `OuterProd_`: the same without allocation. It updates `ans` as
!     `ans = anscoeff * ans + scale * OuterProd(...)` and returns the
!     extents written in `ans`.
!   - `OTimesTilda` (also called `OTimesTilda_`): Kronecker product, used
!     to build a space-time matrix from a time matrix and a space matrix.
!   - `VectorProduct` (also called `Vector_Product` and `Cross_Product`):
!     cross product of two vectors of size 3.
!
!   Routines ending with an underscore do not allocate. They write only
!   the leading block of `ans`, and `ans` must be large enough. This is
!   not checked.

MODULE ProductUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: REAL32
USE GlobalData, ONLY: REAL64
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: I4B
IMPLICIT NONE

PRIVATE
PUBLIC :: OuterProd
PUBLIC :: OuterProd_
PUBLIC :: OTimesTilda
PUBLIC :: OTimesTilda_
PUBLIC :: Cross_Product
PUBLIC :: Vector_Product
PUBLIC :: VectorProduct

!----------------------------------------------------------------------------
!                                                                OTimesTilda
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-08-13
! summary: Kronecker (space-time) product of two matrices
!
! Description:
!   Computes the Kronecker product of the time matrix `a` and the space matrix
!   `b`, and adds it to `ans`, that is `ans = anscoeff * ans + scale * kron(a,
!   b)`. The result is a space-time matrix in DOF format: the block (i, j) of
!   `ans` is `a(i, j) * b`.
!
!   - `nrow = SIZE(a, 1) * SIZE(b, 1)` and `ncol = SIZE(a, 2) * SIZE(b, 2)`,
!     so `a` and `b` must have their exact sizes.
!   - Only `ans(1:nrow, 1:ncol)` is written. `ans` must be at least that
!     large. This is not checked.
!   - `OTimesTilda_` is another name for the same routine.
!
! Arguments:
!   a : REAL(DFP), rank(2) - time matrix
!   b : REAL(DFP), rank(2) - space matrix
!   ans : REAL(DFP), rank(2), INTENT(INOUT) - space-time matrix, `SIZE(ans, 1)
!     >= nrow`, `SIZE(ans, 2) >= ncol`
!   nrow : INTEGER(I4B), INTENT(OUT) - number of rows written in `ans`
!   ncol : INTEGER(I4B), INTENT(OUT) - number of columns written in `ans`
!   anscoeff : REAL(DFP) - coefficient of the old `ans`
!   scale : REAL(DFP) - coefficient of `kron(a, b)`
!
! Returns:
!   `ans(1:nrow, 1:ncol)` is updated in place.
!
! Formula:
!   $$\mathrm{ans}_{(i-1)n+p,\,(j-1)m+q} \leftarrow
!     \alpha\, \mathrm{ans}_{(i-1)n+p,\,(j-1)m+q} + \beta\, a_{ij}\, b_{pq}$$
!   where $n$ = `SIZE(b, 1)`, $m$ = `SIZE(b, 2)`, $\alpha$ is `anscoeff`
!   and $\beta$ is `scale`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OTimesTilda
! USE ProductUtility, ONLY: OTimesTilda_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: a(2, 2), b(2, 3), ans(5, 7), want
! INTEGER(I4B) :: nrow, ncol, ii, jj, pp, qq
! LOGICAL(LGT) :: isok
!
! ! a = [1 2; 3 4], b = [1 2 3; 4 5 6]
! a = RESHAPE([1.0_DFP, 3.0_DFP, 2.0_DFP, 4.0_DFP], [2, 2])
! b = RESHAPE([1.0_DFP, 4.0_DFP, 2.0_DFP, 5.0_DFP, 3.0_DFP, 6.0_DFP], &
!             [2, 3])
!
! ! ans is larger than needed
! ans = 1.0_DFP
! CALL OTimesTilda(a=a, b=b, ans=ans, nrow=nrow, ncol=ncol, &
!                  anscoeff=0.0_DFP, scale=1.0_DFP)
!
! CALL OK(nrow .EQ. 4_I4B, "nrow = SIZE(a, 1) * SIZE(b, 1)")
! CALL OK(ncol .EQ. 6_I4B, "ncol = SIZE(a, 2) * SIZE(b, 2)")
!
! ! ans((ii-1)*2+pp, (jj-1)*3+qq) = a(ii,jj) * b(pp,qq)
! isok = .TRUE.
! DO jj = 1, 2
!   DO ii = 1, 2
!     DO qq = 1, 3
!       DO pp = 1, 2
!         want = a(ii, jj) * b(pp, qq)
!         isok = isok .AND. &
!                (ABS(ans((ii - 1) * 2 + pp, (jj - 1) * 3 + qq) - want) &
!                 .LT. tol)
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "block structure of the Kronecker product")
!
! ! entries computed by hand
! CALL OK(ABS(ans(1, 4) - 2.0_DFP) .LT. tol, "ans(1,4) = a(1,2) * b(1,1)")
! CALL OK(ABS(ans(4, 6) - 24.0_DFP) .LT. tol, "ans(4,6) = a(2,2) * b(2,3)")
!
! ! entries outside ans(1:4, 1:6) are not changed
! CALL OK(ALL(ABS(ans(5, :) - 1.0_DFP) .LT. tol) .AND. &
!         ALL(ABS(ans(:, 7) - 1.0_DFP) .LT. tol), "rest of ans unchanged")
!
! ! ans = 1 * ans + 2 * (a kron b), called through the alias OTimesTilda_
! CALL OTimesTilda_(a=a, b=b, ans=ans, nrow=nrow, ncol=ncol, &
!                   anscoeff=1.0_DFP, scale=2.0_DFP)
! CALL OK(ABS(ans(4, 6) - 72.0_DFP) .LT. tol, "anscoeff and scale")
!
! END PROGRAM main
!```

INTERFACE OTimesTilda
  MODULE PURE SUBROUTINE OTimesTilda1(a, b, ans, nrow, ncol, anscoeff, scale)
    REAL(DFP), INTENT(IN) :: a(:, :)
    !! time matrix
    REAL(DFP), INTENT(IN) :: b(:, :)
    !! space matrix
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space time matrix in DOF Format
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! the number of rows and cols written in ans
    REAL(DFP), INTENT(IN) :: anscoeff
    !! answer coefficient
    REAL(DFP), INTENT(IN) :: scale
    !! scale
  END SUBROUTINE OTimesTilda1
END INTERFACE OTimesTilda

INTERFACE OTimesTilda_
  MODULE PROCEDURE OTimesTilda1
END INTERFACE OTimesTilda_

!----------------------------------------------------------------------------
!                                                                OtimesTilda
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-08-13
! summary: Kronecker (space-time) product of two vectors
!
! Description:
!   Computes the Kronecker product of the time vector `a` and the space vector
!   `b`, and adds it to `ans`, that is `ans = anscoeff * ans + scale * kron(a,
!   b)`.
!
!   - `tsize = SIZE(a) * SIZE(b)`, so `a` and `b` must have their exact sizes.
!   - Only `ans(1:tsize)` is written. `ans` must be at least that large. This
!     is not checked.
!   - `OTimesTilda_` is another name for the same routine.
!
! Arguments:
!   a : REAL(DFP), rank(1) - time vector
!   b : REAL(DFP), rank(1) - space vector
!   ans : REAL(DFP), rank(1), INTENT(INOUT) - space-time vector, `SIZE(ans) >=
!     tsize`
!   tsize : INTEGER(I4B), INTENT(OUT) - number of entries written in `ans`
!   anscoeff : REAL(DFP) - coefficient of the old `ans`
!   scale : REAL(DFP) - coefficient of `kron(a, b)`
!
! Returns:
!   `ans(1:tsize)` is updated in place.
!
! Formula:
!   $$\mathrm{ans}_{(i-1)n+j} \leftarrow
!     \alpha\, \mathrm{ans}_{(i-1)n+j} + \beta\, a_i\, b_j$$
!   where $n$ = `SIZE(b)`, $\alpha$ is `anscoeff` and $\beta$ is `scale`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ProductUtility, ONLY: OTimesTilda
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: a(2), b(3), ans(7)
! INTEGER(I4B) :: tsize
!
! a = [1.0_DFP, 2.0_DFP]
! b = [3.0_DFP, 4.0_DFP, 5.0_DFP]
!
! ! ans is larger than needed
! ans = 1.0_DFP
! CALL OTimesTilda(a=a, b=b, ans=ans, tsize=tsize, anscoeff=0.0_DFP, &
!                  scale=1.0_DFP)
!
! CALL OK(tsize .EQ. 6_I4B, "tsize = SIZE(a) * SIZE(b)")
!
! ! ans((ii-1)*SIZE(b)+jj) = a(ii) * b(jj)
! CALL OK(ALL(ABS(ans(1:6) - [3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP, &
!                             8.0_DFP, 10.0_DFP]) .LT. tol), &
!         "Kronecker product of a and b")
! CALL OK(ABS(ans(7) - 1.0_DFP) .LT. tol, "ans(7) is not changed")
!
! ! ans = 1 * ans + 1 * (a kron b)
! CALL OTimesTilda(a=a, b=b, ans=ans, tsize=tsize, anscoeff=1.0_DFP, &
!                  scale=1.0_DFP)
! CALL OK(ALL(ABS(ans(1:6) - [6.0_DFP, 8.0_DFP, 10.0_DFP, 12.0_DFP, &
!                             16.0_DFP, 20.0_DFP]) .LT. tol), &
!         "anscoeff and scale")
!
! END PROGRAM main
!```

INTERFACE OTimesTilda
  MODULE PURE SUBROUTINE OTimesTilda2(a, b, ans, tsize, anscoeff, scale)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    REAL(DFP), INTENT(IN) :: anscoeff
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE OTimesTilda2
END INTERFACE OTimesTilda

INTERFACE OTimesTilda_
  MODULE PROCEDURE OTimesTilda2
END INTERFACE OTimesTilda_

!----------------------------------------------------------------------------
!                                                                OTimesTilda
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-08-13
! summary: Kronecker (space-time) product of four vectors
!
! Description:
!   Computes the Kronecker product of the time matrix `OuterProd(a, b)` and
!   the space matrix `OuterProd(c, d)` without forming these matrices, and
!   adds it to `ans`.
!
!   - `nrow = SIZE(a) * SIZE(c)` and `ncol = SIZE(b) * SIZE(d)`, so the
!     vectors must have their exact sizes.
!   - Only `ans(1:nrow, 1:ncol)` is written. `ans` must be at least that
!     large. This is not checked.
!   - `OTimesTilda_` is another name for the same routine.
!
! Arguments:
!   a, b : REAL(DFP), rank(1) - vectors of the time matrix `OuterProd(a, b)`
!   c, d : REAL(DFP), rank(1) - vectors of the space matrix `OuterProd(c, d)`
!   ans : REAL(DFP), rank(2), INTENT(INOUT) - space-time matrix, `SIZE(ans, 1)
!     >= nrow`, `SIZE(ans, 2) >= ncol`
!   nrow : INTEGER(I4B), INTENT(OUT) - number of rows written in `ans`
!   ncol : INTEGER(I4B), INTENT(OUT) - number of columns written in `ans`
!   anscoeff : REAL(DFP) - coefficient of the old `ans`
!   scale : REAL(DFP) - coefficient of the product
!
! Returns:
!   `ans(1:nrow, 1:ncol)` is updated in place.
!
! Formula:
!   $$\mathrm{ans}_{(i-1)n+p,\,(j-1)m+q} \leftarrow
!     \alpha\, \mathrm{ans}_{(i-1)n+p,\,(j-1)m+q}
!     + \beta\, a_i\, b_j\, c_p\, d_q$$
!   where $n$ = `SIZE(c)`, $m$ = `SIZE(d)`, $\alpha$ is `anscoeff` and
!   $\beta$ is `scale`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ProductUtility, ONLY: OTimesTilda
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: a(2), b(2), c(2), d(3), ans(4, 6), ans2(4, 6)
! INTEGER(I4B) :: nrow, ncol
!
! a = [1.0_DFP, 2.0_DFP]
! b = [3.0_DFP, 4.0_DFP]
! c = [5.0_DFP, 6.0_DFP]
! d = [7.0_DFP, 8.0_DFP, 9.0_DFP]
!
! ans = 0.0_DFP
! CALL OTimesTilda(a=a, b=b, c=c, d=d, ans=ans, nrow=nrow, ncol=ncol, &
!                  anscoeff=0.0_DFP, scale=1.0_DFP)
!
! CALL OK(nrow .EQ. 4_I4B, "nrow = SIZE(a) * SIZE(c)")
! CALL OK(ncol .EQ. 6_I4B, "ncol = SIZE(b) * SIZE(d)")
!
! ! entries computed by hand
! CALL OK(ABS(ans(1, 1) - 105.0_DFP) .LT. tol, "ans(1,1) = 1 * 3 * 5 * 7")
! CALL OK(ABS(ans(4, 6) - 432.0_DFP) .LT. tol, "ans(4,6) = 2 * 4 * 6 * 9")
!
! ! same as the matrix version with (a outer b) and (c outer d)
! ans2 = 0.0_DFP
! CALL OTimesTilda(a=OuterProd(a, b), b=OuterProd(c, d), ans=ans2, &
!                  nrow=nrow, ncol=ncol, anscoeff=0.0_DFP, scale=1.0_DFP)
! CALL OK(ALL(ABS(ans - ans2) .LT. tol), "same as the matrix version")
!
! END PROGRAM main
!```

INTERFACE OTimesTilda
  MODULE PURE SUBROUTINE OTimesTilda3( &
    a, b, c, d, ans, nrow, ncol, anscoeff, scale)
    REAL(DFP), INTENT(IN) :: a(:), b(:)
    !! time matrix
    REAL(DFP), INTENT(IN) :: c(:), d(:)
    !! space matrix
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space time matrix in DOF Format
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), INTENT(IN) :: anscoeff
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE OTimesTilda3
END INTERFACE OTimesTilda

INTERFACE OTimesTilda_
  MODULE PROCEDURE OTimesTilda3
END INTERFACE OTimesTilda_

!----------------------------------------------------------------------------
!                                                             Vector_Product
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-09
! summary: Vector (cross) product of two vectors of size 3
!
! Description:
!   Returns `c = a x b` for two vectors of size 3. It is available for
!   `REAL32` and `REAL64`, and the result has the same kind as `a` and `b`.
!
!   `Vector_Product`, `VectorProduct` and `Cross_Product` are three names for
!   the same function.
!
! Arguments:
!   a : REAL, rank(1) - first vector, `SIZE(a) = 3`
!   b : REAL, rank(1) - second vector, `SIZE(b) = 3`
!
! Returns:
!   c : REAL, rank(1) - `a x b`, `SIZE(c) = 3`, same kind as `a`
!
! Formula:
!   $$\mathbf{c} = \mathbf{a} \times \mathbf{b} =
!     \begin{bmatrix} a_2 b_3 - a_3 b_2 \\ a_3 b_1 - a_1 b_3 \\
!     a_1 b_2 - a_2 b_1 \end{bmatrix}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE ProductUtility, ONLY: VectorProduct
! USE ProductUtility, ONLY: Vector_Product
! USE ProductUtility, ONLY: Cross_Product
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: e1(3), e2(3), e3(3), a(3), b(3), c(3)
!
! e1 = [1.0_DFP, 0.0_DFP, 0.0_DFP]
! e2 = [0.0_DFP, 1.0_DFP, 0.0_DFP]
! e3 = [0.0_DFP, 0.0_DFP, 1.0_DFP]
!
! ! unit vectors
! CALL OK(ALL(ABS(VectorProduct(e1, e2) - e3) .LT. tol), "e1 x e2 = e3")
! CALL OK(ALL(ABS(VectorProduct(e2, e3) - e1) .LT. tol), "e2 x e3 = e1")
! CALL OK(ALL(ABS(VectorProduct(e2, e1) + e3) .LT. tol), "e2 x e1 = -e3")
!
! ! general vectors
! a = [1.0_DFP, 2.0_DFP, 3.0_DFP]
! b = [4.0_DFP, 5.0_DFP, 6.0_DFP]
! c = VectorProduct(a, b)
! CALL OK(ALL(ABS(c - [-3.0_DFP, 6.0_DFP, -3.0_DFP]) .LT. tol), "a x b")
! CALL OK(ABS(DOT_PRODUCT(a, c)) .LT. tol, "a x b is orthogonal to a")
! CALL OK(ABS(DOT_PRODUCT(b, c)) .LT. tol, "a x b is orthogonal to b")
! CALL OK(ALL(ABS(VectorProduct(a, a)) .LT. tol), "a x a = 0")
!
! ! the three names call the same function
! CALL OK(ALL(ABS(Vector_Product(a, b) - c) .LT. tol), "Vector_Product")
! CALL OK(ALL(ABS(Cross_Product(a, b) - c) .LT. tol), "Cross_Product")
!
! END PROGRAM main
!```

INTERFACE Vector_Product
  MODULE PURE FUNCTION vectorProduct_1(a, b) RESULT(c)
    ! Define INTENT of dummy argument
    REAL(REAL64), INTENT(IN) :: a(3), b(3)
    REAL(REAL64) :: c(3)
  END FUNCTION vectorProduct_1

  MODULE PURE FUNCTION vectorProduct_2(a, b) RESULT(c)
    ! Define INTENT of dummy argument
    REAL(REAL32), INTENT(IN) :: a(3), b(3)
    REAL(REAL32) :: c(3)
  END FUNCTION vectorProduct_2
END INTERFACE Vector_Product

INTERFACE Cross_Product
  MODULE PROCEDURE vectorProduct_1, vectorProduct_2
END INTERFACE Cross_Product

INTERFACE VectorProduct
  MODULE PROCEDURE vectorProduct_1, vectorProduct_2
END INTERFACE VectorProduct

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-09
! summary: Outer product of rank-1 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a` and `b`. The result has rank 2.
!   Its shape is `[SHAPE(a), SHAPE(b)]`, so the indices of the operands follow
!   one after another.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(1) - second operand
!
! Returns:
!   ans : REAL(DFP), rank(2) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2} =
!     a_{i_1}\, b_{i_2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(2) = [2, 3]
! REAL(DFP) :: a(2), b(3)
! REAL(DFP), ALLOCATABLE :: ans(:, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = [(REAL(ii, DFP), ii=1, 3)]
!
! ans = OuterProd(a, b)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i2 = 1, 3
!   DO i1 = 1, 2
!     want = a(i1) * b(i2)
!     isok = isok .AND. (ABS(ans(i1, i2) - want) .LT. tol)
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3) - 6.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:), b(:)
    REAL(DFP) :: ans(SIZE(a), SIZE(b))
  END FUNCTION OuterProd_r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                 OuterProd_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-09
! summary: Outer product of rank-1 and rank-1 arrays, no allocation
!
! Description:
!   Adds the outer product of `a` and `b` to `ans`, that is `ans = anscoeff *
!   ans + scale * OuterProd(a, b)`.
!
!   - Only the leading block `ans(1:nrow, 1:ncol)` is written. The other
!     entries of `ans` are not changed.
!   - `ans` must be at least as large as the result. This is not checked.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   anscoeff : REAL(DFP) - coefficient of the old value of `ans`
!   scale : REAL(DFP) - coefficient of the outer product
!   ans : REAL(DFP), rank(2), INTENT(INOUT) - updated in place, `SIZE(ans, k)`
!     is at least the k-th extent of the result
!   nrow : INTEGER(I4B), INTENT(OUT) - `nrow = SIZE(a)`
!   ncol : INTEGER(I4B), INTENT(OUT) - `ncol = SIZE(b)`
!
! Returns:
!   `ans(1:nrow, 1:ncol)` is overwritten with the new value; `nrow`, `ncol`
!   hold the extents written in `ans`.
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2} \leftarrow
!     \alpha\, \mathrm{ans}_{i_1 i_2}
!     + \beta\, a_{i_1}\, b_{i_2}$$
!   where $\alpha$ is `anscoeff` and $\beta$ is `scale`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE ProductUtility, ONLY: OuterProd_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(2) = [2, 3]
! REAL(DFP) :: a(2), b(3)
! REAL(DFP) :: ans(3, 4)
! REAL(DFP) :: want
! INTEGER(I4B) :: nrow, ncol
! INTEGER(I4B) :: ii, i1, i2
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = [(REAL(ii, DFP), ii=1, 3)]
!
! ! ans is one larger than needed in every dimension
! ans = 1.0_DFP
!
! ! ans = 2 * ans + 3 * (outer product)
! CALL OuterProd_(a=a, b=b, anscoeff=2.0_DFP, scale=3.0_DFP, &
!                 ans=ans, nrow=nrow, ncol=ncol)
!
! CALL OK(ALL([nrow, ncol] .EQ. expected), "sizes written in ans")
!
! isok = .TRUE.
! DO i2 = 1, 3
!   DO i1 = 1, 2
!     want = 2.0_DFP + 3.0_DFP * a(i1) * b(i2)
!     isok = isok .AND. (ABS(ans(i1, i2) - want) .LT. tol)
!   END DO
! END DO
! CALL OK(isok, "leading block of ans")
! CALL OK(ABS(ans(2, 3) - 20.0_DFP) .LT. tol, "last entry")
!
! ! entries outside the leading block are not changed
! isok = .TRUE.
! isok = isok .AND. ALL(ABS(ans(3, :) - 1.0_DFP) .LT. tol)
! isok = isok .AND. ALL(ABS(ans(:, 4) - 1.0_DFP) .LT. tol)
! CALL OK(isok, "rest of ans is unchanged")
!
! ! with anscoeff = 0 and scale = 1 we get OuterProd
! CALL OuterProd_(a=a, b=b, anscoeff=0.0_DFP, scale=1.0_DFP, &
!                 ans=ans, nrow=nrow, ncol=ncol)
! CALL OK(ALL(ABS(ans(1:2, 1:3) - OuterProd(a, b)) &
!         .LT. tol), "same as OuterProd")
!
! END PROGRAM main
!```

INTERFACE OuterProd_
  MODULE PURE SUBROUTINE OuterProd_r1r1_( &
    a, b, anscoeff, scale, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: a(:), b(:)
    !! Size of a and b will be used to determine nrow and ncol
    REAL(DFP), INTENT(IN) :: anscoeff
    !! coefficient of ans
    REAL(DFP), INTENT(IN) :: scale
    !! coefficient of a \otimes b
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! outerprod
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of data written in ans
  END SUBROUTINE OuterProd_r1r1_
END INTERFACE OuterProd_

!----------------------------------------------------------------------------
!                                                                 OuterProd_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-09
! summary: Outer product of the leading parts of two vectors, no allocation
!
! Description:
!   Adds the outer product of `a(1:sizeA)` and `b(1:sizeB)` to `ans`. Use it
!   when `a` and `b` are work arrays that are larger than the data they hold.
!
!   - `nrow = sizeA` and `ncol = sizeB`.
!   - Only `ans(1:nrow, 1:ncol)` is written. `ans` must be at least that
!     large. This is not checked.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first vector, `SIZE(a) >= sizeA`
!   b : REAL(DFP), rank(1) - second vector, `SIZE(b) >= sizeB`
!   sizeA, sizeB : INTEGER(I4B) - number of entries of `a` and `b` to use
!   anscoeff : REAL(DFP) - coefficient of the old `ans`
!   scale : REAL(DFP) - coefficient of the outer product
!   ans : REAL(DFP), rank(2), INTENT(INOUT) - updated in place
!   nrow, ncol : INTEGER(I4B), INTENT(OUT) - number of rows and columns
!     written in `ans`
!
! Returns:
!   `ans(1:nrow, 1:ncol)` is updated in place.
!
! Formula:
!   $$\mathrm{ans}_{ij} \leftarrow \alpha\, \mathrm{ans}_{ij}
!     + \beta\, a_i\, b_j,
!     \quad i = 1, \dots, \mathrm{sizeA},\;
!     j = 1, \dots, \mathrm{sizeB}$$
!   where $\alpha$ is `anscoeff` and $\beta$ is `scale`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ProductUtility, ONLY: OuterProd
! USE ProductUtility, ONLY: OuterProd_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: a(3), b(4), ans(3, 4)
! INTEGER(I4B) :: nrow, ncol
!
! a = [1.0_DFP, 2.0_DFP, 3.0_DFP]
! b = [4.0_DFP, 5.0_DFP, 6.0_DFP, 7.0_DFP]
!
! ! use only a(1:2) and b(1:3)
! ans = 1.0_DFP
! CALL OuterProd_(a=a, b=b, sizeA=2_I4B, sizeB=3_I4B, anscoeff=0.0_DFP, &
!                 scale=1.0_DFP, ans=ans, nrow=nrow, ncol=ncol)
!
! CALL OK(nrow .EQ. 2_I4B, "nrow = sizeA")
! CALL OK(ncol .EQ. 3_I4B, "ncol = sizeB")
! CALL OK(ALL(ABS(ans(1:2, 1:3) - OuterProd(a(1:2), b(1:3))) .LT. tol), &
!         "ans(1:2, 1:3) = a(1:2) x b(1:3)")
! CALL OK(ABS(ans(2, 3) - 12.0_DFP) .LT. tol, "ans(2,3) = a(2) * b(3)")
!
! ! entries outside ans(1:2, 1:3) are not changed
! CALL OK(ALL(ABS(ans(3, :) - 1.0_DFP) .LT. tol) .AND. &
!         ALL(ABS(ans(:, 4) - 1.0_DFP) .LT. tol), "rest of ans unchanged")
!
! ! ans = 1 * ans + 2 * (a x b)
! CALL OuterProd_(a=a, b=b, sizeA=2_I4B, sizeB=3_I4B, anscoeff=1.0_DFP, &
!                 scale=2.0_DFP, ans=ans, nrow=nrow, ncol=ncol)
! CALL OK(ABS(ans(2, 3) - 36.0_DFP) .LT. tol, "anscoeff and scale")
!
! END PROGRAM main
!```

INTERFACE OuterProd_
  MODULE PURE SUBROUTINE SafeOuterProd_r1r1_( &
    a, b, sizeA, sizeB, anscoeff, scale, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: a(:), b(:)
    !! Size of a and b will be used to determine nrow and ncol
    INTEGER(I4B), INTENT(IN) :: sizeA, sizeB
    !! size of a and b
    REAL(DFP), INTENT(IN) :: anscoeff
    !! coefficient of ans
    REAL(DFP), INTENT(IN) :: scale
    !! coefficient of a \otimes b
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! outerprod
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of data written in ans
  END SUBROUTINE SafeOuterProd_r1r1_
END INTERFACE OuterProd_

!----------------------------------------------------------------------------
!                                                                 OuterProd_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-09
! summary: Outer product of two matrix columns without allocation
!
! Description:
!   Adds the outer product of `a(1:rowA, iColA)` and `b(1:rowB, iColB)` to
!   `ans`. This avoids copying the columns into temporary vectors.
!
!   - `nrow = rowA` and `ncol = rowB`.
!   - Only `ans(1:nrow, 1:ncol)` is written. `ans` must be at least that
!     large. This is not checked.
!
! Arguments:
!   a : REAL(DFP), rank(2) - first matrix, `SIZE(a, 1) >= rowA`
!   b : REAL(DFP), rank(2) - second matrix, `SIZE(b, 1) >= rowB`
!   rowA, rowB : INTEGER(I4B) - number of rows of `a` and `b` to use
!   iColA, iColB : INTEGER(I4B) - column of `a` and column of `b` to use
!   anscoeff : REAL(DFP) - coefficient of the old `ans`
!   scale : REAL(DFP) - coefficient of the outer product
!   ans : REAL(DFP), rank(2), INTENT(INOUT) - updated in place
!   nrow, ncol : INTEGER(I4B), INTENT(OUT) - number of rows and columns
!     written in `ans`
!
! Returns:
!   `ans(1:nrow, 1:ncol)` is updated in place.
!
! Formula:
!   $$\mathrm{ans}_{ij} \leftarrow \alpha\, \mathrm{ans}_{ij}
!     + \beta\, a_{i,c_a}\, b_{j,c_b}$$
!   where $c_a$ is `iColA`, $c_b$ is `iColB`, $\alpha$ is `anscoeff` and
!   $\beta$ is `scale`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ProductUtility, ONLY: OuterProd
! USE ProductUtility, ONLY: OuterProd_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: a(3, 2), b(2, 3), ans(3, 3)
! INTEGER(I4B) :: nrow, ncol
!
! ! columns of a: [1, 2, 3] and [4, 5, 6]
! a = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP], &
!             [3, 2])
! ! columns of b: [1, 2], [3, 4] and [7, 8]
! b = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 7.0_DFP, 8.0_DFP], &
!             [2, 3])
!
! ! ans(i, j) = a(i, 2) * b(j, 3) for i = 1:2, j = 1:2
! ans = 1.0_DFP
! CALL OuterProd_(a=a, b=b, rowA=2_I4B, rowB=2_I4B, iColA=2_I4B, &
!                 iColB=3_I4B, anscoeff=0.0_DFP, scale=1.0_DFP, ans=ans, &
!                 nrow=nrow, ncol=ncol)
!
! CALL OK(nrow .EQ. 2_I4B, "nrow = rowA")
! CALL OK(ncol .EQ. 2_I4B, "ncol = rowB")
! CALL OK(ALL(ABS(ans(1:2, 1:2) - OuterProd(a(1:2, 2), b(1:2, 3))) &
!             .LT. tol), "ans(1:2, 1:2) = a(1:2, 2) x b(1:2, 3)")
! CALL OK(ABS(ans(1, 1) - 28.0_DFP) .LT. tol, "ans(1,1) = 4 * 7")
! CALL OK(ABS(ans(2, 2) - 40.0_DFP) .LT. tol, "ans(2,2) = 5 * 8")
!
! ! entries outside ans(1:2, 1:2) are not changed
! CALL OK(ALL(ABS(ans(3, :) - 1.0_DFP) .LT. tol) .AND. &
!         ALL(ABS(ans(:, 3) - 1.0_DFP) .LT. tol), "rest of ans unchanged")
!
! END PROGRAM main
!```

INTERFACE OuterProd_
  MODULE PURE SUBROUTINE SafeOuterProd_r1r1b_( &
    a, b, rowA, rowB, iColA, iColB, anscoeff, scale, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: a(:, :), b(:, :)
    !! Size of a and b will be used to determine nrow and ncol
    INTEGER(I4B), INTENT(IN) :: rowA, rowB, iColA, iColB
    !! rowA number of rows of a to use
    !! rowB number of rows of b to use
    !! iColA, column index for a
    !! iColB, column index for b
    REAL(DFP), INTENT(IN) :: anscoeff
    !! coefficient of ans
    REAL(DFP), INTENT(IN) :: scale
    !! coefficient of a \otimes b
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! outerprod
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of data written in ans
  END SUBROUTINE SafeOuterProd_r1r1b_
END INTERFACE OuterProd_

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-09
! summary: Outer product of two vectors, or its symmetric part
!
! Description:
!   Returns the outer product of `a` and `b`. If `sym` is true, the symmetric
!   part `(a x b + b x a) / 2` is returned instead.
!
!   - With `sym = .FALSE.` the result is the same as `OuterProd(a, b)`.
!   - With `sym = .TRUE.`, `SIZE(a)` must be equal to `SIZE(b)`. This is not
!     checked.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first vector
!   b : REAL(DFP), rank(1) - second vector
!   sym : LOGICAL(LGT) - if true, return the symmetric part
!
! Returns:
!   ans : REAL(DFP), rank(2) - `SHAPE(ans) = [SIZE(a), SIZE(b)]`
!
! Formula:
!   $$\mathrm{ans}_{ij} = \begin{cases}
!     a_i\, b_j, & \text{if sym is false} \\
!     \frac{1}{2}\left(a_i\, b_j + b_i\, a_j\right), & \text{if sym is true}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: a(3), b(3), ans(3, 3)
!
! a = [1.0_DFP, 2.0_DFP, 3.0_DFP]
! b = [4.0_DFP, 5.0_DFP, 6.0_DFP]
!
! ! sym = .FALSE. gives the ordinary outer product
! ans = OuterProd(a, b, sym=.FALSE.)
! CALL OK(ALL(ABS(ans - OuterProd(a, b)) .LT. tol), "sym = .FALSE.")
! CALL OK(ABS(ans(1, 2) - 5.0_DFP) .LT. tol, "ans(1,2) = a(1) * b(2)")
!
! ! sym = .TRUE. gives the symmetric part
! ans = OuterProd(a, b, sym=.TRUE.)
! CALL OK(ALL(ABS(ans - TRANSPOSE(ans)) .LT. tol), "ans is symmetric")
! CALL OK(ALL(ABS(ans - 0.5_DFP * (OuterProd(a, b) + OuterProd(b, a))) &
!             .LT. tol), "ans = (a x b + b x a) / 2")
!
! ! entries computed by hand
! CALL OK(ABS(ans(1, 1) - 4.0_DFP) .LT. tol, "ans(1,1) = 1 * 4")
! CALL OK(ABS(ans(1, 2) - 6.5_DFP) .LT. tol, "ans(1,2) = (1*5 + 4*2) / 2")
! CALL OK(ABS(ans(3, 3) - 18.0_DFP) .LT. tol, "ans(3,3) = 3 * 6")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1s(a, b, sym) RESULT(ans)
    ! Define INTENT of dummy variables
    REAL(DFP), INTENT(IN) :: a(:), b(:)
    REAL(DFP), DIMENSION(SIZE(a), SIZE(b)) :: ans
    LOGICAL(LGT), INTENT(IN) :: sym
  END FUNCTION OuterProd_r1r1s
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                 OuterProd_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-09
! summary: Symmetric or plain outer product of two vectors, no allocation
!
! Description:
!   Adds the outer product of `a` and `b` (or its symmetric part when `sym` is
!   true) to `ans`, that is `ans = anscoeff * ans + scale * OuterProd(a, b,
!   sym)`.
!
!   - `nrow = SIZE(a)` and `ncol = SIZE(b)`.
!   - Only `ans(1:nrow, 1:ncol)` is written. `ans` must be at least that
!     large. This is not checked.
!   - With `sym = .TRUE.`, `SIZE(a)` must be equal to `SIZE(b)`. This is not
!     checked.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first vector
!   b : REAL(DFP), rank(1) - second vector
!   sym : LOGICAL(LGT) - if true, use the symmetric part
!   anscoeff : REAL(DFP) - coefficient of the old `ans`
!   scale : REAL(DFP) - coefficient of the product
!   ans : REAL(DFP), rank(2), INTENT(INOUT) - updated in place
!   nrow, ncol : INTEGER(I4B), INTENT(OUT) - number of rows and columns
!     written in `ans`
!
! Returns:
!   `ans(1:nrow, 1:ncol)` is updated in place.
!
! Formula:
!   $$\mathrm{ans}_{ij} \leftarrow \alpha\, \mathrm{ans}_{ij} + \begin{cases}
!     \beta\, a_i\, b_j, & \text{if sym is false} \\
!     \frac{\beta}{2}\left(a_i\, b_j + b_i\, a_j\right),
!     & \text{if sym is true}
!   \end{cases}$$
!   where $\alpha$ is `anscoeff` and $\beta$ is `scale`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ProductUtility, ONLY: OuterProd
! USE ProductUtility, ONLY: OuterProd_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: a(3), b(3), ans(4, 4)
! INTEGER(I4B) :: nrow, ncol
!
! a = [1.0_DFP, 2.0_DFP, 3.0_DFP]
! b = [4.0_DFP, 5.0_DFP, 6.0_DFP]
!
! ! sym = .TRUE.: ans = 0 * ans + 2 * (a x b + b x a) / 2
! ans = 1.0_DFP
! CALL OuterProd_(a=a, b=b, sym=.TRUE., anscoeff=0.0_DFP, scale=2.0_DFP, &
!                 ans=ans, nrow=nrow, ncol=ncol)
!
! CALL OK(nrow .EQ. 3_I4B, "nrow = SIZE(a)")
! CALL OK(ncol .EQ. 3_I4B, "ncol = SIZE(b)")
! CALL OK(ALL(ABS(ans(1:3, 1:3) - (OuterProd(a, b) + OuterProd(b, a))) &
!             .LT. tol), "symmetric part")
! CALL OK(ABS(ans(1, 2) - 13.0_DFP) .LT. tol, "ans(1,2) = 1*5 + 4*2")
!
! ! entries outside ans(1:3, 1:3) are not changed
! CALL OK(ALL(ABS(ans(4, :) - 1.0_DFP) .LT. tol) .AND. &
!         ALL(ABS(ans(:, 4) - 1.0_DFP) .LT. tol), "rest of ans unchanged")
!
! ! sym = .FALSE.: ans = 1 * ans + 1 * (a x b)
! CALL OuterProd_(a=a, b=b, sym=.FALSE., anscoeff=1.0_DFP, scale=1.0_DFP, &
!                 ans=ans, nrow=nrow, ncol=ncol)
! CALL OK(ABS(ans(1, 2) - 18.0_DFP) .LT. tol, "ans(1,2) = 13 + 1*5")
!
! END PROGRAM main
!```

INTERFACE OuterProd_
  MODULE PURE SUBROUTINE OuterProd_r1r1s_( &
    a, b, sym, anscoeff, scale, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: a(:), b(:)
    LOGICAL(LGT), INTENT(IN) :: sym
    REAL(DFP), INTENT(IN) :: anscoeff
    REAL(DFP), INTENT(IN) :: scale
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE OuterProd_r1r1s_
END INTERFACE OuterProd_

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1 and rank-2 arrays
!
! Description:
!   Returns the outer (tensor) product of `a` and `b`. The result has rank 3.
!   Its shape is `[SHAPE(a), SHAPE(b)]`, so the indices of the operands follow
!   one after another.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(2) - second operand
!
! Returns:
!   ans : REAL(DFP), rank(3) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3} =
!     a_{i_1}\, b_{i_2 i_3}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(3) = [2, 3, 2]
! REAL(DFP) :: a(2), b(3, 2)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [3, 2])
!
! ans = OuterProd(a, b)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i3 = 1, 2
!   DO i2 = 1, 3
!     DO i1 = 1, 2
!       want = a(i1) * b(i2, i3)
!       isok = isok .AND. (ABS(ans(i1, i2, i3) - want) .LT. tol)
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2) - 12.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r2(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP) :: ans(SIZE(a), SIZE(b, 1), SIZE(b, 2))
  END FUNCTION OuterProd_r1r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                  OuterProd_
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-05
! summary: Outer product of rank-1 and rank-2 arrays, no allocation
!
! Description:
!   Adds the outer product of `a` and `b` to `ans`, that is `ans = anscoeff *
!   ans + scale * OuterProd(a, b)`.
!
!   - Only the leading block `ans(1:dim1, 1:dim2, 1:dim3)` is written. The
!     other entries of `ans` are not changed.
!   - `ans` must be at least as large as the result. This is not checked.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(2) - second operand
!   anscoeff : REAL(DFP) - coefficient of the old value of `ans`
!   scale : REAL(DFP) - coefficient of the outer product
!   ans : REAL(DFP), rank(3), INTENT(INOUT) - updated in place, `SIZE(ans, k)`
!     is at least the k-th extent of the result
!   dim1 : INTEGER(I4B), INTENT(OUT) - `dim1 = SIZE(a)`
!   dim2 : INTEGER(I4B), INTENT(OUT) - `dim2 = SIZE(b, 1)`
!   dim3 : INTEGER(I4B), INTENT(OUT) - `dim3 = SIZE(b, 2)`
!
! Returns:
!   `ans(1:dim1, 1:dim2, 1:dim3)` is overwritten with the new value; `dim1`,
!   `dim2`, `dim3` hold the extents written in `ans`.
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3} \leftarrow
!     \alpha\, \mathrm{ans}_{i_1 i_2 i_3}
!     + \beta\, a_{i_1}\, b_{i_2 i_3}$$
!   where $\alpha$ is `anscoeff` and $\beta$ is `scale`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE ProductUtility, ONLY: OuterProd_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(3) = [2, 3, 2]
! REAL(DFP) :: a(2), b(3, 2)
! REAL(DFP) :: ans(3, 4, 3)
! REAL(DFP) :: want
! INTEGER(I4B) :: dim1, dim2, dim3
! INTEGER(I4B) :: ii, i1, i2, i3
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [3, 2])
!
! ! ans is one larger than needed in every dimension
! ans = 1.0_DFP
!
! ! ans = 2 * ans + 3 * (outer product)
! CALL OuterProd_(a=a, b=b, anscoeff=2.0_DFP, scale=3.0_DFP, &
!                 ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
!
! CALL OK(ALL([dim1, dim2, dim3] .EQ. expected), "sizes written in ans")
!
! isok = .TRUE.
! DO i3 = 1, 2
!   DO i2 = 1, 3
!     DO i1 = 1, 2
!       want = 2.0_DFP + 3.0_DFP * a(i1) * b(i2, i3)
!       isok = isok .AND. (ABS(ans(i1, i2, i3) - want) .LT. tol)
!     END DO
!   END DO
! END DO
! CALL OK(isok, "leading block of ans")
! CALL OK(ABS(ans(2, 3, 2) - 38.0_DFP) .LT. tol, "last entry")
!
! ! entries outside the leading block are not changed
! isok = .TRUE.
! isok = isok .AND. ALL(ABS(ans(3, :, :) - 1.0_DFP) .LT. tol)
! isok = isok .AND. ALL(ABS(ans(:, 4, :) - 1.0_DFP) .LT. tol)
! isok = isok .AND. ALL(ABS(ans(:, :, 3) - 1.0_DFP) .LT. tol)
! CALL OK(isok, "rest of ans is unchanged")
!
! ! with anscoeff = 0 and scale = 1 we get OuterProd
! CALL OuterProd_(a=a, b=b, anscoeff=0.0_DFP, scale=1.0_DFP, &
!                 ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
! CALL OK(ALL(ABS(ans(1:2, 1:3, 1:2) - OuterProd(a, b)) &
!         .LT. tol), "same as OuterProd")
!
! END PROGRAM main
!```

INTERFACE OuterProd_
  MODULE PURE SUBROUTINE OuterProd_r1r2_(a, b, anscoeff, scale, ans, &
                                         dim1, dim2, dim3)
    REAL(DFP), INTENT(IN) :: a(:)
    !! a vector
    REAL(DFP), INTENT(IN) :: b(:, :)
    !! a matrix
    REAL(DFP), INTENT(IN) :: anscoeff
    !! coefficient of ans
    REAL(DFP), INTENT(IN) :: scale
    !! scale
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! outer product
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! the upperbound of data written in ans
  END SUBROUTINE OuterProd_r1r2_
END INTERFACE OuterProd_

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1 and rank-3 arrays
!
! Description:
!   Returns the outer (tensor) product of `a` and `b`. The result has rank 4.
!   Its shape is `[SHAPE(a), SHAPE(b)]`, so the indices of the operands follow
!   one after another.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(3) - second operand
!
! Returns:
!   ans : REAL(DFP), rank(4) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4} =
!     a_{i_1}\, b_{i_2 i_3 i_4}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(4) = [2, 3, 2, 3]
! REAL(DFP) :: a(2), b(3, 2, 3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = RESHAPE([(REAL(ii, DFP), ii=1, 18)], [3, 2, 3])
!
! ans = OuterProd(a, b)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i4 = 1, 3
!   DO i3 = 1, 2
!     DO i2 = 1, 3
!       DO i1 = 1, 2
!         want = a(i1) * b(i2, i3, i4)
!         isok = isok .AND. (ABS(ans(i1, i2, i3, i4) - want) .LT. tol)
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3) - 36.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r3(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :, :)
    REAL(DFP) :: ans(SIZE(a), SIZE(b, 1), SIZE(b, 2), SIZE(b, 3))
  END FUNCTION OuterProd_r1r3
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1 and rank-4 arrays
!
! Description:
!   Returns the outer (tensor) product of `a` and `b`. The result has rank 5.
!   Its shape is `[SHAPE(a), SHAPE(b)]`, so the indices of the operands follow
!   one after another.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(4) - second operand
!
! Returns:
!   ans : REAL(DFP), rank(5) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5} =
!     a_{i_1}\, b_{i_2 i_3 i_4 i_5}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(5) = [2, 3, 2, 3, 2]
! REAL(DFP) :: a(2), b(3, 2, 3, 2)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = RESHAPE([(REAL(ii, DFP), ii=1, 36)], [3, 2, 3, 2])
!
! ans = OuterProd(a, b)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i5 = 1, 2
!   DO i4 = 1, 3
!     DO i3 = 1, 2
!       DO i2 = 1, 3
!         DO i1 = 1, 2
!           want = a(i1) * b(i2, i3, i4, i5)
!           isok = isok .AND. (ABS(ans(i1, i2, i3, i4, i5) - want) .LT. tol)
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2) - 72.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r4(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :, :, :)
    REAL(DFP) :: ans(SIZE(a), SIZE(b, 1), SIZE(b, 2), SIZE(b, 3), SIZE(b, 4))
  END FUNCTION OuterProd_r1r4
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1 and rank-5 arrays
!
! Description:
!   Returns the outer (tensor) product of `a` and `b`. The result has rank 6.
!   Its shape is `[SHAPE(a), SHAPE(b)]`, so the indices of the operands follow
!   one after another.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(5) - second operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1}\, b_{i_2 i_3 i_4 i_5 i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2), b(3, 2, 3, 2, 3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = RESHAPE([(REAL(ii, DFP), ii=1, 108)], [3, 2, 3, 2, 3])
!
! ans = OuterProd(a, b)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1) * b(i2, i3, i4, i5, i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r5(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :, :, :, :)
    REAL(DFP) :: ans(SIZE(a), SIZE(b, 1), SIZE(b, 2), SIZE(b, 3), &
                     SIZE(b, 4), SIZE(b, 5))
  END FUNCTION OuterProd_r1r5
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Outer product of rank-2 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a` and `b`. The result has rank 3.
!   Its shape is `[SHAPE(a), SHAPE(b)]`, so the indices of the operands follow
!   one after another.
!
! Arguments:
!   a : REAL(DFP), rank(2) - first operand
!   b : REAL(DFP), rank(1) - second operand
!
! Returns:
!   ans : REAL(DFP), rank(3) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3} =
!     a_{i_1 i_2}\, b_{i_3}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(3) = [2, 3, 2]
! REAL(DFP) :: a(2, 3), b(2)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! b = [(REAL(ii, DFP), ii=1, 2)]
!
! ans = OuterProd(a, b)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i3 = 1, 2
!   DO i2 = 1, 3
!     DO i1 = 1, 2
!       want = a(i1, i2) * b(i3)
!       isok = isok .AND. (ABS(ans(i1, i2, i3) - want) .LT. tol)
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2) - 12.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r1(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(b))
  END FUNCTION OuterProd_r2r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                 OuterProd_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-04
! summary: Outer product of rank-2 and rank-1 arrays, no allocation
!
! Description:
!   Adds the outer product of `a` and `b` to `ans`, that is `ans = anscoeff *
!   ans + scale * OuterProd(a, b)`.
!
!   - Only the leading block `ans(1:dim1, 1:dim2, 1:dim3)` is written. The
!     other entries of `ans` are not changed.
!   - `ans` must be at least as large as the result. This is not checked.
!
! Arguments:
!   a : REAL(DFP), rank(2) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   anscoeff : REAL(DFP) - coefficient of the old value of `ans`
!   scale : REAL(DFP) - coefficient of the outer product
!   ans : REAL(DFP), rank(3), INTENT(INOUT) - updated in place, `SIZE(ans, k)`
!     is at least the k-th extent of the result
!   dim1 : INTEGER(I4B), INTENT(OUT) - `dim1 = SIZE(a, 1)`
!   dim2 : INTEGER(I4B), INTENT(OUT) - `dim2 = SIZE(a, 2)`
!   dim3 : INTEGER(I4B), INTENT(OUT) - `dim3 = SIZE(b)`
!
! Returns:
!   `ans(1:dim1, 1:dim2, 1:dim3)` is overwritten with the new value; `dim1`,
!   `dim2`, `dim3` hold the extents written in `ans`.
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3} \leftarrow
!     \alpha\, \mathrm{ans}_{i_1 i_2 i_3}
!     + \beta\, a_{i_1 i_2}\, b_{i_3}$$
!   where $\alpha$ is `anscoeff` and $\beta$ is `scale`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE ProductUtility, ONLY: OuterProd_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(3) = [2, 3, 2]
! REAL(DFP) :: a(2, 3), b(2)
! REAL(DFP) :: ans(3, 4, 3)
! REAL(DFP) :: want
! INTEGER(I4B) :: dim1, dim2, dim3
! INTEGER(I4B) :: ii, i1, i2, i3
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! b = [(REAL(ii, DFP), ii=1, 2)]
!
! ! ans is one larger than needed in every dimension
! ans = 1.0_DFP
!
! ! ans = 2 * ans + 3 * (outer product)
! CALL OuterProd_(a=a, b=b, anscoeff=2.0_DFP, scale=3.0_DFP, &
!                 ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
!
! CALL OK(ALL([dim1, dim2, dim3] .EQ. expected), "sizes written in ans")
!
! isok = .TRUE.
! DO i3 = 1, 2
!   DO i2 = 1, 3
!     DO i1 = 1, 2
!       want = 2.0_DFP + 3.0_DFP * a(i1, i2) * b(i3)
!       isok = isok .AND. (ABS(ans(i1, i2, i3) - want) .LT. tol)
!     END DO
!   END DO
! END DO
! CALL OK(isok, "leading block of ans")
! CALL OK(ABS(ans(2, 3, 2) - 38.0_DFP) .LT. tol, "last entry")
!
! ! entries outside the leading block are not changed
! isok = .TRUE.
! isok = isok .AND. ALL(ABS(ans(3, :, :) - 1.0_DFP) .LT. tol)
! isok = isok .AND. ALL(ABS(ans(:, 4, :) - 1.0_DFP) .LT. tol)
! isok = isok .AND. ALL(ABS(ans(:, :, 3) - 1.0_DFP) .LT. tol)
! CALL OK(isok, "rest of ans is unchanged")
!
! ! with anscoeff = 0 and scale = 1 we get OuterProd
! CALL OuterProd_(a=a, b=b, anscoeff=0.0_DFP, scale=1.0_DFP, &
!                 ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
! CALL OK(ALL(ABS(ans(1:2, 1:3, 1:2) - OuterProd(a, b)) &
!         .LT. tol), "same as OuterProd")
!
! END PROGRAM main
!```

INTERFACE OuterProd_
  MODULE PURE SUBROUTINE OuterProd_r2r1_( &
    a, b, anscoeff, scale, ans, dim1, dim2, dim3)
    REAL(DFP), INTENT(IN) :: a(:, :)
    !! rank 2 array
    REAL(DFP), INTENT(IN) :: b(:)
    !! a vector
    REAL(DFP), INTENT(IN) :: anscoeff
    !! coefficient of ans
    REAL(DFP), INTENT(IN) :: scale
    !! scale
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! outer product
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! upper bound of data written in ans
  END SUBROUTINE OuterProd_r2r1_
END INTERFACE OuterProd_

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-2 and rank-2 arrays
!
! Description:
!   Returns the outer (tensor) product of `a` and `b`. The result has rank 4.
!   Its shape is `[SHAPE(a), SHAPE(b)]`, so the indices of the operands follow
!   one after another.
!
! Arguments:
!   a : REAL(DFP), rank(2) - first operand
!   b : REAL(DFP), rank(2) - second operand
!
! Returns:
!   ans : REAL(DFP), rank(4) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4} =
!     a_{i_1 i_2}\, b_{i_3 i_4}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(4) = [2, 3, 2, 3]
! REAL(DFP) :: a(2, 3), b(2, 3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! b = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
!
! ans = OuterProd(a, b)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i4 = 1, 3
!   DO i3 = 1, 2
!     DO i2 = 1, 3
!       DO i1 = 1, 2
!         want = a(i1, i2) * b(i3, i4)
!         isok = isok .AND. (ABS(ans(i1, i2, i3, i4) - want) .LT. tol)
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3) - 36.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r2(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(b, 1), SIZE(b, 2))
  END FUNCTION OuterProd_r2r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                  OuterProd_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-2 and rank-2 arrays, no allocation
!
! Description:
!   Adds the outer product of `a` and `b` to `ans`, that is `ans = anscoeff *
!   ans + scale * OuterProd(a, b)`.
!
!   - Only the leading block `ans(1:dim1, 1:dim2, 1:dim3, 1:dim4)` is written.
!     The other entries of `ans` are not changed.
!   - `ans` must be at least as large as the result. This is not checked.
!   - `SIZE(b, 2)` should be at least one, otherwise `dim1, dim2, dim3` are
!     not set.
!
! Arguments:
!   a : REAL(DFP), rank(2) - first operand
!   b : REAL(DFP), rank(2) - second operand
!   anscoeff : REAL(DFP) - coefficient of the old value of `ans`
!   scale : REAL(DFP) - coefficient of the outer product
!   ans : REAL(DFP), rank(4), INTENT(INOUT) - updated in place, `SIZE(ans, k)`
!     is at least the k-th extent of the result
!   dim1 : INTEGER(I4B), INTENT(OUT) - `dim1 = SIZE(a, 1)`
!   dim2 : INTEGER(I4B), INTENT(OUT) - `dim2 = SIZE(a, 2)`
!   dim3 : INTEGER(I4B), INTENT(OUT) - `dim3 = SIZE(b, 1)`
!   dim4 : INTEGER(I4B), INTENT(OUT) - `dim4 = SIZE(b, 2)`
!
! Returns:
!   `ans(1:dim1, 1:dim2, 1:dim3, 1:dim4)` is overwritten with the new value;
!   `dim1`, `dim2`, `dim3`, `dim4` hold the extents written in `ans`.
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4} \leftarrow
!     \alpha\, \mathrm{ans}_{i_1 i_2 i_3 i_4}
!     + \beta\, a_{i_1 i_2}\, b_{i_3 i_4}$$
!   where $\alpha$ is `anscoeff` and $\beta$ is `scale`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE ProductUtility, ONLY: OuterProd_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(4) = [2, 3, 2, 3]
! REAL(DFP) :: a(2, 3), b(2, 3)
! REAL(DFP) :: ans(3, 4, 3, 4)
! REAL(DFP) :: want
! INTEGER(I4B) :: dim1, dim2, dim3, dim4
! INTEGER(I4B) :: ii, i1, i2, i3, i4
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! b = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
!
! ! ans is one larger than needed in every dimension
! ans = 1.0_DFP
!
! ! ans = 2 * ans + 3 * (outer product)
! CALL OuterProd_(a=a, b=b, anscoeff=2.0_DFP, scale=3.0_DFP, &
!                 ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4)
!
! CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. expected), "sizes written in ans")
!
! isok = .TRUE.
! DO i4 = 1, 3
!   DO i3 = 1, 2
!     DO i2 = 1, 3
!       DO i1 = 1, 2
!         want = 2.0_DFP + 3.0_DFP * a(i1, i2) * b(i3, i4)
!         isok = isok .AND. (ABS(ans(i1, i2, i3, i4) - want) .LT. tol)
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "leading block of ans")
! CALL OK(ABS(ans(2, 3, 2, 3) - 110.0_DFP) .LT. tol, "last entry")
!
! ! entries outside the leading block are not changed
! isok = .TRUE.
! isok = isok .AND. ALL(ABS(ans(3, :, :, :) - 1.0_DFP) .LT. tol)
! isok = isok .AND. ALL(ABS(ans(:, 4, :, :) - 1.0_DFP) .LT. tol)
! isok = isok .AND. ALL(ABS(ans(:, :, 3, :) - 1.0_DFP) .LT. tol)
! isok = isok .AND. ALL(ABS(ans(:, :, :, 4) - 1.0_DFP) .LT. tol)
! CALL OK(isok, "rest of ans is unchanged")
!
! ! with anscoeff = 0 and scale = 1 we get OuterProd
! CALL OuterProd_(a=a, b=b, anscoeff=0.0_DFP, scale=1.0_DFP, &
!                 ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4)
! CALL OK(ALL(ABS(ans(1:2, 1:3, 1:2, 1:3) - OuterProd(a, b)) &
!         .LT. tol), "same as OuterProd")
!
! END PROGRAM main
!```

INTERFACE OuterProd_
  MODULE PURE SUBROUTINE OuterProd_r2r2_( &
    a, b, ans, dim1, dim2, dim3, dim4, anscoeff, scale)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
    REAL(DFP), INTENT(IN) :: anscoeff, scale
  END SUBROUTINE OuterProd_r2r2_
END INTERFACE OuterProd_

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-2 and rank-3 arrays
!
! Description:
!   Returns the outer (tensor) product of `a` and `b`. The result has rank 5.
!   Its shape is `[SHAPE(a), SHAPE(b)]`, so the indices of the operands follow
!   one after another.
!
! Arguments:
!   a : REAL(DFP), rank(2) - first operand
!   b : REAL(DFP), rank(3) - second operand
!
! Returns:
!   ans : REAL(DFP), rank(5) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5} =
!     a_{i_1 i_2}\, b_{i_3 i_4 i_5}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(5) = [2, 3, 2, 3, 2]
! REAL(DFP) :: a(2, 3), b(2, 3, 2)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! b = RESHAPE([(REAL(ii, DFP), ii=1, 12)], [2, 3, 2])
!
! ans = OuterProd(a, b)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i5 = 1, 2
!   DO i4 = 1, 3
!     DO i3 = 1, 2
!       DO i2 = 1, 3
!         DO i1 = 1, 2
!           want = a(i1, i2) * b(i3, i4, i5)
!           isok = isok .AND. (ABS(ans(i1, i2, i3, i4, i5) - want) .LT. tol)
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2) - 72.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r3(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:, :, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(b, 1), SIZE(b, 2), &
                     SIZE(b, 3))
  END FUNCTION OuterProd_r2r3
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-2 and rank-4 arrays
!
! Description:
!   Returns the outer (tensor) product of `a` and `b`. The result has rank 6.
!   Its shape is `[SHAPE(a), SHAPE(b)]`, so the indices of the operands follow
!   one after another.
!
! Arguments:
!   a : REAL(DFP), rank(2) - first operand
!   b : REAL(DFP), rank(4) - second operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1 i_2}\, b_{i_3 i_4 i_5 i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2, 3), b(2, 3, 2, 3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! b = RESHAPE([(REAL(ii, DFP), ii=1, 36)], [2, 3, 2, 3])
!
! ans = OuterProd(a, b)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1, i2) * b(i3, i4, i5, i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r4(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:, :, :, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(b, 1), SIZE(b, 2), &
                     SIZE(b, 3), SIZE(b, 4))
  END FUNCTION OuterProd_r2r4
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-3 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a` and `b`. The result has rank 4.
!   Its shape is `[SHAPE(a), SHAPE(b)]`, so the indices of the operands follow
!   one after another.
!
! Arguments:
!   a : REAL(DFP), rank(3) - first operand
!   b : REAL(DFP), rank(1) - second operand
!
! Returns:
!   ans : REAL(DFP), rank(4) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4} =
!     a_{i_1 i_2 i_3}\, b_{i_4}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(4) = [2, 3, 2, 3]
! REAL(DFP) :: a(2, 3, 2), b(3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 12)], [2, 3, 2])
! b = [(REAL(ii, DFP), ii=1, 3)]
!
! ans = OuterProd(a, b)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i4 = 1, 3
!   DO i3 = 1, 2
!     DO i2 = 1, 3
!       DO i1 = 1, 2
!         want = a(i1, i2, i3) * b(i4)
!         isok = isok .AND. (ABS(ans(i1, i2, i3, i4) - want) .LT. tol)
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3) - 36.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r3r1(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3), SIZE(b))
  END FUNCTION OuterProd_r3r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-3 and rank-2 arrays
!
! Description:
!   Returns the outer (tensor) product of `a` and `b`. The result has rank 5.
!   Its shape is `[SHAPE(a), SHAPE(b)]`, so the indices of the operands follow
!   one after another.
!
! Arguments:
!   a : REAL(DFP), rank(3) - first operand
!   b : REAL(DFP), rank(2) - second operand
!
! Returns:
!   ans : REAL(DFP), rank(5) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5} =
!     a_{i_1 i_2 i_3}\, b_{i_4 i_5}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(5) = [2, 3, 2, 3, 2]
! REAL(DFP) :: a(2, 3, 2), b(3, 2)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 12)], [2, 3, 2])
! b = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [3, 2])
!
! ans = OuterProd(a, b)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i5 = 1, 2
!   DO i4 = 1, 3
!     DO i3 = 1, 2
!       DO i2 = 1, 3
!         DO i1 = 1, 2
!           want = a(i1, i2, i3) * b(i4, i5)
!           isok = isok .AND. (ABS(ans(i1, i2, i3, i4, i5) - want) .LT. tol)
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2) - 72.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r3r2(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3), SIZE(b, 1), &
                     SIZE(b, 2))
  END FUNCTION OuterProd_r3r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-3 and rank-3 arrays
!
! Description:
!   Returns the outer (tensor) product of `a` and `b`. The result has rank 6.
!   Its shape is `[SHAPE(a), SHAPE(b)]`, so the indices of the operands follow
!   one after another.
!
! Arguments:
!   a : REAL(DFP), rank(3) - first operand
!   b : REAL(DFP), rank(3) - second operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1 i_2 i_3}\, b_{i_4 i_5 i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2, 3, 2), b(3, 2, 3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 12)], [2, 3, 2])
! b = RESHAPE([(REAL(ii, DFP), ii=1, 18)], [3, 2, 3])
!
! ans = OuterProd(a, b)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1, i2, i3) * b(i4, i5, i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r3r3(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :)
    REAL(DFP), INTENT(IN) :: b(:, :, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3), SIZE(b, 1), &
                     SIZE(b, 2), SIZE(b, 3))
  END FUNCTION OuterProd_r3r3
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-4 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a` and `b`. The result has rank 5.
!   Its shape is `[SHAPE(a), SHAPE(b)]`, so the indices of the operands follow
!   one after another.
!
! Arguments:
!   a : REAL(DFP), rank(4) - first operand
!   b : REAL(DFP), rank(1) - second operand
!
! Returns:
!   ans : REAL(DFP), rank(5) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5} =
!     a_{i_1 i_2 i_3 i_4}\, b_{i_5}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(5) = [2, 3, 2, 3, 2]
! REAL(DFP) :: a(2, 3, 2, 3), b(2)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 36)], [2, 3, 2, 3])
! b = [(REAL(ii, DFP), ii=1, 2)]
!
! ans = OuterProd(a, b)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i5 = 1, 2
!   DO i4 = 1, 3
!     DO i3 = 1, 2
!       DO i2 = 1, 3
!         DO i1 = 1, 2
!           want = a(i1, i2, i3, i4) * b(i5)
!           isok = isok .AND. (ABS(ans(i1, i2, i3, i4, i5) - want) .LT. tol)
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2) - 72.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r4r1(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3), SIZE(a, 4), &
                     SIZE(b, 1))
  END FUNCTION OuterProd_r4r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-4 and rank-2 arrays
!
! Description:
!   Returns the outer (tensor) product of `a` and `b`. The result has rank 6.
!   Its shape is `[SHAPE(a), SHAPE(b)]`, so the indices of the operands follow
!   one after another.
!
! Arguments:
!   a : REAL(DFP), rank(4) - first operand
!   b : REAL(DFP), rank(2) - second operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1 i_2 i_3 i_4}\, b_{i_5 i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2, 3, 2, 3), b(2, 3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 36)], [2, 3, 2, 3])
! b = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
!
! ans = OuterProd(a, b)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1, i2, i3, i4) * b(i5, i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r4r2(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3), SIZE(a, 4), &
                     SIZE(b, 1), SIZE(b, 2))
  END FUNCTION OuterProd_r4r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-5 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a` and `b`. The result has rank 6.
!   Its shape is `[SHAPE(a), SHAPE(b)]`, so the indices of the operands follow
!   one after another.
!
! Arguments:
!   a : REAL(DFP), rank(5) - first operand
!   b : REAL(DFP), rank(1) - second operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1 i_2 i_3 i_4 i_5}\, b_{i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2, 3, 2, 3, 2), b(3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 72)], [2, 3, 2, 3, 2])
! b = [(REAL(ii, DFP), ii=1, 3)]
!
! ans = OuterProd(a, b)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1, i2, i3, i4, i5) * b(i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r5r1(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :, :, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3), SIZE(a, 4), &
                     SIZE(a, 5), SIZE(b, 1))
  END FUNCTION OuterProd_r5r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-1 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b` and `c`. The result has
!   rank 3. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c)]`, so the indices of
!   the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, b), c)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(1) - third operand
!
! Returns:
!   ans : REAL(DFP), rank(3) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3} =
!     a_{i_1}\, b_{i_2}\, c_{i_3}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(3) = [2, 3, 2]
! REAL(DFP) :: a(2), b(3), c(2)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = [(REAL(ii, DFP), ii=1, 3)]
! c = [(REAL(ii, DFP), ii=1, 2)]
!
! ans = OuterProd(a, b, c)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i3 = 1, 2
!   DO i2 = 1, 3
!     DO i1 = 1, 2
!       want = a(i1) * b(i2) * c(i3)
!       isok = isok .AND. (ABS(ans(i1, i2, i3) - want) .LT. tol)
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2) - 12.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(c, 1))
  END FUNCTION OuterProd_r1r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-1 and rank-1 arrays, no allocation
!
! Description:
!   Adds the outer product of `a`, `b` and `c` to `ans`, that is `ans =
!   anscoeff * ans + scale * OuterProd(a, b, c)`.
!
!   - Only the leading block `ans(1:dim1, 1:dim2, 1:dim3)` is written. The
!     other entries of `ans` are not changed.
!   - `ans` must be at least as large as the result. This is not checked.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(1) - third operand
!   anscoeff : REAL(DFP) - coefficient of the old value of `ans`
!   scale : REAL(DFP) - coefficient of the outer product
!   ans : REAL(DFP), rank(3), INTENT(INOUT) - updated in place, `SIZE(ans, k)`
!     is at least the k-th extent of the result
!   dim1 : INTEGER(I4B), INTENT(OUT) - `dim1 = SIZE(a)`
!   dim2 : INTEGER(I4B), INTENT(OUT) - `dim2 = SIZE(b)`
!   dim3 : INTEGER(I4B), INTENT(OUT) - `dim3 = SIZE(c)`
!
! Returns:
!   `ans(1:dim1, 1:dim2, 1:dim3)` is overwritten with the new value; `dim1`,
!   `dim2`, `dim3` hold the extents written in `ans`.
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3} \leftarrow
!     \alpha\, \mathrm{ans}_{i_1 i_2 i_3}
!     + \beta\, a_{i_1}\, b_{i_2}\, c_{i_3}$$
!   where $\alpha$ is `anscoeff` and $\beta$ is `scale`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE ProductUtility, ONLY: OuterProd_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(3) = [2, 3, 2]
! REAL(DFP) :: a(2), b(3), c(2)
! REAL(DFP) :: ans(3, 4, 3)
! REAL(DFP) :: want
! INTEGER(I4B) :: dim1, dim2, dim3
! INTEGER(I4B) :: ii, i1, i2, i3
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = [(REAL(ii, DFP), ii=1, 3)]
! c = [(REAL(ii, DFP), ii=1, 2)]
!
! ! ans is one larger than needed in every dimension
! ans = 1.0_DFP
!
! ! ans = 2 * ans + 3 * (outer product)
! CALL OuterProd_(a=a, b=b, c=c, anscoeff=2.0_DFP, scale=3.0_DFP, &
!                 ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
!
! CALL OK(ALL([dim1, dim2, dim3] .EQ. expected), "sizes written in ans")
!
! isok = .TRUE.
! DO i3 = 1, 2
!   DO i2 = 1, 3
!     DO i1 = 1, 2
!       want = 2.0_DFP + 3.0_DFP * a(i1) * b(i2) * c(i3)
!       isok = isok .AND. (ABS(ans(i1, i2, i3) - want) .LT. tol)
!     END DO
!   END DO
! END DO
! CALL OK(isok, "leading block of ans")
! CALL OK(ABS(ans(2, 3, 2) - 38.0_DFP) .LT. tol, "last entry")
!
! ! entries outside the leading block are not changed
! isok = .TRUE.
! isok = isok .AND. ALL(ABS(ans(3, :, :) - 1.0_DFP) .LT. tol)
! isok = isok .AND. ALL(ABS(ans(:, 4, :) - 1.0_DFP) .LT. tol)
! isok = isok .AND. ALL(ABS(ans(:, :, 3) - 1.0_DFP) .LT. tol)
! CALL OK(isok, "rest of ans is unchanged")
!
! ! with anscoeff = 0 and scale = 1 we get OuterProd
! CALL OuterProd_(a=a, b=b, c=c, anscoeff=0.0_DFP, scale=1.0_DFP, &
!                 ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
! CALL OK(ALL(ABS(ans(1:2, 1:3, 1:2) - OuterProd(a, b, c)) &
!         .LT. tol), "same as OuterProd")
!
! END PROGRAM main
!```

INTERFACE OuterProd_
  MODULE PURE SUBROUTINE OuterProd_r1r1r1_( &
    a, b, c, anscoeff, scale, ans, dim1, dim2, dim3)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    REAL(DFP), INTENT(IN) :: anscoeff, scale
  END SUBROUTINE OuterProd_r1r1r1_
END INTERFACE OuterProd_

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-1 and rank-2 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b` and `c`. The result has
!   rank 4. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c)]`, so the indices of
!   the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, b), c)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(2) - third operand
!
! Returns:
!   ans : REAL(DFP), rank(4) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4} =
!     a_{i_1}\, b_{i_2}\, c_{i_3 i_4}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(4) = [2, 3, 2, 3]
! REAL(DFP) :: a(2), b(3), c(2, 3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = [(REAL(ii, DFP), ii=1, 3)]
! c = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
!
! ans = OuterProd(a, b, c)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i4 = 1, 3
!   DO i3 = 1, 2
!     DO i2 = 1, 3
!       DO i1 = 1, 2
!         want = a(i1) * b(i2) * c(i3, i4)
!         isok = isok .AND. (ABS(ans(i1, i2, i3, i4) - want) .LT. tol)
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3) - 36.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r2(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(c, 1), SIZE(c, 2))
  END FUNCTION OuterProd_r1r1r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                 OuterProd_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-1 and rank-2 arrays, no allocation
!
! Description:
!   Adds the outer product of `a`, `b` and `c` to `ans`, that is `ans =
!   anscoeff * ans + scale * OuterProd(a, b, c)`.
!
!   - Only the leading block `ans(1:dim1, 1:dim2, 1:dim3, 1:dim4)` is written.
!     The other entries of `ans` are not changed.
!   - `ans` must be at least as large as the result. This is not checked.
!   - `SIZE(c, 2)` should be at least one, otherwise `dim1, dim2, dim3` are
!     not set.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(2) - third operand
!   anscoeff : REAL(DFP) - coefficient of the old value of `ans`
!   scale : REAL(DFP) - coefficient of the outer product
!   ans : REAL(DFP), rank(4), INTENT(INOUT) - updated in place, `SIZE(ans, k)`
!     is at least the k-th extent of the result
!   dim1 : INTEGER(I4B), INTENT(OUT) - `dim1 = SIZE(a)`
!   dim2 : INTEGER(I4B), INTENT(OUT) - `dim2 = SIZE(b)`
!   dim3 : INTEGER(I4B), INTENT(OUT) - `dim3 = SIZE(c, 1)`
!   dim4 : INTEGER(I4B), INTENT(OUT) - `dim4 = SIZE(c, 2)`
!
! Returns:
!   `ans(1:dim1, 1:dim2, 1:dim3, 1:dim4)` is overwritten with the new value;
!   `dim1`, `dim2`, `dim3`, `dim4` hold the extents written in `ans`.
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4} \leftarrow
!     \alpha\, \mathrm{ans}_{i_1 i_2 i_3 i_4}
!     + \beta\, a_{i_1}\, b_{i_2}\, c_{i_3 i_4}$$
!   where $\alpha$ is `anscoeff` and $\beta$ is `scale`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE ProductUtility, ONLY: OuterProd_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(4) = [2, 3, 2, 3]
! REAL(DFP) :: a(2), b(3), c(2, 3)
! REAL(DFP) :: ans(3, 4, 3, 4)
! REAL(DFP) :: want
! INTEGER(I4B) :: dim1, dim2, dim3, dim4
! INTEGER(I4B) :: ii, i1, i2, i3, i4
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = [(REAL(ii, DFP), ii=1, 3)]
! c = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
!
! ! ans is one larger than needed in every dimension
! ans = 1.0_DFP
!
! ! ans = 2 * ans + 3 * (outer product)
! CALL OuterProd_(a=a, b=b, c=c, anscoeff=2.0_DFP, scale=3.0_DFP, &
!                 ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4)
!
! CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. expected), "sizes written in ans")
!
! isok = .TRUE.
! DO i4 = 1, 3
!   DO i3 = 1, 2
!     DO i2 = 1, 3
!       DO i1 = 1, 2
!         want = 2.0_DFP + 3.0_DFP * a(i1) * b(i2) * c(i3, i4)
!         isok = isok .AND. (ABS(ans(i1, i2, i3, i4) - want) .LT. tol)
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "leading block of ans")
! CALL OK(ABS(ans(2, 3, 2, 3) - 110.0_DFP) .LT. tol, "last entry")
!
! ! entries outside the leading block are not changed
! isok = .TRUE.
! isok = isok .AND. ALL(ABS(ans(3, :, :, :) - 1.0_DFP) .LT. tol)
! isok = isok .AND. ALL(ABS(ans(:, 4, :, :) - 1.0_DFP) .LT. tol)
! isok = isok .AND. ALL(ABS(ans(:, :, 3, :) - 1.0_DFP) .LT. tol)
! isok = isok .AND. ALL(ABS(ans(:, :, :, 4) - 1.0_DFP) .LT. tol)
! CALL OK(isok, "rest of ans is unchanged")
!
! ! with anscoeff = 0 and scale = 1 we get OuterProd
! CALL OuterProd_(a=a, b=b, c=c, anscoeff=0.0_DFP, scale=1.0_DFP, &
!                 ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4)
! CALL OK(ALL(ABS(ans(1:2, 1:3, 1:2, 1:3) - OuterProd(a, b, c)) &
!         .LT. tol), "same as OuterProd")
!
! END PROGRAM main
!```

INTERFACE OuterProd_
  MODULE PURE SUBROUTINE OuterProd_r1r1r2_( &
    a, b, c, anscoeff, scale, ans, dim1, dim2, dim3, dim4)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP), INTENT(IN) :: anscoeff, scale
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  END SUBROUTINE OuterProd_r1r1r2_
END INTERFACE OuterProd_

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-1 and rank-3 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b` and `c`. The result has
!   rank 5. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c)]`, so the indices of
!   the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, b), c)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(3) - third operand
!
! Returns:
!   ans : REAL(DFP), rank(5) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5} =
!     a_{i_1}\, b_{i_2}\, c_{i_3 i_4 i_5}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(5) = [2, 3, 2, 3, 2]
! REAL(DFP) :: a(2), b(3), c(2, 3, 2)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = [(REAL(ii, DFP), ii=1, 3)]
! c = RESHAPE([(REAL(ii, DFP), ii=1, 12)], [2, 3, 2])
!
! ans = OuterProd(a, b, c)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i5 = 1, 2
!   DO i4 = 1, 3
!     DO i3 = 1, 2
!       DO i2 = 1, 3
!         DO i1 = 1, 2
!           want = a(i1) * b(i2) * c(i3, i4, i5)
!           isok = isok .AND. (ABS(ans(i1, i2, i3, i4, i5) - want) .LT. tol)
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2) - 72.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r3(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(c, 1), SIZE(c, 2), &
                     SIZE(c, 3))
  END FUNCTION OuterProd_r1r1r3
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-1 and rank-4 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b` and `c`. The result has
!   rank 6. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c)]`, so the indices of
!   the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, b), c)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(4) - third operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1}\, b_{i_2}\, c_{i_3 i_4 i_5 i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2), b(3), c(2, 3, 2, 3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = [(REAL(ii, DFP), ii=1, 3)]
! c = RESHAPE([(REAL(ii, DFP), ii=1, 36)], [2, 3, 2, 3])
!
! ans = OuterProd(a, b, c)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1) * b(i2) * c(i3, i4, i5, i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r4(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :, :, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(c, 1), SIZE(c, 2), &
                     SIZE(c, 3), SIZE(c, 4))
  END FUNCTION OuterProd_r1r1r4
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-2 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b` and `c`. The result has
!   rank 4. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c)]`, so the indices of
!   the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, b), c)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(2) - second operand
!   c : REAL(DFP), rank(1) - third operand
!
! Returns:
!   ans : REAL(DFP), rank(4) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4} =
!     a_{i_1}\, b_{i_2 i_3}\, c_{i_4}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(4) = [2, 3, 2, 3]
! REAL(DFP) :: a(2), b(3, 2), c(3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [3, 2])
! c = [(REAL(ii, DFP), ii=1, 3)]
!
! ans = OuterProd(a, b, c)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i4 = 1, 3
!   DO i3 = 1, 2
!     DO i2 = 1, 3
!       DO i1 = 1, 2
!         want = a(i1) * b(i2, i3) * c(i4)
!         isok = isok .AND. (ABS(ans(i1, i2, i3, i4) - want) .LT. tol)
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3) - 36.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r2r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(b, 2), SIZE(c, 1))
  END FUNCTION OuterProd_r1r2r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-2 and rank-2 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b` and `c`. The result has
!   rank 5. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c)]`, so the indices of
!   the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, b), c)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(2) - second operand
!   c : REAL(DFP), rank(2) - third operand
!
! Returns:
!   ans : REAL(DFP), rank(5) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5} =
!     a_{i_1}\, b_{i_2 i_3}\, c_{i_4 i_5}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(5) = [2, 3, 2, 3, 2]
! REAL(DFP) :: a(2), b(3, 2), c(3, 2)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [3, 2])
! c = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [3, 2])
!
! ans = OuterProd(a, b, c)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i5 = 1, 2
!   DO i4 = 1, 3
!     DO i3 = 1, 2
!       DO i2 = 1, 3
!         DO i1 = 1, 2
!           want = a(i1) * b(i2, i3) * c(i4, i5)
!           isok = isok .AND. (ABS(ans(i1, i2, i3, i4, i5) - want) .LT. tol)
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2) - 72.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r2r2(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(b, 2), SIZE(c, 1), &
                     SIZE(c, 2))
  END FUNCTION OuterProd_r1r2r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-2 and rank-3 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b` and `c`. The result has
!   rank 6. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c)]`, so the indices of
!   the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, b), c)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(2) - second operand
!   c : REAL(DFP), rank(3) - third operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1}\, b_{i_2 i_3}\, c_{i_4 i_5 i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2), b(3, 2), c(3, 2, 3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [3, 2])
! c = RESHAPE([(REAL(ii, DFP), ii=1, 18)], [3, 2, 3])
!
! ans = OuterProd(a, b, c)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1) * b(i2, i3) * c(i4, i5, i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r2r3(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:, :, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(b, 2), SIZE(c, 1), &
                     SIZE(c, 2), SIZE(c, 3))
  END FUNCTION OuterProd_r1r2r3
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-3 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b` and `c`. The result has
!   rank 5. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c)]`, so the indices of
!   the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, b), c)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(3) - second operand
!   c : REAL(DFP), rank(1) - third operand
!
! Returns:
!   ans : REAL(DFP), rank(5) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5} =
!     a_{i_1}\, b_{i_2 i_3 i_4}\, c_{i_5}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(5) = [2, 3, 2, 3, 2]
! REAL(DFP) :: a(2), b(3, 2, 3), c(2)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = RESHAPE([(REAL(ii, DFP), ii=1, 18)], [3, 2, 3])
! c = [(REAL(ii, DFP), ii=1, 2)]
!
! ans = OuterProd(a, b, c)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i5 = 1, 2
!   DO i4 = 1, 3
!     DO i3 = 1, 2
!       DO i2 = 1, 3
!         DO i1 = 1, 2
!           want = a(i1) * b(i2, i3, i4) * c(i5)
!           isok = isok .AND. (ABS(ans(i1, i2, i3, i4, i5) - want) .LT. tol)
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2) - 72.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r3r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(b, 2), SIZE(b, 3), &
                     SIZE(c, 1))
  END FUNCTION OuterProd_r1r3r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-3 and rank-2 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b` and `c`. The result has
!   rank 6. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c)]`, so the indices of
!   the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, b), c)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(3) - second operand
!   c : REAL(DFP), rank(2) - third operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1}\, b_{i_2 i_3 i_4}\, c_{i_5 i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2), b(3, 2, 3), c(2, 3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = RESHAPE([(REAL(ii, DFP), ii=1, 18)], [3, 2, 3])
! c = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
!
! ans = OuterProd(a, b, c)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1) * b(i2, i3, i4) * c(i5, i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r3r2(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :, :)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(b, 2), SIZE(b, 3), &
                     SIZE(c, 1), SIZE(c, 2))
  END FUNCTION OuterProd_r1r3r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-4 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b` and `c`. The result has
!   rank 6. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c)]`, so the indices of
!   the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, b), c)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(4) - second operand
!   c : REAL(DFP), rank(1) - third operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1}\, b_{i_2 i_3 i_4 i_5}\, c_{i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2), b(3, 2, 3, 2), c(3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = RESHAPE([(REAL(ii, DFP), ii=1, 36)], [3, 2, 3, 2])
! c = [(REAL(ii, DFP), ii=1, 3)]
!
! ans = OuterProd(a, b, c)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1) * b(i2, i3, i4, i5) * c(i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r4r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :, :, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(b, 2), SIZE(b, 3), &
                     SIZE(b, 4), SIZE(c, 1))
  END FUNCTION OuterProd_r1r4r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-2, rank-1 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b` and `c`. The result has
!   rank 4. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c)]`, so the indices of
!   the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, b), c)`.
!
! Arguments:
!   a : REAL(DFP), rank(2) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(1) - third operand
!
! Returns:
!   ans : REAL(DFP), rank(4) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4} =
!     a_{i_1 i_2}\, b_{i_3}\, c_{i_4}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(4) = [2, 3, 2, 3]
! REAL(DFP) :: a(2, 3), b(2), c(3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! b = [(REAL(ii, DFP), ii=1, 2)]
! c = [(REAL(ii, DFP), ii=1, 3)]
!
! ans = OuterProd(a, b, c)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i4 = 1, 3
!   DO i3 = 1, 2
!     DO i2 = 1, 3
!       DO i1 = 1, 2
!         want = a(i1, i2) * b(i3) * c(i4)
!         isok = isok .AND. (ABS(ans(i1, i2, i3, i4) - want) .LT. tol)
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3) - 36.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r1r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(b, 1), SIZE(c, 1))
  END FUNCTION OuterProd_r2r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                  OuterProd_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-2, rank-1 and rank-1 arrays, no allocation
!
! Description:
!   Adds the outer product of `a`, `b` and `c` to `ans`, that is `ans =
!   anscoeff * ans + scale * OuterProd(a, b, c)`.
!
!   - Only the leading block `ans(1:dim1, 1:dim2, 1:dim3, 1:dim4)` is written.
!     The other entries of `ans` are not changed.
!   - `ans` must be at least as large as the result. This is not checked.
!
! Arguments:
!   a : REAL(DFP), rank(2) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(1) - third operand
!   anscoeff : REAL(DFP) - coefficient of the old value of `ans`
!   scale : REAL(DFP) - coefficient of the outer product
!   ans : REAL(DFP), rank(4), INTENT(INOUT) - updated in place, `SIZE(ans, k)`
!     is at least the k-th extent of the result
!   dim1 : INTEGER(I4B), INTENT(OUT) - `dim1 = SIZE(a, 1)`
!   dim2 : INTEGER(I4B), INTENT(OUT) - `dim2 = SIZE(a, 2)`
!   dim3 : INTEGER(I4B), INTENT(OUT) - `dim3 = SIZE(b)`
!   dim4 : INTEGER(I4B), INTENT(OUT) - `dim4 = SIZE(c)`
!
! Returns:
!   `ans(1:dim1, 1:dim2, 1:dim3, 1:dim4)` is overwritten with the new value;
!   `dim1`, `dim2`, `dim3`, `dim4` hold the extents written in `ans`.
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4} \leftarrow
!     \alpha\, \mathrm{ans}_{i_1 i_2 i_3 i_4}
!     + \beta\, a_{i_1 i_2}\, b_{i_3}\, c_{i_4}$$
!   where $\alpha$ is `anscoeff` and $\beta$ is `scale`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE ProductUtility, ONLY: OuterProd_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(4) = [2, 3, 2, 3]
! REAL(DFP) :: a(2, 3), b(2), c(3)
! REAL(DFP) :: ans(3, 4, 3, 4)
! REAL(DFP) :: want
! INTEGER(I4B) :: dim1, dim2, dim3, dim4
! INTEGER(I4B) :: ii, i1, i2, i3, i4
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! b = [(REAL(ii, DFP), ii=1, 2)]
! c = [(REAL(ii, DFP), ii=1, 3)]
!
! ! ans is one larger than needed in every dimension
! ans = 1.0_DFP
!
! ! ans = 2 * ans + 3 * (outer product)
! CALL OuterProd_(a=a, b=b, c=c, anscoeff=2.0_DFP, scale=3.0_DFP, &
!                 ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4)
!
! CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. expected), "sizes written in ans")
!
! isok = .TRUE.
! DO i4 = 1, 3
!   DO i3 = 1, 2
!     DO i2 = 1, 3
!       DO i1 = 1, 2
!         want = 2.0_DFP + 3.0_DFP * a(i1, i2) * b(i3) * c(i4)
!         isok = isok .AND. (ABS(ans(i1, i2, i3, i4) - want) .LT. tol)
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "leading block of ans")
! CALL OK(ABS(ans(2, 3, 2, 3) - 110.0_DFP) .LT. tol, "last entry")
!
! ! entries outside the leading block are not changed
! isok = .TRUE.
! isok = isok .AND. ALL(ABS(ans(3, :, :, :) - 1.0_DFP) .LT. tol)
! isok = isok .AND. ALL(ABS(ans(:, 4, :, :) - 1.0_DFP) .LT. tol)
! isok = isok .AND. ALL(ABS(ans(:, :, 3, :) - 1.0_DFP) .LT. tol)
! isok = isok .AND. ALL(ABS(ans(:, :, :, 4) - 1.0_DFP) .LT. tol)
! CALL OK(isok, "rest of ans is unchanged")
!
! ! with anscoeff = 0 and scale = 1 we get OuterProd
! CALL OuterProd_(a=a, b=b, c=c, anscoeff=0.0_DFP, scale=1.0_DFP, &
!                 ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4)
! CALL OK(ALL(ABS(ans(1:2, 1:3, 1:2, 1:3) - OuterProd(a, b, c)) &
!         .LT. tol), "same as OuterProd")
!
! END PROGRAM main
!```

INTERFACE OuterProd_
  MODULE PURE SUBROUTINE OuterProd_r2r1r1_( &
    a, b, c, ans, dim1, dim2, dim3, dim4, scale, anscoeff)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
    REAL(DFP), INTENT(IN) :: scale, anscoeff
  END SUBROUTINE OuterProd_r2r1r1_
END INTERFACE OuterProd_

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-2, rank-1 and rank-2 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b` and `c`. The result has
!   rank 5. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c)]`, so the indices of
!   the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, b), c)`.
!
! Arguments:
!   a : REAL(DFP), rank(2) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(2) - third operand
!
! Returns:
!   ans : REAL(DFP), rank(5) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5} =
!     a_{i_1 i_2}\, b_{i_3}\, c_{i_4 i_5}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(5) = [2, 3, 2, 3, 2]
! REAL(DFP) :: a(2, 3), b(2), c(3, 2)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! b = [(REAL(ii, DFP), ii=1, 2)]
! c = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [3, 2])
!
! ans = OuterProd(a, b, c)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i5 = 1, 2
!   DO i4 = 1, 3
!     DO i3 = 1, 2
!       DO i2 = 1, 3
!         DO i1 = 1, 2
!           want = a(i1, i2) * b(i3) * c(i4, i5)
!           isok = isok .AND. (ABS(ans(i1, i2, i3, i4, i5) - want) .LT. tol)
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2) - 72.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r1r2(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(b, 1), SIZE(c, 1), &
                     SIZE(c, 2))
  END FUNCTION OuterProd_r2r1r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-2, rank-1 and rank-3 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b` and `c`. The result has
!   rank 6. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c)]`, so the indices of
!   the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, b), c)`.
!
! Arguments:
!   a : REAL(DFP), rank(2) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(3) - third operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1 i_2}\, b_{i_3}\, c_{i_4 i_5 i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2, 3), b(2), c(3, 2, 3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! b = [(REAL(ii, DFP), ii=1, 2)]
! c = RESHAPE([(REAL(ii, DFP), ii=1, 18)], [3, 2, 3])
!
! ans = OuterProd(a, b, c)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1, i2) * b(i3) * c(i4, i5, i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r1r3(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(b, 1), SIZE(c, 1), &
                     SIZE(c, 2), SIZE(c, 3))
  END FUNCTION OuterProd_r2r1r3
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-2, rank-2 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b` and `c`. The result has
!   rank 5. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c)]`, so the indices of
!   the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, b), c)`.
!
! Arguments:
!   a : REAL(DFP), rank(2) - first operand
!   b : REAL(DFP), rank(2) - second operand
!   c : REAL(DFP), rank(1) - third operand
!
! Returns:
!   ans : REAL(DFP), rank(5) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5} =
!     a_{i_1 i_2}\, b_{i_3 i_4}\, c_{i_5}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(5) = [2, 3, 2, 3, 2]
! REAL(DFP) :: a(2, 3), b(2, 3), c(2)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! b = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! c = [(REAL(ii, DFP), ii=1, 2)]
!
! ans = OuterProd(a, b, c)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i5 = 1, 2
!   DO i4 = 1, 3
!     DO i3 = 1, 2
!       DO i2 = 1, 3
!         DO i1 = 1, 2
!           want = a(i1, i2) * b(i3, i4) * c(i5)
!           isok = isok .AND. (ABS(ans(i1, i2, i3, i4, i5) - want) .LT. tol)
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2) - 72.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r2r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(b, 1), SIZE(b, 2), &
                     SIZE(c, 1))
  END FUNCTION OuterProd_r2r2r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-2, rank-2 and rank-2 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b` and `c`. The result has
!   rank 6. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c)]`, so the indices of
!   the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, b), c)`.
!
! Arguments:
!   a : REAL(DFP), rank(2) - first operand
!   b : REAL(DFP), rank(2) - second operand
!   c : REAL(DFP), rank(2) - third operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1 i_2}\, b_{i_3 i_4}\, c_{i_5 i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2, 3), b(2, 3), c(2, 3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! b = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! c = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
!
! ans = OuterProd(a, b, c)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1, i2) * b(i3, i4) * c(i5, i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r2r2(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(b, 1), SIZE(b, 2), &
                     SIZE(c, 1), SIZE(c, 2))
  END FUNCTION OuterProd_r2r2r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-3, rank-1 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b` and `c`. The result has
!   rank 5. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c)]`, so the indices of
!   the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, b), c)`.
!
! Arguments:
!   a : REAL(DFP), rank(3) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(1) - third operand
!
! Returns:
!   ans : REAL(DFP), rank(5) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5} =
!     a_{i_1 i_2 i_3}\, b_{i_4}\, c_{i_5}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(5) = [2, 3, 2, 3, 2]
! REAL(DFP) :: a(2, 3, 2), b(3), c(2)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 12)], [2, 3, 2])
! b = [(REAL(ii, DFP), ii=1, 3)]
! c = [(REAL(ii, DFP), ii=1, 2)]
!
! ans = OuterProd(a, b, c)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i5 = 1, 2
!   DO i4 = 1, 3
!     DO i3 = 1, 2
!       DO i2 = 1, 3
!         DO i1 = 1, 2
!           want = a(i1, i2, i3) * b(i4) * c(i5)
!           isok = isok .AND. (ABS(ans(i1, i2, i3, i4, i5) - want) .LT. tol)
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2) - 72.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r3r1r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3), SIZE(b, 1), &
                     SIZE(c, 1))
  END FUNCTION OuterProd_r3r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-3, rank-1 and rank-2 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b` and `c`. The result has
!   rank 6. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c)]`, so the indices of
!   the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, b), c)`.
!
! Arguments:
!   a : REAL(DFP), rank(3) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(2) - third operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1 i_2 i_3}\, b_{i_4}\, c_{i_5 i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2, 3, 2), b(3), c(2, 3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 12)], [2, 3, 2])
! b = [(REAL(ii, DFP), ii=1, 3)]
! c = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
!
! ans = OuterProd(a, b, c)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1, i2, i3) * b(i4) * c(i5, i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r3r1r2(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3), SIZE(b, 1), &
                     SIZE(c, 1), SIZE(c, 2))
  END FUNCTION OuterProd_r3r1r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-3, rank-2 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b` and `c`. The result has
!   rank 6. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c)]`, so the indices of
!   the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, b), c)`.
!
! Arguments:
!   a : REAL(DFP), rank(3) - first operand
!   b : REAL(DFP), rank(2) - second operand
!   c : REAL(DFP), rank(1) - third operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1 i_2 i_3}\, b_{i_4 i_5}\, c_{i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2, 3, 2), b(3, 2), c(3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 12)], [2, 3, 2])
! b = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [3, 2])
! c = [(REAL(ii, DFP), ii=1, 3)]
!
! ans = OuterProd(a, b, c)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1, i2, i3) * b(i4, i5) * c(i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r3r2r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3), SIZE(b, 1), &
                     SIZE(b, 2), SIZE(c, 1))
  END FUNCTION OuterProd_r3r2r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-4, rank-1 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b` and `c`. The result has
!   rank 6. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c)]`, so the indices of
!   the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, b), c)`.
!
! Arguments:
!   a : REAL(DFP), rank(4) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(1) - third operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1 i_2 i_3 i_4}\, b_{i_5}\, c_{i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2, 3, 2, 3), b(2), c(3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 36)], [2, 3, 2, 3])
! b = [(REAL(ii, DFP), ii=1, 2)]
! c = [(REAL(ii, DFP), ii=1, 3)]
!
! ans = OuterProd(a, b, c)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1, i2, i3, i4) * b(i5) * c(i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r4r1r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3), SIZE(a, 4), &
                     SIZE(b, 1), SIZE(c, 1))
  END FUNCTION OuterProd_r4r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-1, rank-1 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b`, `c` and `d`. The result
!   has rank 4. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c), SHAPE(d)]`, so
!   the indices of the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, OuterProd(b, c)), d)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(1) - third operand
!   d : REAL(DFP), rank(1) - fourth operand
!
! Returns:
!   ans : REAL(DFP), rank(4) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c), SHAPE(d)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4} =
!     a_{i_1}\, b_{i_2}\, c_{i_3}\, d_{i_4}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(4) = [2, 3, 2, 3]
! REAL(DFP) :: a(2), b(3), c(2), d(3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = [(REAL(ii, DFP), ii=1, 3)]
! c = [(REAL(ii, DFP), ii=1, 2)]
! d = [(REAL(ii, DFP), ii=1, 3)]
!
! ans = OuterProd(a, b, c, d)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i4 = 1, 3
!   DO i3 = 1, 2
!     DO i2 = 1, 3
!       DO i1 = 1, 2
!         want = a(i1) * b(i2) * c(i3) * d(i4)
!         isok = isok .AND. (ABS(ans(i1, i2, i3, i4) - want) .LT. tol)
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3) - 36.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r1r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(c, 1), SIZE(d, 1))
  END FUNCTION OuterProd_r1r1r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-1, rank-1 and rank-2 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b`, `c` and `d`. The result
!   has rank 5. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c), SHAPE(d)]`, so
!   the indices of the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, OuterProd(b, c)), d)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(1) - third operand
!   d : REAL(DFP), rank(2) - fourth operand
!
! Returns:
!   ans : REAL(DFP), rank(5) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c), SHAPE(d)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5} =
!     a_{i_1}\, b_{i_2}\, c_{i_3}\, d_{i_4 i_5}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(5) = [2, 3, 2, 3, 2]
! REAL(DFP) :: a(2), b(3), c(2), d(3, 2)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = [(REAL(ii, DFP), ii=1, 3)]
! c = [(REAL(ii, DFP), ii=1, 2)]
! d = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [3, 2])
!
! ans = OuterProd(a, b, c, d)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i5 = 1, 2
!   DO i4 = 1, 3
!     DO i3 = 1, 2
!       DO i2 = 1, 3
!         DO i1 = 1, 2
!           want = a(i1) * b(i2) * c(i3) * d(i4, i5)
!           isok = isok .AND. (ABS(ans(i1, i2, i3, i4, i5) - want) .LT. tol)
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2) - 72.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r1r2(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(c, 1), SIZE(d, 1), &
                     SIZE(d, 2))
  END FUNCTION OuterProd_r1r1r1r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-1, rank-1 and rank-3 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b`, `c` and `d`. The result
!   has rank 6. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c), SHAPE(d)]`, so
!   the indices of the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, OuterProd(b, c)), d)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(1) - third operand
!   d : REAL(DFP), rank(3) - fourth operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c), SHAPE(d)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1}\, b_{i_2}\, c_{i_3}\, d_{i_4 i_5 i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2), b(3), c(2), d(3, 2, 3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = [(REAL(ii, DFP), ii=1, 3)]
! c = [(REAL(ii, DFP), ii=1, 2)]
! d = RESHAPE([(REAL(ii, DFP), ii=1, 18)], [3, 2, 3])
!
! ans = OuterProd(a, b, c, d)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1) * b(i2) * c(i3) * d(i4, i5, i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r1r3(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:, :, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(c, 1), SIZE(d, 1), &
                     SIZE(d, 2), SIZE(d, 3))
  END FUNCTION OuterProd_r1r1r1r3
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-1, rank-2 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b`, `c` and `d`. The result
!   has rank 5. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c), SHAPE(d)]`, so
!   the indices of the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, OuterProd(b, c)), d)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(2) - third operand
!   d : REAL(DFP), rank(1) - fourth operand
!
! Returns:
!   ans : REAL(DFP), rank(5) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c), SHAPE(d)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5} =
!     a_{i_1}\, b_{i_2}\, c_{i_3 i_4}\, d_{i_5}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(5) = [2, 3, 2, 3, 2]
! REAL(DFP) :: a(2), b(3), c(2, 3), d(2)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = [(REAL(ii, DFP), ii=1, 3)]
! c = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! d = [(REAL(ii, DFP), ii=1, 2)]
!
! ans = OuterProd(a, b, c, d)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i5 = 1, 2
!   DO i4 = 1, 3
!     DO i3 = 1, 2
!       DO i2 = 1, 3
!         DO i1 = 1, 2
!           want = a(i1) * b(i2) * c(i3, i4) * d(i5)
!           isok = isok .AND. (ABS(ans(i1, i2, i3, i4, i5) - want) .LT. tol)
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2) - 72.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r2r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(c, 1), SIZE(c, 2), &
                     SIZE(d, 1))
  END FUNCTION OuterProd_r1r1r2r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-1, rank-2 and rank-2 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b`, `c` and `d`. The result
!   has rank 6. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c), SHAPE(d)]`, so
!   the indices of the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, OuterProd(b, c)), d)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(2) - third operand
!   d : REAL(DFP), rank(2) - fourth operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c), SHAPE(d)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1}\, b_{i_2}\, c_{i_3 i_4}\, d_{i_5 i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2), b(3), c(2, 3), d(2, 3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = [(REAL(ii, DFP), ii=1, 3)]
! c = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! d = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
!
! ans = OuterProd(a, b, c, d)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1) * b(i2) * c(i3, i4) * d(i5, i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r2r2(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP), INTENT(IN) :: d(:, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(c, 1), SIZE(c, 2), &
                     SIZE(d, 1), SIZE(d, 2))
  END FUNCTION OuterProd_r1r1r2r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-1, rank-3 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b`, `c` and `d`. The result
!   has rank 6. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c), SHAPE(d)]`, so
!   the indices of the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, OuterProd(b, c)), d)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(3) - third operand
!   d : REAL(DFP), rank(1) - fourth operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c), SHAPE(d)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1}\, b_{i_2}\, c_{i_3 i_4 i_5}\, d_{i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2), b(3), c(2, 3, 2), d(3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = [(REAL(ii, DFP), ii=1, 3)]
! c = RESHAPE([(REAL(ii, DFP), ii=1, 12)], [2, 3, 2])
! d = [(REAL(ii, DFP), ii=1, 3)]
!
! ans = OuterProd(a, b, c, d)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1) * b(i2) * c(i3, i4, i5) * d(i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r3r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :, :)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(c, 1), SIZE(c, 2), &
                     SIZE(c, 3), SIZE(d, 1))
  END FUNCTION OuterProd_r1r1r3r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-2, rank-1 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b`, `c` and `d`. The result
!   has rank 5. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c), SHAPE(d)]`, so
!   the indices of the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, OuterProd(b, c)), d)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(2) - second operand
!   c : REAL(DFP), rank(1) - third operand
!   d : REAL(DFP), rank(1) - fourth operand
!
! Returns:
!   ans : REAL(DFP), rank(5) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c), SHAPE(d)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5} =
!     a_{i_1}\, b_{i_2 i_3}\, c_{i_4}\, d_{i_5}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(5) = [2, 3, 2, 3, 2]
! REAL(DFP) :: a(2), b(3, 2), c(3), d(2)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [3, 2])
! c = [(REAL(ii, DFP), ii=1, 3)]
! d = [(REAL(ii, DFP), ii=1, 2)]
!
! ans = OuterProd(a, b, c, d)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i5 = 1, 2
!   DO i4 = 1, 3
!     DO i3 = 1, 2
!       DO i2 = 1, 3
!         DO i1 = 1, 2
!           want = a(i1) * b(i2, i3) * c(i4) * d(i5)
!           isok = isok .AND. (ABS(ans(i1, i2, i3, i4, i5) - want) .LT. tol)
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2) - 72.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r2r1r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(b, 2), SIZE(c, 1), &
                     SIZE(d, 1))
  END FUNCTION OuterProd_r1r2r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-2, rank-1 and rank-2 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b`, `c` and `d`. The result
!   has rank 6. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c), SHAPE(d)]`, so
!   the indices of the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, OuterProd(b, c)), d)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(2) - second operand
!   c : REAL(DFP), rank(1) - third operand
!   d : REAL(DFP), rank(2) - fourth operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c), SHAPE(d)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1}\, b_{i_2 i_3}\, c_{i_4}\, d_{i_5 i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2), b(3, 2), c(3), d(2, 3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [3, 2])
! c = [(REAL(ii, DFP), ii=1, 3)]
! d = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
!
! ans = OuterProd(a, b, c, d)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1) * b(i2, i3) * c(i4) * d(i5, i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r2r1r2(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(b, 2), SIZE(c, 1), &
                     SIZE(d, 1), SIZE(d, 2))
  END FUNCTION OuterProd_r1r2r1r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-2, rank-2 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b`, `c` and `d`. The result
!   has rank 6. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c), SHAPE(d)]`, so
!   the indices of the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, OuterProd(b, c)), d)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(2) - second operand
!   c : REAL(DFP), rank(2) - third operand
!   d : REAL(DFP), rank(1) - fourth operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c), SHAPE(d)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1}\, b_{i_2 i_3}\, c_{i_4 i_5}\, d_{i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2), b(3, 2), c(3, 2), d(3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [3, 2])
! c = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [3, 2])
! d = [(REAL(ii, DFP), ii=1, 3)]
!
! ans = OuterProd(a, b, c, d)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1) * b(i2, i3) * c(i4, i5) * d(i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r2r2r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(b, 2), SIZE(c, 1), &
                     SIZE(c, 2), SIZE(d, 1))
  END FUNCTION OuterProd_r1r2r2r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-1, rank-3, rank-1 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b`, `c` and `d`. The result
!   has rank 6. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c), SHAPE(d)]`, so
!   the indices of the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, OuterProd(b, c)), d)`.
!
! Arguments:
!   a : REAL(DFP), rank(1) - first operand
!   b : REAL(DFP), rank(3) - second operand
!   c : REAL(DFP), rank(1) - third operand
!   d : REAL(DFP), rank(1) - fourth operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c), SHAPE(d)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1}\, b_{i_2 i_3 i_4}\, c_{i_5}\, d_{i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2), b(3, 2, 3), c(2), d(3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = [(REAL(ii, DFP), ii=1, 2)]
! b = RESHAPE([(REAL(ii, DFP), ii=1, 18)], [3, 2, 3])
! c = [(REAL(ii, DFP), ii=1, 2)]
! d = [(REAL(ii, DFP), ii=1, 3)]
!
! ans = OuterProd(a, b, c, d)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1) * b(i2, i3, i4) * c(i5) * d(i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r3r1r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(b, 1), SIZE(b, 2), SIZE(b, 3), &
                     SIZE(c, 1), SIZE(d, 1))
  END FUNCTION OuterProd_r1r3r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-2, rank-1, rank-1 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b`, `c` and `d`. The result
!   has rank 5. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c), SHAPE(d)]`, so
!   the indices of the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, OuterProd(b, c)), d)`.
!
! Arguments:
!   a : REAL(DFP), rank(2) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(1) - third operand
!   d : REAL(DFP), rank(1) - fourth operand
!
! Returns:
!   ans : REAL(DFP), rank(5) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c), SHAPE(d)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5} =
!     a_{i_1 i_2}\, b_{i_3}\, c_{i_4}\, d_{i_5}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(5) = [2, 3, 2, 3, 2]
! REAL(DFP) :: a(2, 3), b(2), c(3), d(2)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! b = [(REAL(ii, DFP), ii=1, 2)]
! c = [(REAL(ii, DFP), ii=1, 3)]
! d = [(REAL(ii, DFP), ii=1, 2)]
!
! ans = OuterProd(a, b, c, d)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i5 = 1, 2
!   DO i4 = 1, 3
!     DO i3 = 1, 2
!       DO i2 = 1, 3
!         DO i1 = 1, 2
!           want = a(i1, i2) * b(i3) * c(i4) * d(i5)
!           isok = isok .AND. (ABS(ans(i1, i2, i3, i4, i5) - want) .LT. tol)
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2) - 72.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r1r1r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(b, 1), SIZE(c, 1), &
                     SIZE(d, 1))
  END FUNCTION OuterProd_r2r1r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-2, rank-1, rank-1 and rank-2 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b`, `c` and `d`. The result
!   has rank 6. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c), SHAPE(d)]`, so
!   the indices of the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, OuterProd(b, c)), d)`.
!
! Arguments:
!   a : REAL(DFP), rank(2) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(1) - third operand
!   d : REAL(DFP), rank(2) - fourth operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c), SHAPE(d)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1 i_2}\, b_{i_3}\, c_{i_4}\, d_{i_5 i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2, 3), b(2), c(3), d(2, 3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! b = [(REAL(ii, DFP), ii=1, 2)]
! c = [(REAL(ii, DFP), ii=1, 3)]
! d = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
!
! ans = OuterProd(a, b, c, d)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1, i2) * b(i3) * c(i4) * d(i5, i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r1r1r2(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:, :)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(b, 1), SIZE(c, 1), &
                     SIZE(d, 1), SIZE(d, 2))
  END FUNCTION OuterProd_r2r1r1r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-2, rank-1, rank-2 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b`, `c` and `d`. The result
!   has rank 6. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c), SHAPE(d)]`, so
!   the indices of the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, OuterProd(b, c)), d)`.
!
! Arguments:
!   a : REAL(DFP), rank(2) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(2) - third operand
!   d : REAL(DFP), rank(1) - fourth operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c), SHAPE(d)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1 i_2}\, b_{i_3}\, c_{i_4 i_5}\, d_{i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2, 3), b(2), c(3, 2), d(3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! b = [(REAL(ii, DFP), ii=1, 2)]
! c = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [3, 2])
! d = [(REAL(ii, DFP), ii=1, 3)]
!
! ans = OuterProd(a, b, c, d)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1, i2) * b(i3) * c(i4, i5) * d(i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r1r2r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(b, 1), SIZE(c, 1), &
                     SIZE(c, 2), SIZE(d, 1))
  END FUNCTION OuterProd_r2r1r2r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-2, rank-2, rank-1 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b`, `c` and `d`. The result
!   has rank 6. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c), SHAPE(d)]`, so
!   the indices of the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, OuterProd(b, c)), d)`.
!
! Arguments:
!   a : REAL(DFP), rank(2) - first operand
!   b : REAL(DFP), rank(2) - second operand
!   c : REAL(DFP), rank(1) - third operand
!   d : REAL(DFP), rank(1) - fourth operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c), SHAPE(d)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1 i_2}\, b_{i_3 i_4}\, c_{i_5}\, d_{i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2, 3), b(2, 3), c(2), d(3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! b = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! c = [(REAL(ii, DFP), ii=1, 2)]
! d = [(REAL(ii, DFP), ii=1, 3)]
!
! ans = OuterProd(a, b, c, d)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1, i2) * b(i3, i4) * c(i5) * d(i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r2r1r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(b, 1), SIZE(b, 2), &
                     SIZE(c, 1), SIZE(d, 1))
  END FUNCTION OuterProd_r2r2r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                                   OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! summary: Outer product of rank-3, rank-1, rank-1 and rank-1 arrays
!
! Description:
!   Returns the outer (tensor) product of `a`, `b`, `c` and `d`. The result
!   has rank 6. Its shape is `[SHAPE(a), SHAPE(b), SHAPE(c), SHAPE(d)]`, so
!   the indices of the operands follow one after another.
!
!   It is computed as `OuterProd(OuterProd(a, OuterProd(b, c)), d)`.
!
! Arguments:
!   a : REAL(DFP), rank(3) - first operand
!   b : REAL(DFP), rank(1) - second operand
!   c : REAL(DFP), rank(1) - third operand
!   d : REAL(DFP), rank(1) - fourth operand
!
! Returns:
!   ans : REAL(DFP), rank(6) - outer product, `SHAPE(ans) = [SHAPE(a),
!     SHAPE(b), SHAPE(c), SHAPE(d)]`
!
! Formula:
!   $$\mathrm{ans}_{i_1 i_2 i_3 i_4 i_5 i_6} =
!     a_{i_1 i_2 i_3}\, b_{i_4}\, c_{i_5}\, d_{i_6}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE ProductUtility, ONLY: OuterProd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
! REAL(DFP) :: a(2, 3, 2), b(3), c(2), d(3)
! REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
! REAL(DFP) :: want
! INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
! LOGICAL(LGT) :: isok
!
! a = RESHAPE([(REAL(ii, DFP), ii=1, 12)], [2, 3, 2])
! b = [(REAL(ii, DFP), ii=1, 3)]
! c = [(REAL(ii, DFP), ii=1, 2)]
! d = [(REAL(ii, DFP), ii=1, 3)]
!
! ans = OuterProd(a, b, c, d)
!
! ! the shape of ans is the shapes of the operands, one after another
! CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")
!
! ! every entry is the product of the entries of the operands
! isok = .TRUE.
! DO i6 = 1, 3
!   DO i5 = 1, 2
!     DO i4 = 1, 3
!       DO i3 = 1, 2
!         DO i2 = 1, 3
!           DO i1 = 1, 2
!             want = a(i1, i2, i3) * b(i4) * c(i5) * d(i6)
!             isok = isok .AND. &
!                    (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
!           END DO
!         END DO
!       END DO
!     END DO
!   END DO
! END DO
! CALL OK(isok, "all entries of ans")
!
! ! first and last entries computed by hand
! CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
! CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")
!
! END PROGRAM main
!```

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r3r1r1r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3), SIZE(b, 1), &
                     SIZE(c, 1), SIZE(d, 1))
  END FUNCTION OuterProd_r3r1r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ProductUtility
