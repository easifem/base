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
! date:  2024-08-13
! summary: returns a space-time matrix from time and space matrix
!
!# OTimesTilda
!
! - the size of a and b should be exact, as it it used in computing
! nrow and ncol
! - the size of ans should be at least nrow and ncol
! - nrow = sa(1) * sb(1)
! - ncol = sa(2) * sb(2)
! - sa = SHAPE(a)
! - sb = SHAPE(b)
!
! ans = anscoeff * ans + scalar * (a .outerprod. b)

INTERFACE OTimesTilda
  MODULE PURE SUBROUTINE OTimesTilda1(a, b, ans, nrow, ncol, anscoeff, scale)
    REAL(DFP), INTENT(IN) :: a(:, :)
    !! time matrix
    REAL(DFP), INTENT(IN) :: b(:, :)
    !! space matrix
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space time matix in DOF Format
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
! date:  2024-08-13
! summary: returns a space-time vector from time and space vector
!
!# OTimesTilda
!
! - the size of a and b should be exact, as it it used in computing
! nrow and ncol
! - sa = SIZE(a)
! - sb = SIZE(b)
! - tsize = sa * sb
!
! ans = anscoeff * ans + scalar * (a .outerprod. b)
!

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
! date:  2024-08-13
! summary:  returns a space-time matrix from time and space matrix
!
!# OTimesTilda
!
! - the size of a, b, c, and d should be exact, as it it used in computing
! nrow and ncol
!
! - sa(1) = SIZE(a)
! - sa(2) = SIZE(b)
! - sb(1) = SIZE(c)
! - sb(2) = SIZE(d)
! - nrow = sa(1) * sb(1)
! - ncol = sa(2) * sb(2)
!
! ans = anscoeff * ans + scalar * (a, b) .outerprod. (c,d)
!

INTERFACE OTimesTilda
  MODULE PURE SUBROUTINE OTimesTilda3( &
    a, b, c, d, ans, nrow, ncol, anscoeff, scale)
    REAL(DFP), INTENT(IN) :: a(:), b(:)
    !! time matrix
    REAL(DFP), INTENT(IN) :: c(:), d(:)
    !! space matrix
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space time matix in DOF Format
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
! summary: This FUNCTION evaluate vectors product
!
!# VectorProduct
!
! This FUNCTION evaluate vectors products
!
!$$
!\mathbf{ans} = \mathbf{a} \times \mathbf{b}
!$$

INTERFACE Vector_Product
  MODULE PURE FUNCTION vectorProduct_1(a, b) RESULT(c)
    ! Define INTENT of dummy argument
    REAL(REAL64), INTENT(IN) :: a(3), b(3)
    REAL(REAL64) :: c(3)
  END FUNCTION vectorProduct_1
END INTERFACE Vector_Product

INTERFACE Cross_Product
  MODULE PROCEDURE vectorProduct_1
END INTERFACE Cross_Product

INTERFACE VectorProduct
  MODULE PROCEDURE vectorProduct_1
END INTERFACE VectorProduct

!----------------------------------------------------------------------------
!                                                             Vector_Product
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-09
! summary: This FUNCTION evaluate vectors product
!
!# VectorProduct
!
! This FUNCTION evaluate vectors products
!
!$$
!\mathbf{ans} = \mathbf{a} \times \mathbf{b}
!$$

INTERFACE Vector_Product
  MODULE PURE FUNCTION vectorProduct_2(a, b) RESULT(c)
    ! Define INTENT of dummy argument
    REAL(REAL32), INTENT(IN) :: a(3), b(3)
    REAL(REAL32) :: c(3)
  END FUNCTION vectorProduct_2
END INTERFACE Vector_Product

INTERFACE Cross_Product
  MODULE PROCEDURE vectorProduct_2
END INTERFACE Cross_Product

INTERFACE VectorProduct
  MODULE PROCEDURE vectorProduct_2
END INTERFACE VectorProduct

!----------------------------------------------------------------------------
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-09
! summary: This FUNCTION returns OuterProduct(matrix) of two vectors
!
!# OuterProd
!
!
! This function returns outer-product of two vectors. The ans is
! a matrix.
!
!$$
! \mathbf{ans} = \mathbf{a} \otimes \mathbf{b}
!$$

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
! summary: Outer product
!
!# OuterProd_
!
! This method computes the following outer product.
!
!```fortran
! ans(i,j) = anscoeff * ans(i,j) + scale * a(i)*b(j)
!```

INTERFACE OuterProd_
  MODULE PURE SUBROUTINE OuterProd_r1r1_( &
    a, b, anscoeff, scale, ans, nrow, ncol)
    REAL(DFP), DIMENSION(:), INTENT(IN) :: a(:), b(:)
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
!                                                                  OuterProd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2026-03-09
! summary: This FUNCTION returns OuterProduct
!
!# OuterProd
!
! This FUNCTION returns OuterProduct(matrix) of two vectors.
! The following outer product is computed.
!
!$$
! \mathbf{y} = \mathbf{a} \otimes \mathbf{b}
!$$
!
! - If `sym` is .true. THEN symmetric part is returned
!

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
! summary: Outer product of two vector.
!
!# OuterProd_
!
! This method computes the following outer product.
!
!```fortran
! ans(i,j) = anscoeff * ans(i,j) + scale * a(i)*b(j)
!```
!
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
! summary: a x b
!
!# OuterProd
!
! This method computes the following outer product.
!
!$$
!y(i,j,k)=a(i)*b(j,k)
!$$
!

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
! summary: a x b
!
!# OuterProd_
!
! This method computes the following outer product.
!
! The size of a and b should be exact.
!
!```fortran
! ans(i,j,k)=anscoeff*ans(i,j,k)+scale*a(i)*b(j,k)
!```
!

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
! summary: Outer product of a vector and rank 3 matrix.
!
!# Outerprod
!
! This method computes the following outer product.
!
!$$
!y(i,j,k,l)=a(i)*b(j,k,l)
!$$
!

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
! summary: outer product of a vector and rank 4 array.
!
!# OuterProd
!
! This method computes an outer product of a vector and rank 4 array.
! The answer will be a rank 5 array.
!
!$$
!y(i1,i2,i3,i4,i5)=a(i1) * b(i2,i3,i4,i5)
!$$
!

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
! summary: Outerproduct of a vector and rank 5 array.
!
!# Outerprod
!
! This method computes the following outer product between a vector and
! a rank 5 array. The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1)*b(i2,i3,i4,i5,i6)
!$$
!

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
! date:         22 March 2021
! summary:         This FUNCTION returns OuterProduct

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
! summary: This method computes outer product of a matrix and a vector
!
!# OuterProd_
!
! This method computes outer product between a matrix and vector.
!
! The result will be a rank 3 array.
!
!```fortran
! ans(i1,i2,i3)=anscoeff*ans(i1,i2,i3)+scale*a(i1,i2)*b(i3)
!```
!
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
! summary: Outerproduct between rank 2 and rank 2 array.
!
!# OuterProd
!
! This method computes outer product between two rank-2 arrays.
!
!$$
!y(i1,i2,i3,i4)=a(i1,i2)*b(i3,i4)
!$$
!

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
! summary: outer product between rank 2 and rank 2 array.
!
!# OuterProd_
!
! This method computes outer product between a rank 2 and rank2 array.
!
! The result will be a rank 4 array.
!
!```fortran
! ans(i1,i2,i3,i4)=anscoeff*ans(i1,i2,i3,i4)+scale*a(i1,i2)*b(i3,i4)
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
! summary: outer product between rank 2 and rank 3 array.
!
!# OuterProd
!
! This method computes outer product between rank 2 and rank 3
! array. The result will be a rank 5 array.
!
!```fortran
!ans(i1,i2,i3,i4,i5)=anscoeff*ans(i1,i2,i3,i4,i5)+scale*a(i1,i2)*b(i3,i4,i5)
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
! summary: The outer product between rank 2 and rank 4 array
!
!# OuterProd
!
! The outer product between rank 2 and rank 4 array. The answer will
! a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1,i2)*b(i3,i4,i5,i6)
!$$
!
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
! summary: Outerproduct between rank 3 and rank 1 array.
!
!# OuterProd
!
! This method returns outer product between a rank 3 and rank 1 array.
! The result is a rank 4 array.
!
!$$
!y(i1,i2,i3,i4)=a(i1,i2, i3)*b(i4)
!$$
!

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
! summary: Outerproduct of rank 3 and rank 2 array.
!
!# OuterProd
!
! This method computes outer product between rank 3 and rank 2 array.
! The result will be a rank 5 array.
!
!$$
!y(i1,i2,i3,i4,i5)=a(i1,i2, i3)*b(i4,i5)
!$$

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
! summary: Outerproduct between rank 3 and rank 3 array.
!
!# OuterProd
!
! This method computes outer product between rank 3 and rank3 array:
! The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1,i2,i3)*b(i4,i5,i6)
!$$
!

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
! summary: Outerproduct between rank 4 and a vector
!
!# OuterProd
!
! This method computes outer product between a rank 4 and a vector.
! The result will be a rank 5 array.
!
!$$
!y(i1,i2,i3,i4,i5)=a(i1,i2,i3,i4)*b(i5)
!$$
!

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
! summary: Outerproduct between rank 4 and a rank 2 array.
!
!# OuterProd
!
! This method computes outer product between a rank 4 and a rank 2 array.
! The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1,i2,i3,i4)*b(i5,i6)
!$$
!

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
! summary: Outerproduct between rank 4 and a rank 1 array.
!
!# OuterProd
!
! This method computes outer product between a rank 4 and a rank 1 array.
! The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1,i2,i3,i4,i5)*b(i6)
!$$
!

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
! summary: Outerproduct between rank 1, rank 1, rank1 array
!
!# OuterProd
!
! This method computes outer product between a rank 1, rank 1, and rank 1,
! array. The result will be a rank 3 array.
!
!$$
!y(i1,i2,i3)=a(i1)*b(i2)*c(i3)
!$$
!

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
! summary: Outer product between rank1, rank1, rank1 vector.
!
!# OuterProd_
!
! This method computes outer product between rank 1, rank 1, rank 1,
! array. The result will be a rank 3 array.
!
!$$
!y(i1,i2,i3)=a(i1)*b(i2)*c(i3)
!$$
!
!
!```fortran
! ans(i1,i2,i3)=anscoeff*ans(i1,i2,i3)+scale*a(i1)*b(i2)*c(i3)
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
! summary: Outer product between rank1, rank1, rank2 array.
!
!# OuterProd_
!
! This method computes outer product between rank 1, rank 1, rank 2,
! array. The result will be a rank 4 array.
!
!$$
!y(i1,i2,i3,i4)=a(i1)*b(i2)*c(i3,i4)
!$$
!
!
!```fortran
! ans(i1,i2,i3,i4)=anscoeff*ans(i1,i2,i3,i4)+scale*a(i1)*b(i2)*c(i3,i4)
!```
!
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
! summary: Outer product between rank1, rank1, rank2 array.
!
!# OuterProd_
!
! This method computes outer product between rank 1, rank 1, rank 2,
! array. The result will be a rank 4 array.
!
!$$
!y(i1,i2,i3,i4)=a(i1)*b(i2)*c(i3,i4)
!$$
!
!
!```fortran
! ans(i1,i2,i3,i4)=anscoeff*ans(i1,i2,i3,i4)+scale*a(i1)*b(i2)*c(i3,i4)
!```
!

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
! summary: Outer product between rank1, rank1, rank3 array.
!
!# OuterProd
!
! This method computes outer product between rank 1, rank 1, rank 3,
! array. The result will be a rank 5 array.
!
!$$
!y(i1,i2,i3,i4,i5)=a(i1)*b(i2)*c(i3,i4,i5)
!$$
!
!```fortran
! ans(i1,i2,i3,i4,i5)=anscoeff*ans(i1,i2,i3,i4,i5)
!                    +scale*a(i1)*b(i2)*c(i3,i4,i5)
!```
!

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
! summary: Outerproduct between rank 1, rank1, and rank4 array
!
!# OuterProd
!
! The outer product between rank 1, rank 1, and rank 4 array. The result
! will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1)*b(i2)*c(i3,i4,i5,i6)
!$$

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
! summary: Outerproduct of rank1, rank2, rank1 array
!
!# OuterProd
!
! This method returns outer product between rank 1, rank 2, and rank 1
! arary. The result will be a rank 4 array.
!
!$$
!y(i1,i2,i3,i4)=a(i1)*b(i2,i3)*c(i4)
!$$

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
! summary: Outerproduct of rank1, rank2, rank2 array
!
!# OuterProd
!
! This method returns outer product between rank 1, rank 2, and rank 2
! array. The result will be a rank 5 array.
!
!$$
!y(i1,i2,i3,i4,i5)=a(i1)*b(i2,i3)*c(i4,i5)
!$$

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
! summary: Outerproduct of rank1, rank2, rank3 array
!
!# OuterProd
!
! This method returns outer product between rank 1, rank 2, and rank 3
! array. The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1)*b(i2,i3)*c(i4,i5,i6)
!$$

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
! summary: Outerproduct of rank1, rank3, rank1 array
!
!# OuterProd
!
! This method returns outer product between rank 1, rank 3, and rank 1
! array. The result will be a rank 5 array.
!
!$$
!y(i1,i2,i3,i4,i5)=a(i1)*b(i2,i3,i4)*c(i5)
!$$

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
! summary: Outerproduct of rank1, rank3, rank2 array
!
!# OuterProd
!
! This method returns outer product between rank 1, rank 3, and rank 2
! array. The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1)*b(i2,i3,i4)*c(i5,i6)
!$$

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
! summary: Outerproduct of rank1, rank4, rank1 array
!
!# OuterProd
!
! This method returns outer product between rank 1, rank 4, and rank 1
! array. The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1)*b(i2,i3,i4,i5)*c(i6)
!$$

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
! summary: Outerproduct of rank2, rank1, rank1 array
!
!# OuterProd
!
! This method returns outer product between rank 2, rank 1, and rank 1
! array. The result will be a rank 4 array.
!
!$$
!y(i1,i2,i3,i4)=a(i1,i2)*b(i3)*c(i4)
!$$

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
! summary: Outerproduct of rank2, rank1, rank1 array
!
!# OuterProd_
!
! This method returns outer product between rank 2, rank 1, and rank 1
! array. The result will be a rank 4 array.
!
!$$
!y(i1,i2,i3,i4)=a(i1,i2)*b(i3)*c(i4)
!$$
!
!```fortran
!ans(i1,i2,i3,i4)=anscoeff*ans(i1,i2,i3,i4)+scale*a(i1,i2)*b(i3)*c(i4)
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
! summary: Outerproduct of rank2, rank1, rank2 array
!
!# OuterProd
!
! This method returns outer product between rank 2, rank 1, and rank 2
! array. The result will be a rank 5 array.
!
!$$
!y(i1,i2,i3,i4,i5)=a(i1,i2)*b(i3)*c(i4,i5)
!$$

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
! summary: Outerproduct of rank2, rank1, rank3 array
!
!# OuterProd
!
! This method returns outer product between rank 2, rank 1, and rank 3
! array. The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1,i2)*b(i3)*c(i4,i5,i6)
!$$

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
! summary: Outerproduct of rank2, rank2, rank1 array
!
!# OuterProd
!
! This method returns outer product between rank 2, rank 2, and rank 1
! array. The result will be a rank 5 array.
!
!$$
!y(i1,i2,i3,i4,i5)=a(i1,i2)*b(i3,i4)*c(i5)
!$$

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
! summary: Outerproduct of rank2, rank2, rank2 array
!
!# OuterProd
!
! This method returns outer product between rank 2, rank 2, and rank 2
! array. The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1,i2)*b(i3,i4)*c(i5,i6)
!$$

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
! summary: Outerproduct of rank3, rank1, rank1 array
!
!# OuterProd
!
! This method returns outer product between rank 3, rank 1, and rank 1
! array. The result will be a rank 5 array.
!
!$$
!y(i1,i2,i3,i4,i5)=a(i1,i2,i3)*b(i4)*c(i5)
!$$

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
! summary: Outerproduct of rank3, rank1, rank2 array
!
!# OuterProd
!
! This method returns outer product between rank 3, rank 1, and rank 2
! array. The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1,i2,i3)*b(i4)*c(i5,i6)
!$$

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
! summary: Outerproduct of rank3, rank2, rank1 array
!
!# OuterProd
!
! This method returns outer product between rank 3, rank 2, and rank 1
! array. The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1,i2,i3)*b(i4,i5)*c(i6)
!$$

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
! summary: Outerproduct of rank4, rank1, rank1 array
!
!# OuterProd
!
! This method returns outer product between rank 4, rank 1, and rank 1
! array. The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1,i2,i3,i4)*b(i5)*c(i6)
!$$

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
! summary: Outerproduct of rank1, rank1, rank1, rank1 array
!
!# OuterProd
!
! This method returns outer product between rank 1, rank 1, rank 1, and
! rank 1 array. The result will be a rank 4 array.
!
!$$
!y(i1,i2,i3,i4)=a(i1)*b(i2)*c(i3)*d(i4)
!$$

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
! summary: Outerproduct of rank1, rank1, rank1, rank2 array
!
!# OuterProd
!
! This method returns outer product between rank 1, rank 1, rank 1, and
! rank 2 array. The result will be a rank 5 array.
!
!$$
!y(i1,i2,i3,i4,i5)=a(i1)*b(i2)*c(i3)*d(i4,i5)
!$$

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
! summary: Outerproduct of rank1, rank1, rank1, rank3 array
!
!# OuterProd
!
! This method returns outer product between rank 1, rank 1, rank 1, and
! rank 3 array. The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1)*b(i2)*c(i3)*d(i4,i5,i6)
!$$

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
! summary: Outerproduct of rank1, rank1, rank2, rank1 array
!
!# OuterProd
!
! This method returns outer product between rank 1, rank 1, rank 2, and
! rank 1 array. The result will be a rank 5 array.
!
!$$
!y(i1,i2,i3,i4,i5)=a(i1)*b(i2)*c(i3,i4)*d(i5)
!$$

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
! summary: Outerproduct of rank1, rank1, rank2, rank2 array
!
!# OuterProd
!
! This method returns outer product between rank 1, rank 1, rank 2, and
! rank 2 array. The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1)*b(i2)*c(i3,i4)*d(i5,i6)
!$$

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
! summary: Outerproduct of rank1, rank1, rank3, rank1 array
!
!# OuterProd
!
! This method returns outer product between rank 1, rank 1, rank 3, and
! rank 1 array. The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1)*b(i2)*c(i3,i4,i5)*d(i6)
!$$

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
! summary: Outerproduct of rank1, rank2, rank1, rank1 array
!
!# OuterProd
!
! This method returns outer product between rank 1, rank 2, rank 1, and
! rank 1 array. The result will be a rank 5 array.
!
!$$
!y(i1,i2,i3,i4,i5)=a(i1)*b(i2,i3)*c(i4)*d(i5)
!$$

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
! summary: Outerproduct of rank1, rank2, rank1, rank2 array
!
!# OuterProd
!
! This method returns outer product between rank 1, rank 2, rank 1, and
! rank 2 array. The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1)*b(i2,i3)*c(i4)*d(i5,i6)
!$$

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
! summary: Outerproduct of rank1, rank2, rank2, rank1 array
!
!# OuterProd
!
! This method returns outer product between rank 1, rank 2, rank 2, and
! rank 1 array. The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1)*b(i2,i3)*c(i4,i5)*d(i6)
!$$

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
! summary: Outerproduct of rank1, rank3, rank1, rank1 array
!
!# OuterProd
!
! This method returns outer product between rank 1, rank 3, rank 1, and
! rank 1 array. The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1)*b(i2,i3,i4)*c(i5)*d(i6)
!$$

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
! summary: Outerproduct of rank2, rank1, rank1, rank1 array
!
!# OuterProd
!
! This method returns outer product between rank 2, rank 1, rank 1, and
! rank 1 array. The result will be a rank 5 array.
!
!$$
!y(i1,i2,i3,i4,i5)=a(i1,i2)*b(i3)*c(i4)*d(i5)
!$$

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
! summary: Outerproduct of rank2, rank1, rank1, rank2 array
!
!# OuterProd
!
! This method returns outer product between rank 2, rank 1, rank 1, and
! rank 2 array. The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1,i2)*b(i3)*c(i4)*d(i5,i6)
!$$

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
! summary: Outerproduct of rank2, rank1, rank2, rank1 array
!
!# OuterProd
!
! This method returns outer product between rank 2, rank 1, rank 2, and
! rank 1 array. The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1,i2)*b(i3)*c(i4,i5)*d(i6)
!$$

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
! summary: Outerproduct of rank2, rank2, rank1, rank1 array
!
!# OuterProd
!
! This method returns outer product between rank 2, rank 2, rank 1, and
! rank 1 array. The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1,i2)*b(i3,i4)*c(i5)*d(i6)
!$$

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
! summary: Outerproduct of rank3, rank1, rank1, rank1 array
!
!# OuterProd
!
! This method returns outer product between rank 3, rank 1, rank 1, and
! rank 1 array. The result will be a rank 6 array.
!
!$$
!y(i1,i2,i3,i4,i5,i6)=a(i1,i2,i3)*b(i4)*c(i5)*d(i6)
!$$

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
