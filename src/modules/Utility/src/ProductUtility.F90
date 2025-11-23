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
USE GlobalData, ONLY: DFP, REAL32, REAL64, LGT, I4B

IMPLICIT NONE

PRIVATE

PUBLIC :: OuterProd
PUBLIC :: OuterProd_

PUBLIC :: OTimesTilda

PUBLIC :: Cross_Product
PUBLIC :: Vector_Product
PUBLIC :: VectorProduct

!----------------------------------------------------------------------------
!                                                        OTimesTilda@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-13
! summary:  returns a space-time matrix from time and space matrix

INTERFACE OTimesTilda
  MODULE SUBROUTINE OTimesTilda1(a, b, ans, nrow, ncol, anscoeff, scale)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), INTENT(IN) :: anscoeff
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE OTimesTilda1
END INTERFACE OTimesTilda

!----------------------------------------------------------------------------
!                                                       OtimesTilda@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-13
! summary:  returns a space-time vector from time and space vector

INTERFACE OTimesTilda
  MODULE SUBROUTINE OTimesTilda2(a, b, ans, tsize, anscoeff, scale)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    REAL(DFP), INTENT(IN) :: anscoeff
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE OTimesTilda2
END INTERFACE OTimesTilda

!----------------------------------------------------------------------------
!                                            Cross_Product@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: This FUNCTION evaluate vectors product
!
!# Introduction
!         This FUNCTION evaluate vectors products
! $$\mathbf{ans} = \mathbf{a} \times \mathbf{b}$$

INTERFACE Vector_Product
  MODULE PURE FUNCTION vectorProduct_1(a, b) RESULT(c)
    ! Define INTENT of dummy argument
    REAL(REAL64), INTENT(IN) :: a(3), b(3)
    REAL(REAL64) :: c(3)
  END FUNCTION vectorProduct_1
END INTERFACE Vector_Product

INTERFACE Vector_Product
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
!                                                   OuterProd@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         This FUNCTION returns OuterProduct(matrix) of two vectors
!
!# Introduction
!
! $$\mathbf{ans} = \mathbf{a} \otimes \mathbf{b}$$

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1(a, b) RESULT(ans)
    REAL(DFP), DIMENSION(:), INTENT(IN) :: a, b
    REAL(DFP), DIMENSION(SIZE(a), SIZE(b)) :: ans
  END FUNCTION OuterProd_r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                  OuterProd_@ProductMethods
!----------------------------------------------------------------------------

INTERFACE OuterProd_
  MODULE PURE SUBROUTINE OuterProd_r1r1_(a, b, anscoeff, scale, ans, nrow, &
                                         ncol)
    REAL(DFP), DIMENSION(:), INTENT(IN) :: a, b
    REAL(DFP), INTENT(IN) :: anscoeff
    !! coefficient of ans
    !! ans = anscoeff * ans + scale * a \otimes b
    REAL(DFP), INTENT(IN) :: scale
    !! coefficient of a \otimes b
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! outerprod
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of data written in ans
  END SUBROUTINE OuterProd_r1r1_
END INTERFACE OuterProd_

!----------------------------------------------------------------------------
!                                                  OuterProd@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         This FUNCTION returns OuterProduct
!
!# Introduction
!
! This FUNCTION returns OuterProduct(matrix) of two vectors
! - $$\mathbf{ans} = \mathbf{a} \otimes \mathbf{b}$$
! - If `sym` is .true. THEN symmetric part is returned

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1s(a, b, sym) RESULT(ans)
    ! Define INTENT of dummy variables
    REAL(DFP), INTENT(IN) :: a(:), b(:)
    REAL(DFP), DIMENSION(SIZE(a), SIZE(b)) :: ans
    LOGICAL(LGT), INTENT(IN) :: sym
  END FUNCTION OuterProd_r1r1s
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                  OuterProd_@ProductMethods
!----------------------------------------------------------------------------

INTERFACE OuterProd_
  MODULE PURE SUBROUTINE OuterProd_r1r1s_(a, b, sym, anscoeff, scale, ans, &
                                          nrow, ncol)
    ! Define INTENT of dummy variables
    REAL(DFP), INTENT(IN) :: a(:), b(:)
    LOGICAL(LGT), INTENT(IN) :: sym
    REAL(DFP), INTENT(IN) :: anscoeff
    REAL(DFP), INTENT(IN) :: scale
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE OuterProd_r1r1s_
END INTERFACE OuterProd_

!----------------------------------------------------------------------------
!                                                   OuterProd@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r2(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP) :: ans(SIZE(a), SIZE(b, 1), SIZE(b, 2))
  END FUNCTION OuterProd_r1r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                   OuterProd_@ProductMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-05
! summary:  a x b

INTERFACE OuterProd_
  MODULE PURE SUBROUTINE OuterProd_r1r2_(a, b, anscoeff, scale, ans, &
                                         dim1, dim2, dim3)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: anscoeff, scale
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE OuterProd_r1r2_
END INTERFACE OuterProd_

!----------------------------------------------------------------------------
!                                                   OuterProd@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r3(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :, :)
    REAL(DFP) :: ans(SIZE(a), SIZE(b, 1), SIZE(b, 2), SIZE(b, 3))
  END FUNCTION OuterProd_r1r3
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                   OuterProd@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r4(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :, :, :)
    REAL(DFP) :: ans(SIZE(a), SIZE(b, 1), SIZE(b, 2), SIZE(b, 3), SIZE(b, 4))
  END FUNCTION OuterProd_r1r4
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                   OuterProd@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r5(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :, :, :, :)
    REAL(DFP) :: ans(&
        & SIZE(a),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(b, 3),&
        & SIZE(b, 4),&
        & SIZE(b, 5))
  END FUNCTION OuterProd_r1r5
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                  OuterProd@ProductMethods
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
!                                                   OuterProd_@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-04
! summary:  a x b

INTERFACE OuterProd_
  MODULE PURE SUBROUTINE OuterProd_r2r1_(a, b, anscoeff, scale, ans, &
                                         dim1, dim2, dim3)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: anscoeff, scale
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE OuterProd_r2r1_
END INTERFACE OuterProd_

!----------------------------------------------------------------------------
!                                                  OuterProd@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r2(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(b, 1),&
        & SIZE(b, 2))
  END FUNCTION OuterProd_r2r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                  OuterProd@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r3(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:, :, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(b, 3))
  END FUNCTION OuterProd_r2r3
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                  OuterProd@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r4(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:, :, :, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(b, 3),&
        & SIZE(b, 4))
  END FUNCTION OuterProd_r2r4
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                  OuterProd@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r3r1(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP) :: ans(&
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(a, 3),&
        & SIZE(b))
  END FUNCTION OuterProd_r3r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                  OuterProd@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r3r2(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP) :: ans(&
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(a, 3),&
        & SIZE(b, 1),&
        & SIZE(b, 2))
  END FUNCTION OuterProd_r3r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                  OuterProd@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r3r3(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :)
    REAL(DFP), INTENT(IN) :: b(:, :, :)
    REAL(DFP) :: ans(&
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(a, 3),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(b, 3))
  END FUNCTION OuterProd_r3r3
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                  OuterProd@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r4r1(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP) :: ans(&
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(a, 3),&
        & SIZE(a, 4),&
        & SIZE(b, 1))
  END FUNCTION OuterProd_r4r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                  OuterProd@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r4r2(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP) :: ans(&
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(a, 3),&
        & SIZE(a, 4),&
        & SIZE(b, 1),&
        & SIZE(b, 2))
  END FUNCTION OuterProd_r4r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                  OuterProd@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r5r1(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :, :, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP) :: ans(&
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(a, 3),&
        & SIZE(a, 4),&
        & SIZE(a, 5),&
        & SIZE(b, 1))
  END FUNCTION OuterProd_r5r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(c, 1))
  END FUNCTION OuterProd_r1r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE SUBROUTINE OuterProd_r1r1r1_( &
    a, b, c, anscoeff, scale, ans, dim1, dim2, dim3)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    REAL(DFP), INTENT(IN) :: anscoeff, scale
  END SUBROUTINE OuterProd_r1r1r1_
END INTERFACE

INTERFACE OuterProd_
  MODULE PROCEDURE OuterProd_r1r1r1_
END INTERFACE OuterProd_

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r2(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(c, 2))
  END FUNCTION OuterProd_r1r1r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r3(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(c, 2),&
        & SIZE(c, 3))
  END FUNCTION OuterProd_r1r1r3
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r4(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :, :, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(c, 2),&
        & SIZE(c, 3),&
        & SIZE(c, 4))
  END FUNCTION OuterProd_r1r1r4
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r2r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(c, 1))
  END FUNCTION OuterProd_r1r2r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r2r2(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(c, 1),&
        & SIZE(c, 2))
  END FUNCTION OuterProd_r1r2r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r2r3(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:, :, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(c, 1),&
        & SIZE(c, 2),&
        & SIZE(c, 3))
  END FUNCTION OuterProd_r1r2r3
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r3r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(b, 3),&
        & SIZE(c, 1))
  END FUNCTION OuterProd_r1r3r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r3r2(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :, :)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(b, 3),&
        & SIZE(c, 1),&
        & SIZE(c, 2))
  END FUNCTION OuterProd_r1r3r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r4r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :, :, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(b, 3),&
        & SIZE(b, 4),&
        & SIZE(c, 1))
  END FUNCTION OuterProd_r1r4r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION OuterProd_r2r1r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(b, 1), SIZE(c, 1))
  END FUNCTION OuterProd_r2r1r1
END INTERFACE

INTERFACE OuterProd
  MODULE PROCEDURE OuterProd_r2r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd_@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE SUBROUTINE OuterProd_r2r1r1_(a, b, c, ans, dim1, dim2, dim3, &
                                           dim4, scale, anscoeff)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
    REAL(DFP), INTENT(IN) :: scale, anscoeff
  END SUBROUTINE OuterProd_r2r1r1_
END INTERFACE

INTERFACE OuterProd_
  MODULE PROCEDURE OuterProd_r2r1r1_
END INTERFACE OuterProd_

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r1r2(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(c, 2))
  END FUNCTION OuterProd_r2r1r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r1r3(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(c, 2),&
        & SIZE(c, 3))
  END FUNCTION OuterProd_r2r1r3
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r2r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(c, 1))
  END FUNCTION OuterProd_r2r2r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r2r2(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(c, 1),&
        & SIZE(c, 2))
  END FUNCTION OuterProd_r2r2r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r3r1r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(a, 3),&
        & SIZE(b, 1),&
        & SIZE(c, 1))
  END FUNCTION OuterProd_r3r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r3r1r2(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(a, 3),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(c, 2))
  END FUNCTION OuterProd_r3r1r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r3r2r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(a, 3),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(c, 1))
  END FUNCTION OuterProd_r3r2r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r4r1r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(a, 3),&
        & SIZE(a, 4),&
        & SIZE(b, 1),&
        & SIZE(c, 1))
  END FUNCTION OuterProd_r4r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r1r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(d, 1))
  END FUNCTION OuterProd_r1r1r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r1r2(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(d, 1),&
        & SIZE(d, 2))
  END FUNCTION OuterProd_r1r1r1r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r1r3(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:, :, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(d, 1),&
        & SIZE(d, 2),&
        & SIZE(d, 3))
  END FUNCTION OuterProd_r1r1r1r3
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r2r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(c, 2),&
        & SIZE(d, 1))
  END FUNCTION OuterProd_r1r1r2r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r2r2(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP), INTENT(IN) :: d(:, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(c, 2),&
        & SIZE(d, 1),&
        & SIZE(d, 2))
  END FUNCTION OuterProd_r1r1r2r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r1r3r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :, :)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(c, 2),&
        & SIZE(c, 3),&
        & SIZE(d, 1))
  END FUNCTION OuterProd_r1r1r3r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r2r1r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(c, 1),&
        & SIZE(d, 1))
  END FUNCTION OuterProd_r1r2r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r2r1r2(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(c, 1),&
        & SIZE(d, 1),&
        & SIZE(d, 2))
  END FUNCTION OuterProd_r1r2r1r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r2r2r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(c, 1),&
        & SIZE(c, 2),&
        & SIZE(d, 1))
  END FUNCTION OuterProd_r1r2r2r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r1r3r1r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(b, 3),&
        & SIZE(c, 1),&
        & SIZE(d, 1))
  END FUNCTION OuterProd_r1r3r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r1r1r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(d, 1))
  END FUNCTION OuterProd_r2r1r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r1r1r2(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(d, 1),&
        & SIZE(d, 2))
  END FUNCTION OuterProd_r2r1r1r2
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r1r2r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(c, 2),&
        & SIZE(d, 1))
  END FUNCTION OuterProd_r2r1r2r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r2r2r1r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(c, 1),&
        & SIZE(d, 1))
  END FUNCTION OuterProd_r2r2r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!                                                            OuterProd@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE OuterProd
  MODULE PURE FUNCTION OuterProd_r3r1r1r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(a, 3),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(d, 1))
  END FUNCTION OuterProd_r3r1r1r1
END INTERFACE OuterProd

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ProductUtility
