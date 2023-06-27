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
USE GlobalData
IMPLICIT NONE
PRIVATE
PUBLIC :: OUTERPROD
PUBLIC :: Cross_Product
PUBLIC :: Vector_Product
PUBLIC :: VectorProduct

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

INTERFACE
  MODULE PURE FUNCTION vectorProduct_1(a, b) RESULT(c)
    ! Define INTENT of dummy argument
    REAL(REAL64), INTENT(IN) :: a(3), b(3)
    REAL(REAL64) :: c(3)
  END FUNCTION vectorProduct_1
END INTERFACE

INTERFACE
  MODULE PURE FUNCTION vectorProduct_2(a, b) RESULT(c)
    ! Define INTENT of dummy argument
    REAL(REAL32), INTENT(IN) :: a(3), b(3)
    REAL(REAL32) :: c(3)
  END FUNCTION vectorProduct_2
END INTERFACE

INTERFACE Cross_Product
  MODULE PROCEDURE vectorProduct_1, vectorProduct_2
END INTERFACE Cross_Product

INTERFACE Vector_Product
  MODULE PROCEDURE vectorProduct_1, vectorProduct_2
END INTERFACE Vector_Product

INTERFACE VectorProduct
  MODULE PROCEDURE vectorProduct_1, vectorProduct_2
END INTERFACE VectorProduct

!----------------------------------------------------------------------------
!                                                   OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         This FUNCTION returns outerproduct(matrix) of two vectors
!
!# Introduction
!
! $$\mathbf{ans} = \mathbf{a} \otimes \mathbf{b}$$

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r1(a, b) RESULT(ans)
    REAL(DFP), DIMENSION(:), INTENT(IN) :: a, b
    REAL(DFP), DIMENSION(SIZE(a), SIZE(b)) :: ans
  END FUNCTION outerprod_r1r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                  OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         This FUNCTION returns outerproduct
!
!# Introduction
!
! This FUNCTION returns outerproduct(matrix) of two vectors
! - $$\mathbf{ans} = \mathbf{a} \otimes \mathbf{b}$$
! - If `Sym` is .true. THEN symmetric part is returned

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r1s(a, b, Sym) RESULT(ans)
    ! Define INTENT of dummy variables
    REAL(DFP), INTENT(IN) :: a(:), b(:)
    REAL(DFP), DIMENSION(SIZE(a), SIZE(b)) :: ans
    LOGICAL(LGT), INTENT(IN) :: Sym
  END FUNCTION outerprod_r1r1s
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r1s
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                   OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r2(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP) :: ans(SIZE(a), SIZE(b, 1), SIZE(b, 2))
  END FUNCTION outerprod_r1r2
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r2
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                   OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r3(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :, :)
    REAL(DFP) :: ans(SIZE(a), SIZE(b, 1), SIZE(b, 2), SIZE(b, 3))
  END FUNCTION outerprod_r1r3
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r3
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                   OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r4(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :, :, :)
    REAL(DFP) :: ans(SIZE(a), SIZE(b, 1), SIZE(b, 2), SIZE(b, 3), SIZE(b, 4))
  END FUNCTION outerprod_r1r4
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r4
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                   OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r5(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :, :, :, :)
    REAL(DFP) :: ans(&
        & SIZE(a),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(b, 3),&
        & SIZE(b, 4),&
        & SIZE(b, 5))
  END FUNCTION outerprod_r1r5
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r5
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                  OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         This FUNCTION returns outerproduct

INTERFACE
  MODULE PURE FUNCTION outerprod_r2r1(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(b))
  END FUNCTION outerprod_r2r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r2r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                  OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE
  MODULE PURE FUNCTION outerprod_r2r2(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(b, 1),&
        & SIZE(b, 2))
  END FUNCTION outerprod_r2r2
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r2r2
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                  OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE
  MODULE PURE FUNCTION outerprod_r2r3(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:, :, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(b, 3))
  END FUNCTION outerprod_r2r3
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r2r3
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                  OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE
  MODULE PURE FUNCTION outerprod_r2r4(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:, :, :, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(b, 3),&
        & SIZE(b, 4))
  END FUNCTION outerprod_r2r4
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r2r4
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                  OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE
  MODULE PURE FUNCTION outerprod_r3r1(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP) :: ans(&
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(a, 3),&
        & SIZE(b))
  END FUNCTION outerprod_r3r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r3r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                  OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE
  MODULE PURE FUNCTION outerprod_r3r2(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP) :: ans(&
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(a, 3),&
        & SIZE(b, 1),&
        & SIZE(b, 2))
  END FUNCTION outerprod_r3r2
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r3r2
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                  OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE
  MODULE PURE FUNCTION outerprod_r3r3(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :)
    REAL(DFP), INTENT(IN) :: b(:, :, :)
    REAL(DFP) :: ans(&
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(a, 3),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(b, 3))
  END FUNCTION outerprod_r3r3
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r3r3
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                  OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE
  MODULE PURE FUNCTION outerprod_r4r1(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP) :: ans(&
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(a, 3),&
        & SIZE(a, 4),&
        & SIZE(b, 1))
  END FUNCTION outerprod_r4r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r4r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                  OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE
  MODULE PURE FUNCTION outerprod_r4r2(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP) :: ans(&
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(a, 3),&
        & SIZE(a, 4),&
        & SIZE(b, 1),&
        & SIZE(b, 2))
  END FUNCTION outerprod_r4r2
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r4r2
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                  OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a x b

INTERFACE
  MODULE PURE FUNCTION outerprod_r5r1(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :, :, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP) :: ans(&
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(a, 3),&
        & SIZE(a, 4),&
        & SIZE(a, 5),&
        & SIZE(b, 1))
  END FUNCTION outerprod_r5r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r5r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r1r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(c, 1))
  END FUNCTION outerprod_r1r1r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r1r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r1r2(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(c, 2))
  END FUNCTION outerprod_r1r1r2
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r1r2
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r1r3(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(c, 2),&
        & SIZE(c, 3))
  END FUNCTION outerprod_r1r1r3
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r1r3
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r1r4(a, b, c) RESULT(ans)
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
  END FUNCTION outerprod_r1r1r4
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r1r4
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r2r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(c, 1))
  END FUNCTION outerprod_r1r2r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r2r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r2r2(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(c, 1),&
        & SIZE(c, 2))
  END FUNCTION outerprod_r1r2r2
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r2r2
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r2r3(a, b, c) RESULT(ans)
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
  END FUNCTION outerprod_r1r2r3
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r2r3
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r3r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:, :, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(b, 3),&
        & SIZE(c, 1))
  END FUNCTION outerprod_r1r3r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r3r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r3r2(a, b, c) RESULT(ans)
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
  END FUNCTION outerprod_r1r3r2
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r3r2
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r4r1(a, b, c) RESULT(ans)
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
  END FUNCTION outerprod_r1r4r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r4r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION outerprod_r2r1r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(b, 1),&
        & SIZE(c, 1))
  END FUNCTION outerprod_r2r1r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r2r1r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION outerprod_r2r1r2(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:, :)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(c, 2))
  END FUNCTION outerprod_r2r1r2
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r2r1r2
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION outerprod_r2r1r3(a, b, c) RESULT(ans)
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
  END FUNCTION outerprod_r2r1r3
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r2r1r3
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION outerprod_r2r2r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:, :)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(b, 1),&
        & SIZE(b, 2),&
        & SIZE(c, 1))
  END FUNCTION outerprod_r2r2r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r2r2r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION outerprod_r2r2r2(a, b, c) RESULT(ans)
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
  END FUNCTION outerprod_r2r2r2
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r2r2r2
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION outerprod_r3r1r1(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(a, 2),&
        & SIZE(a, 3),&
        & SIZE(b, 1),&
        & SIZE(c, 1))
  END FUNCTION outerprod_r3r1r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r3r1r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION outerprod_r3r1r2(a, b, c) RESULT(ans)
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
  END FUNCTION outerprod_r3r1r2
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r3r1r2
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION outerprod_r3r2r1(a, b, c) RESULT(ans)
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
  END FUNCTION outerprod_r3r2r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r3r2r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c

INTERFACE
  MODULE PURE FUNCTION outerprod_r4r1r1(a, b, c) RESULT(ans)
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
  END FUNCTION outerprod_r4r1r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r4r1r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r1r1r1(a, b, c, d) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(IN) :: d(:)
    REAL(DFP) :: ans( &
        & SIZE(a, 1),&
        & SIZE(b, 1),&
        & SIZE(c, 1),&
        & SIZE(d, 1))
  END FUNCTION outerprod_r1r1r1r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r1r1r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r1r1r2(a, b, c, d) RESULT(ans)
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
  END FUNCTION outerprod_r1r1r1r2
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r1r1r2
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r1r1r3(a, b, c, d) RESULT(ans)
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
  END FUNCTION outerprod_r1r1r1r3
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r1r1r3
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r1r2r1(a, b, c, d) RESULT(ans)
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
  END FUNCTION outerprod_r1r1r2r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r1r2r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r1r2r2(a, b, c, d) RESULT(ans)
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
  END FUNCTION outerprod_r1r1r2r2
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r1r2r2
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r1r3r1(a, b, c, d) RESULT(ans)
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
  END FUNCTION outerprod_r1r1r3r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r1r3r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r2r1r1(a, b, c, d) RESULT(ans)
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
  END FUNCTION outerprod_r1r2r1r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r2r1r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r2r1r2(a, b, c, d) RESULT(ans)
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
  END FUNCTION outerprod_r1r2r1r2
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r2r1r2
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r2r2r1(a, b, c, d) RESULT(ans)
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
  END FUNCTION outerprod_r1r2r2r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r2r2r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE
  MODULE PURE FUNCTION outerprod_r1r3r1r1(a, b, c, d) RESULT(ans)
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
  END FUNCTION outerprod_r1r3r1r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r1r3r1r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE
  MODULE PURE FUNCTION outerprod_r2r1r1r1(a, b, c, d) RESULT(ans)
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
  END FUNCTION outerprod_r2r1r1r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r2r1r1r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE
  MODULE PURE FUNCTION outerprod_r2r1r1r2(a, b, c, d) RESULT(ans)
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
  END FUNCTION outerprod_r2r1r1r2
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r2r1r1r2
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE
  MODULE PURE FUNCTION outerprod_r2r1r2r1(a, b, c, d) RESULT(ans)
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
  END FUNCTION outerprod_r2r1r2r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r2r1r2r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE
  MODULE PURE FUNCTION outerprod_r2r2r1r1(a, b, c, d) RESULT(ans)
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
  END FUNCTION outerprod_r2r2r1r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r2r2r1r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-19
! update: 2021-12-19
! summary: a b c d

INTERFACE
  MODULE PURE FUNCTION outerprod_r3r1r1r1(a, b, c, d) RESULT(ans)
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
  END FUNCTION outerprod_r3r1r1r1
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE outerprod_r3r1r1r1
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ProductUtility
