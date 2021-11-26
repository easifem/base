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


!----------------------------------------------------------------------------
!                                            Cross_Product@ProductMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:          This FUNCTION evaluate vectors product
!
!# Introduction
!         This FUNCTION evaluate vectors products
! $$\mathbf{ans} = \mathbf{a} \times \mathbf{b}$$

INTERFACE
  MODULE PURE FUNCTION vectorProduct_1(a, b) RESULT(c)
    ! Define INTENT of dummy argument
    REAL(Real64), INTENT(IN) :: a(3), b(3)
    REAL(Real64) :: c(3)
  END FUNCTION vectorProduct_1
END INTERFACE

INTERFACE
  MODULE PURE FUNCTION vectorProduct_2(a, b) RESULT(c)
    ! Define INTENT of dummy argument
    REAL(Real32), INTENT(IN) :: a(3), b(3)
    REAL(Real32) :: c(3)
  END FUNCTION vectorProduct_2
END INTERFACE

INTERFACE Cross_Product
  MODULE PROCEDURE vectorProduct_1, vectorProduct_2
END INTERFACE Cross_Product

PUBLIC :: Cross_Product

INTERFACE Vector_Product
  MODULE PROCEDURE vectorProduct_1, vectorProduct_2
END INTERFACE Vector_Product

PUBLIC :: Vector_Product

INTERFACE VectorProduct
  MODULE PROCEDURE vectorProduct_1, vectorProduct_2
END INTERFACE VectorProduct

PUBLIC :: VectorProduct

!----------------------------------------------------------------------------
!                                                   OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         This FUNCTION returns outerproduct(matrix) of two vectors
!
!# Introduction
!
! $$\mathbf{ans} = \mathbf{a} \otimes \mathbf{b}$$

INTERFACE
  MODULE PURE FUNCTION outerprod_1(a, b) RESULT(Ans)
    REAL(DFP), DIMENSION(:), INTENT(IN) :: a, b
    REAL(DFP), DIMENSION(SIZE(a), SIZE(b)) :: Ans
  END FUNCTION outerprod_1
END INTERFACE

INTERFACE OUTERPROD
   MODULE PROCEDURE outerprod_1
END INTERFACE OUTERPROD

PUBLIC :: OUTERPROD

!----------------------------------------------------------------------------
!                                                  OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         This FUNCTION returns outerproduct
!
!# Introduction
!
! This FUNCTION returns outerproduct(matrix) of two vectors
! - $$\mathbf{ans} = \mathbf{a} \otimes \mathbf{b}$$
! - If `Sym` is .true. THEN symmetric part is returned

INTERFACE
  MODULE PURE FUNCTION outerprod_2(a, b, Sym) RESULT(Ans)
    ! Define INTENT of dummy variables
    REAL(DFP), INTENT(IN) :: a(:), b(:)
    REAL(DFP), DIMENSION(SIZE(a), SIZE(b)) :: Ans
    LOGICAL(LGT), INTENT(IN) :: Sym
  END FUNCTION outerprod_2
END INTERFACE

INTERFACE OUTERPROD
   MODULE PROCEDURE outerprod_2
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                  OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         This FUNCTION returns outerproduct
!
!# Introduction
!
! This FUNCTION returns outerprod between a matrix and a vector
! `Ans(:,:,i) = a(:,:) * b(i)`

INTERFACE
  MODULE PURE FUNCTION outerprod_3(a, b) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP) :: Ans(SIZE(a, 1), SIZE(a, 2), SIZE(b))
  END FUNCTION outerprod_3
END INTERFACE

INTERFACE OUTERPROD
   MODULE PROCEDURE outerprod_3
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                  OUTERPROD@ProductMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         This FUNCTION returns outerproduct
!
!# Introduction
!
! This FUNCTION evaluate outerproduct between a 3D matrix and a vector
! - `Ans( :, :, :,  I ) = a( :, :, : ) * b( I )`

INTERFACE
  MODULE PURE FUNCTION outerprod_4(a, b) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: a(:, :, :)
    REAL(DFP), INTENT(IN) :: b(:)
    REAL(DFP) :: Ans(SIZE(a, 1), SIZE(a, 2), SIZE(a, 3), SIZE(b))
  END FUNCTION outerprod_4
END INTERFACE

INTERFACE OUTERPROD
   MODULE PROCEDURE outerprod_4
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         This FUNCTION returns outerproduct
!
!# Introduction
!
! This FUNCTION evaluates outer product between a matrix and two vector
!
! $$Ans = a \otimes b \otimes c$$

INTERFACE
  MODULE PURE FUNCTION outerprod_5(a, b, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:), c(:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(b), SIZE(c))
  END FUNCTION outerprod_5
END INTERFACE

INTERFACE OUTERPROD
   MODULE PROCEDURE outerprod_5
END INTERFACE OUTERPROD

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         This FUNCTION returns outerproduct
!
!# Introduction
!
! This FUNCTION evaluates outer product between a matrix and two vector
!
! $$Ans = a \otimes b \otimes c$$

INTERFACE
  MODULE PURE FUNCTION outerprod_6(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(:, :)
    REAL(DFP), INTENT(IN) :: b(:,:)
    REAL(DFP) :: ans(SIZE(a, 1), SIZE(a, 2), SIZE(b,1), SIZE(b,2))
  END FUNCTION outerprod_6
END INTERFACE

INTERFACE OUTERPROD
   MODULE PROCEDURE outerprod_6
END INTERFACE OUTERPROD