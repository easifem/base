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

!> [[Utility]] module contains useful general purpose routines
MODULE Utility
USE GlobalData
USE Display_Method
USE ErrorHandling
IMPLICIT NONE

PRIVATE
INTEGER( I4B ), PARAMETER :: NPAR_ARTH=16,NPAR2_ARTH=8
INTEGER( I4B ), PARAMETER :: NPAR_GEOP=4,NPAR2_GEOP=2
INTEGER( I4B ), PARAMETER :: NPAR_CUMSUM=16
INTEGER( I4B ), PARAMETER :: NPAR_CUMPROD=8
INTEGER( I4B ), PARAMETER :: NPAR_POLY=8
INTEGER( I4B ), PARAMETER :: NPAR_POLYTERM=8


!----------------------------------------------------------------------------
!                                                  arange@FunctionalFortran
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 	3 March 2021
! summary: Returns a vector of reals given `start`,  `end`,  and `increment` values.
!
!### Introduction
!

INTERFACE
MODULE PURE FUNCTION arange_real( istart, iend, increment ) result( Ans )
  REAL( DFP ), INTENT( IN ) :: istart !! Start value of the array
  REAL( DFP ), INTENT( IN ) :: iend !! End value of the array
  REAL( DFP ), INTENT( IN ), OPTIONAL :: increment !! Array increment
  REAL( DFP ), DIMENSION( : ), ALLOCATABLE :: Ans
END FUNCTION
END INTERFACE
!----------------------------------------------------------------------------
!                                                                    arange
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 March 2021
! summary: Returns a vector of integer
!
!### Introduction
! Returns an array of integers given `istart`,  `iend`,  and `increment` values.
! Default value of increment is 1
! This function belongs to the generic function [[Utility:arange]]
!
!### Usage
!
!```fortran
!	arange(1,10,1)
! arange(1,10,2)
!```

INTERFACE
MODULE PURE FUNCTION arange_int (istart, iend, increment) result(Ans)
  integer(i4b), intent(in) :: istart
  integer(i4b), intent(in) :: iend
  integer(i4b), intent(in), optional :: increment
  integer(i4b), dimension(:), allocatable :: Ans
END FUNCTION
END INTERFACE

INTERFACE arange
  MODULE PROCEDURE arange_int, arange_real
END INTERFACE arange

PUBLIC arange


!----------------------------------------------------------------------------
!                                                     Head@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Returns the first element of array `x`.


INTERFACE
MODULE PURE FUNCTION head_int(x) RESULT(Ans)
  INTEGER( I4B ), INTENT( IN ) ::  x( : )
  INTEGER( I4B ) :: Ans
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Head@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Returns the first element of array `x`.


INTERFACE
MODULE PURE FUNCTION head_real(x) RESULT(Ans)
  REAL( DFP ), INTENT( IN ) ::  x( : )
  REAL( DFP ) :: Ans
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Head@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Returns the first element of array `x`.


INTERFACE
MODULE PURE FUNCTION head_char( x ) RESULT(Ans)
  CHARACTER( LEN = * ), INTENT( IN ) :: x
  CHARACTER( LEN = 1 ) :: Ans
END FUNCTION
END INTERFACE

INTERFACE HEAD
  MODULE PROCEDURE head_real, head_int, head_char
END INTERFACE HEAD

PUBLIC :: HEAD

!----------------------------------------------------------------------------
!                                                     Tail@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION tail_int( x ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: x( : )
  INTEGER( I4B ) :: Ans(SIZE(x)-1)
END FUNCTION tail_int
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Tail@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION tail_real( x ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: x( : )
  REAL( DFP ) :: Ans(SIZE(x)-1)
END FUNCTION tail_real
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Tail@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION tail_char( x ) RESULT( Ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: x
  CHARACTER( LEN = LEN(x) - 1 ) :: Ans
END FUNCTION tail_char
END INTERFACE

INTERFACE TAIL
  MODULE PROCEDURE tail_int, tail_real, tail_char
END INTERFACE TAIL

PUBLIC :: TAIL

!----------------------------------------------------------------------------
!                                                    SPLIT@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Returns the first half of the array `x` if `section == 1`,
!
!### Introduction
!
! Returns the first half of the array `x` if `section == 1`, the second half
! of the array `x` if `section == 2`, and an empty array otherwise. If `size
! (x) == 1`,  `split(x, 1)`  returns and empty array,  and `split(x, 2)`
! returns `x(1)`.


INTERFACE
MODULE PURE FUNCTION split_int( x, section ) RESULT( Ans )
  INTEGER( I4B ), DIMENSION( : ), INTENT(IN) :: x !! Input array
  INTEGER( I4B ), INTENT(IN) :: section !! Array section to return
  INTEGER( I4B ), DIMENSION( : ), ALLOCATABLE :: Ans
END FUNCTION split_int
END INTERFACE


!----------------------------------------------------------------------------
!                                                    SPLIT@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Returns the first half of the array `x` if `section == 1`,
!
!### Introduction
!
! Returns the first half of the array `x` if `section == 1`, the second half
! of the array `x` if `section == 2`, and an empty array otherwise. If `size
! (x) == 1`,  `split(x, 1)`  returns and empty array,  and `split(x, 2)`
! returns `x(1)`.

INTERFACE
MODULE PURE FUNCTION split_real( x, section ) RESULT( Ans )
  REAL( DFP ), DIMENSION( : ), INTENT(IN) :: x !! Input array
  INTEGER( I4B ), INTENT(IN) :: section !! Array section to return
  REAL( DFP ), DIMENSION( : ), ALLOCATABLE :: Ans
END FUNCTION split_real
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SPLIT@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Returns the first half of the array `x` if `section == 1`,
!
!### Introduction
!
! Returns the first half of the array `x` if `section == 1`, the second half
! of the array `x` if `section == 2`, and an empty array otherwise. If `size
! (x) == 1`,  `split(x, 1)`  returns and empty array,  and `split(x, 2)`
! returns `x(1)`.

INTERFACE
MODULE PURE FUNCTION split_char( x, section ) RESULT( Ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: x !! Input array
  INTEGER( I4B ), INTENT( IN ) :: section !! Array section to return
  CHARACTER( LEN = : ), ALLOCATABLE :: Ans
END FUNCTION split_char
END INTERFACE

INTERFACE SPLIT
  MODULE PROCEDURE split_int, split_char, split_real
END INTERFACE SPLIT

PUBLIC :: SPLIT

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate1( Mat, row, col )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, : )
  INTEGER( I4B ), INTENT( IN ) :: row, col
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate2( Mat, row )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Mat( : )
  INTEGER( I4B ), INTENT( IN ) :: row
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate3( Mat, i1, i2, i3 )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate4( Mat, row, col )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, : )
  INTEGER( I4B ), INTENT( IN ) :: row, col
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate5( Mat, row )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: Mat( : )
  INTEGER( I4B ), INTENT( IN ) :: row
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate6( Vec1, n1, Vec2, n2, Vec3, n3, Vec4, n4, &
  & Vec5, n5, Vec6, n6 )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT) :: Vec1( : ), Vec2( : )
  INTEGER( I4B ), ALLOCATABLE, OPTIONAL, INTENT( INOUT) :: Vec3( : ), &
    & Vec4( : ), Vec5( : ), Vec6( : )
  INTEGER( I4B ), INTENT( IN ) :: n1, n2
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: n3, n4, n5, n6
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate7( Vec1, n1, Vec2, n2, Vec3, n3, Vec4, n4, &
  & Vec5, n5, Vec6, n6 )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT) :: Vec1( : ), Vec2( : )
  REAL( DFP ), ALLOCATABLE, OPTIONAL, INTENT( INOUT) :: Vec3( : ), &
    & Vec4( : ), Vec5( : ), Vec6( : )
  INTEGER( I4B ), INTENT( IN ) :: n1, n2
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: n3, n4, n5, n6
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate8( A, nA, IA, nIA, JA, nJA )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: A( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: IA( : ), JA( : )
  INTEGER( I4B ), INTENT( IN ) :: nA, nIA, nJA
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate9( A, nA, IA, nIA )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: A( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: IA( : )
  INTEGER( I4B ), INTENT( IN ) :: nA, nIA
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate10( Mat, i1, i2, i3, i4 )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3, i4
END SUBROUTINE
END INTERFACE

INTERFACE Reallocate
  MODULE PROCEDURE Reallocate1, &
    & Reallocate2, &
    & Reallocate3, &
    & Reallocate4, &
    & Reallocate5, &
    & Reallocate6, &
    & Reallocate7, &
    & Reallocate8, &
    & Reallocate9, &
    & Reallocate10
END INTERFACE Reallocate

PUBLIC :: Reallocate

!----------------------------------------------------------------------------
!                                                         Cross_Product@Prod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This FUNCTION evaluate vectors product
!
!### Introduction
! 	This FUNCTION evaluate vectors products
! $$\mathbf{ans} = \mathbf{a} \times \mathbf{b}$$

INTERFACE
MODULE PURE FUNCTION vec_prod( a, b ) RESULT( c )
  ! Define INTENT of dummy argument
  REAL( DFP ), INTENT( IN ) :: a( 3 ), b( 3 )
  REAL( DFP ) :: c( 3 )
END FUNCTION vec_prod
END INTERFACE

INTERFACE Cross_Product
  MODULE PROCEDURE vec_prod
END INTERFACE Cross_Product

PUBLIC :: Cross_Product

INTERFACE Vector_Product
  MODULE PROCEDURE vec_prod
END INTERFACE Vector_Product

PUBLIC :: Vector_Product

INTERFACE VectorProduct
  MODULE PROCEDURE vec_prod
END INTERFACE VectorProduct

PUBLIC :: VectorProduct

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	This FUNCTION returns outerproduct(matrix) of two vectors
!
!### Introduction
!
! $$\mathbf{ans} = \mathbf{a} \otimes \mathbf{b}$$

INTERFACE
MODULE PURE FUNCTION OUTERPROD1_1( a,b ) RESULT( Ans )
  REAL(DFP), DIMENSION(:), INTENT(IN) :: a,b
  REAL(DFP), DIMENSION(SIZE(a),SIZE(b)) :: Ans
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	This FUNCTION returns outerproduct
!
!### Introduction
!
! This FUNCTION returns outerproduct(matrix) of two vectors
! - $$\mathbf{ans} = \mathbf{a} \otimes \mathbf{b}$$
! - If `Sym` is .true. THEN symmetric part is returned

INTERFACE
MODULE PURE FUNCTION OUTERPROD1_1_sym(a,b, Sym) RESULT( Ans )
  ! Define INTENT of dummy variables
  REAL(DFP), INTENT(IN) :: a ( : ), b ( : )
  REAL(DFP), DIMENSION(SIZE(a),SIZE(b)) :: Ans
  LOGICAL( LGT ), INTENT( IN ) :: Sym
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	This FUNCTION returns outerproduct
!
!### Introduction
!
! This FUNCTION returns outerprod between a matrix and a vector
! `Ans(:,:,i) = a(:,:) * b(i)`

INTERFACE
MODULE PURE FUNCTION OUTERPROD2_1(a,b) RESULT( Ans )
    REAL(DFP), INTENT(IN) :: a( :, : )
    REAL(DFP), INTENT(IN) :: b( : )
    REAL(DFP) :: Ans( SIZE( a, 1 ), SIZE( a, 2 ), SIZE(b) )
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	This FUNCTION returns outerproduct
!
!### Introduction
!
! This FUNCTION evaluate outerproduct between a 3D matrix and a vector
! - `Ans( :, :, :,  I ) = a( :, :, : ) * b( I )`

INTERFACE
MODULE PURE FUNCTION OUTERPROD3_1(a,b) RESULT( Ans )
  REAL(DFP), INTENT( IN ) :: a(:,:,:)
  REAL(DFP), INTENT( IN ) :: b(:)
  REAL(DFP) :: Ans( SIZE( a, 1 ), SIZE( a, 2 ), SIZE( a, 3 ), SIZE(  b ) )
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                            OUTERPROD@PROD
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	This FUNCTION returns outerproduct
!
!### Introduction
!
! This FUNCTION evaluates outer product between a matrix and two vector
!
! $$Ans = a \otimes b \otimes c$$

INTERFACE
MODULE PURE FUNCTION OUTERPROD2_11(a,b,c) RESULT( ANS )
  REAL(DFP), INTENT( IN ) :: a( :, : )
  REAL(DFP), INTENT( IN ) :: b( : ), c( : )
  REAL(DFP) :: ANS( SIZE( a, 1 ), SIZE( a, 2 ), SIZE( b ), SIZE(  c ) )
END FUNCTION
END INTERFACE

INTERFACE OUTERPROD
  MODULE PROCEDURE OUTERPROD1_1, OUTERPROD2_1, OUTERPROD3_1, OUTERPROD2_11, &
    & OUTERPROD1_1_sym
END INTERFACE OUTERPROD

PUBLIC :: OUTERPROD

!----------------------------------------------------------------------------
!                                                              Append@Append
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Append scalar INTEGER  to  INTEGER  vec tor

INTERFACE
MODULE PURE SUBROUTINE Append_I1( A, Entry )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: A( : )
  INTEGER( I4B ), INTENT( IN ) :: Entry
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Append@Append
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Append vector of INTEGER to INTEGER vector

INTERFACE
MODULE PURE SUBROUTINE Append_I2( A, Entry )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: A( : )
  INTEGER( I4B ), INTENT( IN ) :: Entry( : )
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Append@Append
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Append scalar REAL to the REAL vector

INTERFACE
MODULE PURE SUBROUTINE Append_R1( A, Entry )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: A( : )
  REAL( DFP ), INTENT( IN ) :: Entry
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Append@Append
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Append vector of REAL to REAL-vector

INTERFACE
MODULE PURE SUBROUTINE Append_R2( A, Entry )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: A( : )
  REAL( DFP ), INTENT( IN ) :: Entry( : )
END SUBROUTINE
END INTERFACE

INTERFACE Append
  MODULE PROCEDURE Append_I1, Append_I2, Append_R1, &
    Append_R2
END INTERFACE

PUBLIC :: Append

!----------------------------------------------------------------------------
!                                                         EvaluatePolynomial
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function can evaluate a polynomial
INTERFACE EvaluatePolynomial
  MODULE PROCEDURE eval_poly
END INTERFACE EvaluatePolynomial

PUBLIC :: EvaluatePolynomial

!----------------------------------------------------------------------------
!                                                             ExecuteCommand
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Generic function to execute a command

INTERFACE ExecuteCommand
  MODULE PROCEDURE exe_cmd
END INTERFACE ExecuteCommand

PUBLIC :: ExecuteCommand

!----------------------------------------------------------------------------
!                                                                 getUnitNo
!----------------------------------------------------------------------------

PUBLIC :: getUnitNo, Factorial

!----------------------------------------------------------------------------
!                                                                    Int2Str
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine converts real value to characters

INTERFACE Real2Str
  MODULE PROCEDURE SP2STR, DP2STR
END INTERFACE

PUBLIC :: Real2Str, Int2Str

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE outerdIFf
  MODULE PROCEDURE outerdIFf_r, outerdIFf_i, outerdIFf_d
END INTERFACE

PUBLIC :: outerdIFf

INTERFACE arth
  MODULE PROCEDURE arth_r, arth_d, arth_i
END INTERFACE

PUBLIC :: ARTH

INTERFACE assert_eq
  MODULE PROCEDURE assert_eq2, assert_eq3, assert_eq4, assert_eqn
END INTERFACE

PUBLIC :: ASSERT_EQ

INTERFACE ASSERT
  MODULE PROCEDURE assert_shape_2, assert_shape_3, assert_shape_4
END INTERFACE ASSERT

PUBLIC :: ASSERT

!----------------------------------------------------------------------------
!                                                                  SWAP@SWAP
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Swap two integer

INTERFACE
MODULE PURE SUBROUTINE swap_i( a, b )
  INTEGER( I4B ), INTENT( INOUT) :: a, b
END SUBROUTINE swap_i
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  SWAP@SWAP
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Swap two real

INTERFACE
MODULE PURE SUBROUTINE swap_r( a, b )
  REAL( DFP ), INTENT( INOUT) :: a, b
END SUBROUTINE swap_r
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  SWAP@SWAP
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Swap two real

INTERFACE
MODULE PURE SUBROUTINE swap_rv( a, b )
  REAL(DFP), INTENT(INOUT) :: a(:), b(:)
END SUBROUTINE swap_rv
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  SWAP@SWAP
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Subroutine for interchanging two complex numbers

INTERFACE
MODULE PURE SUBROUTINE swap_c( a, b )
  COMPLEX(DFPC), INTENT(INOUT) :: a,b
END SUBROUTINE swap_c
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SWAP@SWAP
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE swap_cv( a, b )
  COMPLEX(DFPC), INTENT(INOUT) :: a(:), b(:)
END SUBROUTINE swap_cv
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SWAP@SWAP
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE swap_cm( a, b )
  COMPLEX(DFPC), INTENT(INOUT) :: a(:,:), b(:,:)
END SUBROUTINE swap_cm
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SWAP@SWAP
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE masked_swap_rs( a, b, mask )
  REAL(DFP), INTENT(INOUT) :: a,b
  LOGICAL(LGT), INTENT(IN) :: mask
END SUBROUTINE masked_swap_rs
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SWAP@SWAP
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE masked_swap_rv( a, b, mask )
  REAL(DFP), INTENT(INOUT) :: a( : ), b( : )
  LOGICAL(LGT), INTENT(IN) :: mask( : )
END SUBROUTINE masked_swap_rv
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SWAP@SWAP
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE masked_swap_rm( a, b, mask )
  REAL(DFP), INTENT(INOUT) :: a( :, : ), b( :, : )
  LOGICAL(LGT), INTENT(IN) :: mask( :, : )
END SUBROUTINE masked_swap_rm
END INTERFACE

!>
! Generic subroutine for swapping
! swap_rv and swap_cv have been removed... they belong to EASIFEM_BLAS now
INTERFACE SWAP
  MODULE PROCEDURE swap_i,swap_r,swap_c,swap_cm, &
    & masked_swap_rs, masked_swap_rv, masked_swap_rm
END INTERFACE

PUBLIC :: SWAP

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Generic FUNCTION to get local of maximum value
INTERFACE IMAXLOC
  MODULE PROCEDURE imaxloc_r,imaxloc_i
END INTERFACE

PUBLIC :: IMAXLOC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Generic FUNCTION for getting location of minmum value
INTERFACE IMINLOC
  MODULE PROCEDURE iminloc_r
END INTERFACE IMINLOC

PUBLIC :: IMINLOC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Generic FUNCTION to get determinent of `2x2` and `3x3` matrix
INTERFACE Det
  MODULE PROCEDURE det_2D, det_3D
END INTERFACE Det

PUBLIC :: DET

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Generic subroutine to get inverse of `2x2` and `3x3` matrix
INTERFACE Inv
    MODULE PROCEDURE Inv_2D, Inv_3D
END INTERFACE Inv

PUBLIC :: INV

INTERFACE matmul
  MODULE PROCEDURE matmul_r3_r1, matmul_r4_r1, matmul_r3_r2, &
    & matmul_r1_r3, matmul_r2_r3
END INTERFACE matmul

PUBLIC :: matmul

!----------------------------------------------------------------------------
!                                                                     Radian
!----------------------------------------------------------------------------

INTERFACE radian
  MODULE PROCEDURE radian_dfp, radian_int
END INTERFACE

PUBLIC :: radian

!----------------------------------------------------------------------------
!                                                                    Degrees
!----------------------------------------------------------------------------

INTERFACE Degrees
  MODULE PROCEDURE degrees_dfp
END INTERFACE Degrees

PUBLIC :: Degrees

!----------------------------------------------------------------------------
!                                                          LOC_NearestPoint
!----------------------------------------------------------------------------

INTERFACE LOC_NearestPoint
  MODULE PROCEDURE Loc_Nearest_Point
END INTERFACE LOC_NearestPoint

PUBLIC :: LOC_NearestPoint

INTERFACE SearchNearestCoord
  MODULE PROCEDURE Loc_Nearest_Point
END INTERFACE SearchNearestCoord

PUBLIC :: SearchNearestCoord


!----------------------------------------------------------------------------
!                                                              HeapSort@Sort
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Heap sort

INTERFACE
MODULE PURE SUBROUTINE HEAPSORT_INT( array )
  INTEGER( I4B ), INTENT( INOUT) :: array( : )
END SUBROUTINE HEAPSORT_INT
END INTERFACE

!----------------------------------------------------------------------------
!                                                             HeapSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE HEAPSORT_REAL( array )
  REAL( DFP ), INTENT( INOUT) :: array( : )
END SUBROUTINE HEAPSORT_REAL
END INTERFACE

INTERFACE HeapSort
  MODULE PROCEDURE HEAPSORT_INT, HEAPSORT_REAL
END INTERFACE HeapSort

PUBLIC :: HeapSort

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort1vectR(vect1, low, high)
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect1
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE quickSort1vectR
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort1vectI(vect1, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect1
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE quickSort1vectI
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort2vectIR(vect1, vect2, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect1
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect2
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort2vectII(vect1, vect2, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect1
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect2
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort2vectRI(vect1, vect2, low, high)
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect1
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect2
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort2vectRR(vect1, vect2, low, high)
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect1
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect2
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort3vectIII(vect1, vect2, vect3, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort3vectIIR(vect1, vect2, vect3, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect1, vect2
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect3
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort3vectIRR(vect1, vect2, vect3, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect1
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect2, vect3
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort3vectIRI(vect1, vect2, vect3, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect1, vect3
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect2
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort3vectRRR(vect1, vect2, vect3, low, high)
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort3vectRRI(vect1, vect2, vect3, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect3
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect1, vect2
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort3vectRIR(vect1, vect2, vect3, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect2
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect1, vect3
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort3vectRII(vect1, vect2, vect3, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect2, vect3
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect1
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectIIII(vect1, vect2, vect3, vect4, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3, vect4
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectIIIR(vect1, vect2, vect3, vect4, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect4
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectIIRI(vect1, vect2, vect3, vect4, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect4
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect3
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectIIRR(vect1, vect2, vect3, vect4, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect1, vect2
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect3, vect4
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectIRRR(vect1, vect2, vect3, vect4, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect1
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect2, vect3, vect4
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectIRRI(vect1, vect2, vect3, vect4, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect1, vect4
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect2, vect3
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectIRIR(vect1, vect2, vect3, vect4, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect1, vect3
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect2, vect4
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectIRII(vect1, vect2, vect3, vect4, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect1, vect3, vect4
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect2
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectRRRR(vect1, vect2, vect3, vect4, low, high)
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3, vect4
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectRRRI(vect1, vect2, vect3, vect4, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect4
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectRRIR(vect1, vect2, vect3, vect4, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect3
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect4
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectRRII(vect1, vect2, vect3, vect4, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect3, vect4
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect1, vect2
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectRIRR(vect1, vect2, vect3, vect4, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect2
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect1, vect3, vect4
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectRIRI(vect1, vect2, vect3, vect4, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect2, vect4
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect1, vect3
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectRIIR(vect1, vect2, vect3, vect4, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect2, vect3
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect1, vect4
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectRIII(vect1, vect2, vect3, vect4, low, high)
  INTEGER( I4B ), DIMENSION(:), INTENT(INOUT) :: vect2, vect3, vect4
  REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: vect1
  INTEGER( I4B ), INTENT(IN) :: low, high
END SUBROUTINE
END INTERFACE


INTERFACE QUICKSORT
  MODULE PROCEDURE quickSort1vectI, quickSort1vectR, quickSort2vectII, &
    & quickSort2vectIR, quickSort2vectRR, quickSort2vectRI, &
    & quickSort3vectIII, quickSort3vectIIR, quickSort3vectIRI, &
    & quickSort3vectIRR, quickSort3vectRRR, quickSort3vectRRI, &
    & quickSort3vectRIR, quickSort3vectRII, quickSort4vectIIII, &
    & quickSort4vectIIIR, quickSort4vectIIRI, quickSort4vectIIRR, &
    & quickSort4vectIRII, quickSort4vectIRIR, quickSort4vectIRRI, &
    & quickSort4vectIRRR, quickSort4vectRIII, quickSort4vectRIIR, &
    & quickSort4vectRIRI, quickSort4vectRIRR, quickSort4vectRRII, &
    & quickSort4vectRRIR, quickSort4vectRRRI, quickSort4vectRRRR
END INTERFACE QUICKSORT

PUBLIC :: QUICKSORT

!----------------------------------------------------------------------------
!                                                                 SORT@SORT
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Recursive quicksort using binary tree pivot.

INTERFACE
MODULE PURE RECURSIVE FUNCTION SORT_INT( x ) RESULT( Ans )
  INTEGER( I4B ), DIMENSION(:), INTENT( IN ) :: x
  INTEGER( I4B ), DIMENSION( SIZE(x) ) :: Ans
END FUNCTION SORT_INT
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SORT@SORT
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Recursive quicksort using binary tree pivot.

INTERFACE
MODULE PURE RECURSIVE FUNCTION SORT_REAL( x ) RESULT( Ans )
  REAL( DFP ), DIMENSION(:), INTENT( IN ) :: x
  REAL( DFP ), DIMENSION( SIZE(x) ) :: Ans
END FUNCTION SORT_REAL
END INTERFACE

INTERFACE SORT
  MODULE PROCEDURE SORT_INT, SORT_REAL
END INTERFACE SORT

PUBLIC :: SORT

!----------------------------------------------------------------------------
!                                                                     Input
!----------------------------------------------------------------------------

INTERFACE Input
  MODULE PROCEDURE input_Int,input_Real,input_IntVec,input_RealVec,input_IntArray,input_RealArray,input_String,input_logical
END INTERFACE Input

PUBLIC :: Input

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: getExtension

!----------------------------------------------------------------------------
!                                                                   CONTAINS
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                               getExtension
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function get the extension from a file
!
! ## Usage
! ```fortran
! call display( getExtension("helloworld.f90") .EQ. "f90", &
! & msg="test1:: ")
! ```

FUNCTION getExtension( char ) RESULT(ext)
  CHARACTER( LEN=* ), INTENT( IN ) :: char
  CHARACTER(7) :: ext

  ! Define internal variables
  integer(int32) :: n,m
  ext="       "
  n=0
  n = index(char,".", back=.true.)
  m = len(char)
  ext(1:m-n+1) = char(n+1:m)
END FUNCTION

!----------------------------------------------------------------------------
!                                                                     Radian
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Convert degrees into radian
PURE FUNCTION radian_dfp( deg ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: deg
  REAL( DFP ) :: Ans
  Ans = deg / 180.0_DFP * 3.1415926535_DFP
END FUNCTION radian_dfp

!> authors: Dr. Vikas Sharma
!
! Converts degrees into radian
PURE FUNCTION radian_int( deg ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: deg
  REAL( DFP ) :: Ans
  Ans = REAL( deg, KIND=DFP ) / 180.0_DFP * 3.1415926535_DFP
END FUNCTION radian_int

!----------------------------------------------------------------------------
!                                                                    Degrees
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function converts radian into degrees
! Belongs to `Degrees`

PURE FUNCTION degrees_dfp( rad ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: rad
  REAL( DFP ) :: Ans
  Ans = rad / 3.1415926535_DFP * 180.0_DFP
END FUNCTION degrees_dfp

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine search the location of nearest point to x in the
! array of coordinates; Array
!
! ## Usage
! ```fortran
! real( dfp ) :: xij( 2, 20 ), x( 2 )
! integer( i4b ) :: id
!
! call random_number( xij )
! x = [11.0, 100.0]
! xij( 1:2, 15 ) = x
! id = searchNearestCoord(Array=xij, x=x)
! call display( id==15, "test4:: " )
!```

FUNCTION Loc_Nearest_Point( Array, x )  RESULT( id )
  REAL( DFP ), INTENT( IN ) :: Array( :, : )
  !! Nodal coordinates in XiJ format
  REAL( DFP ), INTENT( IN ) :: x( : )
  INTEGER( I4B ) :: id

  ! Define internal variables
  REAL( DFP ) :: xr( 3 )
  INTEGER( I4B ) :: i, n,m,norm,tr_norm

  n = SIZE( Array, 1 )
  m = SIZE( Array, 2 )

  IF( n .NE. SIZE(x) ) THEN
    CALL ErrorMSG(&
      & Msg="SearchNearestCoord >> size(Array,1) should be =size(x)", &
      & File= __FILE__, &
      & Line = __LINE__, &
      & Routine = "Loc_Nearest_Point(Array, x)" &
    )
    STOP
  ENDIF

  DO i = 1, m
    xr( 1:n ) = Array( 1:n, i )
    tr_norm = NORM2( xr(1:n) - x(1:n) )
    IF( i .EQ. 1 ) THEN
      norm = tr_norm
      id  = i
    ELSE
      IF( norm .GT. tr_norm ) THEN
        norm = tr_norm
        id  = i
      ELSE
        CYCLE
      END IF
    END IF
  END DO

END FUNCTION Loc_Nearest_Point

!----------------------------------------------------------------------------
!                                                                     Input
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
PURE FUNCTION input_Int( default, option ) RESULT( val )
  INTEGER( I4B ), INTENT( IN ) :: default
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: option
  INTEGER( I4B ) :: val

  IF(PRESENT(option) )THEN
    val=option
  ELSE
    val=default
  ENDIF

END FUNCTION

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

PURE FUNCTION input_Real(default,option) RESULT(val)
  REAL(DFP),INTENT(in) :: default
  REAL(DFP),OPTIONAL,INTENT(in)::option
  REAL(DFP) :: val

  IF(PRESENT(option) )THEN
    val=option
  ELSE
    val=default
  ENDIF
END FUNCTION

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

PURE FUNCTION input_IntVec( default, option ) RESULT( val )
  INTEGER( I4B ), INTENT( IN ) :: default(:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN )::option(:)
  INTEGER( I4B ), ALLOCATABLE :: val(:)

  IF( PRESENT( option ) ) THEN
    val=option
  ELSE
    val=default
  ENDIF

END FUNCTION

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

PURE FUNCTION input_Realvec( default, option ) RESULT( val )
  REAL( DFP ), INTENT( IN ) :: default(:)
  REAL( DFP ), OPTIONAL,INTENT( IN ) :: option(:)
  REAL( DFP ), ALLOCATABLE :: val(:)

  IF( PRESENT(option) )THEN
    val=option
  ELSE
    val=default
  ENDIF
END FUNCTION

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function input integer array
PURE FUNCTION input_IntArray(default,option) RESULT(val)
  INTEGER( I4B ), INTENT( IN ) :: default(:,:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN )::option(:,:)
  INTEGER( I4B ), ALLOCATABLE :: val(:,:)

  IF(PRESENT(option) )THEN
    val = option
  ELSE
    val = default
  ENDIF
END FUNCTION input_IntArray

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function input real array
PURE FUNCTION input_RealArray(default,option) RESULT(val)
  REAL( DFP ), INTENT( IN ) :: default(:,:)
  REAL( DFP ), OPTIONAL,INTENT( IN )::option(:,:)
  REAL( DFP ), ALLOCATABLE :: val(:,:)

  IF(PRESENT(option) )THEN
    val = option
  ELSE
    val = default
  ENDIF
END FUNCTION input_RealArray

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function input string
PURE FUNCTION input_String(default,option) RESULT(val)
  CHARACTER( LEN=* ), INTENT( IN ) :: default
  CHARACTER( LEN=* ), OPTIONAL, INTENT( IN )::option
  CHARACTER( 200 )  :: val

  IF(PRESENT(option) )THEN
    val=TRIM(option)
  ELSE
    val=TRIM(default)
  ENDIF
END FUNCTION input_String

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function input logical variables

PURE FUNCTION input_logical(default,option) RESULT(val)
  LOGICAL( LGT ), INTENT( IN ) :: default
  LOGICAL( LGT ), OPTIONAL, INTENT( IN )::option
  LOGICAL( LGT )  :: val

  IF(PRESENT(option) )THEN
    val=option
  ELSE
    val=default
  ENDIF

END FUNCTION input_logical

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This fuction performs following task
! `Ans(:,:) = a1(:,:,a)*a2(a)`

PURE FUNCTION matmul_r3_r1( a1, a2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: a1( :, :, : ), a2( : )
  REAL( DFP ) :: Ans( size( a1, 1 ), size( a1, 2 ) )

  INTEGER( I4B ) :: ii
  Ans = a2( 1 ) * a1( :, :, 1 )
  DO ii = 2, SIZE( a2 )
    Ans = Ans + a2( ii ) * a1( :, :, ii )
  END DO
END FUNCTION matmul_r3_r1

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This fuction performs following task
! `Ans(i,j) = a1(a)*a2(a,i,j)`

PURE FUNCTION matmul_r1_r3( a1, a2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: a1( : ), a2( :, :, : )
  REAL( DFP ) :: Ans( size( a2, 2 ), size( a2, 3 ) )

  INTEGER( I4B ) :: ii
  Ans = a1(1)*a2(1,:,:)
  DO ii = 2, SIZE( a1 )
    Ans = Ans + a1(ii)*a2(ii,:,:)
  END DO
END FUNCTION matmul_r1_r3

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This fuction performs following task
! `Ans(i,j,ip) = a1(i,I)*a2(I,j,ip)`

PURE FUNCTION matmul_r2_r3( a1, a2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: a1( :, : ), a2( :, :, : )
  REAL( DFP ) :: Ans( size( a1, 1 ), size( a2, 2 ), size( a2, 3 ) )

  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( a2, 3 )
    Ans( :, :, ii ) = MATMUL( a1, a2( :, :, ii ) )
  END DO
END FUNCTION matmul_r2_r3

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This fuction performs following task
! `Ans(:,:,:) = a1(:,:,:,a)*a2(a)`

PURE FUNCTION matmul_r4_r1( a1, a2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: a1( :, :, :, : ), a2( : )
  REAL( DFP ) :: Ans( size( a1, 1 ), size( a1, 2 ), size( a1, 3 ) )

  INTEGER( I4B ) :: ii
  Ans = a2( 1 ) * a1( :, :, :, 1 )
  DO ii = 2, SIZE( a2 )
    Ans = Ans + a2( ii ) * a1( :, :, :, ii )
  END DO
END FUNCTION matmul_r4_r1

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This fuction performs following task
! `Ans(i,j,ip) = a1(i,j,I)*a2(I,ip)`

PURE FUNCTION matmul_r3_r2( a1, a2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: a1( :, :, : ), a2( :, : )
  REAL( DFP ) :: Ans( size( a1, 1 ), size( a1, 2 ), size( a2, 2 ) )

  INTEGER( I4B ) :: ip
  DO ip = 1, SIZE( a2, 2)
    Ans( :,:, ip ) = MATMUL( a1, a2( :, ip ) )
  END DO
END FUNCTION matmul_r3_r2

!-----------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! this FUNCTION evaluate a polynomial
! - Power table contains the power of x, y, z
! - Its shape IF ( tTerms, 3 )
!	- Coeff is vector its size is tTerms
!	- X( 3 ) contains x, y, z

FUNCTION eval_poly( PowerTable, Coeff, X, tTerms ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: tTerms
  REAL( DFP ), DIMENSION( tTerms, 3 ), INTENT( IN ) :: PowerTable
  REAL( DFP ), DIMENSION( tTerms ), INTENT( IN ) :: Coeff
  REAL( DFP ), INTENT( IN ) :: X( 3 )
  REAL( DFP ) :: Ans

  ! Define internal variable
  INTEGER( I4B ) :: i

  Ans = 0.0_DFP
  DO i = 1, tTerms
    IF( Coeff( i ) .NE. 0.0_DFP ) THEN
      Ans = Ans + Coeff( i ) * &
          & ( X( 1 ) ** PowerTable( i, 1 ) &
          & * X( 2 ) ** PowerTable( i, 2 ) &
          & * X( 3 ) ** PowerTable( i, 3 ) &
          & )
    END IF
  END DO
END FUNCTION eval_poly

!------------------------------------------------------------------------------
!                                                                ExecuteCommand
!------------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine run a system commoand on terminal
SUBROUTINE exe_cmd( CMD, Str )
  CHARACTER( LEN = * ), INTENT( IN ) :: CMD, Str

  ! Define internal variables
  INTEGER( I4B ) :: CMDSTAT, EXITSTAT
  LOGICAL( LGT ) :: WAIT = .TRUE.
  CHARACTER( LEN = 300 ) :: CMDMSG = ""

  CALL EXECUTE_COMMAND_LINE( TRIM(CMD), CMDSTAT = CMDSTAT, &
    & EXITSTAT = EXITSTAT, WAIT = WAIT, CMDMSG = CMDMSG )

  IF( CMDSTAT .NE. 0 ) THEN

    IF( CMDSTAT .EQ. -1 ) THEN
      CALL ErrorMsg( &
        & File = __FILE__, &
        & Routine = "exe_cmd()", &
        & Line = __LINE__, &
        & MSG = "following command failed " // TRIM( CMDMSG ) )
    END IF

    CALL ErrorMsg( &
      & File = __FILE__, &
      & Routine = "exe_cmd()", &
      & Line = __LINE__, &
      & MSG = "following command failed " // TRIM( CMDMSG ) )

    STOP

  END IF

END SUBROUTINE exe_cmd

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This FUNCTION returns valid unit no for input output

FUNCTION getUnitNo( Str )
  ! Define INTENT of dumy varibales
  INTEGER( I4B ) :: getUnitNo
  CHARACTER( LEN = * ), INTENT( IN ) :: Str

  ! Define internal variables
  LOGICAL( LGT ) :: isOpen, isExist
  INTEGER( I4B ) :: Imin, Imax, I

  Imin = 10
  Imax = 1000

  DO I = Imin, Imax, 1
    INQUIRE( UNIT = I, OPENED = isOpen, EXIST = isExist )
    IF( isExist .AND. .NOT. isOpen ) EXIT
  END DO

  IF( isOpen .OR. .NOT. isExist ) THEN

    CALL ErrorMsg( &
      & File = __FILE__, &
      & Routine = "getUnitNo()", &
      & Line = __LINE__, &
      & MSG = " cannot find a valid unit number; Program Stopped" )
    STOP
  END IF

  getUnitNo = I

END FUNCTION getUnitNo

!------------------------------------------------------------------------------
!                                                                  Rank1ToRank3
!------------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Returns a 3D arrays of pointers

SUBROUTINE Rank1ToRank3( R1, R3, NSD, NNS, NNT )
  REAL( DFP ), DIMENSION( : ), CONTIGUOUS, TARGET :: R1
  REAL( DFP ), DIMENSION( :, :, : ), POINTER :: R3
  INTEGER( I4B ), INTENT( IN ) :: NSD, NNS, NNT

  INTEGER( I4B ) :: I, N, a, b, K
  ! Free the memory IF R3 is already allocated
  IF( ASSOCIATED( R3 ) ) DEALLOCATE( R3 )
  NULLIFY( R3 )
  N = SIZE( R1 )
  ! Flag-1
  IF( N .NE. ( NSD*NNS*NNT ) )THEN
    CALL ErrorMsg( &
      & File = __FILE__, &
      & Routine = "Rank1ToRank3()", &
      & Line = __LINE__, &
      & MSG = " Factor Problem" )
    RETURN
  END IF

  DO K = 1, NNT
    DO I = 1, NSD
      a = ( K - 1 ) * NSD * NNS  + ( I - 1 ) * NNS + 1
      b = a + NNS - 1
      R3( I:I, 1 : NNS, K:K ) => R1( a : b )
    END DO
  END DO
END SUBROUTINE Rank1ToRank3

!----------------------------------------------------------------------------
!                                                                 Factorial
!----------------------------------------------------------------------------
!> authors: Dr. Vikas Sharma
!
! This FUNCTION computes the factorial of an INTEGER

RECURSIVE FUNCTION Factorial( N ) RESULT( Fact )
    INTEGER( I4B ), INTENT( IN ) :: N
    INTEGER( I4B ) :: Fact
    IF ( N .EQ. 0 ) THEN
        Fact = 1
    ELSE
        Fact = N * Factorial( N - 1 )
    END IF
END FUNCTION Factorial

!------------------------------------------------------------------------------
!                                                                      Int2Str
!------------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Convert INTEGER  to  string

PURE FUNCTION Int2Str( I )
    INTEGER( I4B ), INTENT( IN ) :: I
    CHARACTER( LEN = 15 ) :: Int2Str
    CHARACTER( LEN = 15 ) :: Str
    WRITE( Str, "(I15)" ) I
    Int2Str = TRIM( ADJUSTL( Str ) )
END FUNCTION Int2Str

!----------------------------------------------------------------------------
!                                                                  Real2Str
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Convert REAL to string

FUNCTION SP2Str( I )
    REAL( SP ), INTENT( IN ) :: I
    CHARACTER( LEN = 20 ) :: SP2Str
    CHARACTER( LEN = 20 ) :: Str
    WRITE( Str, "(G17.7)" ) I
    SP2Str = TRIM( ADJUSTL( Str ) )
END FUNCTION SP2Str

!----------------------------------------------------------------------------
!                                                                  Real2Str
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Convert REAL to string

FUNCTION DP2Str( I )
    REAL( DP ), INTENT( IN ) :: I
    CHARACTER( LEN = 20 ) :: DP2Str
    CHARACTER( LEN = 20 ) :: Str
    WRITE( Str, "(G17.7)" ) I
    DP2Str = TRIM( ADJUSTL( Str ) )
END FUNCTION DP2Str

!------------------------------------------------------------------------------
!                                                                         arth
!------------------------------------------------------------------------------

PURE FUNCTION arth_r(first,increment,n)
  REAL(SP), INTENT(IN) :: first,increment
  INTEGER(I4B), INTENT(IN) :: n
  REAL(SP), DIMENSION(n) :: arth_r
  INTEGER(I4B) :: k,k2
  REAL(SP) :: temp
  IF (n > 0) arth_r(1)=first
  IF (n <= NPAR_ARTH) THEN
    DO k=2,n
      arth_r(k)=arth_r(k-1)+increment
    END DO
  ELSE
    DO k=2,NPAR2_ARTH
      arth_r(k)=arth_r(k-1)+increment
    END DO
    temp=increment*NPAR2_ARTH
    k=NPAR2_ARTH
    DO
      IF (k >= n) exit
      k2=k+k
      arth_r(k+1:min(k2,n))=temp+arth_r(1:min(k,n-k))
      temp=temp+temp
      k=k2
    END DO
  END IF
END FUNCTION arth_r
    !BL
PURE FUNCTION arth_d(first,increment,n)
  REAL(DP), INTENT(IN) :: first,increment
  INTEGER(I4B), INTENT(IN) :: n
  REAL(DP), DIMENSION(n) :: arth_d
  INTEGER(I4B) :: k,k2
  REAL(DP) :: temp
  IF (n > 0) arth_d(1)=first
  IF (n <= NPAR_ARTH) THEN
    DO k=2,n
      arth_d(k)=arth_d(k-1)+increment
    END DO
  ELSE
    DO k=2,NPAR2_ARTH
      arth_d(k)=arth_d(k-1)+increment
    END DO
    temp=increment*NPAR2_ARTH
    k=NPAR2_ARTH
    DO
      IF (k >= n) exit
      k2=k+k
      arth_d(k+1:min(k2,n))=temp+arth_d(1:min(k,n-k))
      temp=temp+temp
      k=k2
    END DO
  END IF
END FUNCTION arth_d
!
PURE FUNCTION arth_i(first,increment,n)
  INTEGER(I4B), INTENT(IN) :: first,increment,n
  INTEGER(I4B), DIMENSION(n) :: arth_i
  INTEGER(I4B) :: k,k2,temp
  IF (n > 0) arth_i(1)=first
  IF (n <= NPAR_ARTH) THEN
    DO k=2,n
      arth_i(k)=arth_i(k-1)+increment
    END DO
  ELSE
    DO k=2,NPAR2_ARTH
      arth_i(k)=arth_i(k-1)+increment
    END DO
    temp=increment*NPAR2_ARTH
    k=NPAR2_ARTH
    DO
      IF (k >= n) exit
      k2=k+k
      arth_i(k+1:min(k2,n))=temp+arth_i(1:min(k,n-k))
      temp=temp+temp
      k=k2
    END DO
  END IF
END FUNCTION arth_i

!------------------------------------------------------------------------------
!                                                                    OuterDIFf
!------------------------------------------------------------------------------

!BL
PURE FUNCTION outerdIFf_r(a,b)
  REAL( SP ), DIMENSION(:), INTENT(IN) :: a,b
  REAL( SP ), DIMENSION(size(a),size(b)) :: outerdIFf_r
  outerdIFf_r = SPREAD(a,dim=2,ncopies=size(b)) - &
    SPREAD(b,dim=1,ncopies=size(a))
END FUNCTION outerdIFf_r
!
PURE FUNCTION outerdIFf_d(a,b)
  REAL( DP ), DIMENSION(:), INTENT(IN) :: a,b
  REAL( DP ), DIMENSION(size(a),size(b)) :: outerdIFf_d
  outerdIFf_d = SPREAD(a,dim=2,ncopies=size(b)) - &
    SPREAD(b,dim=1,ncopies=size(a))
END FUNCTION outerdIFf_d
!BL
PURE FUNCTION outerdIFf_i(a,b)
  INTEGER( I4B ), DIMENSION(:), INTENT(IN) :: a,b
  INTEGER( I4B ), DIMENSION(size(a),size(b)) :: outerdIFf_i
  outerdIFf_i = SPREAD(a,dim=2,ncopies=size(b)) - &
    SPREAD(b,dim=1,ncopies=size(a))
END FUNCTION outerdIFf_i

!------------------------------------------------------------------------------
!                                                                      nrerror
!------------------------------------------------------------------------------

SUBROUTINE nrerror( string )
  CHARACTER(LEN=*), INTENT(IN) :: string
  write (*,*) 'nrerror: ', string
  STOP 'program terminated by nrerror'
END SUBROUTINE nrerror

!----------------------------------------------------------------------------
!                                                                  Assert_EQ
!----------------------------------------------------------------------------

FUNCTION assert_eq2(n1,n2,string)
  CHARACTER(LEN=*), INTENT(IN) :: string
  INTEGER( I4B ), INTENT(IN) :: n1,n2
  INTEGER( I4B ) :: assert_eq2
  IF (n1 .EQ. n2) THEN
    assert_eq2=n1
  ELSE
    CALL ErrorMsg( &
      & File = __FILE__, &
      & Routine = "Assert_Eq()", &
      & Line = __LINE__, &
      & MSG = " Sizes of Matrices are not the same; Program Stopped " )
    STOP
  END IF
END FUNCTION assert_eq2

!----------------------------------------------------------------------------
!                                                                  Assert_EQ
!----------------------------------------------------------------------------

FUNCTION assert_eq3(n1,n2,n3,string)
  CHARACTER(LEN=*), INTENT(IN) :: string
  INTEGER( I4B ), INTENT(IN) :: n1,n2,n3
  INTEGER( I4B ) :: assert_eq3
  IF (n1 == n2 .and. n2 == n3) THEN
    assert_eq3=n1
  ELSE
    CALL ErrorMsg( &
      & File = __FILE__, &
      & Routine = "Assert_Eq()", &
      & Line = __LINE__, &
      & MSG = " Sizes of Matrices are not the same; Program Stopped " )
    STOP
  END IF
END FUNCTION assert_eq3

!----------------------------------------------------------------------------
!                                                                  Assert_EQ
!----------------------------------------------------------------------------

FUNCTION assert_eq4(n1,n2,n3,n4,string)
  CHARACTER(LEN=*), INTENT(IN) :: string
  INTEGER( I4B ), INTENT(IN) :: n1,n2,n3,n4
  INTEGER( I4B ) :: assert_eq4
  IF (n1 == n2 .and. n2 == n3 .and. n3 == n4) THEN
    assert_eq4=n1
  ELSE
    CALL ErrorMsg( &
      & File = __FILE__, &
      & Routine = "Assert_Eq()", &
      & Line = __LINE__, &
      & MSG = " Sizes of Matrices are not the same; Program Stopped " )
    STOP
  END IF
END FUNCTION assert_eq4

!----------------------------------------------------------------------------
!                                                                  Assert_EQ
!----------------------------------------------------------------------------

FUNCTION assert_eqn(nn,string)
  CHARACTER( LEN=* ), INTENT( IN ) :: string
  INTEGER( I4B ), DIMENSION( : ), INTENT(IN) :: nn
  INTEGER( I4B ):: assert_eqn
  IF (all(nn(2:) == nn(1))) THEN
    assert_eqn=nn(1)
  ELSE
    CALL ErrorMsg( &
      & File = __FILE__, &
      & Routine = "Assert_Eq()", &
      & Line = __LINE__, &
      & MSG = " Sizes of Matrices are not the same; Program Stopped " )
    STOP
  END IF
END FUNCTION assert_eqn

!----------------------------------------------------------------------------
!                                                               ASSERT_SHAPE
!----------------------------------------------------------------------------

SUBROUTINE assert_shape_2( Mat, s, msg, file, line, routine )
  REAL( DFP ), INTENT( IN ) :: Mat( :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( 2 )
  INTEGER( I4B ), INTENT( IN ) :: line
  CHARACTER( LEN = * ), INTENT( IN ) :: msg, file, routine

  ! define internal variable
  INTEGER( I4B ) :: shape_mat( 2 )
  shape_mat = SHAPE( Mat )

  IF (all(shape_mat == s) ) THEN
    RETURN
  ELSE
    CALL ErrorMsg( &
      & File = file, &
      & Routine = routine, &
      & Line = line, &
      & MSG = msg )
    STOP
  END IF
END SUBROUTINE assert_shape_2

!----------------------------------------------------------------------------
!                                                               ASSERT_SHAPE
!----------------------------------------------------------------------------

SUBROUTINE assert_shape_3( Mat, s,  msg, file, line, routine )
  REAL( DFP ), INTENT( IN ) :: Mat( :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( 3 )
  INTEGER( I4B ), INTENT( IN ) :: line
  CHARACTER( LEN = * ), INTENT( IN ) :: msg, file, routine

  ! define internal variable
  INTEGER( I4B ) :: shape_mat( 3 )
  shape_mat = SHAPE( Mat )

  IF (all(shape_mat == s) ) THEN
    RETURN
  ELSE
    CALL ErrorMsg( File = file, Routine = routine, Line = line, &
      & MSG = msg )
    STOP
  END IF
END SUBROUTINE assert_shape_3

!----------------------------------------------------------------------------
!                                                               ASSERT_SHAPE
!----------------------------------------------------------------------------

SUBROUTINE assert_shape_4( Mat, s, msg, file, line, routine )
  REAL( DFP ), INTENT( IN ) :: Mat( :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( 4 )
  INTEGER( I4B ), INTENT( IN ) :: line
  CHARACTER( LEN = * ), INTENT( IN ) :: msg, file, routine

  ! define internal variable
  INTEGER( I4B ) :: shape_mat( 4 )
  shape_mat = SHAPE( Mat )

  IF (all(shape_mat == s) ) THEN
    RETURN
  ELSE
    CALL ErrorMsg( File = file, Routine = routine, Line = line, &
      & MSG = msg )
    STOP
  END IF
END SUBROUTINE assert_shape_4

!----------------------------------------------------------------------------
!                                                                   IMAXLOC
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Function for getting location of maximum value

PURE FUNCTION imaxloc_r(arr)
  REAL( DFP ), DIMENSION(:), INTENT(IN) :: arr
  INTEGER( I4B ) :: imaxloc_r
  INTEGER( I4B ), DIMENSION(1) :: imax
  imax = MAXLOC( arr(:) )
  imaxloc_r = imax(1)
END FUNCTION imaxloc_r

!----------------------------------------------------------------------------
!                                                                   IMAXLOC
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Function for getting location of maximum value

PURE FUNCTION imaxloc_i(iarr)
  INTEGER( I4B ), DIMENSION(:), INTENT(IN) :: iarr
  INTEGER( I4B ), DIMENSION(1) :: imax
  INTEGER( I4B ) :: imaxloc_i
  imax = MAXLOC( iarr( : ) )
  imaxloc_i = imax(1)
END FUNCTION imaxloc_i

!----------------------------------------------------------------------------
!                                                                    IMINLOC
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Function for getting location of minimum value

FUNCTION iminloc_r(arr)
  REAL(DFP), DIMENSION(:), INTENT(IN) :: arr
  INTEGER(I4B), DIMENSION(1) :: imin
  INTEGER(I4B) :: iminloc_r
  imin=MINLOC(arr(:))
  iminloc_r=imin(1)
END FUNCTION iminloc_r

!----------------------------------------------------------------------------
!                                                                        DET
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This FUNCTION returns determinent of 2 by 2 and 3 by 3 matrix

PURE FUNCTION det_2D( A ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: A( :, : )
  REAL( DFP ) :: Ans

  SELECT CASE( SIZE( A, 1 ) )
  CASE( 1 )
    Ans = A( 1, 1 )
  CASE( 2 )
    Ans = A(1,1)*A(2,2)-A(1,2)*A(2,1)
  CASE( 3 )
    Ans = A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2)) &
      & - A(1,2)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)) &
      & + A(1,3)*(A(2,1)*A(3,2)-A(3,1)*A(2,2))
  CASE( 4 )
    Ans =  A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))&
      & + A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4)) &
      & + A(2,4)*(A(3,2)*A(4,3) &
      & - A(3,3)*A(4,2)))-A(1,2)*(A(2,1)*(A(3,3)*A(4,4) &
      & - A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4)) &
      & + A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))) &
      & + A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2)) &
      & + A(2,2)*(A(3,4)*A(4,1) &
      & - A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))) &
      & - A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)) &
      & + A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3)) &
      & + A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
  END SELECT
END FUNCTION det_2D

!----------------------------------------------------------------------------
!                                                                        DET
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This FUNCTION returns the determinent of matrix

PURE FUNCTION det_3D( A ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: A( :, :, : )
  REAL( DFP ), ALLOCATABLE :: Ans( : )
  INTEGER( I4B ) :: i, n
  n = SIZE( A, 3 )
  ALLOCATE( Ans( n ) )
  DO i = 1, n
    Ans( i ) = Det( A( :, :, i ) )
  END DO
END FUNCTION det_3D

!----------------------------------------------------------------------------
!                                                                        Inv
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine returns inverse of 2 by 2 and 3 by 3 matrix

PURE SUBROUTINE Inv_2D( invA, A )
  REAL( DFP ), INTENT( INOUT ) :: invA( :, : )
  REAL( DFP ), INTENT( IN ) :: A( :, : )

  !Define internal variables
  REAL( DFP ) :: d, co( 4, 4 )

  d = det( A )

  IF( ABS( d ) .LT. ZERO ) THEN
    invA = 0.0_DFP
  ELSE
    SELECT CASE( SIZE( A, 1 ) )
    CASE( 1 )

      invA = 1.0 / d

    CASE( 2 )

      invA(1,1) =  A(2,2)/d
      invA(1,2) = -A(1,2)/d
      invA(2,1) = -A(2,1)/d
      invA(2,2) =  A(1,1)/d

    CASE( 3 )

      co(1,1) =  (A(2,2)*A(3,3)-A(2,3)*A(3,2))
      co(1,2) = -(A(2,1)*A(3,3)-A(2,3)*A(3,1))
      co(1,3) = +(A(2,1)*A(3,2)-A(2,2)*A(3,1))
      co(2,1) = -(A(1,2)*A(3,3)-A(1,3)*A(3,2))
      co(2,2) = +(A(1,1)*A(3,3)-A(1,3)*A(3,1))
      co(2,3) = -(A(1,1)*A(3,2)-A(1,2)*A(3,1))
      co(3,1) = +(A(1,2)*A(2,3)-A(1,3)*A(2,2))
      co(3,2) = -(A(1,1)*A(2,3)-A(1,3)*A(2,1))
      co(3,3) = +(A(1,1)*A(2,2)-A(1,2)*A(2,1))

      invA = TRANSPOSE(co( 1:3, 1:3 ) ) / d

    CASE( 4 )

      co(1,1) = A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+ &
                A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+ &
                A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))
      co(1,2) = A(2,1)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+ &
                A(2,3)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+ &
                A(2,4)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))
      co(1,3) = A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+ &
                A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+ &
                A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))
      co(1,4) = A(2,1)*(A(3,3)*A(4,2)-A(3,2)*A(4,3))+ &
                A(2,2)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))+ &
                A(2,3)*(A(3,2)*A(4,1)-A(3,1)*A(4,2))
      co(2,1) = A(1,2)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+ &
                A(1,3)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+ &
                A(1,4)*(A(3,3)*A(4,2)-A(3,2)*A(4,3))
      co(2,2) = A(1,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+ &
                A(1,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+ &
                A(1,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))
      co(2,3) = A(1,1)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+ &
                A(1,2)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+ &
                A(1,4)*(A(3,2)*A(4,1)-A(3,1)*A(4,2))
      co(2,4) = A(1,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+ &
                A(1,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+ &
                A(1,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))
      co(3,1) = A(1,2)*(A(2,3)*A(4,4)-A(2,4)*A(4,3))+ &
                A(1,3)*(A(2,4)*A(4,2)-A(2,2)*A(4,4))+ &
                A(1,4)*(A(2,2)*A(4,3)-A(2,3)*A(4,2))
      co(3,2) = A(1,1)*(A(2,4)*A(4,3)-A(2,3)*A(4,4))+ &
                A(1,3)*(A(2,1)*A(4,4)-A(2,4)*A(4,1))+ &
                A(1,4)*(A(2,3)*A(4,1)-A(2,1)*A(4,3))
      co(3,3) = A(1,1)*(A(2,2)*A(4,4)-A(2,4)*A(4,2))+ &
                A(1,2)*(A(2,4)*A(4,1)-A(2,1)*A(4,4))+ &
                A(1,4)*(A(2,1)*A(4,2)-A(2,2)*A(4,1))
      co(3,4) = A(1,1)*(A(2,3)*A(4,2)-A(2,2)*A(4,3))+ &
                A(1,2)*(A(2,1)*A(4,3)-A(2,3)*A(4,1))+ &
                A(1,3)*(A(2,2)*A(4,1)-A(2,1)*A(4,2))
      co(4,1) = A(1,2)*(A(2,4)*A(3,3)-A(2,3)*A(3,4))+ &
                A(1,3)*(A(2,2)*A(3,4)-A(2,4)*A(3,2))+ &
                A(1,4)*(A(2,3)*A(3,2)-A(2,2)*A(3,3))
      co(4,2) = A(1,1)*(A(2,3)*A(3,4)-A(2,4)*A(3,3))+ &
                A(1,3)*(A(2,4)*A(3,1)-A(2,1)*A(3,4))+ &
                A(1,4)*(A(2,1)*A(3,3)-A(2,3)*A(3,1))
      co(4,3) = A(1,1)*(A(2,4)*A(3,2)-A(2,2)*A(3,4))+ &
                A(1,2)*(A(2,1)*A(3,4)-A(2,4)*A(3,1))+ &
                A(1,4)*(A(2,2)*A(3,1)-A(2,1)*A(3,2))
      co(4,4) = A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))+ &
                A(1,2)*(A(2,3)*A(3,1)-A(2,1)*A(3,3))+ &
                A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1))

      invA = TRANSPOSE(co)/d

    END SELECT
  END IF

END SUBROUTINE Inv_2D

!----------------------------------------------------------------------------
!                                                                        Inv
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine returns inverse of 2 by 2 and 3 by 3 matrix

PURE SUBROUTINE Inv_3D( invA, A )
  REAL( DFP ), INTENT( INOUT ) :: invA( :, :, : )
  REAL( DFP ), INTENT( IN ) :: A( :, :, : )

  ! define internal variables
  INTEGER( I4B ) :: i, n

  n = SIZE( A, 3 )

  DO i = 1,n
    CALL Inv( invA = invA( :, :, i ), A = A( :, :, i ) )
  END DO
END SUBROUTINE Inv_3D

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Utility
