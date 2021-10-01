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

!> This module contains useful general purpose routines
MODULE Utility
USE GlobalData
USE Display_Method
USE ErrorHandling
USE ISO_C_BINDING
IMPLICIT NONE

PRIVATE
INTEGER( I4B ), PARAMETER :: NPAR_ARTH=16,NPAR2_ARTH=8
INTEGER( I4B ), PARAMETER :: NPAR_GEOP=4,NPAR2_GEOP=2
INTEGER( I4B ), PARAMETER :: NPAR_CUMSUM=16
INTEGER( I4B ), PARAMETER :: NPAR_CUMPROD=8
INTEGER( I4B ), PARAMETER :: NPAR_POLY=8
INTEGER( I4B ), PARAMETER :: NPAR_POLYTERM=8

!----------------------------------------------------------------------------
!                                                            APPROXEQ@APPROX
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 Apr 2021
! summary: 	returns bool logical indicating if a and b are approximately equal
!
!### Introduction
! This routine just does a simple absolute comparison using an epsilon that is
! a compile time constant. It should be used whenever possible because it has
! the least overhead. However, it is not appropriate to use when a and b
! are either very large or very small.
!

INTERFACE
MODULE ELEMENTAL FUNCTION approxeq_abs_real(a,b) RESULT(ans)
  REAL( DFP ), INTENT( IN ) :: a, b
  LOGICAL( LGT ) :: ans
END FUNCTION
END INTERFACE

INTERFACE OPERATOR(.APPROXEQ.)
  MODULE PROCEDURE approxeq_abs_real
ENDINTERFACE

INTERFACE OPERATOR(.APPROXEQA.)
  MODULE PROCEDURE approxeq_abs_real
END INTERFACE

PUBLIC :: OPERATOR( .APPROXEQ. )
PUBLIC :: OPERATOR( .APPROXEQA. )

!----------------------------------------------------------------------------
!                                                           APPROXR@APPROX
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 April 2021
! summary: returns bool logical indicating if a and b are approximately equal
!
!### Introduction
! This performs a relative comparison by scaling the default epsilon value to
! the size of the larger of the two. It should be used when @c and @b are of
! the same magnitude and very large or very small. If either @c a or @c b is
! zero (exactly) then this routine is equivalent to an absolute comparison.

INTERFACE
MODULE ELEMENTAL FUNCTION approxeq_rel_real(a,b) RESULT(Ans)
  REAL( DFP ),INTENT(IN) :: a,b
  LOGICAL( LGT ) :: Ans
END FUNCTION approxeq_rel_real
END INTERFACE

INTERFACE OPERATOR( .APPROXEQR. )
  MODULE PROCEDURE approxeq_rel_real
END INTERFACE

PUBLIC :: OPERATOR( .APPROXEQR. )

!----------------------------------------------------------------------------
!                                                          APPROXEQF@APPROX
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 Apr 2021
! summary: 	returns bool logical indicating if a and b are approximately equal
!
!### Introduction
! This performs a comparison of the binary representation of the two reals
! to compare the binary units in the last place (ULP). If the two reals differ
! on the floating point number line by 10 or less representable floating point
! reals then they are considered equal. In theory, this is the most appropriate comparison to use, but will break down near zero.


INTERFACE
MODULE ELEMENTAL FUNCTION approxeq_ulp_real(a,b) RESULT(Ans)
  REAL( DFP ), INTENT( IN ) :: a, b
  LOGICAL( LGT ) :: Ans
END FUNCTION
END INTERFACE

INTERFACE OPERATOR(.APPROXEQF.)
  MODULE PROCEDURE approxeq_ulp_real
ENDINTERFACE

PUBLIC :: OPERATOR(.APPROXEQF.)

!----------------------------------------------------------------------------
!                                                            APPROXLE@APPROX
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 Apr 2021
! summary: 	Defines the operation when comparing two single precision reals
! with .APPROXLE.

INTERFACE
MODULE ELEMENTAL FUNCTION approxle_real( r1, r2 ) RESULT( Ans )
  REAL( DFP ),INTENT(IN) :: r1
  REAL( DFP ),INTENT(IN) :: r2
  LOGICAL( LGT ) :: Ans
END FUNCTION
END INTERFACE

INTERFACE OPERATOR( .ARROXLE. )
  MODULE PROCEDURE approxle_real
END INTERFACE

PUBLIC :: OPERATOR( .ARROXLE. )

!----------------------------------------------------------------------------
!                                                            APPROXGE@APPROX
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 April 2021
! summary: 	Defines the operation when comparing two single precision reals
! with .APPROXGE.

INTERFACE
MODULE ELEMENTAL FUNCTION approxge_real(r1,r2) RESULT(Ans)
  REAL( DFP ), INTENT( IN ) :: r1
  REAL( DFP ), INTENT( IN ) :: r2
  LOGICAL( LGT ) :: Ans
END FUNCTION
END INTERFACE

INTERFACE OPERATOR( .ARROXGE. )
  MODULE PROCEDURE approxge_real
END INTERFACE

PUBLIC :: OPERATOR( .ARROXGE. )

!----------------------------------------------------------------------------
!                                                              SOFTEQ@APPROX
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 April 2021
! summary: 	Defines the operation when comparing two single precision reals
! with SOFTEQ

INTERFACE
MODULE ELEMENTAL FUNCTION softeq_real(r1,r2,tol) RESULT(Ans)
  REAL( DFP ), INTENT( IN ) :: r1
  REAL( DFP ), INTENT( IN ) :: r2
  REAL( DFP ), INTENT( IN ) :: tol
  LOGICAL( LGT ) :: Ans
END FUNCTION
END INTERFACE
INTERFACE SOFTEQ
  MODULE PROCEDURE softeq_real
ENDINTERFACE

PUBLIC :: SOFTEQ

!----------------------------------------------------------------------------
!                                                             SOFTEQR@APPROX
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 April 2021
! summary: 	Defines the operation when comparing two single precision reals
! with SOFTEQR

INTERFACE
MODULE ELEMENTAL FUNCTION softeqr_real(r1,r2,tol) RESULT(Ans)
  REAL( DFP ), INTENT( IN ) :: r1
  REAL( DFP ), INTENT( IN ) :: r2
  REAL( DFP ), INTENT( IN ) :: tol
  LOGICAL( LGT ) :: Ans
END FUNCTION
END INTERFACE

INTERFACE SOFTEQR
  MODULE PROCEDURE softeqr_real
END INTERFACE SOFTEQR

PUBLIC :: SOFTEQR

!----------------------------------------------------------------------------
!                                                              SOFTLE@APPROX
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 April 2021
! summary: 	Defines the operation when comparing two single precision reals
! with SOFTLE

INTERFACE
MODULE ELEMENTAL FUNCTION softle_real(r1,r2,tol) RESULT(Ans)
  REAL( DFP ), INTENT( IN ) :: r1
  REAL( DFP ), INTENT( IN ) :: r2
  REAL( DFP ), INTENT( IN ) :: tol
  LOGICAL( LGT ) :: Ans
END FUNCTION
END INTERFACE

INTERFACE SOFTLE
  MODULE PROCEDURE softle_real
END INTERFACE SOFTLE

PUBLIC :: SOFTLE

!----------------------------------------------------------------------------
!                                                              SOFTLT@APPROX
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 April 2021
! summary: Defines the operation when comparing two single precision reals with SOFTLT

INTERFACE
MODULE ELEMENTAL FUNCTION softlt_real(r1,r2,tol) RESULT(Ans)
  REAL( DFP ), INTENT( IN ) :: r1
  REAL( DFP ), INTENT( IN ) :: r2
  REAL( DFP ), INTENT( IN ) :: tol
  LOGICAL( LGT ) :: Ans
END FUNCTION
END INTERFACE

INTERFACE SOFTLT
  MODULE PROCEDURE softlt_real
END INTERFACE SOFTLT

PUBLIC :: SOFTLT

!----------------------------------------------------------------------------
!                                                              SOFTGE@APPROX
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION softge_real(r1,r2,tol) RESULT(Ans)
  REAL( DFP ), INTENT( IN ) :: r1
  REAL( DFP ), INTENT( IN ) :: r2
  REAL( DFP ), INTENT( IN ) :: tol
  LOGICAL( LGT ) :: Ans
END FUNCTION
END INTERFACE

INTERFACE SOFTGE
  MODULE PROCEDURE softge_real
END INTERFACE SOFTGE

PUBLIC :: SOFTGE

!----------------------------------------------------------------------------
!                                                              SOFTGT@APPROX
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION softgt_real(r1,r2,tol) RESULT(Ans)
  REAL( DFP ), INTENT( IN ) :: r1
  REAL( DFP ), INTENT( IN ) :: r2
  REAL( DFP ), INTENT( IN ) :: tol
  LOGICAL( LGT ) :: Ans
END FUNCTION
END INTERFACE

INTERFACE SOFTGT
  MODULE PROCEDURE softgt_real
END INTERFACE SOFTGT

PUBLIC :: SOFTGT

!----------------------------------------------------------------------------
!                                                                 @APPROX
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION equalto_logical(l1,l2) RESULT(Ans)
  LOGICAL( LGT ), INTENT( IN ) :: l1
  LOGICAL( LGT ), INTENT( IN ) :: l2
  LOGICAL( LGT )  :: Ans
END FUNCTION
END INTERFACE

INTERFACE OPERATOR(==)
  MODULE PROCEDURE equalto_logical
END INTERFACE

PUBLIC :: OPERATOR(==)

!----------------------------------------------------------------------------
!                                                                   @APPROX
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION notequalto_logical(l1,l2) RESULT( Ans )
  LOGICAL( LGT ), INTENT( IN ) :: l1
  LOGICAL( LGT ), INTENT( IN ) :: l2
  LOGICAL( LGT ) :: Ans
END FUNCTION
END INTERFACE

INTERFACE OPERATOR(/=)
  MODULE PROCEDURE notequalto_logical
END INTERFACE

PUBLIC :: OPERATOR(/=)

!----------------------------------------------------------------------------
!                                                             ASSIGN@APPROX
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL SUBROUTINE assign_char_to_int(i,c)
  INTEGER( I4B ), INTENT( OUT ) :: i
  CHARACTER( LEN=* ), INTENT( IN ) :: c
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             ASSIGN@APPROX
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL SUBROUTINE assign_char_to_bool(b,c)
  LOGICAL( LGT ), INTENT( OUT ) :: b
  CHARACTER( LEN=* ), INTENT( IN ) :: c
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             ASSIGN@APPROX
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL SUBROUTINE assign_char_to_real(s,c)
  REAL( DFP ), INTENT( OUT ) :: s
  CHARACTER( LEN=* ), INTENT( IN ) :: c
END SUBROUTINE
END INTERFACE

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE assign_char_to_int
  MODULE PROCEDURE assign_char_to_bool
  MODULE PROCEDURE assign_char_to_real
ENDINTERFACE

PUBLIC :: ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                                 @APPROX
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION isNumeric(char_str) RESULT(bool)
  CHARACTER( LEN=* ),INTENT( IN ) :: char_str
  LOGICAL( LGT ) :: bool
END FUNCTION
END INTERFACE

PUBLIC :: isNumeric


!----------------------------------------------------------------------------
!                                                  ExpMesh@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Sept 2021
! summary: Exponential mesh

INTERFACE
MODULE PURE FUNCTION ExpMesh_Real64( rmin, rmax, a, N ) RESULT( Ans )
  REAL( Real64 ), INTENT( IN ) :: rmin
    !! left end of 1D domain
  REAL( Real64 ), INTENT( IN ) :: rmax
    !! right end of 1D domain
  REAL( Real64 ), INTENT( IN ) :: a
    !! Ratio of largest to smallest element, a should be positive
    !! a = 1, then we get uniform mesh
  INTEGER( I4B ), INTENT( IN ) :: N
    !! Number of elements present in mesh
  REAL( Real64 ) :: ans( N+1 )
    !! Number of nodes in mesh
END FUNCTION ExpMesh_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ExpMesh@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Sept 2021
! summary: Exponential mesh

INTERFACE
MODULE PURE FUNCTION ExpMesh_Real32( rmin, rmax, a, N ) RESULT( Ans )
  REAL( Real32 ), INTENT( IN ) :: rmin
    !! left end of 1D domain
  REAL( Real32 ), INTENT( IN ) :: rmax
    !! right end of 1D domain
  REAL( Real32 ), INTENT( IN ) :: a
    !! Ratio of largest to smallest element, a should be positive
    !! a = 1, then we get uniform mesh
  INTEGER( I4B ), INTENT( IN ) :: N
    !! Number of elements present in mesh
  REAL( Real32 ) :: ans( N+1 )
    !! Number of nodes in mesh
END FUNCTION ExpMesh_Real32
END INTERFACE

INTERFACE ExpMesh
  MODULE PROCEDURE ExpMesh_Real32, ExpMesh_Real64
END INTERFACE ExpMesh

PUBLIC :: ExpMesh

!----------------------------------------------------------------------------
!                                                  Linspace@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Sept 2021
! summary: linspace

INTERFACE
MODULE PURE FUNCTION Linspace_Real64( a, b, N ) RESULT( Ans )
  REAL( Real64 ), INTENT( IN ) :: a
    !! left end of 1D domain
  REAL( Real64 ), INTENT( IN ) :: b
    !! right end of 1D domain
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: N
    !! Number of points including a and b
  REAL( Real64 ), ALLOCATABLE :: ans( : )
    !! Number of nodes in mesh
END FUNCTION Linspace_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Linspace@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Sept 2021
! summary: Returns a linearly spaced vector
!
!### Introduction
! Returns a linearly spaced vector with n points in [a, b]
! if n is omitted, 100 points will be considered

INTERFACE
MODULE PURE FUNCTION Linspace_Real32( a, b, N ) RESULT( Ans )
  REAL( Real32 ), INTENT( IN ) :: a
    !! left end of 1D domain
  REAL( Real32 ), INTENT( IN ) :: b
    !! right end of 1D domain
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: N
    !! Number of points including a and b
  REAL( Real32 ), ALLOCATABLE :: ans( : )
    !! Number of nodes in mesh
END FUNCTION Linspace_Real32
END INTERFACE


INTERFACE Linspace
  MODULE PROCEDURE Linspace_Real32, Linspace_Real64
END INTERFACE Linspace

PUBLIC :: Linspace

!----------------------------------------------------------------------------
!                                                  MeshGrid@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Sept 2021
! summary: meshgrid generate mesh grid over a rectangular domain of [xmin xmax, ymin, ymax]
!
!### Introduction
!
! Meshgrid generate mesh grid over a rectangular domain of [xmin xmax, ymin, ymax]
! - xgv, ygv are grid vectors in form of full grid data
! - X and Y are matrix each of size [ny by nx] contains the grid data.
! - The coordinates of point (i,j) is [X(i,j), Y(i,j)]
!
!### Usage
!
!```fortran
! call meshgrid(X, Y, [0.,1.,2.,3.],[5.,6.,7.,8.])
!
!  X =
!  [0.0, 1.0, 2.0, 3.0,
!   0.0, 1.0, 2.0, 3.0,
!   0.0, 1.0, 2.0, 3.0,
!   0.0, 1.0, 2.0, 3.0]
!
! Y =
! [ 5.0, 5.0, 5.0, 5.0,
!   6.0, 6.0, 6.0, 6.0,
!   7.0, 7.0, 7.0, 7.0,
!   8.0, 8.0, 8.0, 8.0]
!```

INTERFACE
MODULE PURE SUBROUTINE MeshGrid2D_Real64( x, y, xgv, ygv )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: x( :, : )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: y( :, : )
  REAL( Real64 ), INTENT( IN ) :: xgv( : )
  REAL( Real64 ), INTENT( IN ) :: ygv( : )
END SUBROUTINE MeshGrid2D_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                                MeshGrid@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE MeshGrid2D_Real32( x, y, xgv, ygv )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: x( :, : )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: y( :, : )
  REAL( Real32 ), INTENT( IN ) :: xgv( : )
  REAL( Real32 ), INTENT( IN ) :: ygv( : )
END SUBROUTINE MeshGrid2D_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                               MeshGrid@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE MeshGrid3D_Real64( x, y, z, xgv, ygv, zgv )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: x( :, :, : )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: y( :, :, : )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: z( :, :, : )
  REAL( Real64 ), INTENT( IN ) :: xgv( : )
  REAL( Real64 ), INTENT( IN ) :: ygv( : )
  REAL( Real64 ), INTENT( IN ) :: zgv( : )
END SUBROUTINE MeshGrid3D_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                               MeshGrid@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE MeshGrid3D_Real32( x, y, z, xgv, ygv, zgv )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: x( :, :, : )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: y( :, :, : )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: z( :, :, : )
  REAL( Real32 ), INTENT( IN ) :: xgv( : )
  REAL( Real32 ), INTENT( IN ) :: ygv( : )
  REAL( Real32 ), INTENT( IN ) :: zgv( : )
END SUBROUTINE MeshGrid3D_Real32
END INTERFACE


INTERFACE MeshGrid
  MODULE PROCEDURE MeshGrid2D_Real32, MeshGrid2D_Real64, &
    & MeshGrid3D_Real32, MeshGrid3D_Real64
END INTERFACE MeshGrid

PUBLIC :: MeshGrid

!----------------------------------------------------------------------------
!                                                   arange@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 March 2021
! summary: Returns a vector of reals given `start`,  `end`,  and `increment`
! values.

INTERFACE
MODULE PURE FUNCTION arange_Real64( istart, iend, increment ) result( Ans )
  REAL( Real64 ), INTENT( IN ) :: istart
    !! Start value of the array
  REAL( Real64 ), INTENT( IN ) :: iend
    !! End value of the array
  REAL( Real64 ), INTENT( IN ), OPTIONAL :: increment
    !! Array increment
  REAL( Real64 ), DIMENSION( : ), ALLOCATABLE :: Ans
END FUNCTION arange_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                                   arange@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 March 2021
! summary: Returns a vector of reals given `start`,  `end`,  and `increment`
! values.

INTERFACE
MODULE PURE FUNCTION arange_Real32( istart, iend, increment ) result( Ans )
  REAL( Real32 ), INTENT( IN ) :: istart
    !! Start value of the array
  REAL( Real32 ), INTENT( IN ) :: iend
    !! End value of the array
  REAL( Real32 ), INTENT( IN ), OPTIONAL :: increment
    !! Array increment
  REAL( Real32 ), DIMENSION( : ), ALLOCATABLE :: Ans
END FUNCTION arange_Real32
END INTERFACE


!----------------------------------------------------------------------------
!                                                    arangeFunctionalFortran
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
  MODULE PROCEDURE arange_int, arange_real64, arange_Real32
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
!                                                    UpperCase@StringMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary: 	Returns the upperCase version of chars

INTERFACE
MODULE PURE FUNCTION UpperCase_char( chars ) RESULT( Ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: chars
  CHARACTER( LEN = LEN(chars) ):: ans
END FUNCTION UpperCase_char
END INTERFACE

INTERFACE UpperCase
  MODULE PROCEDURE UpperCase_char
END INTERFACE UpperCase

PUBLIC :: UpperCase

!----------------------------------------------------------------------------
!                                                  toUpperCase@StringMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary: 	Returns the upperCase version of chars

INTERFACE
MODULE PURE SUBROUTINE ToUpperCase_Char( chars )
  CHARACTER( LEN = * ), INTENT( INOUT ) ::chars
ENDSUBROUTINE ToUpperCase_Char
END INTERFACE

INTERFACE toUpperCase
  MODULE PROCEDURE ToUpperCase_Char
END INTERFACE toUpperCase

PUBLIC :: toUpperCase

!----------------------------------------------------------------------------
!                                                    LowerCase@StringMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary: 	Returns the LowerCase version of chars

INTERFACE
MODULE PURE FUNCTION LowerCase_char( chars ) RESULT( Ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: chars
  CHARACTER( LEN = LEN(chars) ):: ans
END FUNCTION LowerCase_char
END INTERFACE

INTERFACE LowerCase
  MODULE PROCEDURE LowerCase_char
END INTERFACE LowerCase

PUBLIC :: LowerCase

!----------------------------------------------------------------------------
!                                                  toLowerCase@StringMethods
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary: 	Returns the LowerCase version of chars

INTERFACE
MODULE PURE SUBROUTINE ToLowerCase_Char( chars )
  CHARACTER( LEN = * ), INTENT( INOUT ) ::chars
ENDSUBROUTINE ToLowerCase_Char
END INTERFACE

INTERFACE toLowerCase
  MODULE PROCEDURE ToLowerCase_Char
END INTERFACE toLowerCase

PUBLIC :: toLowerCase

!----------------------------------------------------------------------------
!                                                  isWhiteChar@StringMethods
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary: 	Returns true if the char is a space(32) or a tab(9).

INTERFACE
MODULE PURE FUNCTION isWhiteChar_char( char ) RESULT( Ans )
  CHARACTER( LEN = 1 ), INTENT( IN ) :: char
  LOGICAL( LGT ) :: ans
END FUNCTION isWhiteChar_char
END INTERFACE

INTERFACE isWhiteChar
  MODULE PROCEDURE isWhiteChar_char
END INTERFACE isWhiteChar

PUBLIC :: isWhiteChar

!----------------------------------------------------------------------------
!                                                  isBlank@StringMethods
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary: 	Returns true of the entire string is blank

INTERFACE
MODULE PURE FUNCTION isBlank_chars( chars ) RESULT( Ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: chars
  LOGICAL( LGT ) :: ans
END FUNCTION isBlank_chars
END INTERFACE

INTERFACE isBlank
  MODULE PROCEDURE isBlank_chars
END INTERFACE isBlank

PUBLIC :: isBlank


!----------------------------------------------------------------------------
!                                                    numString@StringMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary: Returns number of substrings contained in input string 'chars'
! delimited by white space.
!
!### Introduction
! Returns number of substrings contained in input string 'chars' delimited by
! white space.
! This routien has been adopted from
! [https://github.com/CASL/Futility/blob/master/src/IO_Strings.F90](https://github.com/CASL/Futility/blob/master/src/IO_Strings.F90)
!

INTERFACE
MODULE PURE FUNCTION numStrings_chars( chars ) RESULT( Ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: chars
  INTEGER( I4B ) :: ans
END FUNCTION numStrings_chars
END INTERFACE

INTERFACE numStrings
  MODULE PROCEDURE numStrings_chars
END INTERFACE numStrings

PUBLIC :: numStrings

!----------------------------------------------------------------------------
!                                                   nmatchstr@StringMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 5 sept 2021
! summary: Returns the total number of times the substring pattern is
! found in the main string

INTERFACE
MODULE PURE FUNCTION numMatchStr_chars( chars, pattern ) RESULT( Ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: chars
  CHARACTER( LEN = * ), INTENT( IN ) :: pattern
  INTEGER( I4B ) :: ans
END FUNCTION numMatchStr_chars
END INTERFACE

INTERFACE numMatchStr
  MODULE PROCEDURE numMatchStr_chars
END INTERFACE numMatchStr

PUBLIC :: numMatchStr

!----------------------------------------------------------------------------
!                                                  isPresent@StringMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 5 sept 2021
! summary: Returns whether or not a substring pattern is found within string
!
!### Introduction
! Returns whether or not a substring pattern is found within string
!
!@note
! Does not handle trailing spaces that can be eliminated by TRIM() so
! strings should be trimmed when passing into function.
!@endnote

INTERFACE
MODULE PURE FUNCTION isPresent_chars( chars, pattern ) RESULT( Ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: chars
  CHARACTER( LEN = * ), INTENT( IN ) :: pattern
  LOGICAL( LGT ) :: ans
END FUNCTION isPresent_chars
END INTERFACE

INTERFACE isPresent
  MODULE PROCEDURE isPresent_chars
END INTERFACE isPresent

PUBLIC :: isPresent

!----------------------------------------------------------------------------
!                                                       strFind@StringMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 5 sept 2021
! summary: Function returns the indices in a string where substring pattern


INTERFACE
MODULE PURE SUBROUTINE strFind_chars( chars,pattern,indices )
  CHARACTER(LEN=*),INTENT(IN) :: chars
  CHARACTER(LEN=*),INTENT(IN) :: pattern
  INTEGER(I4B),ALLOCATABLE,INTENT(OUT) :: indices(:)
END SUBROUTINE strFind_chars
END INTERFACE

INTERFACE strFind
  MODULE PROCEDURE strFind_chars
END INTERFACE strFind

PUBLIC :: strFind

!----------------------------------------------------------------------------
!                                                 FindReplace@StringMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 5 sept 2021
! summary: Replaces a substring pattern with a different substring in a string
!
!### Introduction
! Replaces a substring pattern with a different substring in a string.
! - chars the string which will have substrings replaced.
! - findp the substring pattern to find and replace
! - repp the new substring that will be replace parts of string
!
!@note
! repp can be larger than @c findp and as long as the size of string can
! accomodate the increased length of all replacements. Trailing and preceding
! spaces are counted in all strings.
!@endnote

INTERFACE
MODULE PURE SUBROUTINE FindReplace_chars( chars, findp, repp )
  CHARACTER(LEN=*),INTENT(INOUT) :: chars
  CHARACTER(LEN=*),INTENT(IN) :: findp
  CHARACTER(LEN=*),INTENT(IN) :: repp
END SUBROUTINE FindReplace_chars
END INTERFACE

INTERFACE FindReplace
  MODULE PROCEDURE FindReplace_chars
END INTERFACE FindReplace

PUBLIC :: FindReplace

!----------------------------------------------------------------------------
!                                                    getField@StringMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 sept 2021
! summary: Replaces a substring pattern with a different substring in a string
!
!### Introduction
! Replaces a substring pattern with a different substring in a string.
! - chars the string which will have substrings replaced.
! - findp the substring pattern to find and replace
! - repp the new substring that will be replace parts of string
!
!@note
! repp can be larger than @c findp and as long as the size of string can
! accomodate the increased length of all replacements. Trailing and preceding
! spaces are counted in all strings.
!@endnote


INTERFACE
MODULE PURE SUBROUTINE getField_chars( i,chars,field,ierr )
  INTEGER(I4B),INTENT(IN) :: i
  CHARACTER(LEN=*),INTENT(IN) :: chars
  CHARACTER(LEN=:),ALLOCATABLE,INTENT(OUT) :: field
  INTEGER(I4B),INTENT(OUT),OPTIONAL :: ierr
END SUBROUTINE getField_chars
END INTERFACE

INTERFACE getField
  MODULE PROCEDURE getField_chars
END INTERFACE getField

PUBLIC :: getField

!----------------------------------------------------------------------------
!                                                    SlashRep@StringMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 sept 2021
! summary: routine replaces slash character in file path names with
! the system appropriate file separator slash.
!
!### Introduction
! This routine returns the path, filename, and extension.

INTERFACE
MODULE PURE SUBROUTINE SlashRep_chars( chars )
  CHARACTER( LEN = * ), INTENT( INOUT ) :: chars
END SUBROUTINE SlashRep_chars
END INTERFACE

INTERFACE SlashRep
  MODULE PROCEDURE SlashRep_chars
END INTERFACE SlashRep

PUBLIC :: SlashRep

!----------------------------------------------------------------------------
!                                                getFileParts@StringMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 sept 2021
! summary: Returns the path,filename, and extension
!
!### Introduction
! This routine returns the path, filename, and extension.

INTERFACE
MODULE PURE SUBROUTINE getFileParts_chars( chars,path,fname,ext )
  CHARACTER(LEN=*),INTENT(IN) :: chars
  CHARACTER(LEN=*),INTENT(OUT) :: path
  CHARACTER(LEN=*),INTENT(OUT) :: fname
  CHARACTER(LEN=*),INTENT(OUT) :: ext
END SUBROUTINE getFileParts_chars
END INTERFACE

INTERFACE getFileParts
  MODULE PROCEDURE getFileParts_chars
END INTERFACE getFileParts

PUBLIC :: getFileParts

!----------------------------------------------------------------------------
!                                                getPath@StringMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE getPath_chars( chars,path )
  CHARACTER(LEN=*),INTENT(IN) :: chars
  CHARACTER(LEN=*),INTENT(OUT) :: path
END SUBROUTINE getPath_chars
END INTERFACE

INTERFACE getPath
  MODULE PROCEDURE getPath_chars
END INTERFACE getPath

PUBLIC :: getPath

!----------------------------------------------------------------------------
!                                                getFileName@StringMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE getFileName_chars( chars,fname )
  CHARACTER(LEN=*),INTENT(IN) :: chars
  CHARACTER(LEN=*),INTENT(OUT) :: fname
END SUBROUTINE getFileName_chars
END INTERFACE

INTERFACE getFileName
  MODULE PROCEDURE getFileName_chars
END INTERFACE getFileName

PUBLIC :: getFileName

!----------------------------------------------------------------------------
!                                           getFileNameExt@StringMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE getFileNameExt_chars( chars,ext )
  CHARACTER(LEN=*),INTENT(IN) :: chars
  CHARACTER(LEN=*),INTENT(OUT) :: ext
END SUBROUTINE getFileNameExt_chars
END INTERFACE

INTERFACE getFileNameExt
  MODULE PROCEDURE getFileNameExt_chars
END INTERFACE getFileNameExt

PUBLIC :: getFileNameExt

!----------------------------------------------------------------------------
!                                                getExtension@StringMethods
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function get the extension from a file
!
! ## Usage
! ```fortran
! call display( getExtension("helloworld.F90") .EQ. "f90", &
! & msg="test1:: ")
! ```

INTERFACE
MODULE FUNCTION getExtension_chars( char ) RESULT(ext)
  CHARACTER( LEN=* ), INTENT( IN ) :: char
  CHARACTER(7) :: ext
END FUNCTION
END INTERFACE

INTERFACE getExtension
  MODULE PROCEDURE getExtension_chars
END INTERFACE getExtension

PUBLIC :: getExtension

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real64_R1( Mat, row )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( : )
  INTEGER( I4B ), INTENT( IN ) :: row
END SUBROUTINE Reallocate_Real64_R1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real64_R1b( Mat, s )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( : )
  INTEGER( I4B ), INTENT( IN ) :: s(:)
END SUBROUTINE Reallocate_Real64_R1b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real32_R1( Mat, row )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( : )
  INTEGER( I4B ), INTENT( IN ) :: row
END SUBROUTINE Reallocate_Real32_R1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real32_R1b( Mat, s )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( : )
  INTEGER( I4B ), INTENT( IN ) :: s( : )
END SUBROUTINE Reallocate_Real32_R1b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real64_R2( Mat, row, col )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, : )
  INTEGER( I4B ), INTENT( IN ) :: row, col
END SUBROUTINE Reallocate_Real64_R2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real64_R2b( Mat, s )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, : )
  INTEGER( I4B ), INTENT( IN ) :: s(:)
END SUBROUTINE Reallocate_Real64_R2b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real32_R2( Mat, row, col )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, : )
  INTEGER( I4B ), INTENT( IN ) :: row, col
END SUBROUTINE Reallocate_Real32_R2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real32_R2b( Mat, s )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, : )
  INTEGER( I4B ), INTENT( IN ) :: s(:)
END SUBROUTINE Reallocate_Real32_R2b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real64_R3( Mat, i1, i2, i3 )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3
END SUBROUTINE Reallocate_Real64_R3
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real64_R3b( Mat, s )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s(:)
END SUBROUTINE Reallocate_Real64_R3b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real32_R3( Mat, i1, i2, i3 )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3
END SUBROUTINE Reallocate_Real32_R3
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real32_R3b( Mat, s )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s(:)
END SUBROUTINE Reallocate_Real32_R3b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real64_R4 ( Mat, i1, i2, i3, i4 )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3, i4
END SUBROUTINE Reallocate_Real64_R4
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real64_R4b( Mat, s )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( : )
END SUBROUTINE Reallocate_Real64_R4b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real32_R4 ( Mat, i1, i2, i3, i4 )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3, i4
END SUBROUTINE Reallocate_Real32_R4
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real32_R4b( Mat, s )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( : )
END SUBROUTINE Reallocate_Real32_R4b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real64_R5 ( Mat, i1, i2, i3, i4, i5 )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3, i4, i5
END SUBROUTINE Reallocate_Real64_R5
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real64_R5b( Mat, s )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( : )
END SUBROUTINE Reallocate_Real64_R5b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real32_R5( Mat, i1, i2, i3, i4, i5 )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3, i4, i5
END SUBROUTINE Reallocate_Real32_R5
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real32_R5b( Mat, s )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( : )
END SUBROUTINE Reallocate_Real32_R5b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real64_R6 ( Mat, i1, i2, i3, i4, i5, i6 )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3, i4, i5, i6
END SUBROUTINE Reallocate_Real64_R6
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real64_R6b( Mat, s )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( : )
END SUBROUTINE Reallocate_Real64_R6b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real32_R6( Mat, i1, i2, i3, i4, i5, i6 )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3, i4, i5, i6
END SUBROUTINE Reallocate_Real32_R6
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real32_R6b( Mat, s )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( : )
END SUBROUTINE Reallocate_Real32_R6b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real64_R7 ( Mat, i1, i2, i3, i4, i5, &
  & i6, i7 )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3, i4, i5, i6, i7
END SUBROUTINE Reallocate_Real64_R7
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real64_R7b( Mat, s )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( : )
END SUBROUTINE Reallocate_Real64_R7b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real32_R7( Mat, i1, i2, i3, i4, i5, i6, i7)
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3, i4, i5, i6, i7
END SUBROUTINE Reallocate_Real32_R7
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real32_R7b( Mat, s )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( : )
END SUBROUTINE Reallocate_Real32_R7b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int64_R1( Mat, row )
  INTEGER( Int64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( : )
  INTEGER( I4B ), INTENT( IN ) :: row
END SUBROUTINE Reallocate_Int64_R1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int64_R1b( Mat, s )
  INTEGER( Int64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( : )
  INTEGER( I4B ), INTENT( IN ) :: s(:)
END SUBROUTINE Reallocate_Int64_R1b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int32_R1( Mat, row )
  INTEGER( Int32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( : )
  INTEGER( I4B ), INTENT( IN ) :: row
END SUBROUTINE Reallocate_Int32_R1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int32_R1b( Mat, s )
  INTEGER( Int32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( : )
  INTEGER( I4B ), INTENT( IN ) :: s( : )
END SUBROUTINE Reallocate_Int32_R1b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int64_R2( Mat, row, col )
  INTEGER( Int64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, : )
  INTEGER( I4B ), INTENT( IN ) :: row, col
END SUBROUTINE Reallocate_Int64_R2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int64_R2b( Mat, s )
  INTEGER( Int64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, : )
  INTEGER( I4B ), INTENT( IN ) :: s(:)
END SUBROUTINE Reallocate_Int64_R2b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int32_R2( Mat, row, col )
  INTEGER( Int32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, : )
  INTEGER( I4B ), INTENT( IN ) :: row, col
END SUBROUTINE Reallocate_Int32_R2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int32_R2b( Mat, s )
  INTEGER( Int32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, : )
  INTEGER( I4B ), INTENT( IN ) :: s(:)
END SUBROUTINE Reallocate_Int32_R2b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int64_R3( Mat, i1, i2, i3 )
  INTEGER( Int64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3
END SUBROUTINE Reallocate_Int64_R3
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int64_R3b( Mat, s )
  INTEGER( Int64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s(:)
END SUBROUTINE Reallocate_Int64_R3b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int32_R3( Mat, i1, i2, i3 )
  INTEGER( Int32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3
END SUBROUTINE Reallocate_Int32_R3
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int32_R3b( Mat, s )
  INTEGER( Int32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s(:)
END SUBROUTINE Reallocate_Int32_R3b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int64_R4 ( Mat, i1, i2, i3, i4 )
  INTEGER( Int64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3, i4
END SUBROUTINE Reallocate_Int64_R4
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int64_R4b( Mat, s )
  INTEGER( Int64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( : )
END SUBROUTINE Reallocate_Int64_R4b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int32_R4 ( Mat, i1, i2, i3, i4 )
  INTEGER( Int32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3, i4
END SUBROUTINE Reallocate_Int32_R4
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int32_R4b( Mat, s )
  INTEGER( Int32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( : )
END SUBROUTINE Reallocate_Int32_R4b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int64_R5 ( Mat, i1, i2, i3, i4, i5 )
  INTEGER( Int64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3, i4, i5
END SUBROUTINE Reallocate_Int64_R5
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int64_R5b( Mat, s )
  INTEGER( Int64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( : )
END SUBROUTINE Reallocate_Int64_R5b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int32_R5( Mat, i1, i2, i3, i4, i5 )
  INTEGER( Int32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3, i4, i5
END SUBROUTINE Reallocate_Int32_R5
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int32_R5b( Mat, s )
  INTEGER( Int32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( : )
END SUBROUTINE Reallocate_Int32_R5b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int64_R6 ( Mat, i1, i2, i3, i4, i5, i6 )
  INTEGER( Int64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3, i4, i5, i6
END SUBROUTINE Reallocate_Int64_R6
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int64_R6b( Mat, s )
  INTEGER( Int64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( : )
END SUBROUTINE Reallocate_Int64_R6b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int32_R6( Mat, i1, i2, i3, i4, i5, i6 )
  INTEGER( Int32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3, i4, i5, i6
END SUBROUTINE Reallocate_Int32_R6
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int32_R6b( Mat, s )
  INTEGER( Int32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( : )
END SUBROUTINE Reallocate_Int32_R6b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int64_R7 ( Mat, i1, i2, i3, i4, i5, &
  & i6, i7 )
  INTEGER( Int64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3, i4, i5, i6, i7
END SUBROUTINE Reallocate_Int64_R7
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int64_R7b( Mat, s )
  INTEGER( Int64 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( : )
END SUBROUTINE Reallocate_Int64_R7b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int32_R7( Mat, i1, i2, i3, i4, i5, i6, i7)
  INTEGER( Int32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3, i4, i5, i6, i7
END SUBROUTINE Reallocate_Int32_R7
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int32_R7b( Mat, s )
  INTEGER( Int32 ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( : )
END SUBROUTINE Reallocate_Int32_R7b
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Int32_R1_6( Vec1, n1, Vec2, n2, Vec3, &
  & n3, Vec4, n4, Vec5, n5, Vec6, n6 )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT) :: Vec1( : ), Vec2( : )
  INTEGER( I4B ), ALLOCATABLE, OPTIONAL, INTENT( INOUT) :: Vec3( : ), &
    & Vec4( : ), Vec5( : ), Vec6( : )
  INTEGER( I4B ), INTENT( IN ) :: n1, n2
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: n3, n4, n5, n6
END SUBROUTINE Reallocate_Int32_R1_6
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real64_R1_6( Vec1, n1, Vec2, &
  & n2, Vec3, n3, Vec4, n4, Vec5, n5, Vec6, n6 )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT) :: Vec1( : ), Vec2( : )
  REAL( Real64 ), ALLOCATABLE, OPTIONAL, INTENT( INOUT) :: Vec3( : ), &
    & Vec4( : ), Vec5( : ), Vec6( : )
  INTEGER( I4B ), INTENT( IN ) :: n1, n2
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: n3, n4, n5, n6
END SUBROUTINE Reallocate_Real64_R1_6
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real32_R1_6( Vec1, n1, Vec2, &
  & n2, Vec3, n3, Vec4, n4, Vec5, n5, Vec6, n6 )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT) :: Vec1( : ), Vec2( : )
  REAL( Real32 ), ALLOCATABLE, OPTIONAL, INTENT( INOUT) :: Vec3( : ), &
    & Vec4( : ), Vec5( : ), Vec6( : )
  INTEGER( I4B ), INTENT( IN ) :: n1, n2
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: n3, n4, n5, n6
END SUBROUTINE Reallocate_Real32_R1_6
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real64_AIJ( A, nA, IA, nIA, JA, nJA )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: A( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: IA( : ), JA( : )
  INTEGER( I4B ), INTENT( IN ) :: nA, nIA, nJA
END SUBROUTINE Reallocate_Real64_AIJ
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real32_AIJ( A, nA, IA, nIA, JA, nJA )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: A( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: IA( : ), JA( : )
  INTEGER( I4B ), INTENT( IN ) :: nA, nIA, nJA
END SUBROUTINE Reallocate_Real32_AIJ
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real64_AI( A, nA, IA, nIA )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: A( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: IA( : )
  INTEGER( I4B ), INTENT( IN ) :: nA, nIA
END SUBROUTINE Reallocate_Real64_AI
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Reallocate@Reallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Reallocate_Real32_AI( A, nA, IA, nIA )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: A( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: IA( : )
  INTEGER( I4B ), INTENT( IN ) :: nA, nIA
END SUBROUTINE Reallocate_Real32_AI
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PROCEDURE &
    & Reallocate_Real64_R1, Reallocate_Real32_R1, &
    & Reallocate_Real64_R1b, Reallocate_Real32_R1b, &
    & Reallocate_Real64_R2, Reallocate_Real32_R2, &
    & Reallocate_Real64_R2b, Reallocate_Real32_R2b, &
    & Reallocate_Real64_R3, Reallocate_Real32_R3, &
    & Reallocate_Real64_R3b, Reallocate_Real32_R3b, &
    & Reallocate_Real64_R4, Reallocate_Real32_R4, &
    & Reallocate_Real64_R4b, Reallocate_Real32_R4b, &
    & Reallocate_Real64_R5, Reallocate_Real32_R5, &
    & Reallocate_Real64_R5b, Reallocate_Real32_R5b, &
    & Reallocate_Real64_R6, Reallocate_Real32_R6, &
    & Reallocate_Real64_R6b, Reallocate_Real32_R6b, &
    & Reallocate_Real64_R7, Reallocate_Real32_R7, &
    & Reallocate_Real64_R7b, Reallocate_Real32_R7b, &
    & Reallocate_Int64_R1, Reallocate_Int32_R1, &
    & Reallocate_Int64_R1b, Reallocate_Int32_R1b, &
    & Reallocate_Int64_R2, Reallocate_Int32_R2, &
    & Reallocate_Int64_R2b, Reallocate_Int32_R2b, &
    & Reallocate_Int64_R3, Reallocate_Int32_R3, &
    & Reallocate_Int64_R3b, Reallocate_Int32_R3b, &
    & Reallocate_Int64_R4, Reallocate_Int32_R4, &
    & Reallocate_Int64_R4b, Reallocate_Int32_R4b, &
    & Reallocate_Int64_R5, Reallocate_Int32_R5, &
    & Reallocate_Int64_R5b, Reallocate_Int32_R5b, &
    & Reallocate_Int64_R6, Reallocate_Int32_R6, &
    & Reallocate_Int64_R6b, Reallocate_Int32_R6b, &
    & Reallocate_Int64_R7, Reallocate_Int32_R7, &
    & Reallocate_Int64_R7b, Reallocate_Int32_R7b, &
    & Reallocate_Int32_R1_6, Reallocate_Real64_R1_6, &
    & Reallocate_Real32_R1_6, &
    & Reallocate_Real64_AIJ, Reallocate_Real32_AIJ, &
    & Reallocate_Real64_AI, Reallocate_Real32_AI
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
!                                                             Assert@Assert
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION assert_eq2(n1,n2,string)
  CHARACTER(LEN=*), INTENT(IN) :: string
  INTEGER( I4B ), INTENT(IN) :: n1,n2
  INTEGER( I4B ) :: assert_eq2
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Assert@Assert
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION assert_eq3(n1,n2,n3,string)
  CHARACTER(LEN=*), INTENT(IN) :: string
  INTEGER( I4B ), INTENT(IN) :: n1,n2,n3
  INTEGER( I4B ) :: assert_eq3
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Assert@Assert
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION assert_eq4(n1,n2,n3,n4,string)
  CHARACTER(LEN=*), INTENT(IN) :: string
  INTEGER( I4B ), INTENT(IN) :: n1,n2,n3,n4
  INTEGER( I4B ) :: assert_eq4
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Assert@Assert
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION assert_eqn(nn,string)
  CHARACTER( LEN=* ), INTENT( IN ) :: string
  INTEGER( I4B ), DIMENSION( : ), INTENT(IN) :: nn
  INTEGER( I4B ):: assert_eqn
END FUNCTION
END INTERFACE

INTERFACE assert_eq
  MODULE PROCEDURE assert_eqn, assert_eq2, assert_eq3, assert_eq4
END INTERFACE

PUBLIC :: ASSERT_EQ

!----------------------------------------------------------------------------
!                                                              Assert@Assert
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE assert_shape_2( Mat, s, msg, file, line, routine )
  REAL( DFP ), INTENT( IN ) :: Mat( :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( 2 )
  INTEGER( I4B ), INTENT( IN ) :: line
  CHARACTER( LEN = * ), INTENT( IN ) :: msg, file, routine
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Assert@Assert
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE assert_shape_3( Mat, s,  msg, file, line, routine )
  REAL( DFP ), INTENT( IN ) :: Mat( :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( 3 )
  INTEGER( I4B ), INTENT( IN ) :: line
  CHARACTER( LEN = * ), INTENT( IN ) :: msg, file, routine
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Assert@Assert
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE assert_shape_4( Mat, s, msg, file, line, routine )
  REAL( DFP ), INTENT( IN ) :: Mat( :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: s( 4 )
  INTEGER( I4B ), INTENT( IN ) :: line
  CHARACTER( LEN = * ), INTENT( IN ) :: msg, file, routine
END SUBROUTINE
END INTERFACE

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
!                                                             Matmul@Matmul
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: matmul for rank3  and rank1 array
!
!### Introduction
!
! This fuction performs following task
! `Ans(:,:) = a1(:,:,a)*a2(a)`

INTERFACE
MODULE PURE FUNCTION matmul_r3_r1( a1, a2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: a1( :, :, : ), a2( : )
  REAL( DFP ) :: Ans( size( a1, 1 ), size( a1, 2 ) )
END FUNCTION
END INTERFACE


!----------------------------------------------------------------------------
!                                                              Matmul@Matmul
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: matmul for rank1 and rank3 array
!
!### Introduction
!
! This fuction performs following task
! `Ans(i,j) = a1(a)*a2(a,i,j)`

INTERFACE
MODULE PURE FUNCTION matmul_r1_r3( a1, a2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: a1( : ), a2( :, :, : )
  REAL( DFP ) :: Ans( size( a2, 2 ), size( a2, 3 ) )
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Matmul@Matmul
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 April 2021
! summary: 	matmul for rank2 and rank3 array
!
!### Introduction
!
! This fuction performs following task
! `Ans(i,j,ip) = a1(i,I)*a2(I,j,ip)`

INTERFACE
MODULE PURE FUNCTION matmul_r2_r3( a1, a2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: a1( :, : ), a2( :, :, : )
  REAL( DFP ) :: Ans( size( a1, 1 ), size( a2, 2 ), size( a2, 3 ) )
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Matmul@Matmul
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 April 2021
! summary: 	matmul for rank4 and rank1 array
!
!### Introduction
!
! `Ans(:,:,:) = a1(:,:,:,a)*a2(a)`

INTERFACE
MODULE PURE FUNCTION matmul_r4_r1( a1, a2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: a1( :, :, :, : ), a2( : )
  REAL( DFP ) :: Ans( size( a1, 1 ), size( a1, 2 ), size( a1, 3 ) )
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Matmul@Matmul
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 April 2021
! summary: 	matmul for rank3 and rank2
!
!### Introduction
! This fuction performs following task
! `Ans(i,j,ip) = a1(i,j,I)*a2(I,ip)`

INTERFACE
MODULE PURE FUNCTION matmul_r3_r2( a1, a2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: a1( :, :, : ), a2( :, : )
  REAL( DFP ) :: Ans( size( a1, 1 ), size( a1, 2 ), size( a2, 2 ) )
END FUNCTION
END INTERFACE

INTERFACE MATMUL
  MODULE PROCEDURE matmul_r3_r1, matmul_r4_r1, matmul_r3_r2, &
    & matmul_r1_r3, matmul_r2_r3
END INTERFACE MATMUL

PUBLIC :: MATMUL

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
!                                                               Input@Input
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION input_int( default, option ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: default
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: option
  INTEGER( I4B ) :: Ans
END FUNCTION input_int
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION input_Real64(default,option) RESULT(val)
  REAL( Real64 ),INTENT(in) :: default
  REAL( Real64 ),OPTIONAL,INTENT(in)::option
  REAL( Real64 ) :: val
END FUNCTION input_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION input_Real32(default,option) RESULT(val)
  REAL( Real32 ),INTENT(in) :: default
  REAL( Real32 ),OPTIONAL,INTENT(in)::option
  REAL( Real32 ) :: val
END FUNCTION input_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION input_IntVec( default, option ) RESULT( val )
  INTEGER( I4B ), INTENT( IN ) :: default(:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN )::option(:)
  INTEGER( I4B ), ALLOCATABLE :: val(:)
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION input_Real64vec( default, option ) RESULT( val )
  REAL( Real64 ), INTENT( IN ) :: default(:)
  REAL( Real64 ), OPTIONAL,INTENT( IN ) :: option(:)
  REAL( Real64 ), ALLOCATABLE :: val(:)
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION input_Real32vec( default, option ) RESULT( val )
  REAL( Real32 ), INTENT( IN ) :: default(:)
  REAL( Real32 ), OPTIONAL,INTENT( IN ) :: option(:)
  REAL( Real32 ), ALLOCATABLE :: val(:)
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION input_IntArray(default,option) RESULT(val)
  INTEGER( I4B ), INTENT( IN ) :: default(:,:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN )::option(:,:)
  INTEGER( I4B ), ALLOCATABLE :: val(:,:)
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION input_Real64Array(default,option) RESULT(val)
  REAL( Real64 ), INTENT( IN ) :: default(:,:)
  REAL( Real64 ), OPTIONAL,INTENT( IN )::option(:,:)
  REAL( Real64 ), ALLOCATABLE :: val(:,:)
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION input_Real32Array(default,option) RESULT(val)
  REAL( Real32 ), INTENT( IN ) :: default(:,:)
  REAL( Real32 ), OPTIONAL,INTENT( IN )::option(:,:)
  REAL( Real32 ), ALLOCATABLE :: val(:,:)
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION input_String(default,option) RESULT(val)
  CHARACTER( LEN=* ), INTENT( IN ) :: default
  CHARACTER( LEN=* ), OPTIONAL, INTENT( IN )::option
  CHARACTER( 200 )  :: val
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION input_logical(default,option) RESULT(val)
  LOGICAL( LGT ), INTENT( IN ) :: default
  LOGICAL( LGT ), OPTIONAL, INTENT( IN )::option
  LOGICAL( LGT )  :: val
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

INTERFACE Input
  MODULE PROCEDURE input_Int,input_Real64, input_Real32, input_IntVec, &
    & input_Real64Vec, input_Real32Vec, input_IntArray, &
    & input_Real64Array, input_Real32Array, input_String, &
    & input_logical
END INTERFACE Input

PUBLIC :: Input

!----------------------------------------------------------------------------
!                                                                     Det@Inv
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION det_2D( A ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: A( :, : )
  REAL( DFP ) :: Ans
END FUNCTION det_2D
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Det@Inv
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION det_3D( A ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: A( :, :, : )
  REAL( DFP ), ALLOCATABLE :: Ans( : )
END FUNCTION det_3D
END INTERFACE

INTERFACE Det
  MODULE PROCEDURE det_2D, det_3D
END INTERFACE Det

PUBLIC :: DET

!----------------------------------------------------------------------------
!                                                                     INV@Inv
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Inv_2D( invA, A )
  REAL( DFP ), INTENT( INOUT ) :: invA( :, : )
  REAL( DFP ), INTENT( IN ) :: A( :, : )
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     INV@Inv
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Inv_3D( invA, A )
  REAL( DFP ), INTENT( INOUT ) :: invA( :, :, : )
  REAL( DFP ), INTENT( IN ) :: A( :, :, : )
END SUBROUTINE
END INTERFACE

INTERFACE Inv
    MODULE PROCEDURE Inv_2D, Inv_3D
END INTERFACE Inv

PUBLIC :: INV

!----------------------------------------------------------------------------
!                                                               Radian@MISC
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Convert degrees into radian

INTERFACE
MODULE PURE FUNCTION radian_dfp( deg ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: deg
  REAL( DFP ) :: Ans
END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Radian@MISC
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Converts degrees into radian

INTERFACE
MODULE PURE FUNCTION radian_int( deg ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: deg
  REAL( DFP ) :: Ans
END FUNCTION
END INTERFACE

INTERFACE radian
  MODULE PROCEDURE radian_dfp, radian_int
END INTERFACE

PUBLIC :: radian

!----------------------------------------------------------------------------
!                                                              Degrees@MISC
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function converts radian into degrees
! Belongs to `Degrees`

INTERFACE
MODULE PURE FUNCTION degrees_dfp( rad ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: rad
  REAL( DFP ) :: Ans
END FUNCTION
END INTERFACE

INTERFACE Degrees
  MODULE PROCEDURE degrees_dfp
END INTERFACE Degrees

PUBLIC :: Degrees


!----------------------------------------------------------------------------
!                                                                 @MISC
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 April 2021
! summary: 	This subroutine search the location of nearest point to x in the array of coordinates; Array
!
!### Introduction
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

INTERFACE
MODULE FUNCTION Loc_Nearest_Point( Array, x )  RESULT( id )
  REAL( DFP ), INTENT( IN ) :: Array( :, : )
  !! Nodal coordinates in XiJ format
  REAL( DFP ), INTENT( IN ) :: x( : )
  INTEGER( I4B ) :: id
END FUNCTION
END INTERFACE

INTERFACE LOC_NearestPoint
  MODULE PROCEDURE Loc_Nearest_Point
END INTERFACE LOC_NearestPoint

PUBLIC :: LOC_NearestPoint

INTERFACE SearchNearestCoord
  MODULE PROCEDURE Loc_Nearest_Point
END INTERFACE SearchNearestCoord

PUBLIC :: SearchNearestCoord

!----------------------------------------------------------------------------
!                                                                     @MISC
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 April 2021
! summary: 	This subroutine run a system commoand on terminal

INTERFACE
MODULE SUBROUTINE exe_cmd( CMD, Str )
  CHARACTER( LEN = * ), INTENT( IN ) :: CMD, Str
END SUBROUTINE
END INTERFACE

INTERFACE ExecuteCommand
  MODULE PROCEDURE exe_cmd
END INTERFACE ExecuteCommand

PUBLIC :: ExecuteCommand

!----------------------------------------------------------------------------
!                                                             getUnitNo@MISC
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION getUnitNo_1( ) RESULT( ans )
  INTEGER( I4B ) :: ans
END FUNCTION getUnitNo_1
END INTERFACE

INTERFACE getUnitNo
  MODULE PROCEDURE getUnitNo_1
END INTERFACE getUnitNo

PUBLIC :: getUnitNo

!----------------------------------------------------------------------------
!                                                             Factorial@MISC
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 April 2021
! summary: This FUNCTION computes the factorial of an INTEGER

INTERFACE
MODULE RECURSIVE FUNCTION Factorial( N ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: N
  INTEGER( I4B ) :: Ans
END FUNCTION
END INTERFACE

PUBLIC :: Factorial

!----------------------------------------------------------------------------
!                                                               Int2STR@MISC
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 April 2021
! summary: 	Convert INTEGER  to  string

INTERFACE
MODULE PURE FUNCTION Int2Str( I )
  INTEGER( I4B ), INTENT( IN ) :: I
  CHARACTER( LEN = 15 ) :: Int2Str
END FUNCTION
END INTERFACE

PUBLIC :: Int2Str

!----------------------------------------------------------------------------
!                                                              Real2Str@MISC
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION SP2Str( I )
  REAL( SP ), INTENT( IN ) :: I
  CHARACTER( LEN = 20 ) :: SP2Str
END FUNCTION
END INTERFACE

INTERFACE
MODULE FUNCTION DP2Str( I )
  REAL( DP ), INTENT( IN ) :: I
  CHARACTER( LEN = 20 ) :: DP2Str
END FUNCTION
END INTERFACE

INTERFACE Real2Str
  MODULE PROCEDURE SP2Str, DP2Str
END INTERFACE Real2Str

PUBLIC :: Real2Str

!----------------------------------------------------------------------------
!                                                                       ARTH
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION arth_r(first,increment,n)
  REAL( SP ), INTENT( IN ) :: first,increment
  INTEGER( I4B ), INTENT( IN ) :: n
  REAL( SP ) :: arth_r( n )
END FUNCTION
END INTERFACE

INTERFACE
MODULE PURE FUNCTION arth_d(first,increment,n)
  REAL( DP ), INTENT(IN) :: first,increment
  INTEGER( I4B ), INTENT(IN) :: n
  REAL( DP ) :: arth_d( n )
END FUNCTION
END INTERFACE

INTERFACE
MODULE PURE FUNCTION arth_i(first,increment,n)
  INTEGER( I4B ), INTENT( IN ) :: first,increment,n
  INTEGER( I4B ) :: arth_i( n )
END FUNCTION
END INTERFACE

INTERFACE ARTH
  MODULE PROCEDURE arth_d, arth_i, arth_r
END INTERFACE ARTH

PUBLIC :: ARTH

!----------------------------------------------------------------------------
!                                                                 outerDIFF
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION outerdIFf_r(a,b)
  REAL( SP ), DIMENSION(:), INTENT(IN) :: a,b
  REAL( SP ), DIMENSION(size(a),size(b)) :: outerdIFf_r
END FUNCTION
END INTERFACE

INTERFACE
MODULE PURE FUNCTION outerdIFf_d(a,b)
  REAL( DP ), DIMENSION(:), INTENT(IN) :: a,b
  REAL( DP ), DIMENSION(size(a),size(b)) :: outerdIFf_d
END FUNCTION
END INTERFACE

INTERFACE
MODULE PURE FUNCTION outerdIFf_i(a,b)
  INTEGER( I4B ), DIMENSION(:), INTENT(IN) :: a,b
  INTEGER( I4B ), DIMENSION(size(a),size(b)) :: outerdIFf_i
END FUNCTION
END INTERFACE

INTERFACE outerDIFF
  MODULE PROCEDURE outerdIFf_r, outerdIFf_i, outerdIFf_d
END INTERFACE

PUBLIC :: outerDIFF

!----------------------------------------------------------------------------
!                                                              IMAXLOC@MISC
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION imaxloc_r(arr)
  REAL( DFP ), INTENT( IN ) :: arr( : )
  INTEGER( I4B ) :: imaxloc_r
END FUNCTION
END INTERFACE

INTERFACE
MODULE PURE FUNCTION imaxloc_i(iarr)
  INTEGER( I4B ), INTENT( IN ) :: iarr( : )
  INTEGER( I4B ) :: imaxloc_i
END FUNCTION
END INTERFACE

INTERFACE IMAXLOC
  MODULE PROCEDURE imaxloc_r,imaxloc_i
END INTERFACE

PUBLIC :: IMAXLOC

!----------------------------------------------------------------------------
!                                                                 IMIN@MISC
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION iminloc_r(arr)
  REAL( DFP ), INTENT( IN ) :: arr( : )
  INTEGER( I4B ) :: iminloc_r
END FUNCTION
END INTERFACE

INTERFACE IMINLOC
  MODULE PROCEDURE iminloc_r
END INTERFACE IMINLOC

PUBLIC :: IMINLOC

!----------------------------------------------------------------------------
!                                                StringToUID@HashingMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 Aug 2021
! summary: This function returns a unique number for a given string
!
!### Introduction
! This function returns a unique number for a given string
!
! Reference
!  https://cp-algorithms.com/string/string-hashing.html

INTERFACE
MODULE FUNCTION StringToUID_PolyRoll( charVar ) RESULT( Ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: charVar
  INTEGER( I4B ) :: ans
END FUNCTION StringToUID_PolyRoll
END INTERFACE

INTERFACE StringToUID
  MODULE PROCEDURE StringToUID_PolyRoll
END INTERFACE StringToUID

PUBLIC :: StringToUID

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Utility
