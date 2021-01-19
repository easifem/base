MODULE Utility
  !! Utility module contains useful general purpose routines
USE GlobalData
USE IO
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
!                                                                 Reallocate
!----------------------------------------------------------------------------

!>
! Generic subroutine to reallocate arrays
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
!                                                         EvaluatePolynomial
!----------------------------------------------------------------------------

!>
! Generic FUNCTION to evaluate a polynomial
INTERFACE EvaluatePolynomial
  MODULE PROCEDURE eval_poly
END INTERFACE EvaluatePolynomial

PUBLIC :: EvaluatePolynomial

!----------------------------------------------------------------------------
!                                                              VectorProduct
!----------------------------------------------------------------------------

!>
! Generic FUNCTION to evaluate vector product
INTERFACE VectorProduct
  MODULE PROCEDURE vec_prod
END INTERFACE VectorProduct

PUBLIC :: VectorProduct

!----------------------------------------------------------------------------
!                                                                 OUTERPROD
!----------------------------------------------------------------------------

!>
! Generic FUNCTION to evaluate outerproduct.
INTERFACE OUTERPROD
  MODULE PROCEDURE OUTERPROD1_1, OUTERPROD2_1, OUTERPROD3_1, OUTERPROD2_11, &
    & OUTERPROD1_1_sym
END INTERFACE OUTERPROD

PUBLIC :: OUTERPROD

!----------------------------------------------------------------------------
!                                                             ExecuteCommand
!----------------------------------------------------------------------------

INTERFACE ExecuteCommand
  MODULE PROCEDURE exe_cmd
END INTERFACE ExecuteCommand

PUBLIC :: ExecuteCommand

!----------------------------------------------------------------------------
!                                                                 getUnitNo
!----------------------------------------------------------------------------

PUBLIC :: getUnitNo, Factorial

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

INTERFACE Append
    MODULE PROCEDURE Append_I1, Append_I2, Append_R1, &
    Append_R2
END INTERFACE

PUBLIC :: Append

!----------------------------------------------------------------------------
!                                                                    Int2Str
!----------------------------------------------------------------------------

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
! Generic subroutine for swapping
INTERFACE SWAP
  MODULE PROCEDURE swap_i,swap_r,swap_rv,swap_c, &
    & swap_cv,swap_cm, &
    & masked_swap_rs,masked_swap_rv,masked_swap_rm
END INTERFACE

PUBLIC :: SWAP

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Generic FUNCTION to get local of maximum value
INTERFACE IMAXLOC
  MODULE PROCEDURE imaxloc_r,imaxloc_i
END INTERFACE

!> Generic FUNCTION for getting location of minmum value
INTERFACE IMINLOC
  MODULE PROCEDURE iminloc_r
END INTERFACE IMINLOC

PUBLIC :: IMINLOC
PUBLIC :: IMAXLOC

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
!                                                                 HeapSort
!----------------------------------------------------------------------------

INTERFACE HeapSort
  MODULE PROCEDURE HEAPSORT_INT, HEAPSORT_REAL
END INTERFACE HeapSort

PUBLIC :: HeapSort

!----------------------------------------------------------------------------
!                                                              Cross_Product
!----------------------------------------------------------------------------

INTERFACE Cross_Product
  MODULE PROCEDURE CROSS_PRODUCT_R1_R1
END INTERFACE Cross_Product

PUBLIC :: Cross_Product

!----------------------------------------------------------------------------
!                                                                     Input
!----------------------------------------------------------------------------

INTERFACE Input
  MODULE PROCEDURE input_Int,input_Real,input_IntVec,input_RealVec,input_IntArray,input_RealArray,input_String,input_logical
END INTERFACE Input

PUBLIC :: Input

PUBLIC :: getExtension

!----------------------------------------------------------------------------
!                                                                   CONTAINS
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                               getExtension
!----------------------------------------------------------------------------

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

PURE FUNCTION radian_dfp( deg ) RESULT( Ans )
	REAL( DFP ), INTENT( IN ) :: deg
	REAL( DFP ) :: Ans
	Ans = deg / 180.0_DFP * 3.1415926535_DFP
END FUNCTION radian_dfp

PURE FUNCTION radian_int( deg ) RESULT( Ans )
	INTEGER( I4B ), INTENT( IN ) :: deg
	REAL( DFP ) :: Ans
	Ans = REAL( deg, KIND=DFP ) / 180.0_DFP * 3.1415926535_DFP
END FUNCTION radian_int

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine search the location of nearest point to x in the
! array of coordinates; Array

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
    CALL Display( __FILE__, "ERROR :: In File :: " )
    CALL Display( __LINE__, "         At line number :: " )
    CALL Display( "Loc_Nearest_Point()", "In routine :: ")
    CALL Display( "SearchNearestCoord >> size(Array,1) should be =size(x)")
    STOP
	ENDIF

	DO i = 1, m
		xr( 1:n ) = Array( 1:n, i )
		tr_norm = DOT_PRODUCT( xr(1:n) - x(1:n), xr(1:n) - x(1:n) )
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
!                                                                   HeapSort
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Heap Sort algorithm for Integer
PURE SUBROUTINE HEAPSORT_INT( array )
  INTEGER( I4B ), INTENT( INOUT ) :: array( : )

  INTEGER( I4B ) :: n, i,k,j,l, t

  n = SIZE( array )

  IF( n .EQ. 1) RETURN

	l=n/2+1
  k=n

	DO WHILE( k .NE. 1 )
    IF( l .GT. 1 ) THEN
      l=l-1
      t=array(L)
    ELSE
      t=array(k)
      array(k)=array(1)
      k=k-1
      IF( k .EQ. 1 ) THEN
          array(1)=t
        EXIT
      ENDIF
    ENDIF

    i=l
    j=l+l
    DO WHILE( j .LE. k )
      IF( j .LT. k ) THEN
        IF( array( j ) .LT. array( j+1 ) ) j=j+1
      ENDIF
      IF ( t .LT. array(j) ) THEN
        array(i)=array(j)
        i=j
        j=j+j
      ELSE
        j=k+1
      ENDIF
    END DO
    array(i)=t
  ENDDO

END SUBROUTINE HEAPSORT_INT

!----------------------------------------------------------------------------
!                                                                   HeapSort
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Heap Sort algorithm for Real
PURE SUBROUTINE HEAPSORT_REAL( array )
  REAL( DFP ), INTENT( INOUT ) :: array( : )

  INTEGER( I4B ) :: n, i,k,j,l
  REAL( DFP ) :: t

  n = SIZE( array )

  IF( n .EQ. 1) RETURN

	l=n/2+1
  k=n

	DO WHILE( k .NE. 1 )
    IF( l .GT. 1 ) THEN
      l=l-1
      t=array(L)
    ELSE
      t=array(k)
      array(k)=array(1)
      k=k-1
      IF( k .EQ. 1 ) THEN
          array(1)=t
        EXIT
      ENDIF
    ENDIF

    i=l
    j=l+l
    DO WHILE( j .LE. k )
      IF( j .LT. k ) THEN
        IF( array( j ) .LT. array( j+1 ) ) j=j+1
      ENDIF
      IF ( t .LT. array(j) ) THEN
        array(i)=array(j)
        i=j
        j=j+j
      ELSE
        j=k+1
      ENDIF
    END DO
    array(i)=t
  ENDDO

END SUBROUTINE HEAPSORT_REAL

!----------------------------------------------------------------------------
!                                                             Cross_Product
!----------------------------------------------------------------------------

PURE FUNCTION CROSS_PRODUCT_R1_R1( a, b ) RESULT ( Ans )
	REAL( DFP ), INTENT( IN ) :: a( 3 ), b( 3 )
	REAL( DFP ) :: Ans( 3 )

  Ans(1) = a(2)*b(3) - a(3)*b(2)
  Ans(2) = a(3)*b(1) - a(1)*b(3)
  Ans(3) = a(1)*b(2) - a(2)*b(1)

END FUNCTION CROSS_PRODUCT_R1_R1

!----------------------------------------------------------------------------
!                                                                    Degrees
!----------------------------------------------------------------------------

PURE FUNCTION degrees_dfp( rad ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: rad
  REAL( DFP ) :: Ans
  Ans = rad / 3.1415926535_DFP * 180.0_DFP
END FUNCTION degrees_dfp

!----------------------------------------------------------------------------
!                                                                     Input
!----------------------------------------------------------------------------

PURE FUNCTION input_Int( default, option ) RESULT( val )
	INTEGER( I4B ), INTENT( IN ) :: default
	INTEGER( I4B ), OPTIONAL,INTENT( IN ) :: option
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

FUNCTION input_Real(default,option) RESULT(val)
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

FUNCTION input_IntVec( default, option ) RESULT( val )
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

FUNCTION input_Realvec( default, option ) RESULT( val )
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
!                                                                 Reallocate1
!-----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Reallocate a 2D array
PURE SUBROUTINE Reallocate1( Mat, row, col )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, : )
  INTEGER( I4B ), INTENT( IN ) :: row, col

  IF( ALLOCATED( Mat ) ) THEN
    IF( (SIZE( Mat, 1 ) .NE. row) .OR. (SIZE( Mat, 2 ) .NE. col) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( row, col ) )
    END IF
  ELSE
    ALLOCATE( Mat( row, col ) )
  END IF
  Mat = 0.0_DFP
END SUBROUTINE Reallocate1

!----------------------------------------------------------------------------
!                                                                 Reallocate2
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine reallocates a vector
PURE SUBROUTINE Reallocate2( Mat, row )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Mat( : )
  INTEGER( I4B ), INTENT( IN ) :: row

  IF( ALLOCATED( Mat ) ) THEN
    IF( SIZE( Mat ) .NE. row ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( row ) )
    END IF
  ELSE
    ALLOCATE( Mat( row ) )
  END IF
  Mat = 0.0_DFP

END SUBROUTINE Reallocate2

!---------------------------------------------------------------------------
!                                                                 Reallocate
!---------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine reallocates a 3D array
PURE SUBROUTINE Reallocate3( Mat, i1, i2, i3 )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3

  IF( ALLOCATED( Mat ) ) THEN
    IF( (SIZE( Mat, 1 ) .NE. i1) &
      & .OR. (SIZE( Mat, 2 ) .NE. i2) &
      & .OR. (SIZE( Mat, 3 ) .NE. i3) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3 ) )
  END IF
  Mat = 0.0_DFP

END SUBROUTINE Reallocate3

!-----------------------------------------------------------------------------
!                                                                 Reallocate4
!-----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine reallocates a 2D matrix
PURE SUBROUTINE Reallocate4( Mat, row, col )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, : )
  INTEGER( I4B ), INTENT( IN ) :: row, col

  IF( ALLOCATED( Mat ) ) THEN
    IF( (SIZE( Mat, 1 ) .NE. row) .OR. (SIZE( Mat, 2 ) .NE. col) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( row, col ) )
    END IF
  ELSE
    ALLOCATE( Mat( row, col ) )
  END IF
  Mat = 0

END SUBROUTINE Reallocate4

!-----------------------------------------------------------------------------
!                                                                 Reallocate5
!-----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine reallocates a vector
PURE SUBROUTINE Reallocate5( Mat, row )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: Mat( : )
  INTEGER( I4B ), INTENT( IN ) :: row

  IF( ALLOCATED( Mat ) ) THEN
    IF( SIZE( Mat ) .NE. row ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( row ) )
    END IF
  ELSE
    ALLOCATE( Mat( row ) )
  END IF
  Mat = 0

END SUBROUTINE Reallocate5

!----------------------------------------------------------------------------
!                                                                 Reallocate6
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine can reallocates upto six vectors
PURE SUBROUTINE Reallocate6( Vec1, n1, Vec2, n2, Vec3, n3, Vec4, n4, &
  & Vec5, n5, Vec6, n6 )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT) :: Vec1( : ), Vec2( : )
  INTEGER( I4B ), ALLOCATABLE, OPTIONAL, INTENT( INOUT) :: Vec3( : ), &
    & Vec4( : ), Vec5( : ), Vec6( : )
  INTEGER( I4B ), INTENT( IN ) :: n1, n2
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: n3, n4, n5, n6

  IF( ALLOCATED( Vec1 ) ) THEN
    IF( SIZE( Vec1 ) .NE. n1 ) THEN
      DEALLOCATE( Vec1 )
      ALLOCATE( Vec1( n1 ) )
    END IF
  ELSE
    ALLOCATE( Vec1( n1 ) )
  END IF
  Vec1 = 0

  IF( ALLOCATED( Vec2 ) ) THEN
    IF( SIZE( Vec2 ) .NE. n2 ) THEN
      DEALLOCATE( Vec2 )
      ALLOCATE( Vec2( n2 ) )
    END IF
  ELSE
    ALLOCATE( Vec2( n2 ) )
  END IF
  Vec2 = 0

  IF( PRESENT( Vec3 ) ) THEN
    IF( ALLOCATED( Vec3 ) ) THEN
      IF( SIZE( Vec3 ) .NE. n3 ) THEN
        DEALLOCATE( Vec3 )
        ALLOCATE( Vec3( n3 ) )
      END IF
    ELSE
      ALLOCATE( Vec3( n3 ) )
    END IF
    Vec3 = 0
  END IF

  IF( PRESENT( Vec4 ) ) THEN
    IF( ALLOCATED( Vec4 ) ) THEN
      IF( SIZE( Vec4 ) .NE. n4 ) THEN
        DEALLOCATE( Vec4 )
        ALLOCATE( Vec4( n4 ) )
      END IF
    ELSE
      ALLOCATE( Vec4( n4 ) )
    END IF
    Vec4 = 0
  END IF

  IF( PRESENT( Vec5 ) ) THEN
    IF( ALLOCATED( Vec5 ) ) THEN
      IF( SIZE( Vec5 ) .NE. n5 ) THEN
        DEALLOCATE( Vec5 )
        ALLOCATE( Vec5( n5 ) )
      END IF
    ELSE
      ALLOCATE( Vec5( n5 ) )
    END IF
    Vec5 = 0
  END IF

  IF( PRESENT( Vec6 ) ) THEN
    IF( ALLOCATED( Vec6 ) ) THEN
      IF( SIZE( Vec6 ) .NE. n6 ) THEN
        DEALLOCATE( Vec6 )
        ALLOCATE( Vec6( n6 ) )
      END IF
    ELSE
      ALLOCATE( Vec6( n6 ) )
    END IF
    Vec6 = 0
  END IF

END SUBROUTINE Reallocate6

!----------------------------------------------------------------------------
!                                                                 Reallocate7
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine can reallocate upto six vectors
PURE SUBROUTINE Reallocate7( Vec1, n1, Vec2, n2, Vec3, n3, Vec4, n4, &
  & Vec5, n5, Vec6, n6 )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT) :: Vec1( : ), Vec2( : )
  REAL( DFP ), ALLOCATABLE, OPTIONAL, INTENT( INOUT) :: Vec3( : ), &
    & Vec4( : ), Vec5( : ), Vec6( : )
  INTEGER( I4B ), INTENT( IN ) :: n1, n2
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: n3, n4, n5, n6

  IF( ALLOCATED( Vec1 ) ) THEN
    IF( SIZE( Vec1 ) .NE. n1 ) THEN
      DEALLOCATE( Vec1 )
      ALLOCATE( Vec1( n1 ) )
    END IF
  ELSE
    ALLOCATE( Vec1( n1 ) )
  END IF
  Vec1 = 0.0

  IF( ALLOCATED( Vec2 ) ) THEN
    IF( SIZE( Vec2 ) .NE. n2 ) THEN
      DEALLOCATE( Vec2 )
      ALLOCATE( Vec2( n2 ) )
    END IF
  ELSE
    ALLOCATE( Vec2( n2 ) )
  END IF
  Vec2 = 0.0

  IF( PRESENT( Vec3 ) ) THEN
    IF( ALLOCATED( Vec3 ) ) THEN
      IF( SIZE( Vec3 ) .NE. n3 ) THEN
        DEALLOCATE( Vec3 )
        ALLOCATE( Vec3( n3 ) )
      END IF
    ELSE
      ALLOCATE( Vec3( n3 ) )
    END IF
    Vec3 = 0.0
  END IF

  IF( PRESENT( Vec4 ) ) THEN
    IF( ALLOCATED( Vec4 ) ) THEN
      IF( SIZE( Vec4 ) .NE. n4 ) THEN
        DEALLOCATE( Vec4 )
        ALLOCATE( Vec4( n4 ) )
      END IF
    ELSE
      ALLOCATE( Vec4( n4 ) )
    END IF
    Vec4 = 0.0
  END IF

  IF( PRESENT( Vec5 ) ) THEN
    IF( ALLOCATED( Vec5 ) ) THEN
      IF( SIZE( Vec5 ) .NE. n5 ) THEN
        DEALLOCATE( Vec5 )
        ALLOCATE( Vec5( n5 ) )
      END IF
    ELSE
      ALLOCATE( Vec5( n5 ) )
    END IF
    Vec5 = 0.0
  END IF

  IF( PRESENT( Vec6 ) ) THEN
    IF( ALLOCATED( Vec6 ) ) THEN
      IF( SIZE( Vec6 ) .NE. n6 ) THEN
        DEALLOCATE( Vec6 )
        ALLOCATE( Vec6( n6 ) )
      END IF
    ELSE
      ALLOCATE( Vec6( n6 ) )
    END IF
    Vec6 = 0.0
  END IF
END SUBROUTINE Reallocate7

!----------------------------------------------------------------------------
!                                                                Reallocate
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine can reallocate three vectors; it is useful for
! sparse matrix related methods

PURE SUBROUTINE Reallocate8( A, nA, IA, nIA, JA, nJA )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: A( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: IA( : ), JA( : )
  INTEGER( I4B ), INTENT( IN ) :: nA, nIA, nJA

  IF( ALLOCATED( A ) ) THEN
    IF( SIZE( A ) .NE. nA ) THEN
      DEALLOCATE( A )
      ALLOCATE( A( nA ) )
    END IF
  ELSE
    ALLOCATE( A( nA ) )
  END IF
  A = 0.0

  IF( ALLOCATED( IA ) ) THEN
    IF( SIZE( IA ) .NE. nIA ) THEN
      DEALLOCATE( IA )
      ALLOCATE( IA( nIA ) )
    END IF
  ELSE
    ALLOCATE( IA( nIA ) )
  END IF
  IA = 0

  IF( ALLOCATED( JA ) ) THEN
    IF( SIZE( JA ) .NE. nJA ) THEN
      DEALLOCATE( JA )
      ALLOCATE( JA( nJA ) )
    END IF
  ELSE
    ALLOCATE( JA( nJA ) )
  END IF
  JA = 0

END SUBROUTINE Reallocate8

!----------------------------------------------------------------------------
!                                                                Reallocate
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine can reallocate two vectors
PURE SUBROUTINE Reallocate9( A, nA, IA, nIA )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: A( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: IA( : )
  INTEGER( I4B ), INTENT( IN ) :: nA, nIA

  IF( ALLOCATED( A ) ) THEN
    IF( SIZE( A ) .NE. nA ) THEN
      DEALLOCATE( A )
      ALLOCATE( A( nA ) )
    END IF
  ELSE
    ALLOCATE( A( nA ) )
  END IF
  A = 0.0

  IF( ALLOCATED( IA ) ) THEN
    IF( SIZE( IA ) .NE. nIA ) THEN
      DEALLOCATE( IA )
      ALLOCATE( IA( nIA ) )
    END IF
  ELSE
    ALLOCATE( IA( nIA ) )
  END IF
  IA = 0

END SUBROUTINE Reallocate9

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine reallocates a 3D array
PURE SUBROUTINE Reallocate10( Mat, i1, i2, i3, i4 )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Mat( :, :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: i1, i2, i3, i4

  IF( ALLOCATED( Mat ) ) THEN
    IF( (SIZE( Mat, 1 ) .NE. i1) &
      & .OR. (SIZE( Mat, 2 ) .NE. i2) &
      & .OR. (SIZE( Mat, 3 ) .NE. i3) &
      & .OR. (SIZE( Mat, 4 ) .NE. i4) &
      & ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3, i4 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3, i4 ) )
  END IF
  Mat = 0.0_DFP

END SUBROUTINE Reallocate10

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
!
!------------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This FUNCTION evaluate vectors product
! $$\mathbf{ans} = \mathbf{a} \times \mathbf{b}$$

PURE FUNCTION vec_prod( a, b ) RESULT( c )
  ! Define INTENT of dummy argument
  REAL( DFP ), INTENT( IN ) :: a( 3 ), b( 3 )
  REAL( DFP ) :: c( 3 )
  c(1) = a(2) * b(3) - a(3) * b(2)
  c(2) = a(3) * b(1) - a(1) * b(3)
  c(3) = a(1) * b(2) - a(2) * b(1)
END FUNCTION vec_prod

!--------------------------------------------------------------------
!
!--------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This FUNCTION returns outerproduct(matrix) of two vectors
! $$\mathbf{ans} = \mathbf{a} \otimes \mathbf{b}$$

PURE FUNCTION OUTERPROD1_1( a,b ) RESULT( Ans )
  REAL(DFP), DIMENSION(:), INTENT(IN) :: a,b
  REAL(DFP), DIMENSION(SIZE(a),SIZE(b)) :: Ans
  Ans = 0.0_DFP
  Ans = SPREAD(a,dim=2,ncopies=size(b)) * &
          & SPREAD(b,dim=1,ncopies=size(a))
END FUNCTION OUTERPROD1_1

!--------------------------------------------------------------------
!
!--------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This FUNCTION returns outerproduct(matrix) of two vectors
! - $$\mathbf{ans} = \mathbf{a} \otimes \mathbf{b}$$
! - If `Sym` is .true. THEN symmetric part is returned

PURE FUNCTION OUTERPROD1_1_sym(a,b, Sym) RESULT( Ans )
  ! Define INTENT of dummy variables
  REAL(DFP), INTENT(IN) :: a ( : ), b ( : )
  REAL(DFP), DIMENSION(SIZE(a),SIZE(b)) :: Ans
  LOGICAL( LGT ), INTENT( IN ) :: Sym

  Ans = 0.0_DFP
  IF( Sym ) THEN
    Ans =   SPREAD( 0.5_DFP * a, dim=2, ncopies=size(b) ) &
        & * SPREAD( b, dim=1, ncopies=size(a) ) &
        & + SPREAD( 0.5_DFP * b, dim=2, ncopies=size(a) ) &
        & * SPREAD( a, dim=1, ncopies=size(b) )
  ELSE
    Ans = SPREAD(a,dim=2,ncopies=size(b)) * &
            & SPREAD(b,dim=1,ncopies=size(a))
  END IF

END FUNCTION OUTERPROD1_1_sym

!--------------------------------------------------------------------
!
!--------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This FUNCTION returns outerprod between a matrix and a vector
! `Ans(:,:,i) = a(:,:) * b(i)`

PURE FUNCTION OUTERPROD2_1(a,b) RESULT( Ans )
    REAL(DFP), INTENT(IN) :: a( :, : )
    REAL(DFP), INTENT(IN) :: b( : )
    REAL(DFP) :: Ans( SIZE( a, 1 ), SIZE( a, 2 ), SIZE(b) )

    ! Definen internal variables
    INTEGER( I4B ) :: I
    Ans = 0.0_DFP
    FORALL (I =1:SIZE( b ))
      Ans( :, :, I ) = a( :, : ) * b( I )
    END FORALL

END FUNCTION OUTERPROD2_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This FUNCTION evaluate outerproduct between a 3D matrix and a vector
! - `Ans( :, :, :,  I ) = a( :, :, : ) * b( I )`
PURE FUNCTION OUTERPROD3_1(a,b) RESULT( Ans )
  REAL(DFP), INTENT( IN ) :: a(:,:,:)
  REAL(DFP), INTENT( IN ) :: b(:)
  REAL(DFP) :: Ans( SIZE( a, 1 ), SIZE( a, 2 ), SIZE( a, 3 ), SIZE(  b ) )

  INTEGER( I4B ) :: I

  Ans = 0.0_DFP
  FORALL (I =1:SIZE( b ) )
    Ans( :, :, :,  I ) = a( :, :, : ) * b( I )
  END FORALL
END FUNCTION OUTERPROD3_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This FUNCTION evaluates outer product between a matrix and two vector
!
! $$Ans = a \otimes b \otimes c$$

PURE FUNCTION OUTERPROD2_11(a,b,c) RESULT( ANS )
  REAL(DFP), INTENT( IN ) :: a( :, : )
  REAL(DFP), INTENT( IN ) :: b( : ), c( : )
  REAL(DFP) :: ANS( SIZE( a, 1 ), SIZE( a, 2 ), SIZE( b ), SIZE(  c ) )

  ! Definen internal variables
  REAL(DFP), DIMENSION( SIZE( a, 1 ), SIZE( a, 2 ), SIZE( b ) ) :: Dummy3
  Dummy3 = OUTERPROD2_1(a,b)
  ANS = OUTERPROD3_1( Dummy3, c )

END FUNCTION OUTERPROD2_11

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

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Append scalar INTEGER  to  INTEGER  vec tor

PURE SUBROUTINE Append_I1( A, Entry )
  INTEGER(I4B), ALLOCATABLE, INTENT( INOUT ) :: A( : )
  INTEGER(I4B), INTENT( IN ) :: Entry
  INTEGER(I4B),  ALLOCATABLE :: Dummy( : )
  INTEGER(I4B) :: tSize
  IF( .NOT. ALLOCATED( A ) ) THEN
    A = [Entry]
  ELSE
    tSize = SIZE( A ); ALLOCATE( Dummy( tSize + 1 ) )
    Dummy( 1 : tSize ) = A; Dummy( tSize + 1 ) = Entry
    CALL MOVE_ALLOC( From = Dummy, To = A )
  END IF
END SUBROUTINE Append_I1

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Append vector of INTEGER  to  INTEGER  vec tor
!------------------------------------------------------------------------------

PURE SUBROUTINE Append_I2( A, Entry )
  INTEGER(I4B), ALLOCATABLE, INTENT( INOUT ) :: A( : )
  INTEGER(I4B), INTENT( IN ) :: Entry( : )

  INTEGER(I4B),  ALLOCATABLE :: Dummy( : )
  INTEGER(I4B) :: n, m

  IF( .NOT. ALLOCATED( A ) ) THEN
    A = Entry
  ELSE
    m = SIZE( Entry ); n = SIZE( A )
    ALLOCATE( Dummy( n + m ) ); Dummy( 1 : n ) = A; Dummy( n+1 : ) = Entry
    CALL MOVE_ALLOC( From = Dummy, To = A )
  END IF

END SUBROUTINE Append_I2

!----------------------------------------------------------------------------
!                                                                    Append
!----------------------------------------------------------------------------
!> authors: Dr. Vikas Sharma
!
! Append scalar REAL to the REAL-vector
!------------------------------------------------------------------------------

PURE SUBROUTINE Append_R1( A, Entry )
  REAL(DFP), ALLOCATABLE, INTENT( INOUT ) :: A( : )
  REAL(DFP), INTENT( IN ) :: Entry

  REAL(DFP),  ALLOCATABLE :: Dummy( : )
  INTEGER(I4B) :: n


  IF( .NOT. ALLOCATED( A ) ) THEN
    A = [Entry]
  ELSE
    n = SIZE( A ); ALLOCATE( Dummy( n + 1 ) )
    Dummy( 1 : n ) = A; Dummy( 1 + n ) = Entry
    CALL MOVE_ALLOC( From = Dummy, TO = A )
  END IF

END SUBROUTINE Append_R1

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Append vector of REAL to REAL-vector

PURE SUBROUTINE Append_R2( A, Entry )
  REAL(DFP), ALLOCATABLE, INTENT( INOUT ) :: A( : )
  REAL(DFP), INTENT( IN ) :: Entry( : )

  REAL(DFP),  ALLOCATABLE :: Dummy( : )
  INTEGER(I4B) :: n, m

  IF( .NOT. ALLOCATED( A ) ) THEN
    A = Entry
  ELSE
    m = SIZE( Entry ); n = SIZE( A ); ALLOCATE( Dummy( n + m ) )
    Dummy( 1 : n ) = A; Dummy( 1 + n : ) = Entry
    CALL MOVE_ALLOC( FROM = Dummy, TO = A )
  END IF

END SUBROUTINE Append_R2

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

!------------------------------------------------------------------------------
!                                                                   assert_eq2
!------------------------------------------------------------------------------

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
!                                                                      SWAP
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Subroutine for interchanging two INTEGER

PURE SUBROUTINE swap_i(a,b)
  INTEGER(I4B), INTENT(INOUT) :: a,b
  INTEGER(I4B) :: dum
  dum=a
  a=b
  b=dum
END SUBROUTINE swap_i

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Subroutine for interchanging two REAL numbers

PURE SUBROUTINE swap_r(a,b)
  REAL(DFP), INTENT(INOUT) :: a,b
  REAL(DFP) :: dum
  dum=a
  a=b
  b=dum
END SUBROUTINE swap_r

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Subroutine for interchanging two REAL valued vectors

PURE SUBROUTINE swap_rv(a,b)
  REAL(DFP), DIMENSION(:), INTENT(INOUT) :: a,b
  REAL(DFP), DIMENSION(SIZE(a)) :: dum
  dum=a
  a=b
  b=dum
END SUBROUTINE swap_rv

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Subroutine for interchanging two complex numbers

PURE SUBROUTINE swap_c(a,b)
  COMPLEX(DFPC), INTENT(INOUT) :: a,b
  COMPLEX(DFPC) :: dum
  dum=a
  a=b
  b=dum
END SUBROUTINE swap_c

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Subroutine for interchanging two complexed valued vectors

PURE SUBROUTINE swap_cv(a,b)
  COMPLEX(DFPC), DIMENSION(:), INTENT(INOUT) :: a,b
  COMPLEX(DFPC), DIMENSION(SIZE(a)) :: dum
  dum=a
  a=b
  b=dum
END SUBROUTINE swap_cv

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Subroutine for interchanging two complexed valued matrices

PURE SUBROUTINE swap_cm(a,b)
  COMPLEX(DFPC), DIMENSION(:,:), INTENT(INOUT) :: a,b
  COMPLEX(DFPC), DIMENSION(size(a,1),size(a,2)) :: dum
  dum=a
  a=b
  b=dum
END SUBROUTINE swap_cm

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Subroutine for interchanging two REAL valued number

PURE SUBROUTINE masked_swap_rs(a,b,mask)
  REAL(DFP), INTENT(INOUT) :: a,b
  LOGICAL(LGT), INTENT(IN) :: mask
  REAL(DFP) :: swp
  IF (mask) THEN
    swp=a
    a=b
    b=swp
  END IF
END SUBROUTINE masked_swap_rs

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
!Subroutine for interchanging two REAL valued vectors

PURE SUBROUTINE masked_swap_rv(a,b,mask)
  REAL(DFP), DIMENSION(:), INTENT(INOUT) :: a,b
  LOGICAL(LGT), DIMENSION(:), INTENT(IN) :: mask
  REAL(DFP), DIMENSION(size(a)) :: swp
  WHERE(mask)
    swp=a
    a=b
    b=swp
  END WHERE
END SUBROUTINE masked_swap_rv

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Subroutine for interchanging two REAL valued matrices

PURE SUBROUTINE masked_swap_rm(a,b,mask)
  REAL(DFP), DIMENSION(:,:), INTENT(INOUT) :: a,b
  LOGICAL(LGT), DIMENSION(:,:), INTENT(IN) :: mask
  REAL(DFP), DIMENSION(size(a,1),size(a,2)) :: swp
  where (mask)
    swp=a
    a=b
    b=swp
  END where
END SUBROUTINE masked_swap_rm

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
