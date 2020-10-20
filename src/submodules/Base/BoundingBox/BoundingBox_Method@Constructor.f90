SUBMODULE( BoundingBox_Method ) Constructor
IMPLICIT NONE

CONTAINS

!-----------------------------------------------------------------------------
!                                                                    Initiate
!-----------------------------------------------------------------------------

MODULE PROCEDURE initiate_1
  Obj % Box = 0.0_DFP
  Obj % NSD = NSD
  Obj % Box( 1, 1 ) = lim( 1 ) !xmin
  Obj % Box( 1, 2 ) = lim( 3 ) !ymin
  Obj % Box( 1, 3 ) = lim( 5 ) !zmin
  Obj % Box( 2, 1 ) = lim( 2 ) !xmax
  Obj % Box( 2, 2 ) = lim( 4 ) !ymax
  Obj % Box( 2, 3 ) = lim( 6 ) !zmax
END PROCEDURE initiate_1

!-----------------------------------------------------------------------------
!                                                                    Initiate
!-----------------------------------------------------------------------------

MODULE PROCEDURE initiate_2
  Obj % Box = AnotherObj % Box
  Obj % NSD = AnotherObj % NSD
END PROCEDURE initiate_2

!-----------------------------------------------------------------------------
!                                                                BoundingBox
!-----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  CALL Initiate( Ans, nsd, lim )
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                               Bounding box
!----------------------------------------------------------------------------
MODULE PROCEDURE Constructor2
  CALL Initiate( Ans, AnotherObj )
END PROCEDURE Constructor2

!----------------------------------------------------------------------------
!                                                               Bounding box
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor3
  REAL( DFP ) :: lim( 6 )
  INTEGER( I4B ) :: nsd

  lim = 0.0_DFP
  nsd = SIZE( xij, 1 )

  SELECT CASE( nsd )
  CASE( 1 )
    lim( 1 ) = MINVAL( xij( 1, : ) )
    lim( 2 ) = MAXVAL( xij( 1, : ) )
  CASE( 2 )
    lim( 1 ) = MINVAL( xij( 1, : ) )
    lim( 2 ) = MAXVAL( xij( 1, : ) )
    lim( 3 ) = MINVAL( xij( 2, : ) )
    lim( 4 ) = MAXVAL( xij( 2, : ) )
  CASE( 3 )
    lim( 1 ) = MINVAL( xij( 1, : ) )
    lim( 2 ) = MAXVAL( xij( 1, : ) )
    lim( 3 ) = MINVAL( xij( 2, : ) )
    lim( 4 ) = MAXVAL( xij( 2, : ) )
    lim( 5 ) = MINVAL( xij( 3, : ) )
    lim( 6 ) = MAXVAL( xij( 3, : ) )
  END SELECT

  CALL Initiate( Obj = Ans, nsd = nsd, lim = lim )
END PROCEDURE Constructor3

!-----------------------------------------------------------------------------
!                                                         BoundingBox_Pointer
!-----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_1
  ALLOCATE( Ans )
  CALL Initiate( Ans, nsd, lim )
END PROCEDURE Constructor_1

MODULE PROCEDURE Constructor_2
  ALLOCATE( Ans )
  CALL Initiate( Ans, AnotherObj )
END PROCEDURE Constructor_2

!----------------------------------------------------------------------------
!                                                                     Display
!----------------------------------------------------------------------------

MODULE PROCEDURE display_obj
  ! Define internal variable
  INTEGER( I4B ) :: I

  I = stdOut
  IF( PRESENT( UnitNo ) ) I = UnitNo

  WRITE( I, "(A)" ) "Type :: BoundingBox_"
  CALL BlankLines( I )
  WRITE( I, "(A, I4)" ) "NSD :: ", Obj % NSD
  WRITE( I, "(A, G15.7)" ) "Xmin :: ", .Xmin. Obj
  WRITE( I, "(A, G15.7)" ) "Xmax :: ", .Xmax. Obj
  WRITE( I, "(A, G15.7)" ) "Ymin :: ", .Ymin. Obj
  WRITE( I, "(A, G15.7)" ) "Ymax :: ", .Ymax. Obj
  WRITE( I, "(A, G15.7)" ) "Zmin :: ", .Zmin. Obj
  WRITE( I, "(A, G15.7)" ) "Zmax :: ", .Zmax. Obj

END PROCEDURE display_obj

!-----------------------------------------------------------------------------
!                                                                     setXmin
!-----------------------------------------------------------------------------

MODULE PROCEDURE setXmin
  Obj % Box( 1, 1 ) = Val
END PROCEDURE setXmin

!-----------------------------------------------------------------------------
!                                                                     setXmax
!-----------------------------------------------------------------------------

MODULE PROCEDURE setXmax
  Obj % Box( 2, 1 ) = Val
END PROCEDURE setXmax

!-----------------------------------------------------------------------------
!                                                                     setYmin
!-----------------------------------------------------------------------------

MODULE PROCEDURE setYmin
  Obj % Box( 1, 2 ) = Val
END PROCEDURE setYmin

!-----------------------------------------------------------------------------
!                                                                     setYmax
!-----------------------------------------------------------------------------

MODULE PROCEDURE setYmax
  Obj % Box( 2, 2 ) = Val
END PROCEDURE setYmax

!-----------------------------------------------------------------------------
!                                                                     setZmin
!-----------------------------------------------------------------------------

MODULE PROCEDURE setZmin
  Obj % Box( 1, 3 ) = Val
END PROCEDURE setZmin

!-----------------------------------------------------------------------------
!                                                                     setZmax
!-----------------------------------------------------------------------------

MODULE PROCEDURE setZmax
  Obj % Box( 2, 3 ) = Val
END PROCEDURE setZmax

!-----------------------------------------------------------------------------
!                                                                     getXmin
!-----------------------------------------------------------------------------

MODULE PROCEDURE getXmin
  Ans = Obj % Box( 1, 1 )
END PROCEDURE getXmin

!-----------------------------------------------------------------------------
!                                                                     getXmax
!-----------------------------------------------------------------------------

MODULE PROCEDURE getXmax
  Ans = Obj % Box( 2, 1 )
END PROCEDURE getXmax

!-----------------------------------------------------------------------------
!                                                                     getYmin
!-----------------------------------------------------------------------------

MODULE PROCEDURE getYmin
  Ans =  Obj % Box( 1, 2 )
END PROCEDURE getYmin

!-----------------------------------------------------------------------------
!                                                                     getYmax
!-----------------------------------------------------------------------------

MODULE PROCEDURE getYmax
  Ans = Obj % Box( 2, 2 )
END PROCEDURE getYmax

!-----------------------------------------------------------------------------
!                                                                     getZmin
!-----------------------------------------------------------------------------

MODULE PROCEDURE getZmin
  Ans = Obj % Box( 1, 3 )
END PROCEDURE getZmin

!-----------------------------------------------------------------------------
!                                                                     getZmax
!-----------------------------------------------------------------------------

MODULE PROCEDURE getZmax
  Ans = Obj % Box( 2, 3 )
END PROCEDURE getZmax

!----------------------------------------------------------------------------
!                                                          is_intersect_in_X
!----------------------------------------------------------------------------

MODULE PROCEDURE is_intersect_in_X
  ! Define internal variables
  REAL( DFP ) :: Min1, Max1, Min2, Max2
  LOGICAL( LGT ) :: Left, Right

  Min1 = .Xmin. Obj; Max1 = .Xmax. Obj
  Min2 = .Xmin. Obj2; Max2 = .Xmax. Obj2

  Right = ( Min2 .GE. Min1 ) .AND. ( Min2 .LE. Max1 )
  Left = ( Max2 .GE. Min1 ) .AND. ( Max2 .LE. Max1 )

  IF( Left .OR. Right ) THEN
    Ans = .TRUE.
  ELSE
    Ans = .FALSE.
  END IF
END PROCEDURE is_intersect_in_X

!----------------------------------------------------------------------------
!                                                          is_intersect_in_Y
!----------------------------------------------------------------------------

MODULE PROCEDURE is_intersect_in_Y
  ! Define internal variables
  REAL( DFP ) :: Min1, Max1, Min2, Max2
  LOGICAL( LGT ) :: Left, Right

  Min1 = .Ymin. Obj; Max1 = .Ymax. Obj
  Min2 = .Ymin. Obj2; Max2 = .Ymax. Obj2

  Right = ( Min2 .GE. Min1 ) .AND. ( Min2 .LE. Max1 )
  Left = ( Max2 .GE. Min1 ) .AND. ( Max2 .LE. Max1 )

  IF( Left .OR. Right ) THEN
    Ans = .TRUE.
  ELSE
    Ans = .FALSE.
  END IF
END PROCEDURE is_intersect_in_Y

!----------------------------------------------------------------------------
!                                                          is_intersect_in_Z
!----------------------------------------------------------------------------

MODULE PROCEDURE is_intersect_in_Z
  ! Define internal variables
  REAL( DFP ) :: Min1, Max1, Min2, Max2
  LOGICAL( LGT ) :: Left, Right

  Min1 = .Zmin. Obj; Max1 = .Zmax. Obj
  Min2 = .Zmin. Obj2; Max2 = .Zmax. Obj2

  Right = ( Min2 .GE. Min1 ) .AND. ( Min2 .LE. Max1 )
  Left = ( Max2 .GE. Min1 ) .AND. ( Max2 .LE. Max1 )

  IF( Left .OR. Right ) THEN
    Ans = .TRUE.
  ELSE
    Ans = .FALSE.
  END IF
END PROCEDURE is_intersect_in_Z

!----------------------------------------------------------------------------
!                                                               is_intersect
!----------------------------------------------------------------------------

MODULE PROCEDURE is_intersect
  Ans = isIntersectInX( Obj, Obj2 ) &
    & .AND. isIntersectInY( Obj, Obj2 ) &
    & .AND. isIntersectInZ( Obj, Obj2 )
END PROCEDURE is_intersect

!----------------------------------------------------------------------------
!                                                           get_intersection
!----------------------------------------------------------------------------

MODULE PROCEDURE get_intersection
  ! Define internal variables
  REAL( DFP ) :: Min1, Max1, Min2, Max2
  LOGICAL( LGT ) :: Right

  Ans % NSD = MAX( Obj % NSD, Obj2 % NSD )
  Ans % Box = 0.0_DFP

  IF( Obj .isIntersect. Obj2 ) THEN
    Min1 = .Xmin. Obj; Max1 = .Xmax. Obj
    Min2 = .Xmin. Obj2; Max2 = .Xmax. Obj2
    Right = ( Min2 .GE. Min1 ) .AND. ( Min2 .LE. Max1 )
    IF( Right ) THEN
      CALL setXmin( Ans, Min2 )
      CALL setXmax( Ans, Max1 )
    ELSE
      CALL setXmin( Ans, Min1 )
      CALL setXmax( Ans, Max2 )
    END IF
    Min1 = .Ymin. Obj; Max1 = .Ymax. Obj
    Min2 = .Ymin. Obj2; Max2 = .Ymax. Obj2
    Right = ( Min2 .GE. Min1 ) .AND. ( Min2 .LE. Max1 )
    IF( Right ) THEN
      CALL setYmin( Ans, Min2 )
      CALL setYmax( Ans, Max1 )
    ELSE
      CALL setYmin( Ans, Min1 )
      CALL setYmax( Ans, Max2 )
    END IF
    Min1 = .Zmin. Obj; Max1 = .Zmax. Obj
    Min2 = .Zmin. Obj2; Max2 = .Zmax. Obj2
    Right = ( Min2 .GE. Min1 ) .AND. ( Min2 .LE. Max1 )
    IF( Right ) THEN
      CALL setZmin( Ans, Min2 )
      CALL setZmax( Ans, Max1 )
    ELSE
      CALL setZmin( Ans, Min1 )
      CALL setZmax( Ans, Max2 )
    END IF
  END IF
END PROCEDURE get_intersection

!----------------------------------------------------------------------------
!                                                                      Union
!----------------------------------------------------------------------------

MODULE PROCEDURE get_union
  ! Define Internal variables
  REAL( DFP ) :: Val, Val1, Val2

  Ans % NSD = MAX( Obj % NSD, Obj2 % NSD )

  Val1 = .Xmin. Obj; Val2 = .Xmin. Obj2
  Val = MIN( Val1, Val2 )
  CALL SetXMin( Ans, Val )
  Val1 = .Xmax. Obj; Val2 = .Xmax. Obj2
  Val = MAX( Val1, Val2 )
  CALL SetXMax( Ans, Val )

  Val1 = .Ymin. Obj; Val2 = .Ymin. Obj2
  Val = MIN( Val1, Val2 )
  CALL SetYMin( Ans, Val )
  Val1 = .Ymax. Obj; Val2 = .Ymax. Obj2
  Val = MAX( Val1, Val2 )
  CALL SetYMax( Ans, Val )

  Val1 = .Zmin. Obj; Val2 = .Zmin. Obj2
  Val = MIN( Val1, Val2 )
  CALL SetZMin( Ans, Val )
  Val1 = .Zmax. Obj; Val2 = .Zmax. Obj2
  Val = MAX( Val1, Val2 )
  CALL SetZMax( Ans, Val )
END PROCEDURE get_union

!----------------------------------------------------------------------------
!                                                                    Center
!----------------------------------------------------------------------------

MODULE PROCEDURE get_Center
  Ans( 1 ) = SUM( Obj % Box( :, 1 ) ) / 2.0_DFP
  Ans( 2 ) = SUM( Obj % Box( :, 2 ) ) / 2.0_DFP
  Ans( 3 ) = SUM( Obj % Box( :, 3 ) ) / 2.0_DFP
END PROCEDURE get_Center

!----------------------------------------------------------------------------
!                                                                 IsInside
!----------------------------------------------------------------------------

MODULE PROCEDURE is_Inside

  ! internal variables
  INTEGER( I4B ) :: NSD
  REAL( DFP ) :: Min1, Max1
  LOGICAL( LGT ) :: Ans1, Ans2, Ans3

  NSD = SIZE( Val )

  SELECT CASE( NSD )

    CASE( 1 )

      Min1 = .Xmin. Obj; Max1 = .Xmax. Obj
      IF( Val( 1 ) .GE. Min1 .AND. Val( 1 ) .LE. Max1 ) THEN
        Ans = .TRUE.
      ELSE
        Ans = .FALSE.
      END IF

    CASE( 2 )

      Min1 = .Xmin. Obj; Max1 = .Xmax. Obj
      IF( Val( 1 ) .GE. Min1 .AND. Val( 1 ) .LE. Max1 ) THEN
        Ans1 = .TRUE.
      ELSE
        Ans2 = .FALSE.
      END IF

      Min1 = .Ymin. Obj; Max1 = .Ymax. Obj
      IF( Val( 2 ) .GE. Min1 .AND. Val( 2 ) .LE. Max1 ) THEN
        Ans2 = .TRUE.
      ELSE
        Ans2 = .FALSE.
      END IF

      IF( Ans1 .AND. Ans2 ) THEN
        Ans = .TRUE.
      ELSE
        Ans = .FALSE.
      END IF

    CASE DEFAULT

      Min1 = .Xmin. Obj; Max1 = .Xmax. Obj
      IF( Val( 1 ) .GE. Min1 .AND. Val( 1 ) .LE. Max1 ) THEN
        Ans1 = .TRUE.
      ELSE
        Ans1 = .FALSE.
      END IF

      Min1 = .Ymin. Obj; Max1 = .Ymax. Obj
      IF( Val( 2 ) .GE. Min1 .AND. Val( 2 ) .LE. Max1 ) THEN
        Ans2 = .TRUE.
      ELSE
        Ans2 = .FALSE.
      END IF

      Min1 = .Zmin. Obj; Max1 = .Zmax. Obj
      IF( Val( 3 ) .GE. Min1 .AND. Val( 3 ) .LE. Max1 ) THEN
        Ans3 = .TRUE.
      ELSE
        Ans3 = .FALSE.
      END IF

      IF( Ans1 .AND. Ans2 .AND. Ans3 ) THEN
        Ans = .TRUE.
      ELSE
        Ans = .FALSE.
      END IF

  END SELECT
END PROCEDURE is_Inside

!----------------------------------------------------------------------------
!                                                                 IsInside
!----------------------------------------------------------------------------

MODULE PROCEDURE get_nptrs
  INTEGER( I4B ) :: n, i
  LOGICAL( LGT ), ALLOCATABLE :: msk( : )
  INTEGER( I4B ), ALLOCATABLE :: Indx( : )

  n = SIZE( xij, 2 )
  ALLOCATE( msk( n ), Indx( n ) )

  DO i = 1, n
    msk( i ) = isInside( Obj, xij( :, i ) )
    Indx( i ) = i
  END DO

  Ans = PACK( Indx, msk )

  DEALLOCATE( msk, Indx )

END PROCEDURE get_nptrs

END SUBMODULE Constructor
