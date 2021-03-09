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
!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	This submodule contains implementation of get method for [[BoundingBox_]] data type which are defined in [[BoundingBox_Method]] module.

SUBMODULE( BoundingBox_Method ) getMethod
IMPLICIT NONE
CONTAINS


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
!                                                                   getNptrs
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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE getMethod