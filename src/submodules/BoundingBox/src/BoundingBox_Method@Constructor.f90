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
! summary: This submodule of [[BoundingBox_Method]] implements methods defined in parent module
!
!### Introduction
!
! This submodule of [[BoiundingBox_Method]] implements the method defined in the parent module

SUBMODULE( BoundingBox_Method ) Constructor
USE Display_Method, ONLY: BlankLines
IMPLICIT NONE
CONTAINS

!-----------------------------------------------------------------------------
!                                                                    Initiate
!-----------------------------------------------------------------------------

MODULE PROCEDURE initiate_1
  Obj%Box = 0.0_DFP
  Obj%NSD = NSD
  Obj%Box( 1, 1 ) = lim( 1 ) !xmin
  Obj%Box( 1, 2 ) = lim( 3 ) !ymin
  Obj%Box( 1, 3 ) = lim( 5 ) !zmin
  Obj%Box( 2, 1 ) = lim( 2 ) !xmax
  Obj%Box( 2, 2 ) = lim( 4 ) !ymax
  Obj%Box( 2, 3 ) = lim( 6 ) !zmax
END PROCEDURE initiate_1

!-----------------------------------------------------------------------------
!                                                                    Initiate
!-----------------------------------------------------------------------------

MODULE PROCEDURE initiate_2
  Obj%Box = AnotherObj%Box
  Obj%NSD = AnotherObj%NSD
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
  WRITE( I, "(A, I4)" ) "NSD :: ", Obj%NSD
  WRITE( I, "(A, G15.7)" ) "Xmin :: ", .Xmin. Obj
  WRITE( I, "(A, G15.7)" ) "Xmax :: ", .Xmax. Obj
  WRITE( I, "(A, G15.7)" ) "Ymin :: ", .Ymin. Obj
  WRITE( I, "(A, G15.7)" ) "Ymax :: ", .Ymax. Obj
  WRITE( I, "(A, G15.7)" ) "Zmin :: ", .Zmin. Obj
  WRITE( I, "(A, G15.7)" ) "Zmax :: ", .Zmax. Obj

END PROCEDURE display_obj

END SUBMODULE Constructor
