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
! date: 	3 March 2021
! summary: 	This submodule contains information constructor methods for [[QuadraturePoint_]]

SUBMODULE( QuadraturePoint_Method ) Constructor
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj
  obj%Points = Points
  obj%tXi = SIZE( Points, 1 ) - 1
    !! No of row minus one
END PROCEDURE initiate_obj

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj_txi
  obj%tXi = tXi
  CALL Reallocate( obj%Points, tXi + 1, tPoints )
END PROCEDURE initiate_obj_txi

!----------------------------------------------------------------------------
!                                                            QuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  obj%Points = Points
  obj%tXi = SIZE( Points, 1 ) - 1
END PROCEDURE Constructor1

MODULE PROCEDURE Constructor_1
  ALLOCATE( obj )
  obj%Points = Points
  obj%tXi = SIZE( Points, 1 ) - 1
END PROCEDURE Constructor_1

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Data
  IF( ALLOCATED( obj%Points ) ) DEALLOCATE( obj%Points )
  obj%tXi = -1
END PROCEDURE Deallocate_Data

!----------------------------------------------------------------------------
!                                                                       SIZE
!----------------------------------------------------------------------------

MODULE PROCEDURE size_obj
  IF( Dims .EQ. 1 ) THEN
    Ans = SIZE( obj%Points, 1 )
  ELSE IF( Dims .EQ. 2 ) THEN
    Ans = SIZE( obj%Points, 2 )
  ELSE
    Ans = 0
  END IF
END PROCEDURE size_obj

!----------------------------------------------------------------------------
!                                                         getQuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE getQP1
  Point = 0.0_DFP
  Point( 1 : obj%tXi ) = obj%Points( 1 : obj%tXi, Num )
  Weight = obj%Points( obj%tXi + 1, Num )
END PROCEDURE getQP1

!----------------------------------------------------------------------------
!                                                         getQuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE getQP2
  INTEGER( I4B ) :: n
  n = SIZE( obj%Points, 2 ) !#column
  CALL Reallocate( Point, 3, n )
  Point( 1 : obj%tXi, 1:n ) = obj%Points( 1 : obj%tXi, 1:n )
  Weight = obj%Points( obj%tXi + 1, 1:n )
END PROCEDURE getQP2

END SUBMODULE Constructor
