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

SUBMODULE(QuadraturePoint_Method) Constructor
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_initiate1
  obj%Points = Points
  obj%tXi = SIZE( Points, 1 ) - 1
    !! No of row minus one
END PROCEDURE quad_initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_initiate2
  obj%tXi = tXi
  CALL Reallocate( obj%Points, tXi + 1, tPoints )
END PROCEDURE quad_initiate2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_initiate3
  SELECT CASE( TRIM( quadratureType ) )
  CASE( "GaussLegendre" )
    obj = GaussLegendreQuadrature( refElem=refElem, order=order)
  END SELECT
END PROCEDURE quad_initiate3

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_initiate4
  SELECT CASE( TRIM( quadratureType ) )
  CASE( "GaussLegendre" )
    obj = GaussLegendreQuadrature( refElem=refElem, nips=nips)
  END SELECT
END PROCEDURE quad_initiate4

!----------------------------------------------------------------------------
!                                                            QuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_Constructor1
  obj%Points = Points
  obj%tXi = SIZE( Points, 1 ) - 1
END PROCEDURE quad_Constructor1

!----------------------------------------------------------------------------
!                                                   QuadraturePoint_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_Constructor_1
  ALLOCATE( obj )
  obj%Points = Points
  obj%tXi = SIZE( Points, 1 ) - 1
END PROCEDURE quad_Constructor_1

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_DeallocateData
  IF( ALLOCATED( obj%Points ) ) DEALLOCATE( obj%Points )
  obj%tXi = -1
END PROCEDURE quad_DeallocateData

!----------------------------------------------------------------------------
!                                                                       SIZE
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_Size
  IF( Dims .EQ. 1 ) THEN
    Ans = SIZE( obj%Points, 1 )
  ELSE IF( Dims .EQ. 2 ) THEN
    Ans = SIZE( obj%Points, 2 )
  ELSE
    Ans = 0
  END IF
END PROCEDURE quad_Size

!----------------------------------------------------------------------------
!                                                  getTotalQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_getTotalQuadraturePoints
  Ans = SIZE( obj, 2 )
END PROCEDURE quad_getTotalQuadraturePoints

!----------------------------------------------------------------------------
!                                                         getQuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_GetQuadraturePoints1
  Point = 0.0_DFP
  Point( 1 : obj%tXi ) = obj%Points( 1 : obj%tXi, Num )
  Weight = obj%Points( obj%tXi + 1, Num )
END PROCEDURE quad_GetQuadraturePoints1

!----------------------------------------------------------------------------
!                                                         getQuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_GetQuadraturePoints2
  INTEGER( I4B ) :: n
  n = SIZE( obj%Points, 2 ) !#column
  CALL Reallocate( Point, 3, n )
  Point( 1 : obj%tXi, 1:n ) = obj%Points( 1 : obj%tXi, 1:n )
  Weight = obj%Points( obj%tXi + 1, 1:n )
END PROCEDURE quad_GetQuadraturePoints2

END SUBMODULE Constructor
