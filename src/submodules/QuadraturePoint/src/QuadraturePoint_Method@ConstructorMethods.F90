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
! summary:  Constructor methods for [[QuadraturePoint_]]

SUBMODULE(QuadraturePoint_Method) ConstructorMethods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_initiate1
  obj%points = points
  obj%tXi = SIZE( points, 1 ) - 1
    !! No of row minus one
END PROCEDURE quad_initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_initiate2
  obj%tXi = tXi
  CALL Reallocate( obj%points, tXi + 1, tpoints )
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
  CASE( "GaussLobatto" )
  CASE( "Chebyshev" )
  END SELECT
END PROCEDURE quad_initiate4

!----------------------------------------------------------------------------
!                                                            QuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_Constructor1
  obj%points = points
  obj%tXi = SIZE( points, 1 ) - 1
END PROCEDURE quad_Constructor1

!----------------------------------------------------------------------------
!                                                   QuadraturePoint_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_Constructor_1
  ALLOCATE( obj )
  obj%points = points
  obj%tXi = SIZE( points, 1 ) - 1
END PROCEDURE quad_Constructor_1

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_Deallocate
  IF( ALLOCATED( obj%points ) ) DEALLOCATE( obj%points )
  obj%tXi = -1
END PROCEDURE quad_Deallocate

END SUBMODULE ConstructorMethods