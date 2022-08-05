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

!> author: Vikas Sharma, Ph. D.
! date: 	3 March 2021
! summary:  Constructor methods for [[Quadraturepoints_]]

SUBMODULE(QuadraturePoint_Method) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       SIZE
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_Size
  ans = SIZE( obj%points, dims )
END PROCEDURE quad_Size

!----------------------------------------------------------------------------
!                                                  getTotalQuadraturepoints
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_getTotalQuadraturepoints
  ans = SIZE( obj, 2 )
END PROCEDURE quad_getTotalQuadraturepoints

!----------------------------------------------------------------------------
!                                                         getQuadraturepoints
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_GetQuadraturePoints1
  point = 0.0_DFP
  point( 1 : obj%tXi ) = obj%points( 1 : obj%tXi, Num )
  weight = obj%points( obj%tXi + 1, Num )
END PROCEDURE quad_GetQuadraturePoints1

!----------------------------------------------------------------------------
!                                                         getQuadraturepoints
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_GetQuadraturePoints2
  INTEGER( I4B ) :: n
  n = SIZE( obj%points, 2 ) !#column
  CALL Reallocate( point, 3, n )
  point( 1 : obj%tXi, 1:n ) = obj%points( 1 : obj%tXi, 1:n )
  weight = obj%points( obj%tXi + 1, 1:n )
END PROCEDURE quad_GetQuadraturePoints2

END SUBMODULE GetMethods
