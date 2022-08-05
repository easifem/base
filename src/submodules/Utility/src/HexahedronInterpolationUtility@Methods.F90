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

SUBMODULE(HexahedronInterpolationUtility) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                            InterpolationPoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Hexahedron
  !!
  SELECT CASE( ipType )
  !!
  CASE( Equidistance )
    !!
    nodecoord = EquidistanceLIP_Hexahedron( xij=xij, order=order )
    !!
  CASE( GaussLegendre )
  CASE( GaussLobatto )
  CASE( Chebyshev )
  !!
  END SELECT
  !!
END PROCEDURE InterpolationPoint_Hexahedron

!----------------------------------------------------------------------------
!                                                EquidistanceLIP_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceLIP_Hexahedron
END PROCEDURE EquidistanceLIP_Hexahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods