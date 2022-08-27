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
!                                                LagrangeDegree_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Hexahedron

! TODO #163 Implement LagrangeDegree_Hexahedron

END PROCEDURE LagrangeDegree_Hexahedron

!----------------------------------------------------------------------------
!                                                    LagrangeDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Hexahedron
ans = (order + 1)**3
END PROCEDURE LagrangeDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                   LagrangeInDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Hexahedron
ans = (order - 1)**3
END PROCEDURE LagrangeInDOF_Hexahedron

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Hexahedron
! ALLOCATE (nodecoord(3, 8))
! nodecoord(:, 1) = [-1, -1, -1]
! nodecoord(:, 2) = [1, -1, -1]
! nodecoord(:, 3) = [1, 1, -1]
! nodecoord(:, 4) = [-1, 1, -1]
! nodecoord(:, 5) = [-1, -1, 1]
! nodecoord(:, 6) = [1, -1, 1]
! nodecoord(:, 7) = [1, 1, 1]
! nodecoord(:, 8) = [-1, 1, 1]
! TODO #159 Implement EquidistancePoint_Hexahedron routine
END PROCEDURE EquidistancePoint_Hexahedron

!----------------------------------------------------------------------------
!                                            EquidistanceInPoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Hexahedron

! TODO #159 Implement EquidistanceInPoint_Hexahedron routine

END PROCEDURE EquidistanceInPoint_Hexahedron

!----------------------------------------------------------------------------
!                                            InterpolationPoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Hexahedron
SELECT CASE (ipType)
CASE (Equidistance)
  nodecoord = EquidistancePoint_Hexahedron(xij=xij, order=order)
CASE (GaussLegendre)
CASE (GaussLobatto)
CASE (Chebyshev)
END SELECT
END PROCEDURE InterpolationPoint_Hexahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
