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

SUBMODULE(PyramidInterpolationUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                LagrangeDegree_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Pyramid

! TODO #165 Implement LagrangeDegree_Pyramid

END PROCEDURE LagrangeDegree_Pyramid

!----------------------------------------------------------------------------
!                                                        LagrangeDOF_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Pyramid
  ans = (order+1)*(order+2)*(2*order+3)/6
END PROCEDURE LagrangeDOF_Pyramid

!----------------------------------------------------------------------------
!                                                    LagrangeInDOF_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Pyramid
  ans = (order-1)*(order-2)*(2*order-3)/6
END PROCEDURE LagrangeInDOF_Pyramid

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Pyramid

! TODO #161 Implement EquidistancePoint_Pyramid routine

END PROCEDURE EquidistancePoint_Pyramid

!----------------------------------------------------------------------------
!                                            EquidistanceInPoint_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Pyramid

! TODO #161 Implement EquidistanceInPoint_Pyramid routine

END PROCEDURE EquidistanceInPoint_Pyramid

!----------------------------------------------------------------------------
!                                                InterpolationPoint_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Pyramid
  SELECT CASE( ipType )
  CASE( Equidistance )
    nodecoord = EquidistanceLIP_Pyramid( xij=xij, order=order )
  CASE( GaussLegendre )
  CASE( GaussLobatto )
  CASE( Chebyshev )
  END SELECT
END PROCEDURE InterpolationPoint_Pyramid

!----------------------------------------------------------------------------
!                                                   EquidistanceLIP_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceLIP_Pyramid
END PROCEDURE EquidistanceLIP_Pyramid

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods