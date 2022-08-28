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

SUBMODULE(PrismInterpolationUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     LagrangeDegree_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Prism

! TODO #164 Implement LagrangeDegree_Prism

END PROCEDURE LagrangeDegree_Prism

!----------------------------------------------------------------------------
!                                                        LagrangeDOF_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Prism
ans = (order + 1)**2 * (order + 2) / 2_I4B
END PROCEDURE LagrangeDOF_Prism

!----------------------------------------------------------------------------
!                                                    LagrangeInDOF_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Prism
ans = (order - 1)**2 * (order - 2) / 2_I4B
END PROCEDURE LagrangeInDOF_Prism

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Prism

! nodecoord( :, 1 ) = [0,0,-1]
! nodecoord( :, 2 ) = [1,0,-1]
! nodecoord( :, 3 ) = [0,1,-1]
! nodecoord( :, 4 ) = [0,0,1]
! nodecoord( :, 5 ) = [1,0,1]
! nodecoord( :, 6 ) = [0,1,1]
! TODO #160 Implement EquidistancePoint_Prism routine

END PROCEDURE EquidistancePoint_Prism

!----------------------------------------------------------------------------
!                                            EquidistanceInPoint_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Prism

! TODO Implement EquidistanceInPoint_Prism routine

END PROCEDURE EquidistanceInPoint_Prism

!----------------------------------------------------------------------------
!                                                  InterpolationPoint_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Prism
SELECT CASE (ipType)
CASE (Equidistance)
  nodecoord = EquidistancePoint_Prism(xij=xij, order=order)
CASE (GaussLegendre)
CASE (GaussLobatto)
CASE (Chebyshev)
END SELECT
END PROCEDURE InterpolationPoint_Prism

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
