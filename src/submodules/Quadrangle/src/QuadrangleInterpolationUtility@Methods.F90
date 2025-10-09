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

SUBMODULE(QuadrangleInterpolationUtility) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                   RefElemDomain_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE RefElemDomain_Quadrangle
ans = "BIUNIT"
END PROCEDURE RefElemDomain_Quadrangle

!----------------------------------------------------------------------------
!                                                       FacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetConnectivity_Quadrangle
ans(1:2, 1) = [1, 2]
ans(1:2, 2) = [2, 3]
ans(1:2, 3) = [3, 4]
ans(1:2, 4) = [4, 1]
END PROCEDURE FacetConnectivity_Quadrangle

!----------------------------------------------------------------------------
!                                               QuadraturePoint_Quadrangle3
!----------------------------------------------------------------------------

END SUBMODULE Methods
