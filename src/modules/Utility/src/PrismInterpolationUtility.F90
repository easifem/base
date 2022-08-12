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

MODULE PrismInterpolationUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                  InterpolationPoint_Prism
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION InterpolationPoint_Prism( order, ipType, xij ) &
    & RESULT( nodecoord )
    INTEGER( I4B ), INTENT( IN ) :: order
    INTEGER( I4B ), INTENT( IN ) :: ipType
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( :, : )
    REAL( DFP ), ALLOCATABLE :: nodecoord( :, : )
  END FUNCTION InterpolationPoint_Prism
END INTERFACE

PUBLIC :: InterpolationPoint_Prism

INTERFACE PrismInterpolationPoint
  MODULE PROCEDURE InterpolationPoint_Prism
END INTERFACE PrismInterpolationPoint

PUBLIC :: PrismInterpolationPoint

!----------------------------------------------------------------------------
!                                                     EquidistanceLIP_Prism
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION EquidistanceLIP_Prism( order, xij ) &
    & RESULT( nodecoord )
    INTEGER( I4B ), INTENT( IN ) :: order
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( :, : )
    REAL( DFP ), ALLOCATABLE :: nodecoord( :, : )
  END FUNCTION EquidistanceLIP_Prism
END INTERFACE

INTERFACE PrismLagrangeEquidistance
  MODULE PROCEDURE EquidistanceLIP_Prism
END INTERFACE PrismLagrangeEquidistance

PUBLIC :: PrismLagrangeEquidistance

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE PrismInterpolationUtility