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

PUBLIC :: InterpolationPoint_Hexahedron

!----------------------------------------------------------------------------
!                                            InterpolationPoint_Hexahedron
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION InterpolationPoint_Hexahedron( order, ipType, xij ) &
    & RESULT( nodecoord )
    !!
    INTEGER( I4B ), INTENT( IN ) :: order
    INTEGER( I4B ), INTENT( IN ) :: ipType
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( :, : )
    REAL( DFP ), ALLOCATABLE :: nodecoord( :, : )
    !!
  END FUNCTION InterpolationPoint_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                               EquidistanceLIP_Hexahedron
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION EquidistanceLIP_Hexahedron( order, xij ) &
    & RESULT( nodecoord )
    !!
    INTEGER( I4B ), INTENT( IN ) :: order
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( :, : )
    REAL( DFP ), ALLOCATABLE :: nodecoord( :, : )
    !!
  END FUNCTION EquidistanceLIP_Hexahedron
END INTERFACE

