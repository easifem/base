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


MODULE LineInterpolationUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                       LagrangeDegree_Line
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION LagrangeDegree_Line( order ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  INTEGER( I4B ), ALLOCATABLE :: ans(:,:)
END FUNCTION LagrangeDegree_Line
END INTERFACE

PUBLIC :: LagrangeDegree_Line

!----------------------------------------------------------------------------
!                                                          LagrangeDOF_Point
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: 	Returns the total number of degree of freedom for a
! lagrange polynomial on a point of Line

INTERFACE
MODULE PURE FUNCTION LagrangeDOF_Point( order ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  INTEGER( I4B ) :: ans
END FUNCTION LagrangeDOF_Point
END INTERFACE

PUBLIC :: LagrangeDOF_Point

!----------------------------------------------------------------------------
!                                                              GetDOF_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: 	Returns the total number of degree of freedom for a
! lagrange polynomial on Line

INTERFACE
MODULE PURE FUNCTION LagrangeDOF_Line( order ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  INTEGER( I4B ) :: ans
END FUNCTION LagrangeDOF_Line
END INTERFACE

PUBLIC :: LagrangeDOF_Line

!----------------------------------------------------------------------------
!                                                        LagrangeInDOF_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: 	Returns the total number of degree of freedom for a
! lagrange polynomial on an edge of a Line
!
!# Introduction
!
!- Returns the total number of degree of freedom for a
! lagrange polynomial on an edge of a Line
!- These dof are strictly inside the line

INTERFACE
MODULE PURE FUNCTION LagrangeInDOF_Line( order ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  INTEGER( I4B ) :: ans
END FUNCTION LagrangeInDOF_Line
END INTERFACE

PUBLIC :: LagrangeInDOF_Line

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: 	Returns equidistance points on edge
!
!# Introduction
!
!- This function returns the equidistance points on edge
!- All points are inside the interval

INTERFACE
MODULE PURE FUNCTION EquidistanceInPoint_Line1( order, xij ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  !! order
  REAL( DFP ), INTENT( IN ) :: xij(2)
  !! coordinates of point 1 and point 2
  REAL( DFP ), ALLOCATABLE :: ans( : )
END FUNCTION EquidistanceInPoint_Line1
END INTERFACE

INTERFACE EquidistanceInPoint_Line
  MODULE PROCEDURE EquidistanceInPoint_Line1
END INTERFACE EquidistanceInPoint_Line

PUBLIC :: EquidistanceInPoint_Line

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: 	Returns equidistance points on edge
!
!# Introduction
!
!- This function returns the equidistance points on edge
!- All points are inside the interval

INTERFACE
MODULE PURE FUNCTION EquidistanceInPoint_Line2( order, xij ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  !! order
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij(:,:)
  !! coordinates of point 1 and point 2 in $x_{iJ}$ format
  !! number of rows = nsd
  !! number of cols = 2
  REAL( DFP ), ALLOCATABLE :: ans(:,:)
  !! returned coordinates in $x_{iJ}$ format
END FUNCTION EquidistanceInPoint_Line2
END INTERFACE

INTERFACE EquidistanceInPoint_Line
  MODULE PROCEDURE EquidistanceInPoint_Line2
END INTERFACE EquidistanceInPoint_Line

!----------------------------------------------------------------------------
!                                                    EquidistancePoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: Returns equidistance points on edge
!
!# Introduction
!
!- This function returns the equidistance points on edge
!- All points are inside the interval

INTERFACE
  MODULE PURE FUNCTION EquidistancePoint_Line1( order, xij ) &
    & RESULT( ans )
    INTEGER( I4B ), INTENT( IN ) :: order
    !! order
    REAL( DFP ), INTENT( IN ) :: xij(2)
    !! coorindates of point 1 and point 2
    REAL( DFP ), ALLOCATABLE :: ans(:)
    !! equidistance points
  END FUNCTION EquidistancePoint_Line1
END INTERFACE

INTERFACE EquidistancePoint_Line
  MODULE PROCEDURE EquidistancePoint_Line1
END INTERFACE EquidistancePoint_Line

PUBLIC :: EquidistancePoint_Line

!----------------------------------------------------------------------------
!                                                    EquidistancePoint_Line
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION EquidistancePoint_Line2( order, xij ) &
    & RESULT( ans )
    INTEGER( I4B ), INTENT( IN ) :: order
    !! order
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij(:,:)
    !! coordinates of point 1 and point 2 in $x_{iJ}$ format
    !! number of rows = nsd
    !! number of cols = 2
    REAL( DFP ), ALLOCATABLE :: ans(:,:)
    !! equidistance points in $x_{iJ}$ format
  END FUNCTION EquidistancePoint_Line2
END INTERFACE

INTERFACE EquidistancePoint_Line
  MODULE PROCEDURE EquidistancePoint_Line2
END INTERFACE EquidistancePoint_Line

!----------------------------------------------------------------------------
!                                                   InterpolationPoint_Line
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION InterpolationPoint_Line( order, ipType, xij ) &
    & RESULT( ans )
    !!
    INTEGER( I4B ), INTENT( IN ) :: order
    INTEGER( I4B ), INTENT( IN ) :: ipType
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( 3, 2 )
    REAL( DFP ), ALLOCATABLE :: ans( :, : )
    !!
  END FUNCTION InterpolationPoint_Line
END INTERFACE
=======
>>>>>>> dev

INTERFACE LineLagrangeEquidistance
  MODULE PROCEDURE EquidistanceLIP_Line
END INTERFACE LineLagrangeEquidistance

PUBLIC :: LineLagrangeEquidistance

PUBLIC :: InterpolationPoint_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LineInterpolationUtility