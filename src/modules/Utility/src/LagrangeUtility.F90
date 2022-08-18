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

MODULE LagrangeUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                   LagrangeDOF@BasisMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the number of dof for lagrange polynomial
!
!# Introduction
!
! this routine returns the number of dof for lagrange polynomial

INTERFACE
MODULE PURE FUNCTION LagrangeDOF( order, elemType) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
    !! order
  INTEGER( I4B ), INTENT( IN ) :: elemType
  INTEGER( I4B ) :: ans
    !! number of degree of freedom
END FUNCTION LagrangeDOF
END INTERFACE

PUBLIC :: LagrangeDOF

!----------------------------------------------------------------------------
!                                                 LagrangeInDOF@BasisMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the number of dof for lagrange polynomial

INTERFACE
MODULE PURE FUNCTION LagrangeInDOF( order, elemType) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
    !! order
  INTEGER( I4B ), INTENT( IN ) :: elemType
  INTEGER( I4B ) :: ans
    !! number of degree of freedom
END FUNCTION LagrangeInDOF
END INTERFACE

PUBLIC :: LagrangeInDOF

!----------------------------------------------------------------------------
!                                                           LagrangeDegree
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the degrees of monomials for lagrange polynomial
!
!# Introduction
!
! this routine returns the degrees of monomials for lagrange polynomial on
! triangles and quadrilaterals.


INTERFACE
MODULE PURE FUNCTION LagrangeDegree( order, elemType ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  INTEGER( I4B ), INTENT( IN ) :: elemType
  INTEGER( I4B ), ALLOCATABLE :: ans( :, : )
END FUNCTION LagrangeDegree
END INTERFACE

PUBLIC :: LagrangeDegree

!----------------------------------------------------------------------------
!                                                       LagrangeVandermonde
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the degrees of monomials for lagrange polynomial
!
!# Introduction
!
! this routine returns the degrees of monomials for lagrange polynomial on
! triangles and quadrilaterals.

INTERFACE
MODULE PURE FUNCTION LagrangeVandermonde( x, order, elemType ) &
  & RESULT( ans )
  REAL( DFP ), INTENT( IN ) :: x( :, : )
  !!  points in $x_{iJ}$ format
  INTEGER( I4B ), INTENT( IN ) :: order
  !! order
  INTEGER( I4B ), INTENT( IN ) :: elemType
  !! element type
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
  !! vandermonde matrix nrows = number of points
  !! ncols = number of dof
END FUNCTION LagrangeVandermonde
END INTERFACE

PUBLIC :: LagrangeVandermonde

!----------------------------------------------------------------------------
!                                                          EquidistancePoint
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION EquidistancePoint( order, xij, elemType ) &
  & RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: elemType
  REAL( DFP ), ALLOCATABLE :: ans(:,:)
END FUNCTION EquidistancePoint
END INTERFACE

PUBLIC :: EquidistancePoint

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LagrangeUtility