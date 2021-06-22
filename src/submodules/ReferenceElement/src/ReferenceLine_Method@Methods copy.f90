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

!> authors: Vikas Sharma, Ph. D.
! date: 2 March 2021
! summary: 	This submodule contains methods for [[ReferenceLine_]]

SUBMODULE( ReferenceLine_Method ) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_ref_Line
  INTEGER( I4B ) :: s( 2 )
  IF( PRESENT( XiJ ) ) THEN
    CALL Reallocate( Obj%XiJ, 3, 2 )
    s = SHAPE( XiJ )
    Obj%XiJ( 1:s(1), 1:s(2) ) = XiJ(:,:)
  ELSE
    Obj%XiJ = RESHAPE( [-1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP], [3, 2] )
  END IF
  Obj%EntityCounts = [2, 1, 0, 0]
  Obj%XiDimension = 1
  Obj%Order = 1
  Obj%NSD = NSD
  Obj%Name = Line2
  IF( ALLOCATED( Obj%Topology ) ) DEALLOCATE( Obj%Topology )
  ALLOCATE( Obj%Topology( 3 ) )
  Obj%Topology( 1 ) = ReferenceTopology( [1], Point )
  Obj%Topology( 2 ) = ReferenceTopology( [2], Point )
  Obj%Topology( 3 ) = ReferenceTopology( [1, 2], Line2 )
  Obj%LagrangeElement => LagrangeElement_Line
END PROCEDURE initiate_ref_Line

!----------------------------------------------------------------------------
!                                                              ReferenceLine
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Line
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END PROCEDURE reference_Line

!----------------------------------------------------------------------------
!                                                     ReferenceLine_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Line_Pointer_1
  ALLOCATE( Obj )
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END PROCEDURE reference_Line_Pointer_1

!----------------------------------------------------------------------------
!                                                        LagrangePoints@Line
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: This function returns equidistance lagrange points on a line
!
!### Introduction
! * Returns equidistant points on [-1,1] for lagrange interpolation
!	* Nodecoord is a 2D array with 3 rows
!	* First row is xi, second row is eta, third row is zeta

PURE FUNCTION EquidistanceLIP_Line( XiJ, Order ) RESULT( NodeCoord )
  REAL( DFP ), INTENT( IN ) :: XiJ( 3, 2 )
  INTEGER( I4B ), INTENT( IN ) :: Order
  REAL( DFP ) :: NodeCoord( 3, Order+1 )

  ! Define internal variables
  INTEGER( I4B ) :: i,j
  REAL( DFP ) :: Xi( Order + 1 )

  SELECT CASE( Order )
    CASE( 1 )
    Xi = [-1.0_DFP, 1.0_DFP]
    CASE( 2 )
    Xi = [-1.0_DFP, 1.0_DFP, 0.0_DFP]

    CASE( 3 )
    Xi = [ -1.0_DFP, 1.0_DFP, -0.33333333333333333333_DFP, &
      & 0.33333333333333333333_DFP]

    CASE( 4 )
    Xi = [ -1.0_DFP, 1.0_DFP, -0.5_DFP, 0.0_DFP, 0.5_DFP ]

    CASE( 5 )
    Xi = [ -1.0, 1.0, -0.6, -0.2, 0.2, 0.6 ]

    CASE( 6 )
    Xi = [ -1.0_DFP, 1.0_DFP, -0.66666666666666666667_DFP, &
      & -0.33333333333333333333_DFP, 0.0_DFP, 0.33333333333333333333_DFP, &
      & 0.66666666666666666667_DFP ]

    CASE( 7 )
    Xi = [ -1.0_DFP, 1.0_DFP, -0.71428571428571428571_DFP, &
      & -0.42857142857142857143_DFP, &
      & -0.14285714285714285714_DFP, &
      & 0.14285714285714285714_DFP, &
      & 0.42857142857142857143_DFP, &
      & 0.71428571428571428571_DFP ]

    CASE( 8 )
    Xi = [-1.0_DFP, 1.0_DFP, -0.75_DFP, -0.5_DFP, &
      & -0.25_DFP, 0.0_DFP, 0.25_DFP, 0.5_DFP, 0.75_DFP]

    CASE( 9 )
    Xi = [-1.0_DFP, 1.0_DFP, &
      & -0.77777777777777777778_DFP, &
      & -0.55555555555555555556_DFP, &
      & -0.33333333333333333333_DFP, &
      & -0.11111111111111111111_DFP, &
      & 0.11111111111111111111_DFP, &
      & 0.33333333333333333333_DFP, &
      & 0.55555555555555555556_DFP, &
      & 0.77777777777777777778_DFP ]

    CASE( 10 )
    Xi = [ -1.0_DFP, &
      & 1.0_DFP, &
      & -0.8_DFP, &
      & -0.6_DFP, &
      & -0.4_DFP, &
      & -0.2_DFP, &
      & 0.0_DFP, &
      & 0.2_DFP, &
      & 0.4_DFP, &
      & 0.6_DFP, &
      & 0.8_DFP ]

    CASE DEFAULT
    Xi( 1 ) = -1.0_DFP
    Xi( 2 ) = 1.0_DFP
    DO CONCURRENT (i = 3:Order + 1)
      Xi( i ) = ( 2.0_DFP * REAL( i, DFP ) &
        & - REAL( Order, DFP )  - 4.0_DFP ) / ( REAL( Order, DFP )  )
    END DO
  END SELECT

  DO CONCURRENT( i=1:3, j=1:Order+1)
  NodeCoord( i, j ) = 0.5_DFP * ( XiJ( i, 1 ) + XiJ( i, 2 ) ) &
    & +  0.5_DFP * ( XiJ( i, 2 ) - XiJ( i, 1 ) ) * Xi( j )
  END DO
END FUNCTION EquidistanceLIP_Line

!----------------------------------------------------------------------------
!                                                             LagrangeElement
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeElement_Line
  ! Define internal variables
  INTEGER( I4B ) :: nns, i
  Obj%XiJ = EquidistanceLIP_Line( XiJ = RefElem%XiJ, Order = Order)
  Obj%NSD = RefElem%NSD
  nns = SIZE( Obj%XiJ, 2 )
  Obj%EntityCounts = [nns, 1, 0, 0]
  Obj%XiDimension = 1
  Obj%Order = Order
  Obj%Name = ElementType( "Line" // TRIM( INT2STR( nns ) ) )
  ALLOCATE( Obj%Topology( nns + 1 ) )
  DO CONCURRENT (i=1:nns)
    Obj%Topology( i ) = ReferenceTopology( [i], Point )
  END DO
  Obj%Topology( nns + 1 ) = ReferenceTopology( [(i, i=1,nns)], Obj%Name )
END PROCEDURE LagrangeElement_Line

!----------------------------------------------------------------------------
!                                                              MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Line
  Ans = SQRT( ( XiJ( 1, 1 ) -  XiJ( 1, 2 ) ) ** 2 &
    & + ( XiJ( 2, 1 ) -  XiJ( 2, 2 ) ) ** 2 &
    & + ( XiJ( 3, 1 ) -  XiJ( 3, 2 ) ) ** 2 )
END PROCEDURE Measure_Simplex_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Line_quality
  ans = 0.0_DFP
END PROCEDURE Line_quality

END SUBMODULE Methods