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

SUBMODULE(ReferenceLine_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_ref_Line
  INTEGER( I4B ) :: s( 2 )
  !!
  IF( PRESENT( XiJ ) ) THEN
    CALL Reallocate( obj%XiJ, 3, 2 )
    s = SHAPE( XiJ )
    obj%XiJ( 1:s(1), 1:s(2) ) = XiJ(:,:)
  ELSE
    obj%XiJ = RESHAPE( &
      & [-1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP], &
      & [3, 2] )
  END IF
  !!
  obj%EntityCounts = [2, 1, 0, 0]
  obj%XiDimension = 1
  obj%Order = 1
  obj%NSD = NSD
  obj%Name = Line2
  !!
  IF( ALLOCATED( obj%Topology ) ) DEALLOCATE( obj%Topology )
  ALLOCATE( obj%Topology( 3 ) )
  obj%Topology( 1 ) = ReferenceTopology( [1], Point )
  obj%Topology( 2 ) = ReferenceTopology( [2], Point )
  obj%Topology( 3 ) = ReferenceTopology( [1, 2], Line2 )
  obj%LagrangeElement => LagrangeElement_Line
  !!
END PROCEDURE initiate_ref_Line

!----------------------------------------------------------------------------
!                                                              ReferenceLine
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Line
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( obj, NSD, XiJ )
  ELSE
    CALL Initiate( obj, NSD )
  END IF
END PROCEDURE reference_Line

!----------------------------------------------------------------------------
!                                                     ReferenceLine_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Line_Pointer_1
  ALLOCATE( obj )
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( obj, NSD, XiJ )
  ELSE
    CALL Initiate( obj, NSD )
  END IF
END PROCEDURE reference_Line_Pointer_1

!----------------------------------------------------------------------------
!                                                             LagrangeElement
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeElement_Line
  ! Define internal variables
  INTEGER( I4B ) :: nns, i
  obj%XiJ = EquidistanceLIP_Line( XiJ = RefElem%XiJ, Order = Order)
  obj%NSD = RefElem%NSD
  nns = SIZE( obj%XiJ, 2 )
  obj%EntityCounts = [nns, 1, 0, 0]
  obj%XiDimension = 1
  obj%Order = Order
  obj%Name = ElementType( "Line" // TRIM( INT2STR( nns ) ) )
  ALLOCATE( obj%Topology( nns + 1 ) )
  DO CONCURRENT (i=1:nns)
    obj%Topology( i ) = ReferenceTopology( [i], Point )
  END DO
  obj%Topology( nns + 1 ) = ReferenceTopology( [(i, i=1,nns)], obj%Name )
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