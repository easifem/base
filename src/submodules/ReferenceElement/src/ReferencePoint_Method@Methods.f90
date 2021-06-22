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
! summary: 	This submodule contains methods for [[ReferencePoint_]]

SUBMODULE( ReferencePoint_Method ) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE refPoint_Initiate
  INTEGER( I4B ) :: s( 2 )

  CALL Reallocate( Obj%XiJ, 3, 1 )
  IF( PRESENT( XiJ ) ) THEN
    Obj%XiJ = XiJ
  END IF
  Obj%EntityCounts = [1, 0, 0, 0]
  Obj%XiDimension = 0
  Obj%Order = 0
  Obj%NSD = NSD
  Obj%Name = Point1
  IF( ALLOCATED( Obj%Topology ) ) DEALLOCATE( Obj%Topology )
  ALLOCATE( Obj%Topology( 1 ) )
  Obj%Topology( 1 ) = ReferenceTopology( [1], Point )
  Obj%LagrangeElement => LagrangeElement_Point
END PROCEDURE refPoint_Initiate

!----------------------------------------------------------------------------
!                                                              ReferencePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE refPoint_Constructor1
  CALL Initiate( Obj, NSD, XiJ )
END PROCEDURE refPoint_Constructor1

!----------------------------------------------------------------------------
!                                                      ReferencePoint_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE refPoint_Constructor_1
  ALLOCATE( Obj )
  CALL Initiate( Obj, NSD, XiJ )
END PROCEDURE refPoint_Constructor_1

!----------------------------------------------------------------------------
!                                                            LagrangeElement
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeElement_Point
  CALL Initiate( obj=obj, anotherObj=refelem )
END PROCEDURE LagrangeElement_Point

!----------------------------------------------------------------------------
!                                                              MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Point
  Ans = 0.0_DFP
END PROCEDURE Measure_Simplex_Point

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Point_Quality
  ans = 0.0_DFP
END PROCEDURE Point_Quality

END SUBMODULE Methods