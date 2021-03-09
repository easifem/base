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
! summary: This module contains method for [[ReferenceHexahedron_]]

SUBMODULE( ReferenceHexahedron_Method ) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_ref_Hexahedron
END PROCEDURE Initiate_ref_Hexahedron

!----------------------------------------------------------------------------
!                                                      ReferenceHexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Hexahedron
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END PROCEDURE reference_Hexahedron

!----------------------------------------------------------------------------
!                                                      ReferenceHexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Hexahedron_Pointer
  ALLOCATE( Obj )
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END PROCEDURE reference_Hexahedron_Pointer

!----------------------------------------------------------------------------
!                                                             LagrangeElement
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeElement_Hexahedron
END PROCEDURE LagrangeElement_Hexahedron

!----------------------------------------------------------------------------
!                                                              MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Hexahedron
  INTEGER( I4B ) :: Order0( 6 ), Node0( 6, 4 ), FM( 6, 7 ), iFace, b

  Order0 = [4, 4, 4, 4, 4, 4]
  FM = FacetMatrix( RefElem )
  DO iFace = 1, 6
    b = FM( iFace, 3 ) + 3
    Node0( iFace, 1:Order0( iFace ) ) = FM( iFace, 4 : b )
  END DO
  CALL POLYHEDRONVOLUME3D( coord = XiJ( 1:3, 1:8 ), &
    & order_max = 4, face_num = 6,  &
    & node = Node0, node_num = 8, &
    & order = Order0, &
    & volume = Ans )
END PROCEDURE Measure_Simplex_Hexahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Hexahedron_quality
END PROCEDURE Hexahedron_quality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "./modified_burkardt.inc"
END SUBMODULE Methods