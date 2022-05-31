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
! summary: This submodule defines methods for [[ReferencePrism_]]

SUBMODULE(ReferencePrism_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_ref_Prism
END PROCEDURE Initiate_ref_Prism

!----------------------------------------------------------------------------
!                                                      ReferencePrism
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Prism
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( obj, NSD, XiJ )
  ELSE
    CALL Initiate( obj, NSD )
  END IF
END PROCEDURE reference_Prism

!----------------------------------------------------------------------------
!                                                     ReferencePrism_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Prism_Pointer
  ALLOCATE( obj )
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( obj, NSD, XiJ )
  ELSE
    CALL Initiate( obj, NSD )
  END IF
END PROCEDURE reference_Prism_Pointer

!----------------------------------------------------------------------------
!                                                             LagrangeElement
!----------------------------------------------------------------------------

MODULE PROCEDURE highOrderElement_Prism
END PROCEDURE highOrderElement_Prism

!-----------------------------------------------------------------------------
!                                                              MeasureSimplex
!-----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Prism
  INTEGER( I4B ) :: FM( 5, 7 ), Node0( 5, 4 ), Order0( 5 ), b, iFace

  FM = FacetMatrix(RefElem)
  DO iFace = 1, 5
    Order0( iFace ) = FM( iFace, 3 )
    b = Order0( iFace ) + 3
    Node0( iFace, 1:Order0( iFace ) ) = FM( iFace, 4 : b )
  END DO
  CALL POLYHEDRONVOLUME3D( coord = XiJ( 1:3, 1:6 ), &
    & order_max = 4, face_num = 5,  &
    & node = Node0, node_num = 6, &
    & order = Order0, &
    & volume = Ans )
END PROCEDURE Measure_Simplex_Prism

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Prism_quality
END PROCEDURE Prism_quality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "./modified_burkardt.inc"
END SUBMODULE Methods