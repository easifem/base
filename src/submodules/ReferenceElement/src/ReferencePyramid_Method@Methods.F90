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

!> author: Vikas Sharma, Ph. D.
! date: 2 March 2021
! summary: 	This submodule contians methods for [[ReferencePyramid_]]

SUBMODULE(ReferencePyramid_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_ref_Pyramid
END PROCEDURE Initiate_ref_Pyramid

!----------------------------------------------------------------------------
!                                                      ReferencePyramid
!----------------------------------------------------------------------------
MODULE PROCEDURE reference_Pyramid
  CALL Initiate( obj, NSD, XiJ )
END PROCEDURE reference_Pyramid

!----------------------------------------------------------------------------
!                                                      ReferencePyramid
!----------------------------------------------------------------------------
MODULE PROCEDURE reference_Pyramid_Pointer
  ALLOCATE( obj )
  CALL Initiate( obj, NSD, XiJ )
END PROCEDURE reference_Pyramid_Pointer

!----------------------------------------------------------------------------
!                                                             LagrangeElement
!----------------------------------------------------------------------------

MODULE PROCEDURE highOrderElement_Pyramid
END PROCEDURE highOrderElement_Pyramid

!-----------------------------------------------------------------------------
!                                                              MeasureSimplex
!-----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Pyramid
  !!
  INTEGER( I4B ) :: FM( 5, 7 ), Node0( 5, 4 ), Order0( 5 ), iFace, b
  !!
  FM = FacetMatrix(RefElem)
  !!
  DO iFace = 1, 5
    Order0( iFace ) = FM( iFace, 3 )
    b = Order0( iFace ) + 3
    Node0( iFace, 1:Order0( iFace ) ) = FM( iFace, 4 : b )
  END DO
  !!
  CALL POLYHEDRONVOLUME3D( coord = XiJ( 1:3, 1:5 ), &
    & order_max = 4, face_num = 5,  &
    & node = Node0, node_num = 5, &
    & order = Order0, &
    & volume = Ans )
  !!
END PROCEDURE Measure_Simplex_Pyramid

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Pyramid_quality
END PROCEDURE Pyramid_quality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "./modified_burkardt.inc"

END SUBMODULE Methods