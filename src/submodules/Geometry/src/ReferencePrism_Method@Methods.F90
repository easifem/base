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
IF (PRESENT(XiJ)) THEN
  CALL Initiate(obj, NSD, XiJ)
ELSE
  CALL Initiate(obj, NSD)
END IF
END PROCEDURE reference_Prism

!----------------------------------------------------------------------------
!                                                     ReferencePrism_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Prism_Pointer
ALLOCATE (obj)
IF (PRESENT(XiJ)) THEN
  CALL Initiate(obj, NSD, XiJ)
ELSE
  CALL Initiate(obj, NSD)
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
INTEGER(I4B) :: fm(5, 7), node0(5, 4), order0(5), b, iface

fm = FacetMatrix(refelem)
DO iface = 1, 5
  order0(iface) = fm(iface, 3)
  b = order0(iface) + 3
  node0(iface, 1:order0(iface)) = fm(iface, 4:b)
END DO
CALL POLYHEDRONVOLUME3D(coord=XiJ(1:3, 1:6), &
  & order_max=4, face_num=5,  &
  & node=node0, node_num=6, &
  & order=order0, &
  & ans=ans)
END PROCEDURE Measure_Simplex_Prism

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Prism_quality
END PROCEDURE Prism_quality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE PolyhedronVolume3D
INTEGER(I4B), PARAMETER :: dim_num = 3
INTEGER(I4B) :: iFace
INTEGER(I4B) :: n1
INTEGER(I4B) :: n2
INTEGER(I4B) :: n3
INTEGER(I4B) :: v

ans = 0.0_DFP
! Triangulate each iFace.
DO iface = 1, face_num
  n3 = node(iface, order(iface))
  DO v = 1, order(iface) - 2
    n1 = node(iface, v)
    n2 = node(iface, v + 1)
    ans = ans &
          + coord(1, n1) &
          * (coord(2, n2) * coord(3, n3) - coord(2, n3) * coord(3, n2)) &
          + coord(1, n2) &
          * (coord(2, n3) * coord(3, n1) - coord(2, n1) * coord(3, n3)) &
          + coord(1, n3) &
          * (coord(2, n1) * coord(3, n2) - coord(2, n2) * coord(3, n1))
  END DO
END DO
ans = ans / 6.0_DFP
END PROCEDURE PolyhedronVolume3D

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
