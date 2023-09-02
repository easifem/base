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
! date:         5 March 2021
! summary: This module contains methods for [[ReferencePrism_]]

MODULE ReferencePrism_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

PUBLIC :: PolyhedronVolume3D
PUBLIC :: Initiate
PUBLIC :: ReferencePrism
PUBLIC :: ReferencePrism_Pointer
PUBLIC :: highOrderElement_Prism
PUBLIC :: Measure_Simplex_Prism
PUBLIC :: Prism_Quality

!----------------------------------------------------------------------------
!                                                       Initiate@Prism
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         1 March 2021
! summary: This subroutine for constructing the object
!
!### Usage
!
!```fortran
!
!```

INTERFACE Initiate
  MODULE PURE SUBROUTINE initiate_ref_Prism(obj, NSD, XiJ)
    CLASS(ReferencePrism_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: XiJ(:, :)
  END SUBROUTINE initiate_ref_Prism
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                            ReferencePrism@Prism
!----------------------------------------------------------------------------

INTERFACE ReferencePrism
  MODULE PURE FUNCTION reference_Prism(NSD, XiJ) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: XiJ(:, :)
    TYPE(ReferencePrism_) :: obj
  END FUNCTION reference_Prism
END INTERFACE ReferencePrism

!----------------------------------------------------------------------------
!                                   ReferencePrism_Pointer@Prism
!----------------------------------------------------------------------------

INTERFACE ReferencePrism_Pointer
  MODULE PURE FUNCTION reference_Prism_Pointer(NSD, XiJ) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: XiJ(:, :)
    CLASS(ReferencePrism_), POINTER :: obj
  END FUNCTION reference_Prism_Pointer
END INTERFACE ReferencePrism_Pointer

!----------------------------------------------------------------------------
!                                               LagrangeElement@Prism
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE highOrderElement_Prism(RefElem, Order, obj, ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: RefElem
    INTEGER(I4B), INTENT(IN) :: Order
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ipType
  END SUBROUTINE highOrderElement_Prism
END INTERFACE

!----------------------------------------------------------------------------
!                                                  MeasureSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex_Prism(RefElem, XiJ) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: RefElem
    REAL(DFP), INTENT(IN) :: XiJ(:, :)
    REAL(DFP) :: Ans
  END FUNCTION Measure_Simplex_Prism
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Prism_Quality
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION Prism_Quality(refelem, xij, measure) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: Ans
  END FUNCTION Prism_Quality
END INTERFACE

!----------------------------------------------------------------------------
!                                                     POLYHEDRONVOLUME3D
!----------------------------------------------------------------------------

!> author: John Burkardt, Vikas Sharma
! date:  2023-08-08
! summary: computes the volume of a polyhedron in 3D.
!
!  Licensing:
!    This code is distributed under the GNU LGPL license.
!  Modified:
!    19 August 2003
!  Author:
!    John Burkardt
!  Parameters:
!
!    Input, real ( kind = 8 ) COORD(3,NODE_NUM), the coordinates of
!    the vertices.  The vertices may be listed in any order.
!
!    Input, integer ( kind = 4 ) ORDER_MAX, the maximum number of vertices
!    that make up a face of the polyhedron.
!
!    Input, integer ( kind = 4 ) FACE_NUM, the number of faces of the
!    polyhedron.
!
!    Input, integer ( kind = 4 ) NODE(FACE_NUM,ORDER_MAX).  Face I is
! defined by
!    the vertices NODE(I,1) through NODE(I,ORDER(I)).  These vertices
!    are listed in neighboring order.
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of points stored in
! COORD.
!
!    Input, integer ( kind = 4 ) ORDER(FACE_NUM), the number of vertices
! making
!    up each face.
!
!    Output, real ( kind = 8 ) VOLUME, the volume of the polyhedron.

INTERFACE
  MODULE PURE SUBROUTINE PolyhedronVolume3D(  &
     & coord, order_max, face_num, node, &
     & node_num, order, ans)
    INTEGER(I4B), INTENT(IN) :: order_max
    INTEGER(I4B), INTENT(IN) :: face_num
    INTEGER(I4B), INTENT(IN) :: node(face_num, order_max)
    INTEGER(I4B), INTENT(IN) :: node_num
    REAL(DFP), INTENT(IN) :: coord(3, node_num)
    INTEGER(I4B), INTENT(IN) :: order(face_num)
    REAL(DFP), INTENT(OUT) :: ans
  END SUBROUTINE PolyhedronVolume3D
END INTERFACE

END MODULE ReferencePrism_Method
