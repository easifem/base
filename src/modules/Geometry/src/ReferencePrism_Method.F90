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
PUBLIC :: HighOrderElement_Prism
PUBLIC :: Measure_Simplex_Prism
PUBLIC :: Prism_Quality
PUBLIC :: Quality_Prism
PUBLIC :: GetEdgeConnectivity_Prism
PUBLIC :: GetFaceConnectivity_Prism
PUBLIC :: RefCoord_Prism
PUBLIC :: GetFaceElemType_Prism
PUBLIC :: FacetElements_Prism
PUBLIC :: ElementOrder_Prism
PUBLIC :: ElementType_Prism
PUBLIC :: TotalNodesInElement_Prism
PUBLIC :: TotalEntities_Prism
PUBLIC :: FacetTopology_Prism
PUBLIC :: ElementName_Prism
PUBLIC :: MaxOrder_Prism

#ifdef MAX_PRISM_ORDER
INTEGER(I4B), PARAMETER :: MaxOrder_Prism = MAX_PRISM_ORDER
#else
INTEGER(I4B), PARAMETER :: MaxOrder_Prism = 2_I4B
#endif

!----------------------------------------------------------------------------
!                                                               ElementName
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-25
! summary: Returns element name in character from element number/type

INTERFACE
  MODULE PURE FUNCTION ElementName_Prism(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION ElementName_Prism
END INTERFACE

!----------------------------------------------------------------------------
!                                                       FacetTopology_Prism
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-23
! summary:  Returns the topology of tetrahedron

INTERFACE
  MODULE PURE SUBROUTINE FacetTopology_Prism(elemType, nptrs, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nptrs(:)
    TYPE(ReferenceTopology_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetTopology_Prism
END INTERFACE

!----------------------------------------------------------------------------
!                                                 TotalEntities_Prism
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns total entities in Prism

INTERFACE
  MODULE PURE FUNCTION TotalEntities_Prism(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans(4)
  END FUNCTION TotalEntities_Prism
END INTERFACE

!----------------------------------------------------------------------------
!                                            TotalNodesInElement_Prism
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns total nodes in element

INTERFACE
  MODULE PURE FUNCTION TotalNodesInElement_Prism(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION TotalNodesInElement_Prism
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ElementType_Prism
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns the type of element from char name

INTERFACE
  MODULE PURE FUNCTION ElementType_Prism(elemName) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: elemName
    INTEGER(I4B) :: ans
  END FUNCTION ElementType_Prism
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ElementOrder_Prism
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns the order of element

INTERFACE
  MODULE PURE FUNCTION ElementOrder_Prism(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION ElementOrder_Prism
END INTERFACE

!----------------------------------------------------------------------------
!                                                   FacetElements_Prism
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-21
! summary:  Get FacetElements

INTERFACE FacetElements_Prism
  MODULE SUBROUTINE FacetElements_Prism1(refelem, ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Prism1
END INTERFACE FacetElements_Prism

!----------------------------------------------------------------------------
!                                                  FacetElements_Prism
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-21
! summary:  Get FacetElements

INTERFACE FacetElements_Prism
  MODULE SUBROUTINE FacetElements_Prism2(elemType, nsd, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nsd
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Prism2
END INTERFACE FacetElements_Prism

!----------------------------------------------------------------------------
!                                                       Initiate@Prism
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         1 March 2021
! summary: This subroutine for constructing the object

INTERFACE Initiate
  MODULE SUBROUTINE Initiate_Ref_Prism(obj, nsd, xij, domainName)
    CLASS(ReferencePrism_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
  END SUBROUTINE Initiate_Ref_Prism
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                            ReferencePrism@Prism
!----------------------------------------------------------------------------

INTERFACE ReferencePrism
  MODULE FUNCTION Reference_Prism(nsd, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    TYPE(ReferencePrism_) :: obj
  END FUNCTION Reference_Prism
END INTERFACE ReferencePrism

!----------------------------------------------------------------------------
!                                   ReferencePrism_Pointer@Prism
!----------------------------------------------------------------------------

INTERFACE ReferencePrism_Pointer
  MODULE FUNCTION Reference_Prism_Pointer(nsd, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    CLASS(ReferencePrism_), POINTER :: obj
  END FUNCTION Reference_Prism_Pointer
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

INTERFACE Quality_Prism
  MODULE FUNCTION Prism_Quality(refelem, xij, measure) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: Ans
  END FUNCTION Prism_Quality
END INTERFACE Quality_Prism

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

!----------------------------------------------------------------------------
!                                                        GetEdgeConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-08
! summary:  Returns number of edges in the element

INTERFACE
  MODULE PURE SUBROUTINE GetEdgeConnectivity_Prism(con, opt, order, &
                                                   nrow, ncol)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! Connectivity
    !! The columns represents the edge number
    !! The row represents a edge
    !! con should be allocated by the user
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt =2, then edge connectivity for Lagrangian approximation
    !! opt=1 is default
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! Number of rows written in con
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! Numbers of cols written in con
  END SUBROUTINE GetEdgeConnectivity_Prism
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetFaceConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-08
! summary:  Returns number of edges in the element

INTERFACE
  MODULE PURE SUBROUTINE GetFaceConnectivity_Prism(con, opt, order, &
                                                   nrow, ncol)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! Connectivity
    !! The columns represents the face number
    !! The row represents a face
    !! con should be allocated by the user
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then face connectivity for hierarchial approximation
    !! If opt =2, then face connectivity for Lagrangian approximation
    !! opt=1 is default
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! Number of rows written in con
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! Numbers of cols written in con
  END SUBROUTINE GetFaceConnectivity_Prism
END INTERFACE

!----------------------------------------------------------------------------
!                                                            RefCoord_Prism
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Reference Coordinates of prism

INTERFACE
  MODULE PURE FUNCTION RefCoord_Prism(refPrism) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refPrism
    REAL(DFP) :: ans(3, 6)
  END FUNCTION RefCoord_Prism
END INTERFACE

!----------------------------------------------------------------------------
!                                        GetFaceElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-11
! summary:  Returns the element type of each face

INTERFACE
  MODULE PURE SUBROUTINE GetFaceElemType_Prism(faceElemType, opt,  &
    & tFaceNodes, elemType)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: faceElemType(:)
    !! Face element type
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: tFaceNodes(:)
    !! total nodes in each face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt = 2, then edge connectivity for Lagrangian approximation
    !! opt = 1 is default
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! elemType for prism
    !! default is Prism
  END SUBROUTINE GetFaceElemType_Prism
END INTERFACE

END MODULE ReferencePrism_Method
