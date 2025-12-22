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
! summary: This module contains methods for [[ReferencePyramid_]]

MODULE ReferencePyramid_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE
PUBLIC :: Initiate
PUBLIC :: ReferencePyramid
PUBLIC :: ReferencePyramid_Pointer
PUBLIC :: HighOrderElement_Pyramid
PUBLIC :: Measure_Simplex_Pyramid
PUBLIC :: Pyramid_Quality
PUBLIC :: Quality_Pyramid
PUBLIC :: GetEdgeConnectivity_Pyramid
PUBLIC :: GetFaceConnectivity_Pyramid
PUBLIC :: RefCoord_Pyramid
PUBLIC :: GetFaceElemType_Pyramid
PUBLIC :: FacetElements_Pyramid
PUBLIC :: ElementOrder_Pyramid
PUBLIC :: ElementType_Pyramid
PUBLIC :: TotalNodesInElement_Pyramid
PUBLIC :: TotalEntities_Pyramid
PUBLIC :: FacetTopology_Pyramid
PUBLIC :: ElementName_Pyramid
PUBLIC :: MaxOrder_Pyramid

#ifdef MAX_PYRAMID_ORDER
INTEGER(I4B), PARAMETER :: MaxOrder_Pyramid = MAX_PYRAMID_ORDER
#else
INTEGER(I4B), PARAMETER :: MaxOrder_Pyramid = 2_I4B
#endif

!----------------------------------------------------------------------------
!                                                               ElementName
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-25
! summary: Returns element name in character from element number/type

INTERFACE
  MODULE PURE FUNCTION ElementName_Pyramid(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION ElementName_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                             FacetTopology_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-23
! summary:  Returns the topology of tetrahedron

INTERFACE
  MODULE PURE SUBROUTINE FacetTopology_Pyramid(elemType, nptrs, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nptrs(:)
    TYPE(ReferenceTopology_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetTopology_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                 TotalEntities_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns total entities in Pyramid

INTERFACE
  MODULE PURE FUNCTION TotalEntities_Pyramid(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans(4)
  END FUNCTION TotalEntities_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                            TotalNodesInElement_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns total nodes in element

INTERFACE
  MODULE PURE FUNCTION TotalNodesInElement_Pyramid(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION TotalNodesInElement_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ElementType_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns the type of element from char name

INTERFACE
  MODULE PURE FUNCTION ElementType_Pyramid(elemName) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: elemName
    INTEGER(I4B) :: ans
  END FUNCTION ElementType_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ElementOrder_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns the order of element

INTERFACE
  MODULE PURE FUNCTION ElementOrder_Pyramid(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION ElementOrder_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                   FacetElements_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-21
! summary:  Get FacetElements

INTERFACE FacetElements_Pyramid
  MODULE SUBROUTINE FacetElements_Pyramid1(refelem, ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Pyramid1
END INTERFACE FacetElements_Pyramid

!----------------------------------------------------------------------------
!                                                  FacetElements_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-21
! summary:  Get FacetElements

INTERFACE FacetElements_Pyramid
  MODULE SUBROUTINE FacetElements_Pyramid2(elemType, nsd, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nsd
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Pyramid2
END INTERFACE FacetElements_Pyramid

!----------------------------------------------------------------------------
!                                                          Initiate@Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         1 March 2021
! summary: This subroutine for constructing the object

INTERFACE Initiate
  MODULE SUBROUTINE Initiate_Ref_Pyramid(obj, nsd, xij, domainName)
    CLASS(ReferencePyramid_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
  END SUBROUTINE Initiate_Ref_Pyramid
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                  ReferencePyramid@Pyramid
!----------------------------------------------------------------------------

INTERFACE ReferencePyramid
  MODULE FUNCTION Reference_Pyramid(nsd, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    TYPE(ReferencePyramid_) :: obj
  END FUNCTION Reference_Pyramid
END INTERFACE ReferencePyramid

!----------------------------------------------------------------------------
!                                          ReferencePyramid_Pointer@Pyramid
!----------------------------------------------------------------------------

INTERFACE ReferencePyramid_Pointer
  MODULE FUNCTION Reference_Pyramid_Pointer(nsd, xij, domainName) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    CLASS(ReferencePyramid_), POINTER :: obj
  END FUNCTION Reference_Pyramid_Pointer
END INTERFACE ReferencePyramid_Pointer

!----------------------------------------------------------------------------
!                                                    LagrangeElement@Pyramid
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE HighOrderElement_Pyramid(RefElem, Order, obj, ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: RefElem
    INTEGER(I4B), INTENT(IN) :: Order
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ipType
  END SUBROUTINE HighOrderElement_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                   MeasureSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex_Pyramid(RefElem, XiJ) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: RefElem
    REAL(DFP), INTENT(IN) :: XiJ(:, :)
    REAL(DFP) :: Ans
  END FUNCTION Measure_Simplex_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Pyramid_Quality
!----------------------------------------------------------------------------

INTERFACE Quality_Pyramid
  MODULE FUNCTION Pyramid_Quality(refelem, xij, measure) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: Ans
  END FUNCTION Pyramid_Quality
END INTERFACE Quality_Pyramid

!----------------------------------------------------------------------------
!                                                        GetEdgeConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-08
! summary:  Returns number of edges in the element

INTERFACE
  MODULE PURE SUBROUTINE GetEdgeConnectivity_Pyramid(con, opt, order, &
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
    !! Order of element
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! Number of rows written in con
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! Numbers of cols written in con
  END SUBROUTINE GetEdgeConnectivity_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetFaceConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-08
! summary:  Returns number of edges in the element

INTERFACE
  MODULE PURE SUBROUTINE GetFaceConnectivity_Pyramid(con, opt, order, &
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
    !! Order of element
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! Number of rows written in con
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! Numbers of cols written in con
  END SUBROUTINE GetFaceConnectivity_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                          RefCoord_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-09
! summary:  Reference Coordinates of pyramid

INTERFACE
  MODULE PURE FUNCTION RefCoord_Pyramid(refPyramid) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refPyramid
    REAL(DFP) :: ans(3, 5)
  END FUNCTION RefCoord_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                        GetFaceElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-11
! summary:  Returns the element type of each face

INTERFACE GetFaceElemType_Pyramid
  MODULE PURE SUBROUTINE GetFaceElemType_Pyramid1(elemType, faceElemType, &
                                                  tFaceNodes, opt)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !!  Element type
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: faceElemType(:)
    !! Face element type
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: tFaceNodes(:)
    !! total nodes in each face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt = 2, then edge connectivity for Lagrangian approximation
    !! opt = 1 is default
  END SUBROUTINE GetFaceElemType_Pyramid1
END INTERFACE GetFaceElemType_Pyramid

!----------------------------------------------------------------------------
!                                        GetFaceElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-11
! summary:  Returns the element type of each face

INTERFACE GetFaceElemType_Pyramid
  MODULE PURE SUBROUTINE GetFaceElemType_Pyramid2( &
    elemType, localFaceNumber, faceElemType, tFaceNodes, opt)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type for prism
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number
    INTEGER(I4B), INTENT(OUT) :: faceElemType
    !! Face element type
    INTEGER(I4B), INTENT(OUT) :: tFaceNodes
    !! total nodes in each face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt = 2, then edge connectivity for Lagrangian approximation
    !! opt = 1 is default
  END SUBROUTINE GetFaceElemType_Pyramid2
END INTERFACE GetFaceElemType_Pyramid

END MODULE ReferencePyramid_Method
