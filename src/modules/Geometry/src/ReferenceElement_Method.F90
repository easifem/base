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
! date: 1 March 2021
! summary: This submodule contains method for [[ReferenceElement_]]

MODULE ReferenceElement_Method
USE BaseType
USE String_Class, ONLY: String
USE GlobalData

IMPLICIT NONE

PRIVATE

PUBLIC :: Display
PUBLIC :: MdEncode
PUBLIC :: ReactEncode
PUBLIC :: ReferenceTopology
PUBLIC :: DEALLOCATE
PUBLIC :: OPERATOR(.NNE.)
PUBLIC :: Initiate
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: ReferenceElement_Pointer
PUBLIC :: GetConnectivity
PUBLIC :: ElementType
PUBLIC :: ElementName
PUBLIC :: TotalNodesInElement
PUBLIC :: ElementOrder
PUBLIC :: OPERATOR(.order.)
PUBLIC :: XiDimension
PUBLIC :: IsVolume
PUBLIC :: IsSurface
PUBLIC :: IsLine
PUBLIC :: IsPoint
PUBLIC :: IsTriangle
PUBLIC :: IsQuadrangle
PUBLIC :: IsTetrahedron
PUBLIC :: IsHexahedron
PUBLIC :: IsPrism
PUBLIC :: IsPyramid
PUBLIC :: IsSerendipityElement
PUBLIC :: ElementTopology
PUBLIC :: OPERATOR(.topology.)
PUBLIC :: FacetMatrix
PUBLIC :: GetFacetElements
PUBLIC :: LocalNodeCoord
PUBLIC :: MeasureSimplex
PUBLIC :: ElementQuality
PUBLIC :: ContainsPoint
PUBLIC :: TotalEntities
PUBLIC :: GetFacetTopology
PUBLIC :: GetVTKElementType
PUBLIC :: GetVTKElementType_
PUBLIC :: GetEdgeConnectivity
PUBLIC :: GetFaceConnectivity
PUBLIC :: GetTotalNodes
PUBLIC :: GetTotalEdges
PUBLIC :: GetTotalFaces
PUBLIC :: GetTotalCells
PUBLIC :: ReferenceElementInfo
PUBLIC :: RefElemGetGeoParam
PUBLIC :: GetFaceElemType
PUBLIC :: GetElementIndex
PUBLIC :: Reallocate
PUBLIC :: RefTopoReallocate
PUBLIC :: RefCoord
PUBLIC :: RefCoord_

INTEGER(I4B), PARAMETER, PUBLIC :: PARAM_REFELEM_MAX_FACES = 6
INTEGER(I4B), PARAMETER, PUBLIC :: PARAM_REFELEM_MAX_EDGES = 12
INTEGER(I4B), PARAMETER, PUBLIC :: PARAM_REFELEM_MAX_POINTS = 8

!----------------------------------------------------------------------------
!                                                      ReferenceElementInfo_
!----------------------------------------------------------------------------

TYPE :: ReferenceElementInfo_
  INTEGER(I4B) :: point = 1
  INTEGER(I4B) :: line = 2
  INTEGER(I4B) :: triangle = 3
  INTEGER(I4B) :: quadrangle = 4
  INTEGER(I4B) :: tetrahedron = 5
  INTEGER(I4B) :: hexahedron = 6
  INTEGER(I4B) :: prism = 7
  INTEGER(I4B) :: pyramid = 8
  INTEGER(I4B) :: tElemTopologyType_0D = 1
  INTEGER(I4B) :: tElemTopologyType_1D = 1
  INTEGER(I4B) :: tElemTopologyType_2D = 2
  INTEGER(I4B) :: tElemTopologyType_3D = 4
  INTEGER(I4B) :: tElemTopologyType = 8
  INTEGER(I4B) :: elemTopologyname(8) = &
  [Point, Line, Triangle, Quadrangle, Tetrahedron, Hexahedron, Prism, Pyramid]
  INTEGER(I4B) :: maxFaces = PARAM_REFELEM_MAX_FACES
  INTEGER(I4B) :: maxEdges = PARAM_REFELEM_MAX_EDGES
  INTEGER(I4B) :: maxPoints = PARAM_REFELEM_MAX_POINTS
  INTEGER(I4B) :: tCells(8) = [1, 1, 1, 1, 1, 1, 1, 1]
  !! Here cell is a topology for which xidim = 3
  INTEGER(I4B) :: tFaces(8) = [0, 2, 3, 4, 4, 6, 5, 5]
  !! Here facet is topology entity for which xidim = 2
  INTEGER(I4B) :: tEdges(8) = [0, 0, 0, 0, 6, 12, 9, 8]
  !! Here edge is topology entity for which xidim = 1
  INTEGER(I4B) :: tPoints(8) = [1, 2, 3, 4, 4, 8, 6, 5]
  !! A point is topology entity for which xidim = 0
  !!
  INTEGER(I4B) :: faceElemTypeLine(2) = Point
  !! element types of face of Line
  INTEGER(I4B) :: faceElemTypeTriangle(3) = Line
  !! element types of faces of triangle

  INTEGER(I4B) :: faceElemTypeQuadrangle(4) = Line
  !! element types of faces of triangle

  INTEGER(I4B) :: faceElemTypeTetrahedron(4) = Triangle
  !! element types of faces of triangle

  INTEGER(I4B) :: faceElemTypeHexahedron(6) = Quadrangle
  !! element types of faces of triangle

  INTEGER(I4B) :: faceElemTypePrism(5) = 0
  INTEGER(I4B) :: faceElemTypePyramid(5) = 0
  !! TODO: add faceElemTypePrism and faceElemTypePyramid
  !! element types of faces of triangle

#ifdef MAX_QUADRANGLE_ORDER
  INTEGER(I4B) :: maxOrder_Quadrangle = MAX_QUADRANGLE_ORDER
#else
  INTEGER(I4B) :: maxOrder_Quadrangle = 2_I4B
#endif
END TYPE ReferenceElementInfo_

TYPE(ReferenceElementInfo_), PARAMETER :: ReferenceElementInfo = &
                                          ReferenceElementInfo_()

!----------------------------------------------------------------------------
!                                                                 RefCoord
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-03
! summary:  Returns the coordinate of reference element

INTERFACE
  MODULE PURE FUNCTION RefCoord(elemType, refElem) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Element type
    CHARACTER(*), INTENT(IN) :: refElem
    !! "UNIT"
    !! "BIUNIT"
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION RefCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                   RefCoord_@GeometryMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE RefCoord_(elemType, refElem, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Element type
    CHARACTER(*), INTENT(IN) :: refElem
    !! "UNIT" ! "BIUNIT"
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! xij coordinate
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE RefCoord_
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetElementIndex@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-19
! summary:  Returns the index of an element based on its topology
!
!# Introduction
!
! Point 1
! Line 2
! Triangle 3
! Quadrangle 4
! Tetrahedron 5
! Hexahedron 6
! Prism 7
! Pyramid 8

INTERFACE
  MODULE PURE FUNCTION GetElementIndex(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION GetElementIndex
END INTERFACE

!----------------------------------------------------------------------------
!                                         RefElemGetGeoParam@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-09
! summary:  Returns the geometry parameters

INTERFACE RefElemGetGeoParam
  MODULE PURE SUBROUTINE RefElemGetGeoParam1(elemType, tNodes, tEdges,  &
    & tFaces, tCells, edgeCon, faceCon, edgeOpt, faceOpt, faceElemType,  &
    & tFaceNodes, order)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: tNodes
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: tEdges
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: tFaces
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: tCells
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: edgeCon(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: faceCon(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOpt
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOpt
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: faceElemType(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: tFaceNodes(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Order of element
  END SUBROUTINE RefElemGetGeoParam1
END INTERFACE RefElemGetGeoParam

!----------------------------------------------------------------------------
!                                             GetTotalEdges@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-14
! summary:  Returns number of edges in the element

INTERFACE GetTotalEdges
  MODULE PURE FUNCTION GetTotalEdges1(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION GetTotalEdges1
END INTERFACE GetTotalEdges

!----------------------------------------------------------------------------
!                                        GetEdgeConnectivity@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-07
! summary:  Returns number of edges in the element

INTERFACE GetEdgeConnectivity
  MODULE PURE SUBROUTINE GetEdgeConnectivity1(elemType, con, opt, order, &
                                              nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! name of element
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! Connectivity
    !! The columns represents the edge number
    !! The row represents the connectivity of edge
    !! con should be allocated by the user
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt =2, then edge connectivity for Lagrangian approximation
    !! opt=1 is default
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Order of element
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! Number of rows written to con
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! number of columns written to con
  END SUBROUTINE GetEdgeConnectivity1
END INTERFACE GetEdgeConnectivity

!----------------------------------------------------------------------------
!                                        GetFaceConnectivity@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-07
! summary:  Returns number of edges in the element

INTERFACE GetFaceConnectivity
  MODULE PURE SUBROUTINE GetFaceConnectivity1(elemType, con, opt, order, &
                                              nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! name of element
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! Connectivity
    !! The columns represents the face number
    !! The row represents a face
    !! con should be allocated by the user
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt = 2, then edge connectivity for Lagrangian approximation
    !! opt = 1 is default
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order  of element
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! Number of rows written to con
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! number of columns written to con
  END SUBROUTINE GetFaceConnectivity1
END INTERFACE GetFaceConnectivity

!----------------------------------------------------------------------------
!                                           GetFaceElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-11
! summary:  Returns the element type of each face

INTERFACE GetFaceElemType
  MODULE PURE SUBROUTINE GetFaceElemType1(elemType, faceElemType, opt, &
                                          tFaceNodes)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! name of element
    INTEGER(I4B), INTENT(INOUT) :: faceElemType(:)
    !! Element names of faces
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: tFaceNodes(:)
    !! Total number of nodes in each face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt = 2, then edge connectivity for Lagrangian approximation
    !! opt = 1 is default
  END SUBROUTINE GetFaceElemType1
END INTERFACE GetFaceElemType

!----------------------------------------------------------------------------
!                                           GetFaceElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-11
! summary:  Returns the element type of each face

INTERFACE GetFaceElemType
  MODULE PURE SUBROUTINE GetFaceElemType2(elemType, localFaceNumber, &
                                          faceElemType, opt, tFaceNodes)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! name of element
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number
    INTEGER(I4B), INTENT(OUT) :: faceElemType
    !! Element names of faces
    INTEGER(I4B), INTENT(OUT) :: tFaceNodes
    !! Total number of nodes in each face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt = 2, then edge connectivity for Lagrangian approximation
    !! opt = 1 is default
  END SUBROUTINE GetFaceElemType2
END INTERFACE GetFaceElemType

!----------------------------------------------------------------------------
!                                             GetTotalNodes@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-14
! summary:  Returns number of nodes (vertices) in the element

INTERFACE GetTotalNodes
  MODULE PURE FUNCTION GetTotalNodes1(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION GetTotalNodes1
END INTERFACE GetTotalNodes

!----------------------------------------------------------------------------
!                                             GetTotalFaces@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-14
! summary:  Returns number of faces in the element

INTERFACE GetTotalFaces
  MODULE PURE FUNCTION GetTotalFaces1(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION GetTotalFaces1
END INTERFACE GetTotalFaces

!----------------------------------------------------------------------------
!                                             GetTotalCells@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-14
! summary:  Returns number of faces in the element

INTERFACE GetTotalCells
  MODULE PURE FUNCTION GetTotalCells1(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION GetTotalCells1
END INTERFACE GetTotalCells

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display the ReferenceElement

INTERFACE Display
  MODULE SUBROUTINE refelem_Display(obj, msg, unitno)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
  END SUBROUTINE refelem_Display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Markdown encoding of reference element

INTERFACE MdEncode
  MODULE FUNCTION refelem_MdEncode(obj) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION refelem_MdEncode
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Returns react element for reference element

INTERFACE ReactEncode
  MODULE FUNCTION refelem_ReactEncode(obj) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION refelem_ReactEncode
END INTERFACE ReactEncode

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display reference topology

INTERFACE Display
  MODULE SUBROUTINE reftopo_Display(obj, msg, unitno)
    CLASS(ReferenceTopology_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
  END SUBROUTINE reftopo_Display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display reference topology

INTERFACE MdEncode
  MODULE FUNCTION reftopo_MdEncode(obj) RESULT(ans)
    CLASS(ReferenceTopology_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION reftopo_MdEncode
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                     ReferenceTopology@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: This function returns the instance of [[ReferenceTopology_]]
!
!# Introduction
!
! This function returns the instance of [[ReferenceTopology_]].
!
! The possible valaues of name can be
!
! - `Line, Line2, Line3, Line4, Line5, Line6`
! - `Triangle, Triangle3, Triangle6, Triangle9, Triangle10, Triangle12,
! Triangl15a, Triangl15b, Triangl15, Triangl21`
! - `Quadrangle, Quadrangle4, Quadrangle9, Quadrangle8`
! - `Tetrahedron, Tetrahedron4, Tetrahedron10, Tetrahedron20, Tetrahedron35,
! Tetrahedron56`
! - `Hexahedron, Hexahedron8, Hexahedron27, Hexahedron20, Hexahedron64,
! Hexahedron125`
! - `Prism, Prism6, Prism15, Prism18`
! - `Pyramid, Pyramid5, Pyramid14, Pyramid13`
! - `Point, Point1`
!
!### Usage
!
!```fortran
! type( ReferenceTopology_ ) :: obj
! obj = ReferenceTopology( nptrs = [1,2,3], name=Triangle3 )
! call display( obj, "obj=")
!```

INTERFACE ReferenceTopology
  MODULE PURE FUNCTION refelem_ReferenceTopology(nptrs, name) RESULT(obj)
    TYPE(ReferenceTopology_) :: obj
    INTEGER(I4B), INTENT(IN) :: nptrs(:)
    INTEGER(I4B), INTENT(IN) :: name
  END FUNCTION refelem_ReferenceTopology
END INTERFACE ReferenceTopology

!----------------------------------------------------------------------------
!                                            Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: This subroutine reset the instance of [[ReferenceTopology_]]
!
!### Usage
!
!```fortran
! type( ReferenceTopology_ ) :: obj
! obj = ReferenceTopology( nptrs = [1,2,3], name=Triangle3 )
! call display( obj, "obj=")
! call Deallocate( obj )
!```

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE refelem_Deallocate1(obj)
    CLASS(ReferenceTopology_), INTENT(INOUT) :: obj
  END SUBROUTINE refelem_Deallocate1
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                            Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-24
! summary: Deallocate topology vector

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE RefTopoDeallocate(obj)
    TYPE(ReferenceTopology_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE RefTopoDeallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                            Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-24
! summary: Reallocate topology vector

INTERFACE Reallocate
  MODULE PURE SUBROUTINE RefTopoReallocate(obj, n)
    TYPE(ReferenceTopology_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: n
  END SUBROUTINE RefTopoReallocate
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                            Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Deallocates the data stored inside the [[ReferenceElement_]]

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE refelem_Deallocate2(obj)
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
  END SUBROUTINE refelem_Deallocate2
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                     NNE@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: This function returns the totat nodes inside the referenc topology
!
!### Usage
!
!```fortran
! type( ReferenceTopology_ ) :: obj
! obj = ReferenceTopology( nptrs = [1,2,3], name=Triangle3 )
! call display( obj, "obj=")
! call display( .NNE. obj, "nne =")
!```

INTERFACE OPERATOR(.NNE.)
  MODULE PURE FUNCTION refelem_NNE1(obj) RESULT(ans)
    CLASS(ReferenceTopology_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION refelem_NNE1
END INTERFACE OPERATOR(.NNE.)

INTERFACE TotalNodesInElement
  MODULE PROCEDURE refelem_NNE1
END INTERFACE TotalNodesInElement

!----------------------------------------------------------------------------
!                                                    NNE@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Returns the total number of nodes in the reference element
!

INTERFACE OPERATOR(.NNE.)
  MODULE PURE FUNCTION refelem_NNE2(obj) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION refelem_NNE2
END INTERFACE OPERATOR(.NNE.)

INTERFACE TotalNodesInElement
  MODULE PROCEDURE refelem_NNE2
END INTERFACE TotalNodesInElement

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 March 2021
! summary: This subroutine copies one reference element into other
!
!# Introduction
!
! This subroutine copies one reference element into other
! This subroutine also defines an assignment operator for `obj1=obj2`
! type opertions

INTERFACE Initiate
  MODULE PURE SUBROUTINE refelem_Initiate1(obj, anotherobj)
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    CLASS(ReferenceElement_), INTENT(IN) :: anotherobj
  END SUBROUTINE refelem_Initiate1
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE refelem_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                               ReferenceElement_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns a pointer to an instance of ReferenceElement

INTERFACE ReferenceElement_Pointer
  MODULE FUNCTION refelem_Constructor_1(xidim, nsd, elemType, &
    & ipType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: xidim
      !! xidimension
    INTEGER(I4B), INTENT(IN) :: nsd
      !! spatial dimenstion
    INTEGER(I4B), INTENT(IN) :: elemType
      !! element type
    INTEGER(I4B), INTENT(IN) :: ipType
      !! interpolationType
    CLASS(ReferenceElement_), POINTER :: ans
      !! reference element
  END FUNCTION refelem_Constructor_1
END INTERFACE ReferenceElement_Pointer

!----------------------------------------------------------------------------
!                                ReferenceElementPointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns a pointer to an instance of ReferenceElement

INTERFACE ReferenceElement_Pointer
  MODULE FUNCTION refelem_Constructor_2(refelem) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    CLASS(ReferenceElement_), POINTER :: ans
  END FUNCTION refelem_Constructor_2
END INTERFACE ReferenceElement_Pointer

!----------------------------------------------------------------------------
!                                         GetConnectivity@ConstrucorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Returns the node numbers of reference element

INTERFACE GetConnectivity
  MODULE PURE FUNCTION refelem_Getnptrs(obj) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION refelem_Getnptrs
END INTERFACE GetConnectivity

!----------------------------------------------------------------------------
!                                            ElementType@ElementNameMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns element name in integer from element name

INTERFACE ElementType
  MODULE PURE FUNCTION Element_Type(Elemname) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: Elemname
    INTEGER(I4B) :: ans
  END FUNCTION Element_Type
END INTERFACE ElementType

!----------------------------------------------------------------------------
!                                             ElementType@ElementNameMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-22
! summary: Return name of element

INTERFACE ElementType
  MODULE PURE FUNCTION Element_Type_obj(obj) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION Element_Type_obj
END INTERFACE ElementType

!----------------------------------------------------------------------------
!                                           ElementName@ElementNameMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns element name in character from element number/type

INTERFACE ElementName
  MODULE PURE FUNCTION Element_name(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION Element_name
END INTERFACE ElementName

!----------------------------------------------------------------------------
!                                             ElementName@ElementNameMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns element name in character from ReferenceElement

INTERFACE ElementName
  MODULE PURE FUNCTION Element_name_obj(obj) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION Element_name_obj
END INTERFACE ElementName

!----------------------------------------------------------------------------
!                                   TotalNodesInElement@ElementNameMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns total numbers of nodes present in a given element

INTERFACE TotalNodesInElement
  MODULE PURE FUNCTION Total_Nodes_In_Element(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION Total_Nodes_In_Element
END INTERFACE TotalNodesInElement

!----------------------------------------------------------------------------
!                                           ElementOrder@ElementNameMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns the order of an element

INTERFACE ElementOrder
  MODULE PURE FUNCTION Element_Order(elemType) RESULT(ans)
    INTEGER(I4B) :: ans
    INTEGER(I4B), INTENT(IN) :: elemType
  END FUNCTION Element_Order
END INTERFACE ElementOrder

!----------------------------------------------------------------------------
!                                           ElementOrder@ElementNameMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns the order of an element

INTERFACE ElementOrder
  MODULE PURE FUNCTION Element_Order_refelem(refelem) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    INTEGER(I4B) :: ans
  END FUNCTION Element_Order_refelem
END INTERFACE ElementOrder

INTERFACE OPERATOR(.order.)
  MODULE PROCEDURE Element_Order_refelem, Element_Order
END INTERFACE OPERATOR(.order.)

!----------------------------------------------------------------------------
!                                            XiDimension@ElementNameMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-13
! update: 2021-11-13
! summary: Returns the xidimension of an element

INTERFACE XiDimension
  MODULE PURE FUNCTION Elem_XiDimension1(elemType) RESULT(ans)
    INTEGER(I4B) :: ans
    INTEGER(I4B), INTENT(IN) :: elemType
  END FUNCTION Elem_XiDimension1
END INTERFACE Xidimension

!----------------------------------------------------------------------------
!                                             Xidimension@ElementNameMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-13
! update: 2021-11-13
! summary: Returns xidimension of the reference element

INTERFACE Xidimension
  MODULE PURE FUNCTION Elem_Xidimension2(obj) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION Elem_Xidimension2
END INTERFACE XiDimension

!----------------------------------------------------------------------------
!                                                   isVolume@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a volume element

INTERFACE isVolume
  MODULE PURE FUNCTION isVolume1(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    LOGICAL(LGT) :: ans
  END FUNCTION isVolume1
END INTERFACE isVolume

!----------------------------------------------------------------------------
!                                                   isVolume@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a volume element

INTERFACE isVolume
  MODULE PURE FUNCTION isVolume2(obj) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION isVolume2
END INTERFACE isVolume

!----------------------------------------------------------------------------
!                                                 isSurface@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Surface element

INTERFACE isSurface
  MODULE PURE FUNCTION isSurface1(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    LOGICAL(LGT) :: ans
  END FUNCTION isSurface1
END INTERFACE isSurface

!----------------------------------------------------------------------------
!                                                 isSurface@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Surface element

INTERFACE isSurface
  MODULE PURE FUNCTION isSurface2(obj) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION isSurface2
END INTERFACE isSurface

!----------------------------------------------------------------------------
!                                                    isLine@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Line element

INTERFACE isLine
  MODULE PURE FUNCTION isLine1(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    LOGICAL(LGT) :: ans
  END FUNCTION isLine1
END INTERFACE isLine

!----------------------------------------------------------------------------
!                                                    isLine@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Line element

INTERFACE isLine
  MODULE PURE FUNCTION isLine2(obj) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION isLine2
END INTERFACE isLine

!----------------------------------------------------------------------------
!                                                   isPoint@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Point element

INTERFACE isPoint
  MODULE PURE FUNCTION isPoint1(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    LOGICAL(LGT) :: ans
  END FUNCTION isPoint1
END INTERFACE isPoint

!----------------------------------------------------------------------------
!                                                   isPoint@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Point element

INTERFACE isPoint
  MODULE PURE FUNCTION isPoint2(obj) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION isPoint2
END INTERFACE isPoint

!----------------------------------------------------------------------------
!                                                 isTriangle@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Triangle element

INTERFACE isTriangle
  MODULE PURE FUNCTION isTriangle1(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    LOGICAL(LGT) :: ans
  END FUNCTION isTriangle1
END INTERFACE isTriangle

!----------------------------------------------------------------------------
!                                                 isTriangle@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Triangle element

INTERFACE isTriangle
  MODULE PURE FUNCTION isTriangle2(obj) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION isTriangle2
END INTERFACE isTriangle

!----------------------------------------------------------------------------
!                                               isQuadrangle@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Quadrangle element

INTERFACE isQuadrangle
  MODULE PURE FUNCTION isQuadrangle1(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    LOGICAL(LGT) :: ans
  END FUNCTION isQuadrangle1
END INTERFACE isQuadrangle

!----------------------------------------------------------------------------
!                                               isQuadrangle@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Quadrangle element

INTERFACE isQuadrangle
  MODULE PURE FUNCTION isQuadrangle2(obj) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION isQuadrangle2
END INTERFACE isQuadrangle

!----------------------------------------------------------------------------
!                                             isTetrahedron@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Tetrahedron element

INTERFACE isTetrahedron
  MODULE PURE FUNCTION isTetrahedron1(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    LOGICAL(LGT) :: ans
  END FUNCTION isTetrahedron1
END INTERFACE isTetrahedron

!----------------------------------------------------------------------------
!                                             isTetrahedron@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Tetrahedron element

INTERFACE isTetrahedron
  MODULE PURE FUNCTION isTetrahedron2(obj) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION isTetrahedron2
END INTERFACE isTetrahedron

!----------------------------------------------------------------------------
!                                             isHexahedron@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Hexahedron element

INTERFACE isHexahedron
  MODULE PURE FUNCTION isHexahedron1(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    LOGICAL(LGT) :: ans
  END FUNCTION isHexahedron1
END INTERFACE isHexahedron

!----------------------------------------------------------------------------
!                                             isHexahedron@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Hexahedron element

INTERFACE isHexahedron
  MODULE PURE FUNCTION isHexahedron2(obj) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION isHexahedron2
END INTERFACE isHexahedron

!----------------------------------------------------------------------------
!                                                    isPrism@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Prism element

INTERFACE isPrism
  MODULE PURE FUNCTION isPrism1(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    LOGICAL(LGT) :: ans
  END FUNCTION isPrism1
END INTERFACE isPrism

!----------------------------------------------------------------------------
!                                                    isPrism@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Prism element

INTERFACE isPrism
  MODULE PURE FUNCTION isPrism2(obj) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION isPrism2
END INTERFACE isPrism

!----------------------------------------------------------------------------
!                                                 isPyramid@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Pyramid element

INTERFACE isPyramid
  MODULE PURE FUNCTION isPyramid1(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    LOGICAL(LGT) :: ans
  END FUNCTION isPyramid1
END INTERFACE isPyramid

!----------------------------------------------------------------------------
!                                                 isPyramid@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Pyramid element

INTERFACE isPyramid
  MODULE PURE FUNCTION isPyramid2(obj) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION isPyramid2
END INTERFACE isPyramid

!----------------------------------------------------------------------------
!                                       isSerendipityElement@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a SerendipityElement element

INTERFACE isSerendipityElement
  MODULE PURE FUNCTION isSerendipityElement1(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    LOGICAL(LGT) :: ans
  END FUNCTION isSerendipityElement1
END INTERFACE isSerendipityElement

!----------------------------------------------------------------------------
!                                       isSerendipityElement@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a SerendipityElement element

INTERFACE isSerendipityElement
  MODULE PURE FUNCTION isSerendipityElement2(obj) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION isSerendipityElement2
END INTERFACE isSerendipityElement

!----------------------------------------------------------------------------
!                                         ElementTopology@ElementNameMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-10
! update: 2021-11-10
! summary: Return the element topology
!
!# Introduction
!
! This routine returns the topology of the reference element
!  - Line
!  - Triangle
!  - Quadrangle
!  - Tetrahedron

INTERFACE ElementTopology
  MODULE PURE FUNCTION refelem_ElementTopology1(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION refelem_ElementTopology1
END INTERFACE ElementTopology

INTERFACE OPERATOR(.topology.)
  MODULE PROCEDURE refelem_ElementTopology1
END INTERFACE OPERATOR(.topology.)

!----------------------------------------------------------------------------
!                                       ElementTopology@ElementNameMethods
!----------------------------------------------------------------------------

INTERFACE ElementTopology
  MODULE PURE FUNCTION refelem_ElementTopology2(obj) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION refelem_ElementTopology2
END INTERFACE ElementTopology

INTERFACE OPERATOR(.topology.)
  MODULE PROCEDURE refelem_ElementTopology2
END INTERFACE OPERATOR(.topology.)

!----------------------------------------------------------------------------
!                                            FacetMatrix@FacetElementMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Returns the facet matrix
!
!# Introduction
!
! Returns the facet matrix of a reference element.
!
! - Number of rows are equal to the number of facet in an element
! - Number of columns = MAX( NNS )
! - First column => ElementTopology
! - Second Column => XiDimension
! - Third column => NNS
! - 4 to NNS + 3 => Local nptrs

INTERFACE FacetMatrix
  MODULE PURE FUNCTION Facet_Matrix_refelem(refelem) RESULT(FM)
    INTEGER(I4B), ALLOCATABLE :: FM(:, :)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
  END FUNCTION Facet_Matrix_refelem
END INTERFACE FacetMatrix

!----------------------------------------------------------------------------
!                                          FacetElements@FacetElementMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-25
! summary: This routine returns the facet elements

INTERFACE GetFacetElements
  MODULE SUBROUTINE refelem_GetFacetElements1(refelem, ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE refelem_GetFacetElements1
END INTERFACE GetFacetElements

!----------------------------------------------------------------------------
!                                          FacetElements@FacetElementMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-25
! summary: This routine returns the facet elements

INTERFACE GetFacetElements
  MODULE SUBROUTINE refelem_GetFacetElements2(elemType, nsd, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nsd
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE refelem_GetFacetElements2
END INTERFACE GetFacetElements

!----------------------------------------------------------------------------
!                                             FacetTopology@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Returns the facet topology of the given element type

INTERFACE GetFacetTopology
  MODULE PURE SUBROUTINE refelem_GetFacetTopology(elemType, nptrs, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nptrs(:)
    TYPE(ReferenceTopology_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE refelem_GetFacetTopology
END INTERFACE GetFacetTopology

!----------------------------------------------------------------------------
!                                       LocalNodeCoord@LocalNodeCoordMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Deprecated
!
!# Introduction
!
! This routine will be removed in near future
! This routine is not included in generic LocalNodeCoord routine

INTERFACE
  MODULE PURE SUBROUTINE Local_NodeCoord(NodeCoord, elemType)
    ! Define intent of dummy variables
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: NodeCoord(:, :)
    INTEGER(I4B), INTENT(IN) :: elemType
  END SUBROUTINE Local_NodeCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                     LocalNodeCoord@LocalNodeCoordMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Returns the local NodeCoord of an element

INTERFACE LocalNodeCoord
  MODULE PURE FUNCTION Local_NodeCoord_refelem(refelem) RESULT(nodecoord)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), ALLOCATABLE :: nodecoord(:, :)
  END FUNCTION Local_NodeCoord_refelem
END INTERFACE LocalNodeCoord

!----------------------------------------------------------------------------
!                                         MeasureSimplex@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Returns measures for simplex

INTERFACE MeasureSimplex
  MODULE PURE FUNCTION Measure_Simplex(refelem, XiJ) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: XiJ(:, :)
    REAL(DFP) :: ans
  END FUNCTION Measure_Simplex
END INTERFACE MeasureSimplex

!----------------------------------------------------------------------------
!                                            ElementQuality@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Measure the quality of the element

INTERFACE ElementQuality
  MODULE FUNCTION Element_Quality(refelem, xij, measure) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: ans
  END FUNCTION Element_Quality
END INTERFACE ElementQuality

!----------------------------------------------------------------------------
!                                             ContainsPoint@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Returns true if the given point is inside the element

INTERFACE ContainsPoint
  MODULE FUNCTION contains_point(refelem, xij, x) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP), INTENT(IN) :: x(:)
    LOGICAL(LGT) :: ans
  END FUNCTION contains_point
END INTERFACE ContainsPoint

!----------------------------------------------------------------------------
!                                             TotalEntities@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Total entities present in an element

INTERFACE TotalEntities
  MODULE PURE FUNCTION refelem_TotalEntities(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans(4)
  END FUNCTION refelem_TotalEntities
END INTERFACE TotalEntities

!----------------------------------------------------------------------------
!                                              getVTKelementType@VTKMethods
!----------------------------------------------------------------------------

INTERFACE GetVTKElementType
  MODULE PURE SUBROUTINE GetVTKElementType1(elemType, vtk_type, nptrs)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(INT8), INTENT(OUT) :: vtk_type
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: nptrs(:)
  END SUBROUTINE GetVTKElementType1
END INTERFACE GetVTKElementType

!----------------------------------------------------------------------------
!                                               GetVTKElementType@VTKMethods
!----------------------------------------------------------------------------

INTERFACE GetVTKElementType_
  MODULE PURE SUBROUTINE GetVTKElementType1_(elemType, vtk_type, nptrs, tsize)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(INT8), INTENT(OUT) :: vtk_type
    INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE GetVTKElementType1_
END INTERFACE GetVTKElementType_

END MODULE ReferenceElement_Method
