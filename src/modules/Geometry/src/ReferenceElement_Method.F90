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
USE GlobalData
IMPLICIT NONE
PRIVATE
PUBLIC :: Display
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
PUBLIC :: FacetElements
PUBLIC :: LocalNodeCoord
PUBLIC :: MeasureSimplex
PUBLIC :: ElementQuality
PUBLIC :: ContainsPoint
PUBLIC :: TotalEntities
PUBLIC :: FacetTopology
PUBLIC :: GetVTKelementType

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display the ReferenceElement

INTERFACE
  MODULE SUBROUTINE refelem_Display(obj, msg, unitno)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
  END SUBROUTINE refelem_Display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE refelem_Display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display reference topology

INTERFACE
  MODULE SUBROUTINE reftopo_Display(obj, msg, unitno)
    CLASS(ReferenceTopology_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
  END SUBROUTINE reftopo_Display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE reftopo_Display
END INTERFACE Display

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
! The possible valaues of Name can be
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
! obj = ReferenceTopology( Nptrs = [1,2,3], Name=Triangle3 )
! call display( obj, "obj=")
!```

INTERFACE
  MODULE PURE FUNCTION refelem_ReferenceTopology(Nptrs, Name) RESULT(obj)
    TYPE(ReferenceTopology_) :: obj
    INTEGER(I4B), INTENT(IN) :: Nptrs(:)
    INTEGER(I4B), INTENT(IN) :: Name
  END FUNCTION refelem_ReferenceTopology
END INTERFACE

INTERFACE ReferenceTopology
  MODULE PROCEDURE refelem_ReferenceTopology
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
! obj = ReferenceTopology( Nptrs = [1,2,3], Name=Triangle3 )
! call display( obj, "obj=")
! call Deallocate( obj )
!```

INTERFACE
  MODULE PURE SUBROUTINE refelem_Deallocate1(obj)
    CLASS(ReferenceTopology_), INTENT(INOUT) :: obj
  END SUBROUTINE refelem_Deallocate1
END INTERFACE

INTERFACE DEALLOCATE
  MODULE PROCEDURE refelem_Deallocate1
END INTERFACE

!----------------------------------------------------------------------------
!                                            Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Deallocates the data stored inside the [[ReferenceElement_]]

INTERFACE
  MODULE PURE SUBROUTINE refelem_Deallocate2(obj)
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
  END SUBROUTINE refelem_Deallocate2
END INTERFACE

INTERFACE DEALLOCATE
  MODULE PROCEDURE refelem_Deallocate2
END INTERFACE

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
! obj = ReferenceTopology( Nptrs = [1,2,3], Name=Triangle3 )
! call display( obj, "obj=")
! call display( .NNE. obj, "nne =")
!```

INTERFACE
  MODULE PURE FUNCTION refelem_NNE1(obj) RESULT(Ans)
    CLASS(ReferenceTopology_), INTENT(IN) :: obj
    INTEGER(I4B) :: Ans
  END FUNCTION refelem_NNE1
END INTERFACE

INTERFACE OPERATOR(.NNE.)
  MODULE PROCEDURE refelem_NNE1
END INTERFACE

!----------------------------------------------------------------------------
!                                                    NNE@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Returns the total number of nodes in the reference element
!

INTERFACE
  MODULE PURE FUNCTION refelem_NNE2(obj) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    INTEGER(I4B) :: Ans
  END FUNCTION refelem_NNE2
END INTERFACE

INTERFACE OPERATOR(.NNE.)
  MODULE PROCEDURE refelem_NNE2
END INTERFACE

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

INTERFACE
  MODULE PURE SUBROUTINE refelem_Initiate1(obj, Anotherobj)
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    CLASS(ReferenceElement_), INTENT(IN) :: Anotherobj
  END SUBROUTINE refelem_Initiate1
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE refelem_Initiate1
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

INTERFACE
  MODULE FUNCTION refelem_Constructor_1(xidim, nsd, elemType, &
    & ipType) RESULT(Ans)
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
END INTERFACE

INTERFACE ReferenceElement_Pointer
  MODULE PROCEDURE refelem_Constructor_1
END INTERFACE ReferenceElement_Pointer

!----------------------------------------------------------------------------
!                                ReferenceElementPointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns a pointer to an instance of ReferenceElement

INTERFACE
  MODULE FUNCTION refelem_Constructor_2(refelem) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    CLASS(ReferenceElement_), POINTER :: ans
  END FUNCTION refelem_Constructor_2
END INTERFACE

INTERFACE ReferenceElement_Pointer
  MODULE PROCEDURE refelem_Constructor_2
END INTERFACE ReferenceElement_Pointer

!----------------------------------------------------------------------------
!                                                 getNptrs@ConstrucorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Returns the node numbers of reference element

INTERFACE GetConnectivity
  MODULE PURE FUNCTION refelem_GetNptrs(obj) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION refelem_GetNptrs
END INTERFACE GetConnectivity

!----------------------------------------------------------------------------
!                                               ElementType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns element name in integer from element name

INTERFACE ElementType
  MODULE PURE FUNCTION Element_Type(ElemName) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: ElemName
    INTEGER(I4B) :: Ans
  END FUNCTION Element_Type
END INTERFACE ElementType

!----------------------------------------------------------------------------
!                                               ElementName@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns element name in character from element number/type

INTERFACE ElementName
  MODULE PURE FUNCTION Element_Name(ElemType) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: ElemType
    CHARACTER(50) :: Ans
  END FUNCTION Element_Name
END INTERFACE ElementName

!----------------------------------------------------------------------------
!                                       TotalNodesInElement@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns total numbers of nodes present in a given element

INTERFACE TotalNodesInElement
  MODULE PURE FUNCTION Total_Nodes_In_Element(ElemType) RESULT(Ans)
    INTEGER(I4B) :: Ans
    INTEGER(I4B), INTENT(IN) :: ElemType
  END FUNCTION Total_Nodes_In_Element
END INTERFACE TotalNodesInElement

!----------------------------------------------------------------------------
!                                              ElementOrder@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns the order of an element

INTERFACE ElementOrder
  MODULE PURE FUNCTION Element_Order(ElemType) RESULT(Ans)
    INTEGER(I4B) :: Ans
    INTEGER(I4B), INTENT(IN) :: ElemType
  END FUNCTION Element_Order
END INTERFACE ElementOrder

!----------------------------------------------------------------------------
!                                              ElementOrder@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns the order of an element

INTERFACE ElementOrder
  MODULE PURE FUNCTION Element_Order_RefElem(RefElem) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: RefElem
    INTEGER(I4B) :: Ans
  END FUNCTION Element_Order_RefElem
END INTERFACE ElementOrder

INTERFACE OPERATOR(.order.)
  MODULE PROCEDURE Element_Order_RefElem, Element_Order
END INTERFACE OPERATOR(.order.)

!----------------------------------------------------------------------------
!                                               XiDimension@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-13
! update: 2021-11-13
! summary: Returns the xidimension of an element

INTERFACE XiDimension
  MODULE PURE FUNCTION Elem_XiDimension1(ElemType) RESULT(Ans)
    INTEGER(I4B) :: Ans
    INTEGER(I4B), INTENT(IN) :: ElemType
  END FUNCTION Elem_XiDimension1
END INTERFACE Xidimension

!----------------------------------------------------------------------------
!                                                Xidimension@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-13
! update: 2021-11-13
! summary: Returns xidimension of the reference element

INTERFACE Xidimension
  MODULE PURE FUNCTION Elem_Xidimension2(obj) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION Elem_Xidimension2
END INTERFACE XiDimension

!----------------------------------------------------------------------------
!                                                   isVolume@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a volume element

INTERFACE
  MODULE PURE FUNCTION isVolume(ElemType) RESULT(Ans)
    LOGICAL(LGT) :: Ans
    INTEGER(I4B), INTENT(IN) :: ElemType
  END FUNCTION isVolume
END INTERFACE

!----------------------------------------------------------------------------
!                                                 isSurface@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Surface element

INTERFACE
  MODULE PURE FUNCTION isSurface(ElemType) RESULT(Ans)
    LOGICAL(LGT) :: Ans
    INTEGER(I4B), INTENT(IN) :: ElemType
  END FUNCTION isSurface
END INTERFACE

!----------------------------------------------------------------------------
!                                                    isLine@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Line element

INTERFACE
  MODULE PURE FUNCTION isLine(ElemType) RESULT(Ans)
    LOGICAL(LGT) :: Ans
    INTEGER(I4B), INTENT(IN) :: ElemType
  END FUNCTION isLine
END INTERFACE

!----------------------------------------------------------------------------
!                                                   isPoint@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Point element

INTERFACE
  MODULE PURE FUNCTION isPoint(ElemType) RESULT(Ans)
    LOGICAL(LGT) :: Ans
    INTEGER(I4B), INTENT(IN) :: ElemType
  END FUNCTION isPoint
END INTERFACE

!----------------------------------------------------------------------------
!                                                 isTriangle@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Triangle element

INTERFACE
  MODULE PURE FUNCTION isTriangle(ElemType) RESULT(Ans)
    LOGICAL(LGT) :: Ans
    INTEGER(I4B), INTENT(IN) :: ElemType
  END FUNCTION isTriangle
END INTERFACE

!----------------------------------------------------------------------------
!                                               isQuadrangle@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Quadrangle element

INTERFACE
  MODULE PURE FUNCTION isQuadrangle(ElemType) RESULT(Ans)
    LOGICAL(LGT) :: Ans
    INTEGER(I4B), INTENT(IN) :: ElemType
  END FUNCTION isQuadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                             isTetrahedron@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Tetrahedron element

INTERFACE
  MODULE PURE FUNCTION isTetrahedron(ElemType) RESULT(Ans)
    LOGICAL(LGT) :: Ans
    INTEGER(I4B), INTENT(IN) :: ElemType
  END FUNCTION isTetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                             isHexahedron@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Hexahedron element

INTERFACE
  MODULE PURE FUNCTION isHexahedron(ElemType) RESULT(Ans)
    LOGICAL(LGT) :: Ans
    INTEGER(I4B), INTENT(IN) :: ElemType
  END FUNCTION isHexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                    isPrism@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Prism element

INTERFACE
  MODULE PURE FUNCTION isPrism(ElemType) RESULT(Ans)
    LOGICAL(LGT) :: Ans
    INTEGER(I4B), INTENT(IN) :: ElemType
  END FUNCTION isPrism
END INTERFACE

!----------------------------------------------------------------------------
!                                                 isPyramid@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a Pyramid element

INTERFACE
  MODULE PURE FUNCTION isPyramid(ElemType) RESULT(Ans)
    LOGICAL(LGT) :: Ans
    INTEGER(I4B), INTENT(IN) :: ElemType
  END FUNCTION isPyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                       isSerendipityElement@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 May 2022
! summary: Returns true if element is a SerendipityElement element

INTERFACE
  MODULE PURE FUNCTION isSerendipityElement(ElemType) RESULT(Ans)
    LOGICAL(LGT) :: Ans
    INTEGER(I4B), INTENT(IN) :: ElemType
  END FUNCTION isSerendipityElement
END INTERFACE

!----------------------------------------------------------------------------
!                                           ElementTopology@GeometryMethods
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

INTERFACE
  MODULE PURE FUNCTION refelem_ElementTopology1(ElemType) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: ElemType
    INTEGER(I4B) :: Ans
  END FUNCTION refelem_ElementTopology1
END INTERFACE

!----------------------------------------------------------------------------
!                                           ElementTopology@GeometryMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION refelem_ElementTopology2(obj) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    INTEGER(I4B) :: Ans
  END FUNCTION refelem_ElementTopology2
END INTERFACE

INTERFACE ElementTopology
  MODULE PROCEDURE refelem_ElementTopology1, refelem_ElementTopology2
END INTERFACE ElementTopology

INTERFACE OPERATOR(.topology.)
  MODULE PROCEDURE refelem_ElementTopology1, refelem_ElementTopology2
END INTERFACE OPERATOR(.topology.)

!----------------------------------------------------------------------------
!                                               FacetMatrix@GeometryMethods
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
! - 4 to NNS + 3 => Local Nptrs

INTERFACE
  MODULE PURE FUNCTION Facet_Matrix_RefElem(RefElem) RESULT(FM)
    INTEGER(I4B), ALLOCATABLE :: FM(:, :)
    CLASS(ReferenceElement_), INTENT(IN) :: RefElem
  END FUNCTION Facet_Matrix_RefElem
END INTERFACE

INTERFACE FacetMatrix
  MODULE PROCEDURE Facet_Matrix_RefElem
END INTERFACE FacetMatrix

!----------------------------------------------------------------------------
!                                             FacetElements@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: This routine returns the facet elements

INTERFACE
  MODULE FUNCTION RefElem_FacetElements(RefElem) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: RefElem
    TYPE(ReferenceElement_), ALLOCATABLE :: ans(:)
  END FUNCTION RefElem_FacetElements
END INTERFACE

INTERFACE FacetElements
  MODULE PROCEDURE RefElem_FacetElements
END INTERFACE FacetElements

!----------------------------------------------------------------------------
!                                            LocalNodeCoord@GeometryMethods
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
  MODULE PURE SUBROUTINE Local_NodeCoord(NodeCoord, ElemType)
    ! Define intent of dummy variables
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: NodeCoord(:, :)
    INTEGER(I4B), INTENT(IN) :: ElemType
  END SUBROUTINE Local_NodeCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                            LocalNodeCoord@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Returns the local NodeCoord of an element

INTERFACE
  MODULE PURE FUNCTION Local_NodeCoord_RefElem(RefElem) RESULT(NodeCoord)
    CLASS(ReferenceElement_), INTENT(IN) :: RefElem
    REAL(DFP), ALLOCATABLE :: NodeCoord(:, :)
  END FUNCTION Local_NodeCoord_RefElem
END INTERFACE

INTERFACE LocalNodeCoord
  MODULE PROCEDURE Local_NodeCoord_RefElem
END INTERFACE LocalNodeCoord

!----------------------------------------------------------------------------
!                                         MeasureSimplex@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Returns measures for simplex

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex(RefElem, XiJ) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: RefElem
    REAL(DFP), INTENT(IN) :: XiJ(:, :)
    REAL(DFP) :: Ans
  END FUNCTION Measure_Simplex
END INTERFACE

INTERFACE MeasureSimplex
  MODULE PROCEDURE Measure_Simplex
END INTERFACE MeasureSimplex

!----------------------------------------------------------------------------
!                                            ElementQuality@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Measure the quality of the element

INTERFACE
  MODULE FUNCTION Element_Quality(refelem, xij, measure) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: Ans
  END FUNCTION Element_Quality
END INTERFACE

INTERFACE ElementQuality
  MODULE PROCEDURE Element_Quality
END INTERFACE ElementQuality

!----------------------------------------------------------------------------
!                                             ContainsPoint@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Returns true if the given point is inside the element

INTERFACE
  MODULE FUNCTION contains_point(refelem, xij, x) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP), INTENT(IN) :: x(:)
    LOGICAL(LGT) :: Ans
  END FUNCTION contains_point
END INTERFACE

INTERFACE ContainsPoint
  MODULE PROCEDURE contains_point
END INTERFACE ContainsPoint

!----------------------------------------------------------------------------
!                                             TotalEntities@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Total entities present in an element

INTERFACE
  MODULE PURE FUNCTION RefElem_TotalEntities(ElemType) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: ElemType
    INTEGER(I4B) :: ans(4)
  END FUNCTION RefElem_TotalEntities
END INTERFACE

INTERFACE TotalEntities
  MODULE PROCEDURE RefElem_TotalEntities
END INTERFACE TotalEntities

!----------------------------------------------------------------------------
!                                             FacetTopology@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Returns the facet topology of the given element type

INTERFACE
  MODULE PURE FUNCTION RefElem_FacetTopology(ElemType, Nptrs) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: ElemType
    INTEGER(I4B), INTENT(IN) :: Nptrs(:)
    TYPE(ReferenceTopology_), ALLOCATABLE :: ans(:)
  END FUNCTION RefElem_FacetTopology
END INTERFACE

INTERFACE FacetTopology
  MODULE PROCEDURE RefElem_FacetTopology
END INTERFACE FacetTopology

!----------------------------------------------------------------------------
!                                              getVTKelementType@VTKMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE get_vtk_elemType(ElemType, vtk_type, nptrs)
    INTEGER(I4B), INTENT(IN) :: ElemType
    INTEGER(INT8), INTENT(OUT) :: vtk_type
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: nptrs(:)
  END SUBROUTINE get_vtk_elemType
END INTERFACE

INTERFACE getVTKelementType
  MODULE PROCEDURE get_vtk_elemType
END INTERFACE getVTKelementType

END MODULE ReferenceElement_Method
