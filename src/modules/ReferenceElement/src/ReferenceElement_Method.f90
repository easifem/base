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
! date: 1 March 2021
! summary: This submodule contains method for [[ReferenceElement_]]

MODULE ReferenceElement_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE display_ref_elem( obj, msg, unitno )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitno
END SUBROUTINE display_ref_elem
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE display_ref_topo( obj, msg, unitno )
  CLASS( ReferenceTopology_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitno
END SUBROUTINE display_ref_topo
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

INTERFACE Display
  MODULE PROCEDURE display_ref_elem, display_ref_topo
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                              ReferenceTopology@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: 	This function returns the instance of [[ReferenceTopology_]]
!
!### Introduction
! 	This function returns the instance of [[ReferenceTopology_]].
! The possible valaues of Name can be
!
! * `Line, Line2, Line3, Line4, Line5, Line6`
! * `Triangle, Triangle3, Triangle6, Triangle9, Triangle10, Triangle12, Triangl15a, Triangl15b, Triangl15, Triangl21`
! * `Quadrangle, Quadrangle4, Quadrangle9, Quadrangle8`
! * `Tetrahedron, Tetrahedron4, Tetrahedron10, Tetrahedron20, Tetrahedron35, Tetrahedron56`
! * `Hexahedron, Hexahedron8, Hexahedron27, Hexahedron20, Hexahedron64, Hexahedron125`
! * `Prism, Prism6, Prism15, Prism18`
! * `Pyramid, Pyramid5, Pyramid14, Pyramid13`
! * `Point, Point1`
!
!### Usage
!
!```fortran
! type( ReferenceTopology_ ) :: obj
! obj = ReferenceTopology( Nptrs = [1,2,3], Name=Triangle3 )
! call display( obj, "obj=")
!```

INTERFACE
MODULE PURE FUNCTION refelem_ReferenceTopology( Nptrs, Name ) RESULT( obj )
  TYPE( ReferenceTopology_ ) :: obj
  INTEGER( I4B), INTENT( IN ) :: Nptrs( : )
  INTEGER( I4B), INTENT( IN ) :: Name
END FUNCTION refelem_ReferenceTopology
END INTERFACE

INTERFACE ReferenceTopology
  MODULE PROCEDURE refelem_ReferenceTopology
END INTERFACE ReferenceTopology

PUBLIC :: ReferenceTopology

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: 	This subroutine reset the instance of [[ReferenceTopology_]]
!
!### Usage
!
!```fortran
! type( ReferenceTopology_ ) :: obj
! obj = ReferenceTopology( Nptrs = [1,2,3], Name=Triangle3 )
! call display( obj, "obj=")
! call deallocatedata( obj ) !<------
!```

INTERFACE
MODULE PURE SUBROUTINE refelem_DeallocateData1( obj )
  CLASS( ReferenceTopology_ ), INTENT( INOUT ) :: obj
END SUBROUTINE refelem_DeallocateData1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: 	This routine deallocates the data stored inside the [[ReferenceElement_]]
!
!### Usage
!
!```fortran
!	todo
!```

INTERFACE
MODULE PURE SUBROUTINE refelem_DeallocateData2( obj )
  CLASS( ReferenceElement_ ), INTENT( INOUT ) :: obj
END SUBROUTINE refelem_DeallocateData2
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE refelem_DeallocateData1, refelem_DeallocateData2
END INTERFACE

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                           NNE@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: 	This function returns the totat nodes inside the referenc topology
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
MODULE PURE FUNCTION refelem_NNE1( obj ) RESULT( Ans )
  CLASS( ReferenceTopology_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans
END FUNCTION refelem_NNE1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           NNE@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: 	This function returns the total number of nodes in the reference element
!
!@todo
! usage
!@endtodo

INTERFACE
MODULE PURE FUNCTION refelem_NNE2( obj ) RESULT( Ans )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans
END FUNCTION refelem_NNE2
END INTERFACE

!----------------------------------------------------------------------------
!                                                            NNE@Constructor
!----------------------------------------------------------------------------

INTERFACE OPERATOR( .NNE. )
  MODULE PROCEDURE refelem_NNE1, refelem_NNE2
END INTERFACE

PUBLIC :: OPERATOR( .NNE. )

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	2 March 2021
! summary: 	This subroutine copies one reference element into other
!
!### Introduction
!
! This subroutine copies one reference element into other
! This subroutine also defines an assignment operator for `obj1=obj2` type opertions
!
!### Usage
!
!```fortran
!	todo
!```

INTERFACE
MODULE PURE SUBROUTINE refelem_Initiate1( obj, Anotherobj )
  CLASS( ReferenceElement_ ), INTENT( INOUT ) :: obj
  CLASS( ReferenceElement_ ), INTENT( IN ) :: Anotherobj
END SUBROUTINE refelem_Initiate1
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE refelem_Initiate1
END INTERFACE Initiate

PUBLIC :: Initiate

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE refelem_Initiate1
END INTERFACE

PUBLIC :: ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                   ReferenceElement_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION refelem_constructor_1( xidim, nsd, elemType ) RESULT( Ans )
  INTEGER( I4B ),INTENT( IN ) :: xidim, nsd, elemType
  CLASS( ReferenceElement_ ), POINTER :: ans
END FUNCTION refelem_constructor_1
END INTERFACE

INTERFACE ReferenceElement_Pointer
  MODULE PROCEDURE refelem_constructor_1
END INTERFACE ReferenceElement_Pointer

PUBLIC :: ReferenceElement_Pointer

!----------------------------------------------------------------------------
!                                                       getNptrs@Construcor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Returns the node numbers of reference element

INTERFACE
MODULE PURE FUNCTION refelem_getNptrs( obj ) RESULT( Ans )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION refelem_getNptrs
END INTERFACE

INTERFACE getConnectivity
  MODULE PROCEDURE refelem_getNptrs
END INTERFACE getConnectivity

PUBLIC :: getConnectivity

!----------------------------------------------------------------------------
!                                                       ElementType@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns element name in integer from element name
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Element_Type( ElemName ) RESULT( Ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: ElemName
  INTEGER( I4B ) :: Ans
END FUNCTION Element_Type
END INTERFACE

INTERFACE ElementType
  MODULE PROCEDURE Element_Type
END INTERFACE ElementType

PUBLIC :: ElementType

!----------------------------------------------------------------------------
!                                                       ElementName@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns element name in character from element number/type
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Element_Name( ElemType ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: ElemType
  CHARACTER( LEN = 50 ) :: Ans
END FUNCTION Element_Name
END INTERFACE

INTERFACE ElementName
  MODULE PROCEDURE Element_Name
END INTERFACE ElementName

PUBLIC :: ElementName

!----------------------------------------------------------------------------
!                                              TotalNodesInElement@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns total numbers of nodes present in a given element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Total_Nodes_In_Element(ElemType) RESULT( Ans )
  INTEGER( I4B ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION Total_Nodes_In_Element
END INTERFACE

INTERFACE TotalNodesInElement
  MODULE PROCEDURE Total_Nodes_In_Element
END INTERFACE TotalNodesInElement

PUBLIC :: TotalNodesInElement

!----------------------------------------------------------------------------
!                                                     ElementOrder@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns the order of an element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Element_Order( ElemType ) RESULT( Ans )
  INTEGER( I4B ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION Element_Order
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ElementOrder@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns the order of an element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Element_Order_RefElem( RefElem ) RESULT( Ans )
  INTEGER( I4B ) :: Ans
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
END FUNCTION Element_Order_RefElem
END INTERFACE

INTERFACE ElementOrder
  MODULE PROCEDURE Element_Order_RefElem, Element_Order
END INTERFACE ElementOrder

PUBLIC :: ElementOrder

!----------------------------------------------------------------------------
!                                                      XiDimension@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns the xidimension of an element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Elem_XiDimension( ElemType ) RESULT( Ans )
  INTEGER( I4B ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION Elem_XiDimension
END INTERFACE

INTERFACE XiDimension
  MODULE PROCEDURE Elem_XiDimension
END INTERFACE XiDimension

PUBLIC :: XiDimension

!----------------------------------------------------------------------------
!                                                          isVolume@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a volume element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isVolume( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isVolume
END INTERFACE

PUBLIC :: isVolume

!----------------------------------------------------------------------------
!                                                        isSurface@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a Surface element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isSurface( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isSurface
END INTERFACE

PUBLIC :: isSurface

!----------------------------------------------------------------------------
!                                                           isLine@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a Line element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isLine( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isLine
END INTERFACE

PUBLIC :: isLine

!----------------------------------------------------------------------------
!                                                          isPoint@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a Point element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isPoint( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isPoint
END INTERFACE

PUBLIC :: isPoint

!----------------------------------------------------------------------------
!                                                        isTriangle@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a Triangle element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isTriangle( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isTriangle
END INTERFACE

PUBLIC :: isTriangle

!----------------------------------------------------------------------------
!                                                      isQuadrangle@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a Quadrangle element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isQuadrangle( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isQuadrangle
END INTERFACE

PUBLIC :: isQuadrangle

!----------------------------------------------------------------------------
!                                                    isTetrahedron@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a Tetrahedron element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isTetrahedron( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isTetrahedron
END INTERFACE

PUBLIC :: isTetrahedron

!----------------------------------------------------------------------------
!                                                     isHexahedron@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a Hexahedron element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isHexahedron( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isHexahedron
END INTERFACE

PUBLIC :: isHexahedron

!----------------------------------------------------------------------------
!                                                            isPrism@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a Prism element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isPrism( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isPrism
END INTERFACE

PUBLIC :: isPrism

!----------------------------------------------------------------------------
!                                                         isPyramid@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a Pyramid element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isPyramid( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isPyramid
END INTERFACE

PUBLIC :: isPyramid

!----------------------------------------------------------------------------
!                                               isSerendipityElement@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a SerendipityElement element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isSerendipityElement( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isSerendipityElement
END INTERFACE

PUBLIC :: isSerendipityElement

!----------------------------------------------------------------------------
!                                                   ElementTopology@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1.  This will return the element topology
!     Line
!     Triangle
!     Quadrangle
!     Tetrahedron
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Elem_Topology( ElemType ) RESULT( Ans )
  INTEGER( I4B ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION Elem_Topology
END INTERFACE

INTERFACE ElementTopology
  MODULE PROCEDURE Elem_Topology
END INTERFACE ElementTopology

PUBLIC :: ElementTopology

!----------------------------------------------------------------------------
!                                                       FacetMatrix@Geometry
!----------------------------------------------------------------------------
INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
! Returns the facet matrix
! Number of rows are equal to the number of facet in an element
! Number of columns = MAX( NNS )
! First column => ElementTopology
! Second Column => XiDimension
! Third column => NNS
! 4 to NNS + 3 => Local Nptrs
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Facet_Matrix_RefElem( RefElem ) RESULT( FM )
  INTEGER( I4B ), ALLOCATABLE :: FM( :, : )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
END FUNCTION Facet_Matrix_RefElem
END INTERFACE

INTERFACE FacetMatrix
  MODULE PROCEDURE Facet_Matrix_RefElem
END INTERFACE FacetMatrix

PUBLIC :: FacetMatrix

!----------------------------------------------------------------------------
!                                                     FacetElements@Geometry
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	16 June 2021
! summary: This routine returns the facet elements

INTERFACE
MODULE PURE FUNCTION RefElem_FacetElements( RefElem ) RESULT( ans )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  TYPE( ReferenceElement_ ), ALLOCATABLE :: ans( : )
END FUNCTION RefElem_FacetElements
END INTERFACE

INTERFACE FacetElements
  MODULE PROCEDURE RefElem_FacetElements
END INTERFACE FacetElements

PUBLIC :: FacetElements

!----------------------------------------------------------------------------
!                                                    LocalNodeCoord@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
! This routine will be removed in near future
! This routine is not included in generic LocalNodeCoord routine
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE SUBROUTINE Local_NodeCoord( NodeCoord, ElemType )
  ! Define intent of dummy variables
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: NodeCoord( :, : )
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END SUBROUTINE Local_NodeCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                    LocalNodeCoord@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
! Returns the local NodeCoord of an element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Local_NodeCoord_RefElem( RefElem ) RESULT( NodeCoord )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), ALLOCATABLE :: NodeCoord( :, : )
END FUNCTION Local_NodeCoord_RefElem
END INTERFACE

INTERFACE LocalNodeCoord
  MODULE PROCEDURE Local_NodeCoord_RefElem
END INTERFACE LocalNodeCoord

PUBLIC :: LocalNodeCoord

!----------------------------------------------------------------------------
!                                                 MeasureSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Measure_Simplex( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex
END INTERFACE

INTERFACE MeasureSimplex
  MODULE PROCEDURE Measure_Simplex
END INTERFACE MeasureSimplex

PUBLIC :: MeasureSimplex

!----------------------------------------------------------------------------
!                                                    ElementQuality@Geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Element_Quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION Element_Quality
END INTERFACE

INTERFACE ElementQuality
  MODULE PROCEDURE Element_Quality
END INTERFACE ElementQuality

PUBLIC :: ElementQuality

!----------------------------------------------------------------------------
!                                                     ContainsPoint@geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION contains_point( refelem , xij, x ) RESULT( Ans )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij( :, : )
  REAL( DFP ), INTENT( IN ) :: x(:)
  LOGICAL( LGT ) :: Ans
END FUNCTION contains_point
END INTERFACE

INTERFACE ContainsPoint
  MODULE PROCEDURE contains_point
END INTERFACE ContainsPoint

PUBLIC :: ContainsPoint

!----------------------------------------------------------------------------
!                                             TotalEntities@Geometry
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	16 June 2021
! summary: 	Total entities present in an element

INTERFACE
MODULE PURE FUNCTION RefElem_TotalEntities( ElemType ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: ElemType
  INTEGER( I4B ) :: ans( 4 )
END FUNCTION RefElem_TotalEntities
END INTERFACE

INTERFACE TotalEntities
  MODULE PROCEDURE RefElem_TotalEntities
END INTERFACE TotalEntities

PUBLIC :: TotalEntities

!----------------------------------------------------------------------------
!                                                    FacetTopology@Geometry
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	16 June 2021
! summary: 	This routine returns the facet topology of the given element type

INTERFACE
MODULE PURE FUNCTION RefElem_FacetTopology( ElemType, Nptrs ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: ElemType
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
  TYPE( ReferenceTopology_ ), ALLOCATABLE :: ans( : )
END FUNCTION RefElem_FacetTopology
END INTERFACE

INTERFACE FacetTopology
  MODULE PROCEDURE RefElem_FacetTopology
END INTERFACE FacetTopology

PUBLIC :: FacetTopology

!----------------------------------------------------------------------------
!                                                          getVTKelementType
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE get_vtk_elemType( ElemType, vtk_type, nptrs )
  INTEGER( I4B ), INTENT( IN ) :: ElemType
  INTEGER( Int8 ), INTENT (OUT) :: vtk_type
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT) :: nptrs( : )
END SUBROUTINE get_vtk_elemType
END INTERFACE

INTERFACE getVTKelementType
  MODULE PROCEDURE get_vtk_elemType
END INTERFACE getVTKelementType

PUBLIC :: getVTKelementType

END MODULE ReferenceElement_Method