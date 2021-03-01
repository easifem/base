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
MODULE SUBROUTINE display_ref_elem( Obj, msg, unitno )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitno
END SUBROUTINE display_ref_elem
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE display_ref_topo( Obj, msg, unitno )
  CLASS( ReferenceTopology_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitno
END SUBROUTINE display_ref_topo
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Display@Constructor
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
MODULE PURE FUNCTION reference_topology( Nptrs, Name ) RESULT( Obj )
  TYPE( ReferenceTopology_ ) :: Obj
  INTEGER( I4B), INTENT( IN ) :: Nptrs( : )
  INTEGER( I4B), INTENT( IN ) :: Name
END FUNCTION reference_topology
END INTERFACE

INTERFACE ReferenceTopology
  MODULE PROCEDURE reference_topology
END INTERFACE ReferenceTopology

PUBLIC :: ReferenceTopology

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: 	This subroutine reset the instance of [[ReferenceTopology_]]

INTERFACE
MODULE PURE SUBROUTINE deallocatedata_ref_topology( Obj )
  CLASS( ReferenceTopology_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE deallocatedata_ref_topology
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE deallocatedata_ref_topology
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
MODULE PURE FUNCTION tNodes_RefTopo( Obj ) RESULT( Ans )
  CLASS( ReferenceTopology_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
END FUNCTION tNodes_RefTopo
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
MODULE PURE FUNCTION tNodes_RefElem( Obj ) RESULT( Ans )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
END FUNCTION tNodes_RefElem
END INTERFACE

!----------------------------------------------------------------------------
!                                                            NNE@Constructor
!----------------------------------------------------------------------------

INTERFACE OPERATOR( .NNE. )
  MODULE PROCEDURE tNodes_RefTopo, tNodes_RefElem
END INTERFACE

PUBLIC :: OPERATOR( .NNE. )

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
!	CALL DeallocateData(Obj)
!```

INTERFACE
MODULE PURE SUBROUTINE deallocatedata_ref_elem( Obj )
  CLASS( ReferenceElement_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE deallocatedata_ref_elem
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE deallocatedata_ref_elem
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE init_refelem( Obj, AnotherObj )
  CLASS( ReferenceElement_ ), INTENT( INOUT ) :: Obj
  CLASS( ReferenceElement_ ), INTENT( IN ) :: AnotherObj
END SUBROUTINE init_refelem
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE init_refelem
END INTERFACE Initiate

PUBLIC :: Initiate

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE init_refelem
END INTERFACE

PUBLIC :: ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: This subroutine for constructing linear reference line object

INTERFACE
MODULE PURE SUBROUTINE initiate_ref_Line( Obj, NSD, XiJ )
  CLASS( ReferenceLine_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
END SUBROUTINE initiate_ref_Line
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Line
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                 ReferenceLine@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_line(NSD, XiJ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferenceLine_ ) :: Obj
END FUNCTION reference_line
END INTERFACE

INTERFACE ReferenceLine
  MODULE PROCEDURE reference_line
END INTERFACE ReferenceLine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_line_pointer(NSD, XiJ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferenceLine_ ), POINTER :: Obj
END FUNCTION reference_line_pointer
END INTERFACE

INTERFACE ReferenceLine_Pointer
  MODULE PROCEDURE reference_line_Pointer
END INTERFACE ReferenceLine_Pointer

PUBLIC :: ReferenceLine, ReferenceLine_Pointer

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE initiate_ref_Triangle( Obj, NSD, XiJ )
  CLASS( ReferenceTriangle_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
END SUBROUTINE initiate_ref_Triangle
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Triangle
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                             ReferenceTriangle@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Triangle(NSD, XiJ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferenceTriangle_ ) :: Obj
END FUNCTION reference_Triangle
END INTERFACE

INTERFACE ReferenceTriangle
  MODULE PROCEDURE reference_Triangle
END INTERFACE ReferenceTriangle

!----------------------------------------------------------------------------
!                                     ReferenceTriangle_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Triangle_pointer(NSD, XiJ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferenceTriangle_ ), POINTER :: Obj
END FUNCTION reference_Triangle_pointer
END INTERFACE

INTERFACE ReferenceTriangle_Pointer
  MODULE PROCEDURE reference_Triangle_Pointer
END INTERFACE ReferenceTriangle_Pointer

PUBLIC :: ReferenceTriangle, ReferenceTriangle_Pointer

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE initiate_ref_Quadrangle( Obj, NSD, XiJ )
  CLASS( ReferenceQuadrangle_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
END SUBROUTINE initiate_ref_Quadrangle
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Quadrangle
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                            ReferenceQuadrangle@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Quadrangle( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferenceQuadrangle_ ) :: Obj
END FUNCTION reference_Quadrangle
END INTERFACE

INTERFACE ReferenceQuadrangle
  MODULE PROCEDURE reference_Quadrangle
END INTERFACE ReferenceQuadrangle

!----------------------------------------------------------------------------
!                                   ReferenceQuadrangle_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Quadrangle_Pointer( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferenceQuadrangle_ ), POINTER :: Obj
END FUNCTION reference_Quadrangle_Pointer
END INTERFACE

INTERFACE ReferenceQuadrangle_Pointer
  MODULE PROCEDURE reference_Quadrangle_Pointer
END INTERFACE ReferenceQuadrangle_Pointer

PUBLIC :: ReferenceQuadrangle, ReferenceQuadrangle_Pointer

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: This subroutine for constructing the object

INTERFACE
MODULE PURE SUBROUTINE initiate_ref_Tetrahedron( Obj, NSD, XiJ )
  CLASS( ReferenceTetrahedron_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
END SUBROUTINE initiate_ref_Tetrahedron
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Tetrahedron
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                            ReferenceTetrahedron@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Tetrahedron( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferenceTetrahedron_ ) :: Obj
END FUNCTION reference_Tetrahedron
END INTERFACE

INTERFACE ReferenceTetrahedron
  MODULE PROCEDURE reference_Tetrahedron
END INTERFACE ReferenceTetrahedron

!----------------------------------------------------------------------------
!                                   ReferenceTetrahedron_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Tetrahedron_Pointer( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferenceTetrahedron_ ), POINTER :: Obj
END FUNCTION reference_Tetrahedron_Pointer
END INTERFACE

INTERFACE ReferenceTetrahedron_Pointer
  MODULE PROCEDURE reference_Tetrahedron_Pointer
END INTERFACE ReferenceTetrahedron_Pointer

PUBLIC :: ReferenceTetrahedron, ReferenceTetrahedron_Pointer

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: This subroutine for constructing the object

INTERFACE
MODULE PURE SUBROUTINE initiate_ref_Hexahedron( Obj, NSD, XiJ )
  CLASS( ReferenceHexahedron_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
END SUBROUTINE initiate_ref_Hexahedron
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Hexahedron
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                            ReferenceHexahedron@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Hexahedron( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferenceHexahedron_ ) :: Obj
END FUNCTION reference_Hexahedron
END INTERFACE

INTERFACE ReferenceHexahedron
  MODULE PROCEDURE reference_Hexahedron
END INTERFACE ReferenceHexahedron

!----------------------------------------------------------------------------
!                                    ReferenceHexahedron_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Hexahedron_Pointer( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferenceHexahedron_ ), POINTER :: Obj
END FUNCTION reference_Hexahedron_Pointer
END INTERFACE

INTERFACE ReferenceHexahedron_Pointer
  MODULE PROCEDURE reference_Hexahedron_Pointer
END INTERFACE ReferenceHexahedron_Pointer

PUBLIC :: ReferenceHexahedron, ReferenceHexahedron_Pointer

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: This subroutine for constructing the object

INTERFACE
MODULE PURE SUBROUTINE initiate_ref_Pyramid( Obj, NSD, XiJ )
  CLASS( ReferencePyramid_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
END SUBROUTINE initiate_ref_Pyramid
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Pyramid
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               ReferencePyramid@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Pyramid( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferencePyramid_ ) :: Obj
END FUNCTION reference_Pyramid
END INTERFACE

INTERFACE ReferencePyramid
  MODULE PROCEDURE reference_Pyramid
END INTERFACE ReferencePyramid

!----------------------------------------------------------------------------
!                                      ReferencePyramid_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Pyramid_Pointer( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferencePyramid_ ), POINTER :: Obj
END FUNCTION reference_Pyramid_Pointer
END INTERFACE

INTERFACE ReferencePyramid_Pointer
  MODULE PROCEDURE reference_Pyramid_Pointer
END INTERFACE ReferencePyramid_Pointer

PUBLIC :: ReferencePyramid, ReferencePyramid_Pointer

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: This subroutine for constructing the object

INTERFACE
MODULE PURE SUBROUTINE initiate_ref_Prism( Obj, NSD, XiJ )
  CLASS( ReferencePrism_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
END SUBROUTINE initiate_ref_Prism
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Prism
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                ReferencePrism@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Prism( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferencePrism_ ) :: Obj
END FUNCTION reference_Prism
END INTERFACE

INTERFACE ReferencePrism
  MODULE PROCEDURE reference_Prism
END INTERFACE ReferencePrism

!----------------------------------------------------------------------------
!                                        ReferencePrism_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Prism_Pointer( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferencePrism_ ), POINTER :: Obj
END FUNCTION reference_Prism_Pointer
END INTERFACE

INTERFACE ReferencePrism_Pointer
  MODULE PROCEDURE reference_Prism_Pointer
END INTERFACE ReferencePrism_Pointer

PUBLIC :: ReferencePrism, ReferencePrism_Pointer

!----------------------------------------------------------------------------
!                                                    LagrangePoints@Lagrange
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: This function returns equidistance lagrange points on a line
!
!### Introduction
! * Returns equidistant points on [-1,1] for lagrange interpolation
!	* Nodecoord is a 2D array with 3 rows
!	* First row is xi, second row is eta, third row is zeta

INTERFACE
MODULE PURE FUNCTION EquidistanceLIP_Line( RefElem, Order ) &
  & RESULT( NodeCoord )
  CLASS( ReferenceLine_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  REAL( DFP ), ALLOCATABLE :: NodeCoord( :, : )
END FUNCTION EquidistanceLIP_Line
END INTERFACE

INTERFACE LagrangePoints
  MODULE PROCEDURE EquidistanceLIP_Line
END INTERFACE LagrangePoints

PUBLIC :: LagrangePoints

!----------------------------------------------------------------------------
!                                                  LagrangeElement@Lagrange
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: This function returns lagrange element on line
!
!### Introduction
! Returns lagrange line element of different order

INTERFACE
MODULE PURE FUNCTION LagrangeElement_Line( RefElem, Order ) RESULT( Obj )
  CLASS( ReferenceLine_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferenceLine_ ) :: Obj
END FUNCTION LagrangeElement_Line
END INTERFACE

INTERFACE LagrangeElement
  MODULE PROCEDURE LagrangeElement_Line
END INTERFACE LagrangeElement

PUBLIC :: LagrangeElement

!----------------------------------------------------------------------------
!                                                   LagrangePoints@Lagrange
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: 	This subroutine generate Lagrange points on triangle
!
!### Introduction
! * Returns equidistant points on triangle for lagrange interpolation
!	* Nodecoord is a 2D array with 3 rows
!	* first row is xi, second row is eta, third row is zeta

INTERFACE
MODULE PURE FUNCTION EquidistanceLIP_Triangle( RefElem, Order ) &
  & RESULT( NodeCoord )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  REAL( DFP ), ALLOCATABLE :: NodeCoord( :, : )
END FUNCTION EquidistanceLIP_Triangle
END INTERFACE

INTERFACE LagrangePoints
  MODULE PROCEDURE EquidistanceLIP_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   LagrangeElement@Lagrange
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Returns lagrange Triangle element of different order

INTERFACE
MODULE PURE FUNCTION LagrangeElement_Triangle( RefElem, Order ) RESULT( Obj )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferenceTriangle_ ) :: Obj
END FUNCTION LagrangeElement_Triangle
END INTERFACE

INTERFACE LagrangeElement
  MODULE PROCEDURE LagrangeElement_Triangle
END INTERFACE LagrangeElement

!----------------------------------------------------------------------------
!                                                    LagrangePoints@Lagrange
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1. Returns equidistant points on [-1,1] for lagrange interpolation
!		2, Nodecoord is a 2D array with 3 rows
!		3. first row is xi, second row is eta, third row is zeta
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION EquidistanceLIP_Quadrangle( RefElem, Order ) &
  & RESULT( NodeCoord )
  CLASS( ReferenceQuadrangle_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  REAL( DFP ), ALLOCATABLE :: NodeCoord( :, : )
END FUNCTION EquidistanceLIP_Quadrangle
END INTERFACE

INTERFACE LagrangePoints
  MODULE PROCEDURE EquidistanceLIP_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   LagrangeElement@Lagrange
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
! Returns lagrange Quadrangle element of different order
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION LagrangeElement_Quadrangle( RefElem, Order ) &
  & RESULT( Obj )
  CLASS( ReferenceQuadrangle_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferenceQuadrangle_ ) :: Obj
END FUNCTION LagrangeElement_Quadrangle
END INTERFACE

INTERFACE LagrangeElement
  MODULE PROCEDURE LagrangeElement_Quadrangle
END INTERFACE LagrangeElement

!----------------------------------------------------------------------------
!                                                   LagrangePoints@Lagrange
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1. Returns equidistant points on [-1,1] for lagrange interpolation
!		2, Nodecoord is a 2D array with 3 rows
!		3. first row is xi, second row is eta, third row is zeta
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION EquidistanceLIP_Tetrahedron( RefElem, Order ) &
  & RESULT( NodeCoord )
  CLASS( ReferenceTetrahedron_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  REAL( DFP ), ALLOCATABLE :: NodeCoord( :, : )
END FUNCTION EquidistanceLIP_Tetrahedron
END INTERFACE

INTERFACE LagrangePoints
  MODULE PROCEDURE EquidistanceLIP_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                   LagrangeElement@Lagrange
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
! Returns lagrange Tetrahedron element of different order
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION LagrangeElement_Tetrahedron( RefElem, Order ) &
  & RESULT( Obj )
  CLASS( ReferenceTetrahedron_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferenceTetrahedron_ ) :: Obj
END FUNCTION LagrangeElement_Tetrahedron
END INTERFACE

INTERFACE LagrangeElement
  MODULE PROCEDURE LagrangeElement_Tetrahedron
END INTERFACE LagrangeElement

!----------------------------------------------------------------------------
!                                                   LagrangePoints@Lagrange
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1. Returns equidistant points on [-1,1] for lagrange interpolation
!		2, Nodecoord is a 2D array with 3 rows
!		3. first row is xi, second row is eta, third row is zeta
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION EquidistanceLIP_Prism( RefElem, Order ) &
  & RESULT( NodeCoord )
  CLASS( ReferencePrism_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  REAL( DFP ), ALLOCATABLE :: NodeCoord( :, : )
END FUNCTION EquidistanceLIP_Prism
END INTERFACE

INTERFACE LagrangePoints
  MODULE PROCEDURE EquidistanceLIP_Prism
END INTERFACE

!----------------------------------------------------------------------------
!                                                   LagrangeElement@Lagrange
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
! Returns lagrange Prism element of different order
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION LagrangeElement_Prism( RefElem, Order ) RESULT( Obj )
  CLASS( ReferencePrism_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferencePrism_ ) :: Obj
END FUNCTION LagrangeElement_Prism
END INTERFACE

INTERFACE LagrangeElement
  MODULE PROCEDURE LagrangeElement_Prism
END INTERFACE LagrangeElement

!----------------------------------------------------------------------------
!                                                   LagrangePoints@Lagrange
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1. Returns equidistant points on [-1,1] for lagrange interpolation
!   2, Nodecoord is a 2D array with 3 rows
!   3. first row is xi, second row is eta, third row is zeta
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION EquidistanceLIP_Pyramid( RefElem, Order ) &
  & RESULT( NodeCoord )
  CLASS( ReferencePyramid_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  REAL( DFP ), ALLOCATABLE :: NodeCoord( :, : )
END FUNCTION EquidistanceLIP_Pyramid
END INTERFACE

INTERFACE LagrangePoints
  MODULE PROCEDURE EquidistanceLIP_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                  LagrangeElement@Lagrange
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
! Returns lagrange Pyramid element of different order
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION LagrangeElement_Pyramid( RefElem, Order ) RESULT( Obj )
  CLASS( ReferencePyramid_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferencePyramid_ ) :: Obj
END FUNCTION LagrangeElement_Pyramid
END INTERFACE

INTERFACE LagrangeElement
  MODULE PROCEDURE LagrangeElement_Pyramid
END INTERFACE LagrangeElement

!----------------------------------------------------------------------------
!                                                   LagrangePoints@Lagrange
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1. Returns equidistant points on [-1,1] for lagrange interpolation
!		2, Nodecoord is a 2D array with 3 rows
!		3. first row is xi, second row is eta, third row is zeta
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION EquidistanceLIP_Hexahedron( RefElem, Order ) &
  & RESULT( NodeCoord )
  CLASS( ReferenceHexahedron_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  REAL( DFP ), ALLOCATABLE :: NodeCoord( :, : )
END FUNCTION EquidistanceLIP_Hexahedron
END INTERFACE

INTERFACE LagrangePoints
  MODULE PROCEDURE EquidistanceLIP_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                   LagrangeElement@Lagrange
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
! Returns lagrange Hexahedron element of different order
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION LagrangeElement_Hexahedron( RefElem, Order ) RESULT( Obj )
  CLASS( ReferenceHexahedron_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferenceHexahedron_ ) :: Obj
END FUNCTION LagrangeElement_Hexahedron
END INTERFACE

INTERFACE LagrangeElement
  MODULE PROCEDURE LagrangeElement_Hexahedron
END INTERFACE LagrangeElement

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
!                                                  MeasureOfSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Measure_Simplex_Line( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferenceLine_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                          line_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION line_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferenceLine_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION line_quality
END INTERFACE


!----------------------------------------------------------------------------
!                                                 MeasureOfSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Measure_Simplex_Triangle( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Triangle_Angle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_angles( refelem, xij ) RESULT( Ans )
  CLASS(ReferenceTriangle_), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(3)
END FUNCTION triangle_angles
END INTERFACE

INTERFACE Angles
  MODULE PROCEDURE triangle_angles
END INTERFACE Angles

PUBLIC :: Angles

!----------------------------------------------------------------------------
!                                                             Triangle_Area
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_area( refelem, xij ) RESULT( Ans )
  CLASS(ReferenceTriangle_), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans
END FUNCTION triangle_area
END INTERFACE

INTERFACE Area
  MODULE PROCEDURE triangle_area
END INTERFACE Area

PUBLIC :: Area

!----------------------------------------------------------------------------
!                                                        Triangle_AreaVector
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_areaVector( refelem, xij ) RESULT( Ans )
  CLASS(ReferenceTriangle_), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans( 3 )
END FUNCTION triangle_areaVector
END INTERFACE

INTERFACE AreaVector
  MODULE PROCEDURE triangle_areaVector
END INTERFACE AreaVector

PUBLIC :: AreaVector

!----------------------------------------------------------------------------
!                                                       Triangle_Barycentric
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_barycentric( refelem, xij, x ) RESULT( Ans )
  CLASS(ReferenceTriangle_), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ), INTENT( IN ) :: x( : )
  REAL( DFP ) :: Ans( 3 )
END FUNCTION triangle_barycentric
END INTERFACE

INTERFACE Barycentric
  MODULE PROCEDURE triangle_barycentric
END INTERFACE Barycentric

PUBLIC :: Barycentric

!----------------------------------------------------------------------------
!                                                          Triangle_Centroid
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_centroid( refelem, xij ) RESULT( Ans )
  CLASS(ReferenceTriangle_), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(3)
END FUNCTION triangle_centroid
END INTERFACE

INTERFACE Centroid
  MODULE PROCEDURE triangle_centroid
END INTERFACE Centroid

PUBLIC :: Centroid

!----------------------------------------------------------------------------
!                                                      Triangle_CircumCenter
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_circumcentre(  refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(3)
END FUNCTION triangle_circumcentre
END INTERFACE

INTERFACE CircumCenter
  MODULE PROCEDURE triangle_circumcentre
END INTERFACE CircumCenter

PUBLIC :: CircumCenter

!----------------------------------------------------------------------------
!                                                       Triangle_CircumCircle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_circumcircle( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(4)
    !! Ans(1) = radius and Ans(2:4) center
END FUNCTION triangle_circumcircle
END INTERFACE

INTERFACE CircumCircle
  MODULE PROCEDURE triangle_circumcircle
END INTERFACE CircumCircle

PUBLIC :: CircumCircle

!----------------------------------------------------------------------------
!                                                       Triangle_CircumRadius
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_circumradius( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans
END FUNCTION triangle_circumradius
END INTERFACE

INTERFACE CircumRadius
  MODULE PROCEDURE triangle_circumradius
END INTERFACE CircumRadius

PUBLIC :: CircumRadius

!----------------------------------------------------------------------------
!                                              triangle_contains_line
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE triangle_contains_line( refelem, xij, x1, x2, &
  & parametricLine, inside, xint )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:), x1(3), x2(3)
  LOGICAL(LGT), INTENT( IN ) :: parametricLine
  LOGICAL(LGT), INTENT (OUT) :: inside
  REAL( DFP ), INTENT( OUT ) :: xint(3)
END SUBROUTINE triangle_contains_line
END INTERFACE

INTERFACE ContainsLine
  MODULE PROCEDURE triangle_contains_line
END INTERFACE ContainsLine

PUBLIC :: ContainsLine

!----------------------------------------------------------------------------
!                                                   triangle_contains_point
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_contains_point( refelem, xij, x ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:), x(:)
  LOGICAL(LGT) :: Ans
END FUNCTION triangle_contains_point
END INTERFACE

!----------------------------------------------------------------------------
!                                                          triangle_diameter
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_diameter( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans
END FUNCTION triangle_diameter
END INTERFACE

INTERFACE diameter
  MODULE PROCEDURE triangle_diameter
END INTERFACE diameter

PUBLIC :: diameter

!----------------------------------------------------------------------------
!                                                       triangle_edge_length
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_edge_length( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(3)
END FUNCTION triangle_edge_length
END INTERFACE

INTERFACE EdgeLength
  MODULE PROCEDURE triangle_edge_length
END INTERFACE EdgeLength

PUBLIC :: EdgeLength

!----------------------------------------------------------------------------
!                                                         triangle_incenter
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_incenter( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(3)
END FUNCTION triangle_incenter
END INTERFACE

INTERFACE Incenter
  MODULE PROCEDURE triangle_incenter
END INTERFACE Incenter

PUBLIC :: Incenter

!----------------------------------------------------------------------------
!                                                         triangle_incircle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_incircle( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(4)
END FUNCTION triangle_incircle
END INTERFACE

INTERFACE Incircle
  MODULE PROCEDURE triangle_incircle
END INTERFACE Incircle

PUBLIC :: Incircle

!----------------------------------------------------------------------------
!                                                         triangle_inradius
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_inradius( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans
END FUNCTION triangle_inradius
END INTERFACE

INTERFACE Inradius
  MODULE PROCEDURE triangle_inradius
END INTERFACE Inradius

PUBLIC :: Inradius

!----------------------------------------------------------------------------
!                                                      triangle_orthocenter
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_orthocenter( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans( 3 )
END FUNCTION triangle_orthocenter
END INTERFACE

INTERFACE Orthocenter
  MODULE PROCEDURE triangle_orthocenter
END INTERFACE Orthocenter

PUBLIC :: Orthocenter

!----------------------------------------------------------------------------
!                                                        triangle_point_dist
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_point_dist( refelem, xij, x ) &
  & RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:), x(:)
  REAL( DFP ) :: Ans
END FUNCTION triangle_point_dist
END INTERFACE

INTERFACE DistanceFromPoint
  MODULE PROCEDURE triangle_point_dist
END INTERFACE DistanceFromPoint

PUBLIC :: DistanceFromPoint

!----------------------------------------------------------------------------
!                                                    triangle_nearest_point
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE triangle_get_nearest_point( refelem, xij, x, xn, dist )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:), x(:)
  REAL( DFP ) , INTENT (INOUT) :: xn(:)
  REAL( DFP ), INTENT (OUT) :: dist
END SUBROUTINE triangle_get_nearest_point
END INTERFACE

INTERFACE NearestPoint
  MODULE PROCEDURE triangle_get_nearest_point
END INTERFACE NearestPoint

PUBLIC :: NearestPoint

!----------------------------------------------------------------------------
!                                                     triangle_random_point
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_random_point( refelem, xij, n, seed ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ) :: n, seed
  REAL( DFP ) :: Ans(3, n)
END FUNCTION triangle_random_point
END INTERFACE

INTERFACE RandomPoint
  MODULE PROCEDURE triangle_random_point
END INTERFACE RandomPoint

PUBLIC :: RandomPoint

!----------------------------------------------------------------------------
!                                                          triangle_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION triangle_quality
END INTERFACE

!----------------------------------------------------------------------------
!                                                 MeasureOfSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Measure_Simplex_Quadrangle( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferenceQuadrangle_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Quadrangle_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Quadrangle_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferenceQuadrangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION Quadrangle_quality
END INTERFACE

!----------------------------------------------------------------------------
!                                                 MeasureOfSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Measure_Simplex_Tetrahedron( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferenceTetrahedron_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                       tetrahedron_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION tetrahedron_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( Referencetetrahedron_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION tetrahedron_quality
END INTERFACE

!----------------------------------------------------------------------------
!                                                 MeasureOfSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Measure_Simplex_Hexahedron( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferenceHexahedron_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Hexahedron_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Hexahedron_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferenceHexahedron_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION Hexahedron_quality
END INTERFACE

!----------------------------------------------------------------------------
!                                                  MeasureOfSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Measure_Simplex_Prism( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferencePrism_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Prism
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Prism_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Prism_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferencePrism_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION Prism_quality
END INTERFACE


!----------------------------------------------------------------------------
!                                                 MeasureOfSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Measure_Simplex_Pyramid( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferencePyramid_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Pyramid_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Pyramid_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferencePyramid_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION Pyramid_quality
END INTERFACE

!----------------------------------------------------------------------------
!                                                 MeasureOfSimplex@Geometry
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