MODULE ReferenceElement_Method
USE BaseType
USE GlobalData

IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                              ReferenceTopology@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_topology( Nptrs, Name ) RESULT( Obj )
  TYPE( ReferenceTopology_ ) :: Obj
  INTEGER( I4B), INTENT( IN ) :: Nptrs( : ), Name
END FUNCTION reference_topology
END INTERFACE

INTERFACE ReferenceTopology
  MODULE PROCEDURE reference_topology
END INTERFACE ReferenceTopology

PUBLIC :: ReferenceTopology

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

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
INTERFACE
MODULE PURE FUNCTION tNodes_RefTopo( Obj ) RESULT( Ans )
  CLASS( ReferenceTopology_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
END FUNCTION tNodes_RefTopo

MODULE PURE FUNCTION tNodes_RefElem( Obj ) RESULT( Ans )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
END FUNCTION tNodes_RefElem
END INTERFACE

INTERFACE OPERATOR( .NNE. )
  MODULE PROCEDURE tNodes_RefTopo, tNodes_RefElem
END INTERFACE

PUBLIC :: OPERATOR( .NNE. )

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------
INTERFACE
MODULE PURE SUBROUTINE deallocatedata_ref_elem( Obj )
  CLASS( ReferenceElement_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE deallocatedata_ref_elem
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE deallocatedata_ref_elem
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Display@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE display_ref_elem( Obj, msg, unitno )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitno
END SUBROUTINE display_ref_elem

MODULE SUBROUTINE display_ref_topo( Obj, msg, unitno )
  CLASS( ReferenceTopology_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitno
END SUBROUTINE display_ref_topo
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE display_ref_elem, display_ref_topo
END INTERFACE Display

PUBLIC :: Display

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

INTERFACE
!  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
! 1. Subroutine for constructing linear reference line object
!  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
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

INTERFACE ReferenceQuadrangle_Pointer
  MODULE PROCEDURE reference_Quadrangle_Pointer
END INTERFACE ReferenceQuadrangle_Pointer

PUBLIC :: ReferenceQuadrangle, ReferenceQuadrangle_Pointer

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------
INTERFACE
!  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
! 1. Subroutine for constructing the object
!  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
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

INTERFACE ReferenceTetrahedron_Pointer
  MODULE PROCEDURE reference_Tetrahedron_Pointer
END INTERFACE ReferenceTetrahedron_Pointer

PUBLIC :: ReferenceTetrahedron, ReferenceTetrahedron_Pointer

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------
INTERFACE
!  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
! 1. Subroutine for constructing the object
!  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
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

INTERFACE ReferenceHexahedron_Pointer
  MODULE PROCEDURE reference_Hexahedron_Pointer
END INTERFACE ReferenceHexahedron_Pointer

PUBLIC :: ReferenceHexahedron, ReferenceHexahedron_Pointer

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------
INTERFACE
!  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
! 1. Subroutine for constructing the object
!  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
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
!                                            ReferencePyramid@Constructor
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

INTERFACE ReferencePyramid_Pointer
  MODULE PROCEDURE reference_Pyramid_Pointer
END INTERFACE ReferencePyramid_Pointer

PUBLIC :: ReferencePyramid, ReferencePyramid_Pointer

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------
INTERFACE
!  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
! 1. Subroutine for constructing the object
!  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
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

INTERFACE ReferencePrism_Pointer
  MODULE PROCEDURE reference_Prism_Pointer
END INTERFACE ReferencePrism_Pointer

PUBLIC :: ReferencePrism, ReferencePrism_Pointer

!----------------------------------------------------------------------------
!                                                    LagrangePoints@Lagrange
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1. Returns equidistant points on [-1,1] for lagrange interpolation
!		2, Nodecoord is a 2D array with 3 rows
!		3. first row is xi, second row is eta, third row is zeta
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
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

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
! Returns lagrange line element of different order
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
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

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1. Returns equidistant points on triangle for lagrange interpolation
!		2, Nodecoord is a 2D array with 3 rows
!		3. first row is xi, second row is eta, third row is zeta
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
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

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
! Returns lagrange Triangle element of different order
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
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
!
!----------------------------------------------------------------------------

#include "./line.inc"
#include "./triangle.inc"
#include "./quadrangle.inc"
#include "./tetrahedron.inc"
#include "./hexahedron.inc"
#include "./prism.inc"
#include "./pyramid.inc"

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

!----------------------------------------------------------------------------
!                                                                 Contains
!----------------------------------------------------------------------------

! there are some routines which should be described inside the module
! rather than submodules. those routines should be described inside
! the contains.part
! These routines mainly returns pointers.

CONTAINS

!----------------------------------------------------------------------------
!                                                      ReferenceLine_Pointer
!----------------------------------------------------------------------------

PURE FUNCTION reference_Line_Pointer(NSD, XiJ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferenceLine_ ), POINTER :: Obj
  ALLOCATE( Obj )
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END FUNCTION reference_Line_Pointer

!----------------------------------------------------------------------------
!                                                      ReferenceTriangle_Pointer
!----------------------------------------------------------------------------

PURE FUNCTION reference_Triangle_Pointer(NSD, XiJ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferenceTriangle_ ), POINTER :: Obj
  ALLOCATE( Obj )
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END FUNCTION reference_Triangle_Pointer

!----------------------------------------------------------------------------
!                                                      ReferenceQuadrangle_Pointer
!----------------------------------------------------------------------------

PURE FUNCTION reference_Quadrangle_Pointer(NSD, XiJ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferenceQuadrangle_ ), POINTER :: Obj
  ALLOCATE( Obj )
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END FUNCTION reference_Quadrangle_Pointer

!----------------------------------------------------------------------------
!                                               ReferenceTetrahedron_Pointer
!----------------------------------------------------------------------------

PURE FUNCTION reference_Tetrahedron_Pointer(NSD, XiJ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferenceTetrahedron_ ), POINTER :: Obj
  ALLOCATE( Obj )
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END FUNCTION reference_Tetrahedron_Pointer

!----------------------------------------------------------------------------
!                                               ReferenceHexahedron_Pointer
!----------------------------------------------------------------------------

PURE FUNCTION reference_Hexahedron_Pointer(NSD, XiJ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferenceHexahedron_ ), POINTER :: Obj
  ALLOCATE( Obj )
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END FUNCTION reference_Hexahedron_Pointer

!----------------------------------------------------------------------------
!                                               ReferencePyramid_Pointer
!----------------------------------------------------------------------------

PURE FUNCTION reference_Pyramid_Pointer(NSD, XiJ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferencePyramid_ ), POINTER :: Obj
  ALLOCATE( Obj )
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END FUNCTION reference_Pyramid_Pointer

!----------------------------------------------------------------------------
!                                               ReferencePrism_Pointer
!----------------------------------------------------------------------------

PURE FUNCTION reference_Prism_Pointer(NSD, XiJ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferencePrism_ ), POINTER :: Obj
  ALLOCATE( Obj )
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END FUNCTION reference_Prism_Pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ReferenceElement_Method