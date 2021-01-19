# `ReferenceElement` in `EASIFEM`

## Introduction

In `BaseType` following datatype has been defined.

### `ReferenceTopology_`

```fortran
TYPE :: ReferenceTopology_
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )
  INTEGER( I4B ) :: Name, XiDimension
END TYPE ReferenceTopology_

PUBLIC :: ReferenceTopology_

TYPE :: ReferenceTopologyPointer_
  CLASS( ReferenceTopology_ ), POINTER :: Ptr => NULL( )
END TYPE ReferenceTopologyPointer_

PUBLIC :: ReferenceTopologyPointer_
```

### `ReferenceElement_`

```fortran
TYPE :: ReferenceElement_
  REAL( DFP ), ALLOCATABLE :: XiJ( :, : )
  INTEGER( I4B ) :: EntityCounts( 4 )
  INTEGER( I4B ) :: XiDimension, Name, Order, NSD
  TYPE( ReferenceTopology_ ), ALLOCATABLE :: Topology( : )
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: LagrangePoints => lp_refelem
  PROCEDURE, PUBLIC, PASS( Obj ) :: LagrangeElement => lag_elem_refelem
END TYPE ReferenceElement_

PUBLIC :: ReferenceElement_

TYPE :: ReferenceElementPointer_
  CLASS( ReferenceElement_ ), POINTER :: Ptr => NULL( )
END TYPE ReferenceElementPointer_

PUBLIC :: ReferenceElementPointer_

INTERFACE
  MODULE PURE FUNCTION lp_refelem( Obj, Order ) RESULT( Ans )
    CLASS( ReferenceElement_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Order
    REAL( DFP ), ALLOCATABLE :: Ans( :, : )
  END FUNCTION lp_refelem

  MODULE FUNCTION lag_elem_refelem(Obj, Order) RESULT( Ans )
    CLASS( ReferenceElement_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Order
    CLASS( ReferenceElement_ ), POINTER :: Ans
  END FUNCTION lag_elem_refelem
END INTERFACE
```

Following children of `RefereceElement_` are defined

- `ReferenceLine_`
- `ReferenceTriangle_`
- `ReferenceQuadrangle_`
- `ReferenceTetrahedron_`
- `ReferenceHexahedron_`
- `ReferencePrism_`
- `ReferencePyramid_`

Methods related to the `ReferenceElement_` are in module called `ReferenceElement_Method.f90`, which contains following submodules.

- `@Constructor`
- `@VTK`
- `@Lagrange`
- `@Geometry`

## `@Geometry` submodule

It contains following routines

- `Element_Name`
- `Element_Type`
- `Total_Nodes_In_Element`
- `Element_Order`
- `Element_Order_RefElem`
- `Elem_XiDimension`
- `isVolume`
- `isSurface`
- `isLine`
- `isPoint`
- `isTriangle`
- `isQuadrangle`
- `isTetrahedron`
- `isHexahedron`
- `isPrism`
- `isPyramid`
- `isSerendipityElement`
- `Elem_Topology`
- `Facet_Matrix_RefElem`
- `Local_NodeCoord`
- `Local_NodeCoord_RefElem`
- `Measure_Simplex_Line`
- `Measure_Simplex_Triangle`
- `Measure_Simplex_Quadrangle`
- `Measure_Simplex_Tetrahedron`
- `Measure_Simplex_Hexahedron`
- `Measure_Simplex_Prism`
- `Measure_Simplex_Pyramid`
- `Measure_Simplex`
-