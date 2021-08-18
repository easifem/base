title: ElemshapeData
author: Vikas Sharma, Ph.D.
date: 23 July 2021

# `ElemshapeData`: Element Shape Data

## Introduction

`ElemshapeData_` datatype contains the shape function data at the integration points of finite elements. These elements can be spatial, temporal, and/or spatial-temporal elements. The structure of this data type is given below.

```fortran
TYPE :: ElemShapeData_
  REAL( DFP ), ALLOCATABLE :: N( :, : )
    !! Shape function value `N(I,ips)`
  REAL( DFP ), ALLOCATABLE :: dNdXi( :, :, : )
    !! Local derivative of a shape function
  REAL( DFP ), ALLOCATABLE :: Jacobian( :, :, : )
    !! Jacobian of mapping `J(:,:,ips)` also $\mathbf{F}_{\Xi x}$
  REAL( DFP ), ALLOCATABLE :: Js( : )
    !! Determinant of Jacobian at ips
  REAL( DFP ), ALLOCATABLE :: Ws( : )
    !! Weighting functions
  REAL( DFP ), ALLOCATABLE :: dNdXt( :, :, : )
    !! Spatial derivative of shape function
  REAL( DFP ), ALLOCATABLE :: Thickness( : )
    !! Thickness of element
  REAL( DFP ), ALLOCATABLE :: Coord( :, : )
    !! Barycentric coordinate
  REAL( DFP ), ALLOCATABLE :: Normal( :, : )
    !! Normal in case of facet element
  TYPE( ReferenceElement_ ) :: RefElem !=> NULL()
    !! Refererece element
  TYPE( QuadraturePoint_ ) :: Quad
    !! Quadrature points
END TYPE ElemShapeData_
```

## Constructor methods

- One of the essential parameters for defining the shape functions is **continuity of the shape functions**, which is described below.
  - `H1_`: standard finite element shape functions (C0 continuous)
  - `H1DIV_`: shape functions for mixed finite element: Todo
  - `H1CURL_`: shape functions for mixed finite element: Todo
  - `DG_`: shape function for discontinous Galerkin method: Todo
- Another important information to complete the information stored inside an instance of `ElemshapeData_` is `QuadraturePoint_`. The quadrature data is necessary in numerical integration of finite element matrix and vectors.
- Information of the reference element is also required to construct the element shape data
- Lastly, information about the the Interpolation type is also necessary

These informations are provided in a method called `initiate()`, which is described below.

```fortran
INTERFACE
MODULE PURE SUBROUTINE H1_Lagrange( obj, quad, refElem, &
  & continuityType, interpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: quad
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refElem
  CLASS( H1_ ), INTENT( IN ) :: continuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: interpolType
END SUBROUTINE H1_Lagrange
END INTERFACE
```

## Structure of the module

The main module is `ElemshapeData_Method.f90`, which contains following submodules

- `@Constructor.f90`
- `@IO.f90`
- `@getMethod.f90`
- `@setMethod.f90`
- `H1Lagrange.f90`
- `H1DIVLagrange.f90` Todo
- `H1CURLLagrange.f90` Todo
- `DGLagrange.f90` Todo

The submodule `ElemshapeData_Method@H1Lagrange.f90` contains the H1 lagrange polynomial type shape functions, and has following submodules corresponding to the geometry of the reference element.

- `@Line.f90` Arbitrary order
- `@Triangle.f90` upto 3rd order
- `@Quadrangle.f90` upto 1st order
- `@Tetrahedron.f90` Todo
- `@Hexahedron.f90` Todo
- `@Prism.f90` Todo
- `@Pyramid.f90` Todo
