title: QuadraturePoint
author: Vikas Sharma, Ph.D.
date: 23 July 2021

# Quadrature points

## Introduction

`QuadraturePoint_` data type contains quadrature points and corresponding weight. This data type is helpful in calculating the element shape data and finite element matrix and vector. The structure of this data type is given by:

```fortran
TYPE :: QuadraturePoint_
  REAL(DFP), ALLOCATABLE :: Points( :, : )
  INTEGER( I4B ) :: tXi = 0
END TYPE QuadraturePoint_
```

The methods related to this datatype is given in `QuadraturePoint_Method.f90`, which has following submodules.

- `@Constructor`
- `@IO`
- `@GaussLegendre`

The submodule `QuadraturePoint_Method@GaussLegendre.f90` contains following submodule.

- `@Line`, Order up to 23, and integration points upto 12
- `@Triangle`, order up to 11, and integration points upto 28
- `@Quadrangle` order up to 23, and integration points upto 144
- `@Tetrahedron` order up to 8, and integration points uptp 45
- `@Hexahedron` TODO
- `@Pyramid` TODO

## Constructor

To get Gauss Legendre quadrature points of a given order, we have a method called `GaussLegendreQuadrature()`, the interface of this function is given by:

```fortran
MODULE PURE FUNCTION GaussLegendreQuadrature( RefElem, Order ) RESULT( obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION GaussLegendreQuadrature
```

and another interface, which initiates quadrature point based on given NIPS, &
& is given by:

```fortran
MODULE PURE FUNCTION GaussLegendreQuadrature( RefElem, NIPS ) RESULT( obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION GaussLegendreQuadrature
```

Following is an example to construct the quadrature element.

```fortran
  type( ReferenceTriangle_ ) :: refelem
  type( QuadraturePoint_ ) :: obj
  call initiate( refelem, nsd = 2 )
  obj = GaussLegendreQuadrature(refelem, order=1)
  call display( obj, "1st order triangle")
  obj = GaussLegendreQuadrature(refelem, order=2)
  call display( obj, "2st order triangle")
  obj = GaussLegendreQuadrature(refelem, order=3)
  call display( obj, "3st order triangle")
```

