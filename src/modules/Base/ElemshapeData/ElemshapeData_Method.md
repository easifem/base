# Introduction to `ElementShapeData_` data type

## Theory

## Structure of data type

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
  TYPE( ReferenceElement_ ) :: RefElem
    !! Refererece element
  TYPE( QuadraturePoint_ ) :: Quad
    !! Quadrature points
END TYPE ElemShapeData_
```

## Examples

```fortran
spaceQuad = GaussLegendreQuadrature( refelem=elem%refelem, &
  & order=2*(elem%refelem%order-1) )

CALL initiate( obj = spacesd, quad = spacequad, &
  & refelem = elem % refelem, &
  & ContinuityType= Obj % SpaceContinuity, &
  & InterpolType = Obj %  SpaceInterpol, &
  & Order = Order )
```