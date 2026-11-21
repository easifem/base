!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Set the physical shape data of cells, facets and space-time

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE ElemShapeData_Method, ONLY: ASSIGNMENT(=)
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
! physical cell: rectangle [0, 2] x [0, 1], so x = 1 + xi, y = (1 + eta) / 2
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
! physical facet: bottom edge from (0, 0) to (2, 0)
REAL(DFP), PARAMETER :: xfacet(2, 2) = RESHAPE([0.0_DFP, 0.0_DFP, &
                                                2.0_DFP, 0.0_DFP], [2, 2])
TYPE(ElemShapeData_) :: cell, facet
TYPE(QuadraturePoint_) :: quad, facetQuad

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL QP_Initiate(obj=facetQuad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL LagrangeElemShapeData(obj=facet, quad=facetQuad, nsd=2, xidim=1, &
                           elemType=Line2, refelemCoord=lineCoord, &
                           domainName="BIUNIT", order=1)

! Test 1: cell, Jacobian, js, dNdXt and coordinates in one call
BLOCK
  TYPE(ElemShapeData_) :: obj

  obj = cell
  CALL Set(obj=obj, val=xcell, N=obj%N, dNdXi=obj%dNdXi)
  CALL OK(ALL(ABS(obj%jacobian(2, 2, :) - 0.5_DFP) .LT. tol), &
          "cell: jacobian")
  CALL OK(ALL(ABS(obj%js - 0.5_DFP) .LT. tol), "cell: js")
  CALL OK(ALL(ABS(obj%dNdXt(:, 2, :) - 2.0_DFP * obj%dNdXi(:, 2, :)) &
              .LT. tol), "cell: dNdXt")
  CALL OK(ALL(ABS(obj%coord(1, :) - (1.0_DFP + quad%points(1, 1:4))) &
              .LT. tol), "cell: coord")
END BLOCK

! Test 2: facet and cell evaluated at the facet quadrature points
BLOCK
  TYPE(ElemShapeData_) :: facetObj, cellObj

  facetObj = facet
  ! cell shape data at the facet points: use the facet itself as the cell
  ! data with 2 points, only the transfer of normal, js and ws is checked
  cellObj = cell
  cellObj%nips = facet%nips
  CALL Set(facetobj=facetObj, cellobj=cellObj, cellval=xcell, &
           facetval=xfacet, cellN=cell%N(:, 1:2), facetN=facet%N, &
           celldNdXi=cell%dNdXi(:, :, 1:2), facetdNdXi=facet%dNdXi)
  CALL OK(ALL(ABS(facetObj%js - 1.0_DFP) .LT. tol), "facet: js = 1")
  CALL OK(ALL(ABS(facetObj%normal(2, :) + 1.0_DFP) .LT. tol), &
          "facet: normal = (0, -1)")
  CALL OK(ALL(ABS(cellObj%normal(2, 1:2) + 1.0_DFP) .LT. tol), &
          "cell: normal of the facet")
  CALL OK(ALL(ABS(cellObj%js(1:2) - 1.0_DFP) .LT. tol), &
          "cell: js of the facet")
  CALL OK(ALL(ABS(cellObj%ws(1:2) - facet%ws) .LT. tol), &
          "cell: ws of the facet")
END BLOCK

END PROGRAM main
