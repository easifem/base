!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Jacobian determinant js of cells and facets

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: SetJacobian
USE ElemShapeData_Method, ONLY: SetJs
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

! Test 1: cell, js = det(J) = 1/2
BLOCK
  CALL SetJacobian(obj=cell, val=xcell, dNdXi=cell%dNdXi)
  CALL SetJs(obj=cell)
  CALL OK(ALL(ABS(cell%js - 0.5_DFP) .LT. tol), "cell: js = 1/2")
  CALL OK(ABS(SUM(cell%js * cell%ws) - 2.0_DFP) .LT. tol, &
          "cell: area = 2")
END BLOCK

! Test 2: facet in 2D, js = |dx/dxi| = 1
BLOCK
  CALL SetJacobian(obj=facet, val=xfacet, dNdXi=facet%dNdXi)
  CALL SetJs(obj=facet)
  CALL OK(ALL(ABS(facet%js - 1.0_DFP) .LT. tol), "facet: js = 1")
  CALL OK(ABS(SUM(facet%js * facet%ws) - 2.0_DFP) .LT. tol, &
          "facet: length = 2")
END BLOCK

! Test 3: array form, surface in 3D, js = |J1 x J2|
BLOCK
  REAL(DFP) :: jacobian(3, 2, 1), js(1)

  jacobian(:, 1, 1) = [2.0_DFP, 0.0_DFP, 0.0_DFP]
  jacobian(:, 2, 1) = [0.0_DFP, 3.0_DFP, 0.0_DFP]
  CALL SetJs(jacobian=jacobian, js=js, nsd=3, xidim=2, nips=1)
  CALL OK(ABS(js(1) - 6.0_DFP) .LT. tol, "surface in 3D: js = 6")
END BLOCK

END PROGRAM main
