!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Derivatives of shape functions with respect to x

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: SetdNdXt
USE ElemShapeData_Method, ONLY: SetJacobian
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

! Test 1: cell, dN/dx = dN/dxi and dN/dy = 2 dN/deta
BLOCK
  CALL SetJacobian(obj=cell, val=xcell, dNdXi=cell%dNdXi)
  CALL SetdNdXt(obj=cell)
  CALL OK(ALL(ABS(cell%dNdXt(:, 1, :) - cell%dNdXi(:, 1, :)) .LT. tol), &
          "cell: dN/dx = dN/dxi")
  CALL OK(ALL(ABS(cell%dNdXt(:, 2, :) - 2.0_DFP * cell%dNdXi(:, 2, :)) &
              .LT. tol), "cell: dN/dy = 2 dN/deta")
  CALL OK(ALL(ABS(SUM(cell%dNdXt, dim=1)) .LT. tol), &
          "cell: sum of dN/dx = 0")
END BLOCK

! Test 2: facet (nsd /= xidim), dNdXt is set to zero
BLOCK
  CALL SetJacobian(obj=facet, val=xfacet, dNdXi=facet%dNdXi)
  facet%dNdXt = 1.0_DFP
  CALL SetdNdXt(obj=facet)
  CALL OK(ALL(facet%dNdXt .EQ. 0.0_DFP), "facet: dNdXt = 0")
END BLOCK

! Test 3: array form gives the same result
BLOCK
  REAL(DFP) :: dNdXt(4, 2, 4)

  CALL SetdNdXt(dNdXt=dNdXt, dNdXi=cell%dNdXi, jacobian=cell%jacobian, &
                xidim=2, nsd=2, nips=4, nns=4)
  CALL OK(ALL(ABS(dNdXt - cell%dNdXt) .LT. tol), "array form")
END BLOCK

END PROGRAM main
