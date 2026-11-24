!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Unit normal of a facet at quadrature points

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: SetJacobian
USE ElemShapeData_Method, ONLY: SetJs
USE ElemShapeData_Method, ONLY: SetNormal
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

! Test 1: bottom edge from (0, 0) to (2, 0), normal = (0, -1, 0)
BLOCK
  INTEGER(I4B) :: ips

  CALL SetJacobian(obj=facet, val=xfacet, dNdXi=facet%dNdXi)
  CALL SetJs(obj=facet)
  CALL SetNormal(obj=facet)
  DO ips = 1, facet%nips
    CALL OK(ALL(ABS(facet%normal(:, ips) - [0.0_DFP, -1.0_DFP, 0.0_DFP]) &
                .LT. tol), "bottom edge: normal = (0, -1, 0)")
  END DO
END BLOCK

! Test 2: edge from (2, 0) to (2, 1) of length 1, normal = (1, 0, 0)
BLOCK
  REAL(DFP) :: xright(2, 2)

  xright = RESHAPE([2.0_DFP, 0.0_DFP, 2.0_DFP, 1.0_DFP], [2, 2])
  CALL SetJacobian(obj=facet, val=xright, dNdXi=facet%dNdXi)
  CALL SetJs(obj=facet)
  CALL SetNormal(obj=facet)
  CALL OK(ALL(ABS(facet%js - 0.5_DFP) .LT. tol), "right edge: js = 1/2")
  CALL OK(ALL(ABS(facet%normal(:, 1) - [1.0_DFP, 0.0_DFP, 0.0_DFP]) &
              .LT. tol), "right edge: normal = (1, 0, 0)")
END BLOCK

END PROGRAM main
