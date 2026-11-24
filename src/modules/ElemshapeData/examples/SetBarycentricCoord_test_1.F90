!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Physical coordinates of quadrature points

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: SetBarycentricCoord
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

! Test 1: cell, x = 1 + xi and y = (1 + eta) / 2
BLOCK
  CALL SetBarycentricCoord(obj=cell, val=xcell, N=cell%N)
  CALL OK(ALL(ABS(cell%coord(1, :) - (1.0_DFP + quad%points(1, 1:4))) &
              .LT. tol), "cell: x = 1 + xi")
  CALL OK(ALL(ABS(cell%coord(2, :) - 0.5_DFP * (1.0_DFP + &
              quad%points(2, 1:4))) .LT. tol), "cell: y = (1 + eta) / 2")
END BLOCK

! Test 2: space-time form with coordinates that do not change in time
BLOCK
  REAL(DFP) :: xst(2, 2, 2)
  TYPE(STElemShapeData_) :: st

  CALL ALLOCATE (st, nsd=2, xidim=1, nns=2, nips=2, nnt=2)
  st%N = facet%N

  xst(:, :, 1) = xfacet
  xst(:, :, 2) = xfacet
  CALL SetBarycentricCoord(obj=st, val=xst, N=st%N, &
                           T=[0.25_DFP, 0.75_DFP])
  CALL OK(ALL(ABS(st%coord(1, :) - (1.0_DFP + facetQuad%points(1, &
              1:2))) .LT. tol), "space-time: x = 1 + xi")
  CALL OK(ALL(ABS(st%coord(2, :)) .LT. tol), "space-time: y = 0")
END BLOCK

END PROGRAM main
