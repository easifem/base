!> author: Vikas Sharma, Ph. D.
! date: 2026-11-19
! summary: Projection of dN/dx on a vector at quadrature points, no allocation

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE ElemShapeData_Method, ONLY: GetProjectionOfdNdXt_
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])
! physical cell: rectangle [0, 2] x [0, 1], so x = 1 + xi, y = (1 + eta) / 2
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad
! nodal values of u = x + 3 y; with c = (1, 2), c . grad(u) = 7
REAL(DFP), PARAMETER :: u(4) = [0.0_DFP, 2.0_DFP, 5.0_DFP, 3.0_DFP]
REAL(DFP), PARAMETER :: c(2) = [1.0_DFP, 2.0_DFP]

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)

! Test 1: c(:, ips) at quadrature points, written into a larger array
BLOCK
  REAL(DFP) :: cq(2, 4), ans(5, 5)
  INTEGER(I4B) :: nrow, ncol, ips

  ! c = (1, 2) at the first two points and (0, 1) at the last two
  cq(:, 1:2) = SPREAD(c, 2, 2)
  cq(1, 3:4) = 0.0_DFP
  cq(2, 3:4) = 1.0_DFP
  ans = -1.0_DFP
  CALL GetProjectionOfdNdXt_(obj=cell, c=cq, ans=ans, nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 4 .AND. ncol .EQ. 4, "nrow = nns, ncol = nips")
  DO ips = 1, 2
    CALL OK(ABS(DOT_PRODUCT(u, ans(1:4, ips)) - 7.0_DFP) .LT. tol, &
            "c = (1, 2): c . grad(u) = 7")
  END DO
  DO ips = 3, 4
    CALL OK(ABS(DOT_PRODUCT(u, ans(1:4, ips)) - 3.0_DFP) .LT. tol, &
            "c = (0, 1): c . grad(u) = du/dy = 3")
  END DO
  CALL OK(ALL(ans(5, :) .EQ. -1.0_DFP), "row nns + 1 is not touched")
END BLOCK

END PROGRAM main
