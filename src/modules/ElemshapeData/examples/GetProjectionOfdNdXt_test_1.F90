!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Projection of the shape function gradient on a vector, c . dN/dx

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: GetProjectionOfdNdXt
USE ElemShapeData_Method, ONLY: GetProjectionOfdNdXt_
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
USE FEVariable_Method, ONLY: NodalVariable
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

! Test 1: constant vector c, with and without allocation
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :)
  REAL(DFP) :: ans2(5, 5), ans1(5)
  INTEGER(I4B) :: nrow, ncol, tsize

  CALL GetProjectionOfdNdXt(obj=cell, c=c, ans=ans)
  CALL OK(ALL(SHAPE(ans) .EQ. [4, 4]), "ans(nns, nips)")
  CALL OK(ALL(ABS(MATMUL(u, ans) - 7.0_DFP) .LT. tol), &
          "SUM_I u(I) ans(I, ips) = c . grad(u) = 7")
  CALL OK(ALL(ABS(SUM(ans, dim=1)) .LT. tol), "SUM_I ans(I, ips) = 0")

  CALL GetProjectionOfdNdXt_(obj=cell, c=c, ans=ans2, nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 4 .AND. ncol .EQ. 4, "no allocation: nrow, ncol")
  CALL OK(ALL(ABS(ans2(1:4, 1:4) - ans) .LT. tol), "no allocation: values")

  CALL GetProjectionOfdNdXt_(obj=cell, ips=3, c=c, ans=ans1, tsize=tsize)
  CALL OK(tsize .EQ. 4, "one point: tsize = nns")
  CALL OK(ALL(ABS(ans1(1:4) - ans(:, 3)) .LT. tol), "one point: values")
END BLOCK

! Test 2: c as a FEVariable and as values at quadrature points
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :)
  REAL(DFP) :: cq(2, 4), ans2(4, 4)
  INTEGER(I4B) :: nrow, ncol

  CALL GetProjectionOfdNdXt(obj=cell, &
                            c=NodalVariable(c, Vector, Constant), &
                            crank=Vector, ans=ans)
  CALL OK(ALL(ABS(MATMUL(u, ans) - 7.0_DFP) .LT. tol), &
          "FEVariable constant c")

  CALL GetProjectionOfdNdXt_(obj=cell, c=NodalVariable(RESHAPE([c, c, c, &
                             c], [2, 4]), Vector, Space), crank=Vector, &
                             ans=ans2, nrow=nrow, ncol=ncol)
  CALL OK(ALL(ABS(MATMUL(u, ans2) - 7.0_DFP) .LT. tol), &
          "FEVariable nodal space c")

  cq(1, :) = 1.0_DFP
  cq(2, :) = 2.0_DFP
  CALL GetProjectionOfdNdXt(obj=cell, c=cq, ans=ans)
  CALL OK(ALL(ABS(MATMUL(u, ans) - 7.0_DFP) .LT. tol), &
          "c at quadrature points")
END BLOCK

END PROGRAM main
