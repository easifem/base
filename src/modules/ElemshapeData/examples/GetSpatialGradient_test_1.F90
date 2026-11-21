!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Gradient with respect to the physical coordinates x

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: GetSpatialGradient
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
USE ElemShapeData_Method, ONLY: SetdNTdXt
USE ElemShapeData_Method, ONLY: SetJacobian
USE FEVariable_Method, ONLY: NodalVariable
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
! physical cell: rectangle [0, 2] x [0, 1], so x = 1 + xi, y = (1 + eta) / 2
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
! nodal values of u = x + 3 y and w = 2 x - y
REAL(DFP), PARAMETER :: u(4) = [0.0_DFP, 2.0_DFP, 5.0_DFP, 3.0_DFP]
REAL(DFP), PARAMETER :: w(4) = [0.0_DFP, 4.0_DFP, 3.0_DFP, -1.0_DFP]
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)
REAL(DFP) :: ust(2, 2)

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)


! space-time: line [0, 2] in space, u = c(a) x at time node a, c = [1, 3]
BLOCK
  REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                    [1, 2])
  REAL(DFP), PARAMETER :: xst(1, 2, 2) = RESHAPE([0.0_DFP, 2.0_DFP, &
                                                  0.0_DFP, 2.0_DFP], &
                                                 [1, 2, 2])
  TYPE(ElemShapeData_) :: line
  TYPE(QuadraturePoint_) :: lquad
  INTEGER(I4B) :: ipt

  CALL QP_Initiate(obj=lquad, elemType=Line2, domainName="BIUNIT", &
                   nips=[2], quadratureType=qpopt%GaussLegendre)
  CALL LagrangeElemShapeData(obj=line, quad=lquad, nsd=1, xidim=1, &
                             elemType=Line2, refelemCoord=lineCoord, &
                             domainName="BIUNIT", order=1)
  CALL Initiate(stelemsd, line)
  DO ipt = 1, 2
    CALL ALLOCATE (stelemsd(ipt), nsd=1, xidim=1, nns=2, nips=2, nnt=2)
    stelemsd(ipt)%T = line%N(:, ipt)
    stelemsd(ipt)%N = line%N
    stelemsd(ipt)%dNdXi = line%dNdXi
    CALL SetJacobian(obj=stelemsd(ipt), val=xst, dNdXi=line%dNdXi, &
                     T=stelemsd(ipt)%T)
    CALL SetdNTdXt(obj=stelemsd(ipt))
    stelemsd(ipt)%dNdXt(:, 1, :) = line%dNdXi(:, 1, :)
  END DO
END BLOCK
ust = RESHAPE([0.0_DFP, 2.0_DFP, 0.0_DFP, 6.0_DFP], [2, 2])

! Test 1: scalar, vector and matrix nodal values
BLOCK
  REAL(DFP), ALLOCATABLE :: g2(:, :), g3(:, :, :), g4(:, :, :, :)
  REAL(DFP) :: v(2, 4), m(2, 2, 4)

  CALL GetSpatialGradient(obj=cell, ans=g2, val=u)
  CALL OK(ALL(SHAPE(g2) .EQ. [2, 4]), "scalar: ans(nsd, nips)")
  CALL OK(ALL(ABS(g2(1, :) - 1.0_DFP) .LT. tol), "scalar: du/dx = 1")
  CALL OK(ALL(ABS(g2(2, :) - 3.0_DFP) .LT. tol), "scalar: du/dy = 3")

  v(1, :) = u
  v(2, :) = w
  CALL GetSpatialGradient(obj=cell, ans=g3, val=v)
  CALL OK(ALL(SHAPE(g3) .EQ. [2, 2, 4]), "vector: ans(ncomp, nsd, nips)")
  CALL OK(ALL(ABS(g3(2, 1, :) - 2.0_DFP) .LT. tol), "vector: dw/dx = 2")
  CALL OK(ALL(ABS(g3(2, 2, :) + 1.0_DFP) .LT. tol), "vector: dw/dy = -1")

  m(1, 1, :) = u
  m(2, 1, :) = w
  m(1, 2, :) = 0.0_DFP
  m(2, 2, :) = 1.0_DFP
  CALL GetSpatialGradient(obj=cell, ans=g4, val=m)
  CALL OK(ALL(SHAPE(g4) .EQ. [2, 2, 2, 4]), &
          "matrix: ans(nrow, ncol, nsd, nips)")
  CALL OK(ALL(ABS(g4(1, 1, 2, :) - 3.0_DFP) .LT. tol), &
          "matrix: dM11/dy = 3")
END BLOCK

! Test 2: FEVariable input
BLOCK
  REAL(DFP), ALLOCATABLE :: g2(:, :), g3(:, :, :), g4(:, :, :, :)
  REAL(DFP) :: v(2, 4), m(2, 2, 4)

  CALL GetSpatialGradient(obj=cell, ans=g2, &
                          val=NodalVariable(u, Scalar, Space), &
                          valRank=Scalar)
  CALL OK(ALL(ABS(g2(2, :) - 3.0_DFP) .LT. tol), "FEVariable: scalar")

  CALL GetSpatialGradient(obj=cell, ans=g2, &
                          val=NodalVariable(2.0_DFP, Scalar, Constant), &
                          valRank=Scalar)
  CALL OK(ALL(g2 .EQ. 0.0_DFP), "FEVariable: constant gives zero")

  v(1, :) = u
  v(2, :) = w
  CALL GetSpatialGradient(obj=cell, ans=g3, &
                          val=NodalVariable(v, Vector, Space), &
                          valRank=Vector)
  CALL OK(ALL(ABS(g3(2, 2, :) + 1.0_DFP) .LT. tol), "FEVariable: vector")

  m(1, 1, :) = u
  m(2, 1, :) = w
  m(1, 2, :) = 0.0_DFP
  m(2, 2, :) = 1.0_DFP
  CALL GetSpatialGradient(obj=cell, ans=g4, &
                          val=NodalVariable(m, Matrix, Space))
  CALL OK(ALL(ABS(g4(2, 1, 1, :) - 2.0_DFP) .LT. tol), &
          "FEVariable: matrix")
END BLOCK

! Test 3: space-time nodal values, du/dx = T1 c1 + T2 c2
BLOCK
  REAL(DFP), ALLOCATABLE :: g2(:, :)
  REAL(DFP) :: c

  CALL GetSpatialGradient(obj=stelemsd(1), ans=g2, val=ust)
  c = stelemsd(1)%T(1) * 1.0_DFP + stelemsd(1)%T(2) * 3.0_DFP
  CALL OK(ALL(SHAPE(g2) .EQ. [1, 2]), "space-time: ans(nsd, nips)")
  CALL OK(ALL(ABS(g2(1, :) - c) .LT. tol), "space-time: du/dx")

  CALL GetSpatialGradient(obj=stelemsd(1), ans=g2, &
                          val=NodalVariable(ust, Scalar, SpaceTime), &
                          valRank=Scalar)
  CALL OK(ALL(ABS(g2(1, :) - c) .LT. tol), "space-time: FEVariable")
END BLOCK

END PROGRAM main
