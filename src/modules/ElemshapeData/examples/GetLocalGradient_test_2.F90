!> author: Vikas Sharma, Ph. D.
! date: 2026-11-19
! summary: Local gradient of vector and matrix space-time and FEVariable data

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: GetLocalGradient
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
! space-time values on the line [0, 2]: c(a) x at time node a, c = [1, 3]
REAL(DFP), PARAMETER :: ust(2, 2) = RESHAPE([0.0_DFP, 2.0_DFP, &
                                             0.0_DFP, 6.0_DFP], [2, 2])
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)
REAL(DFP) :: v(2, 4), m(2, 2, 4), c

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)

! vector (u, w) and matrix [u, 0; w, 1] at the nodes of the cell
v(1, :) = u
v(2, :) = w
m(1, 1, :) = u
m(2, 1, :) = w
m(1, 2, :) = 0.0_DFP
m(2, 2, :) = 1.0_DFP

! space-time: line [0, 2] in space with two time nodes
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
  END DO
END BLOCK

! at time point 2: du/dxi = (T1 c1 + T2 c2) dx/dxi, with dx/dxi = 1
c = stelemsd(2)%T(1) * 1.0_DFP + stelemsd(2)%T(2) * 3.0_DFP

! Test 1: vector space-time nodal values, val(ncomp, nns, nnt)
BLOCK
  REAL(DFP), ALLOCATABLE :: lg(:, :, :)
  REAL(DFP) :: vst(2, 2, 2)

  ! first component c(a) x, second component constant 5
  vst(1, :, :) = ust
  vst(2, :, :) = 5.0_DFP
  CALL GetLocalGradient(obj=stelemsd(2), lg=lg, val=vst)
  CALL OK(ALL(SHAPE(lg) .EQ. [2, 1, 2]), &
          "vector space-time: lg(ncomp, xidim, nips)")
  CALL OK(ALL(ABS(lg(1, 1, :) - c) .LT. tol), &
          "vector space-time: dv1/dxi = T1 c1 + T2 c2")
  CALL OK(ALL(ABS(lg(2, 1, :)) .LT. tol), &
          "vector space-time: dv2/dxi = 0")
END BLOCK

! Test 2: vector FEVariable, nodal space, constant and space-time
BLOCK
  REAL(DFP), ALLOCATABLE :: lg(:, :, :)
  REAL(DFP) :: vst(2, 2, 2)

  CALL GetLocalGradient(obj=cell, lg=lg, val=NodalVariable(v, Vector, Space))
  CALL OK(ALL(SHAPE(lg) .EQ. [2, 2, 4]), &
          "vector FEVariable: lg(ncomp, xidim, nips)")
  CALL OK(ALL(ABS(lg(1, 2, :) - 1.5_DFP) .LT. tol), &
          "vector FEVariable: du/deta = 3/2")
  CALL OK(ALL(ABS(lg(2, 1, :) - 2.0_DFP) .LT. tol), &
          "vector FEVariable: dw/dxi = 2")

  CALL GetLocalGradient(obj=cell, lg=lg, &
                        val=NodalVariable([1.0_DFP, 2.0_DFP], Vector, &
                                          Constant))
  CALL OK(ALL(SHAPE(lg) .EQ. [2, 2, 4]), "vector constant: shape")
  CALL OK(ALL(lg .EQ. 0.0_DFP), "vector constant: gradient is zero")

  vst(1, :, :) = ust
  vst(2, :, :) = 5.0_DFP
  CALL GetLocalGradient(obj=stelemsd(2), lg=lg, &
                        val=NodalVariable(vst, Vector, SpaceTime))
  CALL OK(ALL(ABS(lg(1, 1, :) - c) .LT. tol), &
          "vector space-time FEVariable: dv1/dxi")
END BLOCK

! Test 3: matrix space-time nodal values, val(nrow, ncol, nns, nnt)
BLOCK
  REAL(DFP), ALLOCATABLE :: lg(:, :, :, :)
  REAL(DFP) :: mst(2, 2, 2, 2)

  ! M11 = c(a) x, M22 = 7, other entries zero
  mst = 0.0_DFP
  mst(1, 1, :, :) = ust
  mst(2, 2, :, :) = 7.0_DFP
  CALL GetLocalGradient(obj=stelemsd(2), lg=lg, val=mst)
  CALL OK(ALL(SHAPE(lg) .EQ. [2, 2, 1, 2]), &
          "matrix space-time: lg(nrow, ncol, xidim, nips)")
  CALL OK(ALL(ABS(lg(1, 1, 1, :) - c) .LT. tol), &
          "matrix space-time: dM11/dxi = T1 c1 + T2 c2")
  CALL OK(ALL(ABS(lg(2, 2, 1, :)) .LT. tol), &
          "matrix space-time: dM22/dxi = 0")
END BLOCK

! Test 4: matrix FEVariable, nodal space and space-time
BLOCK
  REAL(DFP), ALLOCATABLE :: lg(:, :, :, :)
  REAL(DFP) :: mst(2, 2, 2, 2)

  CALL GetLocalGradient(obj=cell, lg=lg, val=NodalVariable(m, Matrix, Space))
  CALL OK(ALL(SHAPE(lg) .EQ. [2, 2, 2, 4]), &
          "matrix FEVariable: lg(nrow, ncol, xidim, nips)")
  CALL OK(ALL(ABS(lg(1, 1, 2, :) - 1.5_DFP) .LT. tol), &
          "matrix FEVariable: dM11/deta = 3/2")
  CALL OK(ALL(ABS(lg(2, 1, 1, :) - 2.0_DFP) .LT. tol), &
          "matrix FEVariable: dM21/dxi = 2")
  CALL OK(ALL(ABS(lg(2, 2, :, :)) .LT. tol), "matrix FEVariable: dM22 = 0")

  mst = 0.0_DFP
  mst(1, 1, :, :) = ust
  CALL GetLocalGradient(obj=stelemsd(2), lg=lg, &
                        val=NodalVariable(mst, Matrix, SpaceTime))
  CALL OK(ALL(ABS(lg(1, 1, 1, :) - c) .LT. tol), &
          "matrix space-time FEVariable: dM11/dxi")
END BLOCK

END PROGRAM main
