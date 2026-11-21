!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Local gradient as a FEVariable, in space and at all time points

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
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
USE FEVariable_Method, ONLY: Get
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
REAL(DFP) :: v(2, 4), c(2)
INTEGER(I4B) :: ipt

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)

! vector (u, w) at the nodes of the cell
v(1, :) = u
v(2, :) = w

! space-time: line [0, 2] in space with two time nodes
BLOCK
  REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                    [1, 2])
  REAL(DFP), PARAMETER :: xst(1, 2, 2) = RESHAPE([0.0_DFP, 2.0_DFP, &
                                                  0.0_DFP, 2.0_DFP], &
                                                 [1, 2, 2])
  TYPE(ElemShapeData_) :: line
  TYPE(QuadraturePoint_) :: lquad

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

! at time point ipt: du/dxi = T1 c1 + T2 c2, because dx/dxi = 1
DO ipt = 1, 2
  c(ipt) = stelemsd(ipt)%T(1) * 1.0_DFP + stelemsd(ipt)%T(2) * 3.0_DFP
END DO

! Test 1: vector FEVariable in, quadrature matrix FEVariable out
BLOCK
  TYPE(FEVariable_) :: lg
  REAL(DFP), ALLOCATABLE :: r3(:, :, :)

  CALL GetLocalGradient(obj=cell, lg=lg, val=NodalVariable(v, Vector, Space))
  CALL OK(lg%rank .EQ. fevaropt%matrix, "vector in: rank is matrix")
  CALL OK(lg%defineOn .EQ. fevaropt%quadrature, &
          "vector in: defined on quadrature")
  CALL OK(lg%varType .EQ. fevaropt%space, "vector in: varType is space")
  CALL OK(ALL(lg%s(1:3) .EQ. [2, 2, 4]), &
          "vector in: shape [ncomp, xidim, nips]")
  r3 = Get(lg, Matrix, Space)
  CALL OK(ALL(ABS(r3(1, 1, :) - 1.0_DFP) .LT. tol), "vector in: du/dxi = 1")
  CALL OK(ALL(ABS(r3(1, 2, :) - 1.5_DFP) .LT. tol), &
          "vector in: du/deta = 3/2")
  CALL OK(ALL(ABS(r3(2, 1, :) - 2.0_DFP) .LT. tol), "vector in: dw/dxi = 2")
  CALL OK(ALL(ABS(r3(2, 2, :) + 0.5_DFP) .LT. tol), &
          "vector in: dw/deta = -1/2")
END BLOCK

! Test 2: scalar space-time FEVariable at one time point, real array out
BLOCK
  REAL(DFP), ALLOCATABLE :: lg(:, :)

  CALL GetLocalGradient(obj=stelemsd(1), lg=lg, &
                        val=NodalVariable(ust, Scalar, SpaceTime))
  CALL OK(ALL(SHAPE(lg) .EQ. [1, 2]), "scalar space-time: lg(xidim, nips)")
  CALL OK(ALL(ABS(lg(1, :) - c(1)) .LT. tol), &
          "scalar space-time: du/dxi = T1 c1 + T2 c2")
END BLOCK

! Test 3: matrix constant FEVariable gives zero
BLOCK
  REAL(DFP), ALLOCATABLE :: lg(:, :, :, :)
  REAL(DFP) :: m0(2, 3)

  m0 = 4.0_DFP
  CALL GetLocalGradient(obj=cell, lg=lg, val=NodalVariable(m0, Matrix, &
                                                            Constant))
  CALL OK(ALL(SHAPE(lg) .EQ. [2, 3, 2, 4]), &
          "matrix constant: lg(nrow, ncol, xidim, nips)")
  CALL OK(ALL(lg .EQ. 0.0_DFP), "matrix constant: gradient is zero")
END BLOCK

! Test 4: scalar FEVariable at all time points
BLOCK
  TYPE(FEVariable_) :: lg
  REAL(DFP), ALLOCATABLE :: r3(:, :, :)

  CALL GetLocalGradient(obj=stelemsd, lg=lg, &
                        val=NodalVariable(ust, Scalar, SpaceTime))
  CALL OK(lg%rank .EQ. fevaropt%vector, "all time points: rank is vector")
  CALL OK(lg%defineOn .EQ. fevaropt%quadrature, &
          "all time points: defined on quadrature")
  CALL OK(lg%varType .EQ. fevaropt%spacetime, &
          "all time points: varType is spaceTime")
  CALL OK(ALL(lg%s(1:3) .EQ. [1, 2, 2]), &
          "all time points: shape [xidim, nips, nipt]")
  r3 = Get(lg, Vector, SpaceTime)
  CALL OK(ALL(ABS(r3(1, :, 1) - c(1)) .LT. tol), &
          "all time points: du/dxi at time point 1")
  CALL OK(ALL(ABS(r3(1, :, 2) - c(2)) .LT. tol), &
          "all time points: du/dxi at time point 2")
END BLOCK

! Test 5: vector FEVariable at all time points
BLOCK
  TYPE(FEVariable_) :: lg
  REAL(DFP), ALLOCATABLE :: r4(:, :, :, :)
  REAL(DFP) :: vst(2, 2, 2)

  ! first component c(a) x, second component constant 5
  vst(1, :, :) = ust
  vst(2, :, :) = 5.0_DFP
  CALL GetLocalGradient(obj=stelemsd, lg=lg, &
                        val=NodalVariable(vst, Vector, SpaceTime))
  CALL OK(lg%rank .EQ. fevaropt%matrix, "vector all time: rank is matrix")
  CALL OK(lg%varType .EQ. fevaropt%spacetime, &
          "vector all time: varType is spaceTime")
  CALL OK(ALL(lg%s(1:4) .EQ. [2, 1, 2, 2]), &
          "vector all time: shape [ncomp, xidim, nips, nipt]")
  r4 = Get(lg, Matrix, SpaceTime)
  CALL OK(ALL(ABS(r4(1, 1, :, 1) - c(1)) .LT. tol), &
          "vector all time: dv1/dxi at time point 1")
  CALL OK(ALL(ABS(r4(1, 1, :, 2) - c(2)) .LT. tol), &
          "vector all time: dv1/dxi at time point 2")
  CALL OK(ALL(ABS(r4(2, 1, :, :)) .LT. tol), &
          "vector all time: dv2/dxi = 0")
END BLOCK

END PROGRAM main
