!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Function form of the local gradient at all time points

PROGRAM main
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
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: LocalGradient
USE ElemShapeData_Method, ONLY: SetdNTdXt
USE ElemShapeData_Method, ONLY: SetJacobian
USE FEVariable_Method, ONLY: Get
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! space-time values on the line [0, 2]: c(a) x at time node a, c = [1, 3]
REAL(DFP), PARAMETER :: ust(2, 2) = RESHAPE([0.0_DFP, 2.0_DFP, &
                                             0.0_DFP, 6.0_DFP], [2, 2])
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)
REAL(DFP) :: c(2)
INTEGER(I4B) :: ipt

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

! Test 1: scalar space-time FEVariable at all time points
BLOCK
  TYPE(FEVariable_) :: lg
  REAL(DFP), ALLOCATABLE :: r3(:, :, :)

  lg = LocalGradient(obj=stelemsd, val=NodalVariable(ust, Scalar, SpaceTime))
  CALL OK(lg%rank .EQ. fevaropt%vector, "scalar: rank is vector")
  CALL OK(lg%defineOn .EQ. fevaropt%quadrature, &
          "scalar: defined on quadrature")
  CALL OK(lg%varType .EQ. fevaropt%spacetime, "scalar: varType is spaceTime")
  CALL OK(ALL(lg%s(1:3) .EQ. [1, 2, 2]), &
          "scalar: shape [xidim, nips, nipt]")
  r3 = Get(lg, Vector, SpaceTime)
  CALL OK(ALL(ABS(r3(1, :, 1) - c(1)) .LT. tol), &
          "scalar: du/dxi at time point 1")
  CALL OK(ALL(ABS(r3(1, :, 2) - c(2)) .LT. tol), &
          "scalar: du/dxi at time point 2")
END BLOCK

! Test 2: scalar space FEVariable, same gradient at every time point
BLOCK
  TYPE(FEVariable_) :: lg
  REAL(DFP), ALLOCATABLE :: r3(:, :, :)

  ! u = 3 x on the line [0, 2], so du/dxi = 3
  lg = LocalGradient(obj=stelemsd, &
                     val=NodalVariable([0.0_DFP, 6.0_DFP], Scalar, Space))
  CALL OK(lg%varType .EQ. fevaropt%spacetime, &
          "space in: varType is spaceTime")
  r3 = Get(lg, Vector, SpaceTime)
  CALL OK(ALL(ABS(r3 - 3.0_DFP) .LT. tol), "space in: du/dxi = 3")
END BLOCK

! Test 3: vector space-time FEVariable at all time points
BLOCK
  TYPE(FEVariable_) :: lg
  REAL(DFP), ALLOCATABLE :: r4(:, :, :, :)
  REAL(DFP) :: vst(2, 2, 2)

  ! first component c(a) x, second component constant 5
  vst(1, :, :) = ust
  vst(2, :, :) = 5.0_DFP
  lg = LocalGradient(obj=stelemsd, val=NodalVariable(vst, Vector, SpaceTime))
  CALL OK(lg%rank .EQ. fevaropt%matrix, "vector: rank is matrix")
  CALL OK(ALL(lg%s(1:4) .EQ. [2, 1, 2, 2]), &
          "vector: shape [ncomp, xidim, nips, nipt]")
  r4 = Get(lg, Matrix, SpaceTime)
  CALL OK(ALL(ABS(r4(1, 1, :, 2) - c(2)) .LT. tol), &
          "vector: dv1/dxi at time point 2")
  CALL OK(ALL(ABS(r4(2, 1, :, :)) .LT. tol), "vector: dv2/dxi = 0")
END BLOCK

END PROGRAM main
