!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Function form of the divergence at all time points

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: Divergence
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: SetdNTdXt
USE ElemShapeData_Method, ONLY: SetJacobian
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
REAL(DFP), PARAMETER :: xst(1, 2, 2) = RESHAPE([0.0_DFP, 2.0_DFP, &
                                                0.0_DFP, 2.0_DFP], &
                                               [1, 2, 2])
TYPE(ElemShapeData_) :: line
TYPE(QuadraturePoint_) :: lquad
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)
! vst = c(a) x on the line [0, 2] at time node a, c = [1, 3]
! so at time point ipt, div vst = T(1) * 1 + T(2) * 3
REAL(DFP) :: vst(1, 2, 2), mst(1, 1, 2, 2), c(2)
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
  c(ipt) = stelemsd(ipt)%T(1) * 1.0_DFP + stelemsd(ipt)%T(2) * 3.0_DFP
END DO

vst(1, :, 1) = [0.0_DFP, 2.0_DFP]
vst(1, :, 2) = [0.0_DFP, 6.0_DFP]
mst(1, 1, :, :) = vst(1, :, :)

! Test 1: vector FEVariable gives a scalar at each time point
BLOCK
  TYPE(FEVariable_) :: d

  d = Divergence(obj=stelemsd, val=NodalVariable(vst, Vector, SpaceTime))
  CALL OK(d%rank .EQ. fevaropt%scalar, "vector: scalar result")
  CALL OK(ALL(ABS(d%val(1:2) - c(1)) .LT. tol), &
          "vector: values at time point 1")
  CALL OK(ALL(ABS(d%val(3:4) - c(2)) .LT. tol), &
          "vector: values at time point 2")
END BLOCK

! Test 2: matrix FEVariable gives a vector at each time point
BLOCK
  TYPE(FEVariable_) :: d

  d = Divergence(obj=stelemsd, val=NodalVariable(mst, Matrix, SpaceTime))
  CALL OK(d%rank .EQ. fevaropt%vector, "matrix: vector result")
  CALL OK(ALL(d%s(1:3) .EQ. [1, 2, 2]), "matrix: shape")
  CALL OK(ALL(ABS(d%val(1:2) - c(1)) .LT. tol), &
          "matrix: values at time point 1")
  CALL OK(ALL(ABS(d%val(3:4) - c(2)) .LT. tol), &
          "matrix: values at time point 2")
END BLOCK

END PROGRAM main
