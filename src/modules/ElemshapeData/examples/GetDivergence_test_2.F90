!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Divergence of constant and space-time FEVariables

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
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
USE ElemShapeData_Method, ONLY: GetDivergence
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
! physical cell: rectangle [0, 2] x [0, 1]
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)
! space-time: vst = c(a) x on the line [0, 2] at time node a, c = [1, 3]
! so at time point ipt, div vst = T(1) * 1 + T(2) * 3
REAL(DFP) :: vst(1, 2, 2), mst(1, 1, 2, 2), c(2)
INTEGER(I4B) :: ipt

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)
vst(1, :, 1) = [0.0_DFP, 2.0_DFP]
vst(1, :, 2) = [0.0_DFP, 6.0_DFP]
mst(1, 1, :, :) = vst(1, :, :)

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
    stelemsd(ipt)%dNdXt(:, 1, :) = line%dNdXi(:, 1, :)
  END DO
END BLOCK

DO ipt = 1, 2
  c(ipt) = stelemsd(ipt)%T(1) * 1.0_DFP + stelemsd(ipt)%T(2) * 3.0_DFP
END DO

! Test 1: constant FEVariable gives zero
BLOCK
  REAL(DFP) :: ans2(2, 4)
  INTEGER(I4B) :: nrow, ncol
  TYPE(FEVariable_) :: d

  ans2 = 1.0_DFP
  CALL GetDivergence(obj=cell, val=NodalVariable(RESHAPE([1.0_DFP, &
                     2.0_DFP, 3.0_DFP, 4.0_DFP], [2, 2]), Matrix, &
                     Constant), ans=ans2, nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 4, "constant matrix: nrow, ncol")
  CALL OK(ALL(ans2 .EQ. 0.0_DFP), "constant matrix: zero")

  CALL GetDivergence(obj=cell, val=NodalVariable([1.0_DFP, 2.0_DFP], &
                                                 Vector, Constant), ans=d)
  CALL OK(d%rank .EQ. fevaropt%scalar, "constant vector out: scalar")
  CALL OK(ALL(d%val(1:4) .EQ. 0.0_DFP), "constant vector out: zero")
END BLOCK

! Test 2: space-time FEVariable into arrays, one time point
BLOCK
  REAL(DFP) :: ans(2), ans2(1, 2)
  INTEGER(I4B) :: tsize, nrow, ncol

  CALL GetDivergence(obj=stelemsd(2), val=NodalVariable(vst, Vector, &
                     SpaceTime), ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 2, "space-time vector: tsize = nips")
  CALL OK(ALL(ABS(ans - c(2)) .LT. tol), "space-time vector: values")

  CALL GetDivergence(obj=stelemsd(2), val=NodalVariable(mst, Matrix, &
                     SpaceTime), ans=ans2, nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 1 .AND. ncol .EQ. 2, "space-time matrix: nrow, ncol")
  CALL OK(ALL(ABS(ans2(1, :) - c(2)) .LT. tol), &
          "space-time matrix: values")
END BLOCK

! Test 3: matrix FEVariable at all time points
BLOCK
  TYPE(FEVariable_) :: d

  CALL GetDivergence(obj=stelemsd, val=NodalVariable(mst, Matrix, &
                                                     SpaceTime), ans=d)
  CALL OK(d%rank .EQ. fevaropt%vector, "all time points: vector result")
  CALL OK(d%varType .EQ. fevaropt%spacetime, "all time points: spaceTime")
  CALL OK(ALL(d%s(1:3) .EQ. [1, 2, 2]), "all time points: shape")
  CALL OK(ALL(ABS(d%val(1:2) - c(1)) .LT. tol), &
          "all time points: values at time point 1")
  CALL OK(ALL(ABS(d%val(3:4) - c(2)) .LT. tol), &
          "all time points: values at time point 2")
END BLOCK

END PROGRAM main
