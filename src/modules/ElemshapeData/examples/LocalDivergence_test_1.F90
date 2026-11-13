!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Function form of the divergence with respect to xi

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: LocalDivergence
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
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)
! v = (x + 3 y, 2 x + 4 y): div v = 5, local divergence = 1 + 4 / 2 = 3
! M = [v1 v2; x 0]: row divergence [5, 1], local [3, 1]
REAL(DFP) :: v(2, 4), m(2, 2, 4)
! space-time: vst = c(a) x on the line [0, 2] at time node a, c = [1, 3]
REAL(DFP) :: vst(1, 2, 2), mst(1, 1, 2, 2)

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)
v(1, :) = [0.0_DFP, 2.0_DFP, 5.0_DFP, 3.0_DFP]
v(2, :) = [0.0_DFP, 4.0_DFP, 8.0_DFP, 4.0_DFP]
m(1, 1, :) = v(1, :)
m(1, 2, :) = v(2, :)
m(2, 1, :) = xcell(1, :)
m(2, 2, :) = 0.0_DFP
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

! Test 1: LocalDivergence of a vector FEVariable
BLOCK
  TYPE(FEVariable_) :: lg

  lg = LocalDivergence(obj=cell, val=NodalVariable(v, Vector, Space))
  CALL OK(lg%rank .EQ. fevaropt%scalar, "scalar result")
  CALL OK(ALL(ABS(lg%val(1:4) - 3.0_DFP) .LT. tol), &
          "local divergence = 3")
END BLOCK

! Test 2: space-time shape data at all time points
BLOCK
  TYPE(FEVariable_) :: lg

  lg = LocalDivergence(obj=stelemsd, &
                       val=NodalVariable(vst, Vector, SpaceTime))
  CALL OK(lg%varType .EQ. fevaropt%spacetime, "space-time: spaceTime")
  CALL OK(lg%len .EQ. 4, "space-time: len = nips * nipt")
END BLOCK

END PROGRAM main
