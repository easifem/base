!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Local divergence: constant matrix, FEVariable output, no-op case

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
USE ElemShapeData_Method, ONLY: GetLocalDivergence
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
! v = (x + 3 y, 2 x + 4 y): local divergence = 1 + 4 / 2 = 3
REAL(DFP), PARAMETER :: v(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                           4.0_DFP, 5.0_DFP, 8.0_DFP, &
                                           3.0_DFP, 4.0_DFP], [2, 4])
! space-time: c(a) x on the line [0, 2] at time node a, c = [1, 3]
REAL(DFP), PARAMETER :: vst(1, 2, 2) = RESHAPE([0.0_DFP, 2.0_DFP, &
                                                0.0_DFP, 6.0_DFP], &
                                               [1, 2, 2])
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)

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

! Test 1: a constant matrix FEVariable gives zero rows
BLOCK
  REAL(DFP), ALLOCATABLE :: lg(:, :)

  CALL GetLocalDivergence(obj=cell, lg=lg, &
                          val=NodalVariable(RESHAPE([1.0_DFP, 2.0_DFP, &
                                                     3.0_DFP, 4.0_DFP], &
                                                    [2, 2]), &
                                            Matrix, Constant))
  CALL OK(ALL(SHAPE(lg) .EQ. [2, 4]), "constant matrix: lg(nrow, nips)")
  CALL OK(ALL(lg .EQ. 0.0_DFP), "constant matrix: zero")
END BLOCK

! Test 2: vector FEVariable gives a scalar quadrature FEVariable
BLOCK
  TYPE(FEVariable_) :: lg

  CALL GetLocalDivergence(obj=cell, lg=lg, &
                          val=NodalVariable(v, Vector, Space))
  CALL OK(lg%rank .EQ. fevaropt%scalar, "vector in: scalar out")
  CALL OK(lg%varType .EQ. fevaropt%space, "vector in: space out")
  CALL OK(lg%defineOn .EQ. fevaropt%quadrature, "vector in: quadrature")
  CALL OK(lg%len .EQ. 4, "vector in: len = nips")
  CALL OK(ALL(ABS(lg%val(1:4) - 3.0_DFP) .LT. tol), &
          "vector in: local divergence = 3")
END BLOCK

! Test 3: matrix FEVariable at all time points, shape [nrow, nips, nipt]
BLOCK
  TYPE(FEVariable_) :: lg
  REAL(DFP) :: mst(1, 1, 2, 2), c(2)
  INTEGER(I4B) :: ipt

  mst(1, 1, :, :) = vst(1, :, :)
  DO ipt = 1, 2
    c(ipt) = stelemsd(ipt)%T(1) * 1.0_DFP + stelemsd(ipt)%T(2) * 3.0_DFP
  END DO
  CALL GetLocalDivergence(obj=stelemsd, lg=lg, &
                          val=NodalVariable(mst, Matrix, SpaceTime))
  CALL OK(lg%rank .EQ. fevaropt%vector, "matrix in: vector out")
  CALL OK(lg%varType .EQ. fevaropt%spacetime, "matrix in: spaceTime out")
  CALL OK(lg%len .EQ. 4, "matrix in: len = nrow * nips * nipt")
  CALL OK(ALL(ABS(lg%val(1:2) - c(1)) .LT. tol), &
          "matrix in: time point 1")
  CALL OK(ALL(ABS(lg%val(3:4) - c(2)) .LT. tol), &
          "matrix in: time point 2")
END BLOCK

! Test 4: space-time values with a plain ElemShapeData_ leave lg as is
BLOCK
  REAL(DFP), ALLOCATABLE :: lg(:)

  lg = [7.0_DFP]
  CALL GetLocalDivergence(obj=cell, lg=lg, val=vst)
  CALL OK(SIZE(lg) .EQ. 1, "not space-time obj: size unchanged")
  CALL OK(lg(1) .EQ. 7.0_DFP, "not space-time obj: value unchanged")
END BLOCK

END PROGRAM main
