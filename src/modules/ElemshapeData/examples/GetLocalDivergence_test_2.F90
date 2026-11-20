!> author: Vikas Sharma, Ph. D.
! date: 2026-11-19
! summary: Local row divergence of a matrix FEVariable into an array

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: STElemShapeData_
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
! nodal values of u = x + 3 y and w = 2 x - y
REAL(DFP), PARAMETER :: u(4) = [0.0_DFP, 2.0_DFP, 5.0_DFP, 3.0_DFP]
REAL(DFP), PARAMETER :: w(4) = [0.0_DFP, 4.0_DFP, 3.0_DFP, -1.0_DFP]
! space-time values on the line [0, 2]: c(a) x at time node a, c = [1, 3]
REAL(DFP), PARAMETER :: ust(2, 2) = RESHAPE([0.0_DFP, 2.0_DFP, &
                                             0.0_DFP, 6.0_DFP], [2, 2])
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

! Test 1: nodal space matrix [u, 0; w, 1], row divergence (1, 2)
BLOCK
  REAL(DFP), ALLOCATABLE :: lg(:, :)
  REAL(DFP) :: m(2, 2, 4)

  m(1, 1, :) = u
  m(2, 1, :) = w
  m(1, 2, :) = 0.0_DFP
  m(2, 2, :) = 1.0_DFP
  CALL GetLocalDivergence(obj=cell, lg=lg, &
                          val=NodalVariable(m, Matrix, Space))
  CALL OK(ALL(SHAPE(lg) .EQ. [2, 4]), "space: lg(nrow, nips)")
  CALL OK(ALL(ABS(lg(1, :) - 1.0_DFP) .LT. tol), &
          "space: du/dxi + 0 = 1")
  CALL OK(ALL(ABS(lg(2, :) - 2.0_DFP) .LT. tol), &
          "space: dw/dxi + d(1)/deta = 2")
END BLOCK

! Test 2: space-time matrix, row 1 = c(a) x and row 2 = 4
BLOCK
  REAL(DFP), ALLOCATABLE :: lg(:, :)
  REAL(DFP) :: mst(2, 1, 2, 2), c

  mst(1, 1, :, :) = ust
  mst(2, 1, :, :) = 4.0_DFP
  CALL GetLocalDivergence(obj=stelemsd(2), lg=lg, &
                          val=NodalVariable(mst, Matrix, SpaceTime))
  c = stelemsd(2)%T(1) * 1.0_DFP + stelemsd(2)%T(2) * 3.0_DFP
  CALL OK(ALL(SHAPE(lg) .EQ. [2, 2]), "space-time: lg(nrow, nips)")
  CALL OK(ALL(ABS(lg(1, :) - c) .LT. tol), &
          "space-time: row 1 = T1 c1 + T2 c2")
  CALL OK(ALL(ABS(lg(2, :)) .LT. tol), "space-time: row 2 = 0")
END BLOCK

END PROGRAM main
