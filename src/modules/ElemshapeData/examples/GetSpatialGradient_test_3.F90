!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Spatial gradient on a facet, of constants, and of a matrix
! FEVariable with space-time shape data

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
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
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])
! physical cell: rectangle [0, 2] x [0, 1]
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
! space-time values on the line [0, 2]: c(a) x at time node a, c = [1, 3]
REAL(DFP), PARAMETER :: ust(2, 2) = RESHAPE([0.0_DFP, 2.0_DFP, &
                                             0.0_DFP, 6.0_DFP], [2, 2])
TYPE(ElemShapeData_) :: cell, facet
TYPE(QuadraturePoint_) :: quad, lquad
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)
REAL(DFP) :: mst(2, 2, 2, 2), c

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)

! facet: a line (xidim = 1) in the plane (nsd = 2)
CALL QP_Initiate(obj=lquad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=facet, quad=lquad, nsd=2, xidim=1, &
                           elemType=Line2, refelemCoord=lineCoord, &
                           domainName="BIUNIT", order=1)

! space-time: line [0, 2] in space with two time nodes, so dx/dxi = 1
BLOCK
  REAL(DFP), PARAMETER :: xst(1, 2, 2) = RESHAPE([0.0_DFP, 2.0_DFP, &
                                                  0.0_DFP, 2.0_DFP], &
                                                 [1, 2, 2])
  TYPE(ElemShapeData_) :: line
  INTEGER(I4B) :: ipt

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

! matrix: M11 = c(a) x, M22 = 7, other entries zero
mst = 0.0_DFP
mst(1, 1, :, :) = ust
mst(2, 2, :, :) = 7.0_DFP
! at time point 2: dM11/dx = T1 c1 + T2 c2
c = stelemsd(2)%T(1) * 1.0_DFP + stelemsd(2)%T(2) * 3.0_DFP

! Test 1: on a facet (nsd /= xidim) the gradient is zero
BLOCK
  REAL(DFP), ALLOCATABLE :: g2(:, :), g3(:, :, :), g4(:, :, :, :)
  REAL(DFP) :: v(2, 2), m(2, 2, 2)

  CALL GetSpatialGradient(obj=facet, ans=g2, val=[1.0_DFP, 3.0_DFP])
  CALL OK(ALL(SHAPE(g2) .EQ. [2, 2]), "facet scalar: ans(nsd, nips)")
  CALL OK(ALL(g2 .EQ. 0.0_DFP), "facet scalar: zero")

  v = 1.0_DFP
  v(1, 2) = 5.0_DFP
  CALL GetSpatialGradient(obj=facet, ans=g3, val=v)
  CALL OK(ALL(SHAPE(g3) .EQ. [2, 2, 2]), &
          "facet vector: ans(ncomp, nsd, nips)")
  CALL OK(ALL(g3 .EQ. 0.0_DFP), "facet vector: zero")

  m = 1.0_DFP
  m(1, 1, 2) = 5.0_DFP
  CALL GetSpatialGradient(obj=facet, ans=g4, val=m)
  CALL OK(ALL(SHAPE(g4) .EQ. [2, 2, 2, 2]), &
          "facet matrix: ans(nrow, ncol, nsd, nips)")
  CALL OK(ALL(g4 .EQ. 0.0_DFP), "facet matrix: zero")
END BLOCK

! Test 2: constant vector and matrix FEVariable give zero
BLOCK
  REAL(DFP), ALLOCATABLE :: g3(:, :, :), g4(:, :, :, :)
  REAL(DFP) :: m(2, 2)

  CALL GetSpatialGradient(obj=cell, ans=g3, &
                          val=NodalVariable([1.0_DFP, 2.0_DFP], Vector, &
                                            Constant), &
                          valRank=Vector)
  CALL OK(ALL(SHAPE(g3) .EQ. [2, 2, 4]), &
          "constant vector: ans(ncomp, nsd, nips)")
  CALL OK(ALL(g3 .EQ. 0.0_DFP), "constant vector: zero")

  m = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], [2, 2])
  CALL GetSpatialGradient(obj=cell, ans=g4, &
                          val=NodalVariable(m, Matrix, Constant))
  CALL OK(ALL(SHAPE(g4) .EQ. [2, 2, 2, 4]), &
          "constant matrix: ans(nrow, ncol, nsd, nips)")
  CALL OK(ALL(g4 .EQ. 0.0_DFP), "constant matrix: zero")
END BLOCK

! Test 3: matrix FEVariable without valRank, space-time shape data
BLOCK
  REAL(DFP), ALLOCATABLE :: g4(:, :, :, :)

  CALL GetSpatialGradient(obj=stelemsd(2), ans=g4, &
                          val=NodalVariable(mst, Matrix, SpaceTime))
  CALL OK(ALL(SHAPE(g4) .EQ. [2, 2, 1, 2]), &
          "space-time matrix: ans(nrow, ncol, nsd, nips)")
  CALL OK(ALL(ABS(g4(1, 1, 1, :) - c) .LT. tol), &
          "space-time matrix: dM11/dx = T1 c1 + T2 c2")
  CALL OK(ALL(ABS(g4(2, 2, 1, :)) .LT. tol), &
          "space-time matrix: dM22/dx = 0")
END BLOCK

END PROGRAM main
