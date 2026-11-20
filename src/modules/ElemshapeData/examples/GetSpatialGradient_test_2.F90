!> author: Vikas Sharma, Ph. D.
! date: 2026-11-19
! summary: Spatial gradient of vector and matrix space-time data

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: GetSpatialGradient
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
! space-time values on the line [0, 2]: c(a) x at time node a, c = [1, 3]
REAL(DFP), PARAMETER :: ust(2, 2) = RESHAPE([0.0_DFP, 2.0_DFP, &
                                             0.0_DFP, 6.0_DFP], [2, 2])
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)
REAL(DFP) :: vst(2, 2, 2), mst(2, 2, 2, 2), c

! space-time: line [0, 2] in space with two time nodes, so dx/dxi = 1
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

! vector: (c(a) x, 5); matrix: M11 = c(a) x, M22 = 7, other entries zero
vst(1, :, :) = ust
vst(2, :, :) = 5.0_DFP
mst = 0.0_DFP
mst(1, 1, :, :) = ust
mst(2, 2, :, :) = 7.0_DFP
! at time point 2: du/dx = T1 c1 + T2 c2
c = stelemsd(2)%T(1) * 1.0_DFP + stelemsd(2)%T(2) * 3.0_DFP

! Test 1: vector space-time nodal values, val(ncomp, nns, nnt)
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :, :)

  CALL GetSpatialGradient(obj=stelemsd(2), ans=ans, val=vst)
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 1, 2]), &
          "vector space-time: ans(ncomp, nsd, nips)")
  CALL OK(ALL(ABS(ans(1, 1, :) - c) .LT. tol), &
          "vector space-time: dv1/dx = T1 c1 + T2 c2")
  CALL OK(ALL(ABS(ans(2, 1, :)) .LT. tol), "vector space-time: dv2/dx = 0")
END BLOCK

! Test 2: matrix space-time nodal values, val(nrow, ncol, nns, nnt)
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)

  CALL GetSpatialGradient(obj=stelemsd(2), ans=ans, val=mst)
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 2, 1, 2]), &
          "matrix space-time: ans(nrow, ncol, nsd, nips)")
  CALL OK(ALL(ABS(ans(1, 1, 1, :) - c) .LT. tol), &
          "matrix space-time: dM11/dx = T1 c1 + T2 c2")
  CALL OK(ALL(ABS(ans(2, 2, 1, :)) .LT. tol), &
          "matrix space-time: dM22/dx = 0")
END BLOCK

! Test 3: vector FEVariable with space-time shape data
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :, :)

  CALL GetSpatialGradient(obj=stelemsd(2), ans=ans, &
                          val=NodalVariable(vst, Vector, SpaceTime), &
                          valRank=Vector)
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 1, 2]), &
          "vector FEVariable: ans(ncomp, nsd, nips)")
  CALL OK(ALL(ABS(ans(1, 1, :) - c) .LT. tol), &
          "vector FEVariable: dv1/dx = T1 c1 + T2 c2")
  CALL OK(ALL(ABS(ans(2, 1, :)) .LT. tol), "vector FEVariable: dv2/dx = 0")
END BLOCK

! Test 4: matrix FEVariable with space-time shape data
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)

  CALL GetSpatialGradient(obj=stelemsd(2), ans=ans, &
                          val=NodalVariable(mst, Matrix, SpaceTime), &
                          valRank=Matrix)
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 2, 1, 2]), &
          "matrix FEVariable: ans(nrow, ncol, nsd, nips)")
  CALL OK(ALL(ABS(ans(1, 1, 1, :) - c) .LT. tol), &
          "matrix FEVariable: dM11/dx = T1 c1 + T2 c2")
  CALL OK(ALL(ABS(ans(2, 2, 1, :)) .LT. tol), &
          "matrix FEVariable: dM22/dx = 0")
END BLOCK

END PROGRAM main
