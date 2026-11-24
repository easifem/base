!> author: Vikas Sharma, Ph. D.
! date: 2026-11-24
! summary: Divergence of unsupported inputs is zero
!
! A space-time input with space-only shape data, or a time FEVariable,
! has no supported divergence. Then `GetDivergence` sets `ans` to zero
! and returns the full size.

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Time => TypeFEVariableTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: GetDivergence
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP, 1.0_DFP, &
                                                   -1.0_DFP, &
                                                   1.0_DFP], [2, 4])
! physical cell: rectangle [0, 2] x [0, 1]
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad
! space-time nodal values: 2 components, 2 space dims, 4 nodes, 2 times
REAL(DFP) :: vst(2, 4, 2), mst(3, 2, 4, 2)

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)
vst = 1.0_DFP
mst = 1.0_DFP

! Test 1: space-time arrays with space-only shape data
BLOCK
  REAL(DFP) :: ans(4), ans2(3, 4)
  INTEGER(I4B) :: tsize, nrow, ncol

  ans = 1.0_DFP
  CALL GetDivergence(obj=cell, val=vst, ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 4, "vector array: tsize = nips")
  CALL OK(ALL(ans .EQ. 0.0_DFP), "vector array: ans is zero")

  ans2 = 1.0_DFP
  CALL GetDivergence(obj=cell, val=mst, ans=ans2, nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 3 .AND. ncol .EQ. 4, "matrix array: nrow, ncol")
  CALL OK(ALL(ans2 .EQ. 0.0_DFP), "matrix array: ans is zero")
END BLOCK

! Test 2: space-time FEVariable with space-only shape data
BLOCK
  REAL(DFP) :: ans(4), ans2(3, 4)
  INTEGER(I4B) :: tsize, nrow, ncol

  ans = 1.0_DFP
  CALL GetDivergence(obj=cell, val=NodalVariable(vst, Vector, SpaceTime), &
                     ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 4, "vector spaceTime: tsize = nips")
  CALL OK(ALL(ans .EQ. 0.0_DFP), "vector spaceTime: ans is zero")

  ans2 = 1.0_DFP
  nrow = -1
  ncol = -1
  CALL GetDivergence(obj=cell, val=NodalVariable(mst, Matrix, SpaceTime), &
                     ans=ans2, nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 3 .AND. ncol .EQ. 4, "matrix spaceTime: nrow, ncol")
  CALL OK(ALL(ans2 .EQ. 0.0_DFP), "matrix spaceTime: ans is zero")
END BLOCK

! Test 3: time FEVariable
BLOCK
  REAL(DFP) :: ans(4)
  INTEGER(I4B) :: tsize
  TYPE(FEVariable_) :: d

  ans = 1.0_DFP
  CALL GetDivergence(obj=cell, val=NodalVariable(vst(:, 1, :), Vector, &
                                                 Time), &
                     ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 4, "vector time: tsize = nips")
  CALL OK(ALL(ans .EQ. 0.0_DFP), "vector time: ans is zero")

  CALL GetDivergence(obj=cell, val=NodalVariable(vst(:, 1, :), Vector, &
                                                 Time), ans=d)
  CALL OK(ALL(d%val(1:4) .EQ. 0.0_DFP), "vector time: FEVariable is zero")
END BLOCK

END PROGRAM main
