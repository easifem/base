!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Vector interpolation of quadrature FEVariables, at one point,
!   and with fewer nodal values than nodes

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: GetInterpolation
USE ElemShapeData_Method, ONLY: GetInterpolation_
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
TYPE(ElemShapeData_) :: elemsd, timeElemsd
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)
TYPE(QuadraturePoint_) :: quad
REAL(DFP) :: xi(2), tt(2), q(2, 2), qst(2, 2, 2), vst(2, 2, 2), ref(2, 2)
INTEGER(I4B) :: ipt

! linear line in space and in time, 2 Gauss points each
CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=elemsd, quad=quad, nsd=1, xidim=1, &
                           elemType=Line2, refelemCoord=lineCoord, &
                           domainName="BIUNIT", order=1)
timeElemsd = elemsd
xi = quad%points(1, 1:2)
tt = xi
CALL MakeST(elemsd, timeElemsd, stelemsd)

! quadrature vector in space: q(:, ips)
q = RESHAPE([10.0_DFP, 20.0_DFP, 30.0_DFP, 40.0_DFP], [2, 2])
! quadrature vector in space-time: qst(:, ips, ipt)
qst(:, :, 1) = q
qst(:, :, 2) = 2.0_DFP * q
! space-time nodal vector: v1 = [2 4; 6 8] gives 3 + xi + 4 T2(t), v2 = 1
vst(1, :, :) = RESHAPE([2.0_DFP, 4.0_DFP, 6.0_DFP, 8.0_DFP], [2, 2])
vst(2, :, :) = 1.0_DFP
DO ipt = 1, 2
  ref(:, ipt) = 3.0_DFP + xi + 2.0_DFP * (1.0_DFP + tt(ipt))
END DO

! Test 1: quadrature FEVariable in space is copied
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :)

  CALL GetInterpolation(obj=elemsd, ans=ans, &
                        val=QuadratureVariable(q, Vector, Space))
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 2]), "quadrature space: ans(ncomp, nips)")
  CALL OK(ALL(ABS(ans - q) .LT. tol), "quadrature space: ans = q")
END BLOCK

! Test 2: quadrature FEVariable in space-time, timeIndx picks the time
BLOCK
  REAL(DFP) :: ans(3, 3)
  INTEGER(I4B) :: nrow, ncol

  CALL GetInterpolation_(obj=stelemsd(2), ans=ans, &
                         val=QuadratureVariable(qst, Vector, SpaceTime), &
                         nrow=nrow, ncol=ncol, scale=1.0_DFP, &
                         addContribution=.FALSE., timeIndx=2)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 2, "quadrature space-time: sizes")
  CALL OK(ALL(ABS(ans(1:2, 1:2) - qst(:, :, 2)) .LT. tol), &
          "quadrature space-time: ans = qst(:, :, 2)")
END BLOCK

! Test 3: quadrature FEVariable in space-time at all time points
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :, :)

  CALL GetInterpolation(obj=stelemsd, ans=ans, &
                        val=QuadratureVariable(qst, Vector, SpaceTime))
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 2, 2]), &
          "quadrature all time points: ans(ncomp, nips, nipt)")
  CALL OK(ALL(ABS(ans - qst) .LT. tol), &
          "quadrature all time points: ans = qst")
END BLOCK

! Test 4: vector FEVariable at one quadrature point
BLOCK
  REAL(DFP) :: ans(3)
  INTEGER(I4B) :: tsize

  ans = 1.0_DFP
  CALL GetInterpolation_(obj=elemsd, ans=ans, &
                         val=NodalVariable([1.0_DFP, 2.0_DFP], Vector, &
                                           Constant), tsize=tsize, &
                         scale=2.0_DFP, addContribution=.TRUE., &
                         timeIndx=1, spaceIndx=2)
  CALL OK(tsize .EQ. 2, "one point constant: tsize = ncomp")
  CALL OK(ALL(ABS(ans(1:2) - [3.0_DFP, 5.0_DFP]) .LT. tol), &
          "one point constant: ans = 1 + 2 [1, 2]")

  CALL GetInterpolation_(obj=stelemsd(2), ans=ans, &
                         val=NodalVariable(vst, Vector, SpaceTime), &
                         tsize=tsize, scale=1.0_DFP, &
                         addContribution=.FALSE., timeIndx=2, spaceIndx=1)
  CALL OK(ALL(ABS(ans(1:2) - [ref(1, 2), 1.0_DFP]) .LT. tol), &
          "one point nodal space-time: v at (xi(1), tt(2))")

  CALL GetInterpolation_(obj=stelemsd(2), ans=ans, &
                         val=QuadratureVariable(qst, Vector, SpaceTime), &
                         tsize=tsize, scale=1.0_DFP, &
                         addContribution=.FALSE., timeIndx=2, spaceIndx=2)
  CALL OK(ALL(ABS(ans(1:2) - qst(:, 2, 2)) .LT. tol), &
          "one point quadrature space-time: ans = qst(:, 2, 2)")
END BLOCK

! Test 5: fewer nodal values than nodes uses only the given nodes
BLOCK
  REAL(DFP) :: ans(3, 3), val(2, 1)
  INTEGER(I4B) :: nrow, ncol

  val(:, 1) = [4.0_DFP, 6.0_DFP]
  CALL GetInterpolation_(obj=elemsd, ans=ans, val=val, nrow=nrow, &
                         ncol=ncol, scale=1.0_DFP, addContribution=.FALSE.)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 2, "fewer nodes: sizes")
  CALL OK(ALL(ABS(ans(1, 1:2) - 4.0_DFP * elemsd%N(1, 1:2)) .LT. tol), &
          "fewer nodes: ans(1, :) = 4 N(1, :)")
  CALL OK(ALL(ABS(ans(2, 1:2) - 6.0_DFP * elemsd%N(1, 1:2)) .LT. tol), &
          "fewer nodes: ans(2, :) = 6 N(1, :)")
END BLOCK

CONTAINS

!> Space-time shape data: time data from timeElemsd and the same space
! data from elemsd at every time point
SUBROUTINE MakeST(elemsd, timeElemsd, stelemsd)
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd
  !! space shape data
  TYPE(ElemShapeData_), INTENT(IN) :: timeElemsd
  !! time shape data
  TYPE(STElemShapeData_), ALLOCATABLE, INTENT(INOUT) :: stelemsd(:)
  !! space-time shape data, one per time point
  INTEGER(I4B) :: ipt

  CALL Initiate(stelemsd, timeElemsd)
  DO ipt = 1, SIZE(stelemsd)
    stelemsd(ipt)%nsd = elemsd%nsd
    stelemsd(ipt)%xidim = elemsd%xidim
    stelemsd(ipt)%nns = elemsd%nns
    stelemsd(ipt)%nips = elemsd%nips
    stelemsd(ipt)%N = elemsd%N
    stelemsd(ipt)%dNdXi = elemsd%dNdXi
    stelemsd(ipt)%ws = elemsd%ws
  END DO
END SUBROUTINE MakeST

END PROGRAM main
