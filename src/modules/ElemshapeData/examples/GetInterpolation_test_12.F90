!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Scalar interpolation of quadrature and constant FEVariables

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: STElemShapeData_
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
REAL(DFP) :: xi(2), tt(2), ust(2, 2), ref(2, 2), qst(2, 2)
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

! space-time nodal values [2 4; 6 8] give u = 3 + xi + 4 T2(t)
ust = RESHAPE([2.0_DFP, 4.0_DFP, 6.0_DFP, 8.0_DFP], [2, 2])
DO ipt = 1, 2
  ref(:, ipt) = 3.0_DFP + xi + 2.0_DFP * (1.0_DFP + tt(ipt))
END DO

! quadrature values qst(ips, ipt)
qst = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], [2, 2])

! Test 1: quadrature space variable is copied
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:)
  REAL(DFP) :: ans2(4)
  INTEGER(I4B) :: tsize

  CALL GetInterpolation(obj=elemsd, ans=ans, &
                        val=QuadratureVariable(qst(:, 1), Scalar, Space))
  CALL OK(SIZE(ans) .EQ. 2, "quadrature space: SIZE(ans) = nips")
  CALL OK(ALL(ABS(ans - qst(:, 1)) .LT. tol), "quadrature space: copy")

  ans2 = 1.0_DFP
  CALL GetInterpolation_(obj=elemsd, ans=ans2, &
                         val=QuadratureVariable(qst(:, 1), Scalar, Space), &
                         tsize=tsize, scale=2.0_DFP, addContribution=.TRUE.)
  CALL OK(tsize .EQ. 2, "quadrature space: tsize = nips")
  CALL OK(ALL(ABS(ans2(1:2) - (1.0_DFP + 2.0_DFP * qst(:, 1))) .LT. tol), &
          "quadrature space: add 2 val")
END BLOCK

! Test 2: quadrature space-time variable, timeIndx selects the column
BLOCK
  TYPE(FEVariable_) :: val
  REAL(DFP) :: ans(4), ans2(3, 3)
  INTEGER(I4B) :: tsize, nrow, ncol

  val = QuadratureVariable(qst, Scalar, SpaceTime)

  CALL GetInterpolation_(obj=stelemsd(1), ans=ans, val=val, tsize=tsize, &
                         scale=1.0_DFP, addContribution=.FALSE., timeIndx=2)
  CALL OK(ALL(ABS(ans(1:2) - qst(:, 2)) .LT. tol), &
          "quadrature space-time: timeIndx = 2")

  CALL GetInterpolation_(obj=stelemsd(1), ans=ans, val=val, tsize=tsize, &
                         scale=1.0_DFP, addContribution=.FALSE.)
  CALL OK(ALL(ABS(ans(1:2) - qst(:, 1)) .LT. tol), &
          "quadrature space-time: default timeIndx = 1")

  CALL GetInterpolation_(obj=stelemsd, ans=ans2, val=val, nrow=nrow, &
                         ncol=ncol)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 2, "quadrature all times: sizes")
  CALL OK(ALL(ABS(ans2(1:2, 1:2) - qst) .LT. tol), &
          "quadrature all times: copy")
END BLOCK

! Test 3: one point, constant and space-time variables
BLOCK
  REAL(DFP) :: ans1

  ans1 = 1.0_DFP
  CALL GetInterpolation_(obj=elemsd, ans=ans1, &
                         val=NodalVariable(5.0_DFP, Scalar, Constant), &
                         scale=2.0_DFP, addContribution=.TRUE., &
                         timeIndx=1, spaceIndx=1)
  CALL OK(ABS(ans1 - 11.0_DFP) .LT. tol, "one point: constant, add 2 val")

  CALL GetInterpolation_(obj=stelemsd(2), ans=ans1, &
                         val=NodalVariable(ust, Scalar, SpaceTime), &
                         scale=1.0_DFP, addContribution=.FALSE., &
                         timeIndx=2, spaceIndx=1)
  CALL OK(ABS(ans1 - ref(1, 2)) .LT. tol, "one point: nodal space-time")

  CALL GetInterpolation_(obj=stelemsd(1), ans=ans1, &
                         val=QuadratureVariable(qst, Scalar, SpaceTime), &
                         scale=1.0_DFP, addContribution=.FALSE., &
                         timeIndx=2, spaceIndx=1)
  CALL OK(ABS(ans1 - qst(1, 2)) .LT. tol, "one point: quadrature space-time")
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
