!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Interpolation of scalar nodal values and scalar FEVariables

PROGRAM main
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
USE ElemShapeData_Method, ONLY: Interpolation
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: STInterpolation
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
TYPE(ElemShapeData_) :: elemsd, timeElemsd
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)
TYPE(QuadraturePoint_) :: quad
REAL(DFP) :: xi(2), tt(2)

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

! nodal values [2, 4] give u = 3 + xi
! space-time nodal values [2 4; 6 8] give u = 3 + xi + 4 T2(t)

! Test 1: nodal values, with and without allocation
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:)
  REAL(DFP) :: ans2(4)
  INTEGER(I4B) :: tsize

  CALL GetInterpolation(obj=elemsd, ans=ans, val=[2.0_DFP, 4.0_DFP])
  CALL OK(SIZE(ans) .EQ. 2, "nodal: SIZE(ans) = nips")
  CALL OK(ALL(ABS(ans - (3.0_DFP + xi)) .LT. tol), "nodal: u = 3 + xi")

  CALL GetInterpolation_(obj=elemsd, ans=ans2, val=[2.0_DFP, 4.0_DFP], &
                         tsize=tsize)
  CALL OK(tsize .EQ. 2, "nodal: tsize = nips")
  CALL OK(ALL(ABS(ans2(1:2) - (3.0_DFP + xi)) .LT. tol), &
          "nodal: without allocation")

  CALL GetInterpolation_(obj=elemsd, ans=ans2, val=[2.0_DFP, 4.0_DFP], &
                         tsize=tsize, scale=2.0_DFP, addContribution=.TRUE.)
  CALL OK(ALL(ABS(ans2(1:2) - 3.0_DFP * (3.0_DFP + xi)) .LT. tol), &
          "nodal: scale and addContribution")

  ans = Interpolation(obj=elemsd, val=[2.0_DFP, 4.0_DFP])
  CALL OK(ALL(ABS(ans - (3.0_DFP + xi)) .LT. tol), "nodal: Interpolation")
END BLOCK

! Test 2: space-time nodal values at one and at all time points
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:), ans2(:, :)
  REAL(DFP) :: val(2, 2), T2
  INTEGER(I4B) :: ipt

  val = RESHAPE([2.0_DFP, 4.0_DFP, 6.0_DFP, 8.0_DFP], [2, 2])
  CALL GetInterpolation(obj=stelemsd(1), ans=ans, val=val)
  T2 = 0.5_DFP * (1.0_DFP + tt(1))
  CALL OK(ALL(ABS(ans - (3.0_DFP + xi + 4.0_DFP * T2)) .LT. tol), &
          "space-time: at the first time point")

  ans = STInterpolation(obj=stelemsd(2), val=val)
  T2 = 0.5_DFP * (1.0_DFP + tt(2))
  CALL OK(ALL(ABS(ans - (3.0_DFP + xi + 4.0_DFP * T2)) .LT. tol), &
          "space-time: STInterpolation at the second time point")

  CALL GetInterpolation(obj=stelemsd, ans=ans2, val=val)
  CALL OK(ALL(SHAPE(ans2) .EQ. [2, 2]), "space-time: ans(nips, nipt)")
  DO ipt = 1, 2
    T2 = 0.5_DFP * (1.0_DFP + tt(ipt))
    CALL OK(ALL(ABS(ans2(:, ipt) - (3.0_DFP + xi + 4.0_DFP * T2)) &
                .LT. tol), "space-time: all time points")
  END DO
END BLOCK

! Test 3: scalar FEVariable to an array, and at one point
BLOCK
  TYPE(FEVariable_) :: val
  REAL(DFP), ALLOCATABLE :: ans(:), ans2(:, :)
  REAL(DFP) :: ans1, T2

  val = NodalVariable([2.0_DFP, 4.0_DFP], Scalar, Space)
  CALL GetInterpolation(obj=elemsd, ans=ans, val=val)
  CALL OK(ALL(ABS(ans - (3.0_DFP + xi)) .LT. tol), "FEVariable: space")

  CALL GetInterpolation_(obj=elemsd, ans=ans1, val=val, scale=1.0_DFP, &
                         addContribution=.FALSE., timeIndx=1, spaceIndx=2)
  CALL OK(ABS(ans1 - (3.0_DFP + xi(2))) .LT. tol, "FEVariable: one point")

  val = NodalVariable(RESHAPE([2.0_DFP, 4.0_DFP, 6.0_DFP, 8.0_DFP], &
                              [2, 2]), Scalar, SpaceTime)
  CALL GetInterpolation(obj=stelemsd, ans=ans2, val=val)
  T2 = 0.5_DFP * (1.0_DFP + tt(2))
  CALL OK(ALL(ABS(ans2(:, 2) - (3.0_DFP + xi + 4.0_DFP * T2)) .LT. tol), &
          "FEVariable: space-time at all time points")
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
