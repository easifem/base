!> author: Vikas Sharma, Ph. D.
! date: 2026-11-19
! summary: Scalar interpolation without allocation and with scale

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: GetInterpolation_
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
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
REAL(DFP) :: xi(2), tt(2), ust(2, 2), ref(2, 2)
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

! Test 1: space-time nodal values at one time point, no allocation
BLOCK
  REAL(DFP) :: ans(4)
  INTEGER(I4B) :: tsize

  CALL GetInterpolation_(obj=stelemsd(2), ans=ans, val=ust, tsize=tsize)
  CALL OK(tsize .EQ. 2, "one time point: tsize = nips")
  CALL OK(ALL(ABS(ans(1:2) - ref(:, 2)) .LT. tol), &
          "one time point: u = 3 + xi + 4 T2")

  CALL GetInterpolation_(obj=stelemsd(2), ans=ans, val=ust, tsize=tsize, &
                         scale=0.5_DFP, addContribution=.TRUE.)
  CALL OK(ALL(ABS(ans(1:2) - 1.5_DFP * ref(:, 2)) .LT. tol), &
          "one time point: add 0.5 u")
END BLOCK

! Test 2: space-time nodal values at all time points, no allocation
BLOCK
  REAL(DFP) :: ans(3, 3)
  INTEGER(I4B) :: nrow, ncol

  CALL GetInterpolation_(obj=stelemsd, ans=ans, val=ust, nrow=nrow, &
                         ncol=ncol)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 2, "all time points: nips, nipt")
  CALL OK(ALL(ABS(ans(1:2, 1:2) - ref) .LT. tol), "all time points: u")

  CALL GetInterpolation_(obj=stelemsd, ans=ans, val=ust, nrow=nrow, &
                         ncol=ncol, scale=2.0_DFP, addContribution=.FALSE.)
  CALL OK(ALL(ABS(ans(1:2, 1:2) - 2.0_DFP * ref) .LT. tol), &
          "all time points: 2 u without addContribution")
END BLOCK

! Test 3: scalar FEVariable, no allocation and with scale
BLOCK
  REAL(DFP) :: ans(4)
  INTEGER(I4B) :: tsize

  CALL GetInterpolation_(obj=elemsd, ans=ans, &
                         val=NodalVariable([2.0_DFP, 4.0_DFP], Scalar, &
                                           Space), tsize=tsize)
  CALL OK(tsize .EQ. 2, "FEVariable: tsize = nips")
  CALL OK(ALL(ABS(ans(1:2) - (3.0_DFP + xi)) .LT. tol), &
          "FEVariable nodal space: u = 3 + xi")

  CALL GetInterpolation_(obj=elemsd, ans=ans, &
                         val=NodalVariable(5.0_DFP, Scalar, Constant), &
                         tsize=tsize)
  CALL OK(ALL(ABS(ans(1:2) - 5.0_DFP) .LT. tol), &
          "FEVariable constant: repeated")

  CALL GetInterpolation_(obj=stelemsd(2), ans=ans, &
                         val=NodalVariable(ust, Scalar, SpaceTime), &
                         tsize=tsize, scale=1.0_DFP, &
                         addContribution=.FALSE., timeIndx=2)
  CALL OK(ALL(ABS(ans(1:2) - ref(:, 2)) .LT. tol), &
          "FEVariable space-time with scale: u at time point 2")
END BLOCK

! Test 4: scalar FEVariable at all time points, no allocation
BLOCK
  REAL(DFP) :: ans(3, 3)
  INTEGER(I4B) :: nrow, ncol

  CALL GetInterpolation_(obj=stelemsd, ans=ans, &
                         val=NodalVariable(ust, Scalar, SpaceTime), &
                         nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 2, "FEVariable all times: sizes")
  CALL OK(ALL(ABS(ans(1:2, 1:2) - ref) .LT. tol), "FEVariable all times: u")

  CALL GetInterpolation_(obj=stelemsd, ans=ans, &
                         val=NodalVariable(ust, Scalar, SpaceTime), &
                         nrow=nrow, ncol=ncol, scale=1.0_DFP, &
                         addContribution=.TRUE.)
  CALL OK(ALL(ABS(ans(1:2, 1:2) - 2.0_DFP * ref) .LT. tol), &
          "FEVariable all times: add u")
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
