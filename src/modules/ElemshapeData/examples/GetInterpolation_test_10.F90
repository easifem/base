!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Interpolation with space-time shape data at one time point

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
TYPE(FEVariable_) :: val
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

! u = 3 + xi at the first time node, u = 7 + xi at the second one
val = NodalVariable(RESHAPE([2.0_DFP, 4.0_DFP, 6.0_DFP, 8.0_DFP], &
                            [2, 2]), Scalar, SpaceTime)

! Test 1: with scale, a space-time val is interpolated in time
BLOCK
  TYPE(FEVariable_) :: ans
  REAL(DFP) :: T2
  INTEGER(I4B) :: ipt

  CALL GetInterpolation(obj=elemsd, ans=ans, &
                        val=NodalVariable([0.0_DFP, 0.0_DFP], Scalar, Space))
  DO ipt = 1, 2
    CALL GetInterpolation_(obj=stelemsd(ipt), ans=ans, val=val, &
                           scale=1.0_DFP, addContribution=.FALSE.)
    T2 = 0.5_DFP * (1.0_DFP + tt(ipt))
    CALL OK(ALL(ABS(ans%val(1:2) - (3.0_DFP + xi + 4.0_DFP * T2)) &
                .LT. tol), "scale: u = 3 + xi + 4 T2")
  END DO

  CALL GetInterpolation_(obj=stelemsd(2), ans=ans, val=val, &
                         scale=-1.0_DFP, addContribution=.TRUE.)
  CALL OK(ALL(ABS(ans%val(1:2)) .LT. tol), "addContribution: u - u = 0")
END BLOCK

! Test 2: without scale, only the first time slice of val is used
BLOCK
  TYPE(FEVariable_) :: ans

  CALL GetInterpolation(obj=stelemsd(2), ans=ans, val=val)
  CALL OK(ALL(ABS(ans%val(1:2) - (3.0_DFP + xi)) .LT. tol), &
          "no scale: u = 3 + xi, first time slice")
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
