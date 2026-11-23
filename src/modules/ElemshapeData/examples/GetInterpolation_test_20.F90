!> author: Vikas Sharma, Ph. D.
! date: 2026-11-23
! summary: Space-time val with space-time shape data, with and without scale
!
! For one `STElemShapeData_` and a space-time `val`, GetInterpolation_
! without `scale`, GetInterpolation and Interpolation give the same
! result as GetInterpolation_ with `scale = 1`. The value is
! interpolated in space and in time.

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

! u = 3 + xi at the first time node, u = 7 + xi at the second one,
! so u = 3 + xi + 4 T2 with T2 = (1 + t) / 2
val = NodalVariable(RESHAPE([2.0_DFP, 4.0_DFP, 6.0_DFP, 8.0_DFP], &
                            [2, 2]), Scalar, SpaceTime)

! Test 1: GetInterpolation_ without scale, ans already initiated
BLOCK
  TYPE(FEVariable_) :: ans
  INTEGER(I4B) :: ipt

  CALL GetInterpolation(obj=elemsd, ans=ans, &
                        val=NodalVariable([0.0_DFP, 0.0_DFP], Scalar, Space))
  DO ipt = 1, 2
    CALL GetInterpolation_(obj=stelemsd(ipt), ans=ans, val=val)
    CALL OK(ALL(ABS(ans%val(1:2) - Exact(ipt)) .LT. tol), &
            "GetInterpolation_: u = 3 + xi + 4 T2")
  END DO
END BLOCK

! Test 2: GetInterpolation, ans not initiated
BLOCK
  INTEGER(I4B) :: ipt

  DO ipt = 1, 2
    BLOCK
      TYPE(FEVariable_) :: ans

      CALL GetInterpolation(obj=stelemsd(ipt), ans=ans, val=val)
      CALL OK(ALL(ABS(ans%val(1:2) - Exact(ipt)) .LT. tol), &
              "GetInterpolation: u = 3 + xi + 4 T2")
    END BLOCK
  END DO
END BLOCK

! Test 3: Interpolation, function form
BLOCK
  TYPE(FEVariable_) :: ans
  INTEGER(I4B) :: ipt

  DO ipt = 1, 2
    ans = Interpolation(obj=stelemsd(ipt), val=val)
    CALL OK(ALL(ABS(ans%val(1:2) - Exact(ipt)) .LT. tol), &
            "Interpolation: u = 3 + xi + 4 T2")
  END DO
END BLOCK

CONTAINS

!> Exact value of u at the space quadrature points, time point ipt
FUNCTION Exact(ipt) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: ipt
  !! time quadrature point
  REAL(DFP) :: ans(2)
  !! u = 3 + xi + 4 T2 at both space quadrature points

  ans = 3.0_DFP + xi + 4.0_DFP * 0.5_DFP * (1.0_DFP + tt(ipt))
END FUNCTION Exact

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
