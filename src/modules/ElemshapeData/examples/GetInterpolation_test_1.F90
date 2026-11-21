!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Interpolation of a FEVariable into a quadrature FEVariable

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: Vector => TypeFEVariableVector
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

! Test 1: ans is initiated as a quadrature space variable
BLOCK
  TYPE(FEVariable_) :: val, ans

  val = NodalVariable([2.0_DFP, 4.0_DFP], Scalar, Space)
  CALL GetInterpolation(obj=elemsd, ans=ans, val=val)
  CALL OK(ans%isInit, "ans is initiated")
  CALL OK(ans%defineOn .EQ. fevaropt%quadrature, "ans: quadrature")
  CALL OK(ans%varType .EQ. fevaropt%space, "ans: space")
  CALL OK(ans%rank .EQ. fevaropt%scalar, "ans: rank of val")
  CALL OK(ALL(ABS(ans%val(1:2) - (3.0_DFP + xi)) .LT. tol), &
          "ans: u = 3 + xi")

  ! an initiated ans is only filled
  val = NodalVariable([4.0_DFP, 8.0_DFP], Scalar, Space)
  CALL GetInterpolation(obj=elemsd, ans=ans, val=val)
  CALL OK(ALL(ABS(ans%val(1:2) - 2.0_DFP * (3.0_DFP + xi)) .LT. tol), &
          "ans: refilled")

  CALL GetInterpolation_(obj=elemsd, ans=ans, val=val, scale=0.5_DFP, &
                         addContribution=.TRUE.)
  CALL OK(ALL(ABS(ans%val(1:2) - 3.0_DFP * (3.0_DFP + xi)) .LT. tol), &
          "GetInterpolation_: scale and addContribution")

  ans = Interpolation(obj=elemsd, val=val)
  CALL OK(ALL(ABS(ans%val(1:2) - 2.0_DFP * (3.0_DFP + xi)) .LT. tol), &
          "Interpolation function")
END BLOCK

! Test 2: vector variable keeps its number of components
BLOCK
  TYPE(FEVariable_) :: val, ans

  val = NodalVariable(RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 5.0_DFP], &
                              [2, 2]), Vector, Space)
  CALL GetInterpolation(obj=elemsd, ans=ans, val=val)
  CALL OK(ALL(ans%s(1:2) .EQ. [2, 2]), "vector: shape [ncomp, nips]")
  CALL OK(ABS(ans%val(3) - (2.0_DFP + xi(2))) .LT. tol, &
          "vector: first component at the second point")
END BLOCK

! Test 3: space-time shape data gives a quadrature space-time variable
BLOCK
  TYPE(FEVariable_) :: val, ans
  REAL(DFP) :: T2
  INTEGER(I4B) :: ipt

  val = NodalVariable(RESHAPE([2.0_DFP, 4.0_DFP, 6.0_DFP, 8.0_DFP], &
                              [2, 2]), Scalar, SpaceTime)
  CALL GetInterpolation(obj=stelemsd, ans=ans, val=val)
  CALL OK(ans%varType .EQ. fevaropt%spacetime, "space-time: varType")
  CALL OK(ALL(ans%s(1:2) .EQ. [2, 2]), "space-time: shape [nips, nipt]")
  CALL OK(ans%len .EQ. 4, "space-time: len = nips * nipt")
  DO ipt = 1, 2
    T2 = 0.5_DFP * (1.0_DFP + tt(ipt))
    CALL OK(ALL(ABS(ans%val(2 * ipt - 1:2 * ipt) - (3.0_DFP + xi + &
                                                    4.0_DFP * T2)) &
                .LT. tol), "space-time: values at each time point")
  END DO
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
