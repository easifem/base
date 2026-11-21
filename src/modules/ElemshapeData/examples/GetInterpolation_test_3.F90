!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Interpolation of vector nodal values and vector FEVariables

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: Vector => TypeFEVariableVector
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

! nodal vectors [1, 2] and [3, 5] give u = [2 + xi, 3.5 + 1.5 xi]

! Test 1: nodal values, with and without allocation
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :)
  REAL(DFP) :: val(2, 2), ans2(3, 3)
  INTEGER(I4B) :: nrow, ncol

  val = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 5.0_DFP], [2, 2])
  CALL GetInterpolation(obj=elemsd, ans=ans, val=val)
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 2]), "nodal: ans(ncomp, nips)")
  CALL OK(ALL(ABS(ans(1, :) - (2.0_DFP + xi)) .LT. tol), &
          "nodal: first component")
  CALL OK(ALL(ABS(ans(2, :) - (3.5_DFP + 1.5_DFP * xi)) .LT. tol), &
          "nodal: second component")

  CALL GetInterpolation_(obj=elemsd, ans=ans2, val=val, nrow=nrow, &
                         ncol=ncol)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 2, "nodal: nrow, ncol")
  CALL OK(ALL(ABS(ans2(1:2, 1:2) - ans) .LT. tol), &
          "nodal: without allocation")

  ans = Interpolation(obj=elemsd, val=val)
  CALL OK(ALL(ABS(ans2(1:2, 1:2) - ans) .LT. tol), "nodal: Interpolation")
END BLOCK

! Test 2: space-time nodal values with the same vectors at both times
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :), ans3(:, :, :)
  REAL(DFP) :: val(2, 2, 2)

  val(:, :, 1) = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 5.0_DFP], [2, 2])
  val(:, :, 2) = val(:, :, 1)
  CALL GetInterpolation(obj=stelemsd(1), ans=ans, val=val)
  CALL OK(ALL(ABS(ans(1, :) - (2.0_DFP + xi)) .LT. tol), &
          "space-time: at one time point")

  ans = STInterpolation(obj=stelemsd(2), val=val)
  CALL OK(ALL(ABS(ans(2, :) - (3.5_DFP + 1.5_DFP * xi)) .LT. tol), &
          "space-time: STInterpolation")

  CALL GetInterpolation(obj=stelemsd, ans=ans3, val=val)
  CALL OK(ALL(SHAPE(ans3) .EQ. [2, 2, 2]), &
          "space-time: ans(ncomp, nips, nipt)")
  CALL OK(ALL(ABS(ans3(2, :, 2) - (3.5_DFP + 1.5_DFP * xi)) .LT. tol), &
          "space-time: at all time points")
END BLOCK

! Test 3: vector FEVariable to an array, and at one point
BLOCK
  TYPE(FEVariable_) :: val
  REAL(DFP), ALLOCATABLE :: ans(:, :)
  REAL(DFP) :: ans1(3)
  INTEGER(I4B) :: tsize

  val = NodalVariable(RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 5.0_DFP], &
                              [2, 2]), Vector, Space)
  CALL GetInterpolation(obj=elemsd, ans=ans, val=val)
  CALL OK(ALL(ABS(ans(1, :) - (2.0_DFP + xi)) .LT. tol), &
          "FEVariable: space")

  CALL GetInterpolation_(obj=elemsd, ans=ans1, val=val, tsize=tsize, &
                         scale=1.0_DFP, addContribution=.FALSE., &
                         timeIndx=1, spaceIndx=1)
  CALL OK(tsize .EQ. 2, "FEVariable: one point, tsize = ncomp")
  CALL OK(ALL(ABS(ans1(1:2) - [2.0_DFP + xi(1), 3.5_DFP + 1.5_DFP * &
                                                   xi(1)]) .LT. tol), &
          "FEVariable: one point")
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
