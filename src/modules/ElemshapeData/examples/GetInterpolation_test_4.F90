!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Interpolation of matrix nodal values and matrix FEVariables

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Space => TypeFEVariableSpace
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

! nodal matrices m1 and m2 give u = m1 N1 + m2 N2

! Test 1: nodal values and a matrix FEVariable
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  REAL(DFP) :: val(2, 2, 2), ref(2, 2)
  TYPE(FEVariable_) :: fevar
  INTEGER(I4B) :: ips

  val(:, :, 1) = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], [2, 2])
  val(:, :, 2) = RESHAPE([5.0_DFP, 6.0_DFP, 7.0_DFP, 8.0_DFP], [2, 2])
  CALL GetInterpolation(obj=elemsd, ans=ans, val=val)
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 2, 2]), "nodal: ans(nrow, ncol, nips)")
  DO ips = 1, 2
    ref = val(:, :, 1) * elemsd%N(1, ips) + val(:, :, 2) * elemsd%N(2, ips)
    CALL OK(ALL(ABS(ans(:, :, ips) - ref) .LT. tol), "nodal: values")
  END DO

  fevar = NodalVariable(val, Matrix, Space)
  CALL GetInterpolation(obj=elemsd, ans=ans, val=fevar)
  CALL OK(ALL(ABS(ans(:, :, 2) - ref) .LT. tol), "FEVariable: space")

  ans = Interpolation(obj=elemsd, val=val)
  CALL OK(ALL(ABS(ans(:, :, 2) - ref) .LT. tol), "nodal: Interpolation")
END BLOCK

! Test 2: space-time nodal values with the same matrices at both times
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  REAL(DFP) :: val(2, 2, 2, 2), ref(2, 2)

  val(:, :, 1, 1) = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], [2, 2])
  val(:, :, 2, 1) = RESHAPE([5.0_DFP, 6.0_DFP, 7.0_DFP, 8.0_DFP], [2, 2])
  val(:, :, :, 2) = val(:, :, :, 1)
  CALL GetInterpolation(obj=stelemsd, ans=ans, val=val)
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 2, 2, 2]), &
          "space-time: ans(nrow, ncol, nips, nipt)")
  ref = val(:, :, 1, 1) * elemsd%N(1, 1) + val(:, :, 2, 1) * elemsd%N(2, 1)
  CALL OK(ALL(ABS(ans(:, :, 1, 2) - ref) .LT. tol), &
          "space-time: values")
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
