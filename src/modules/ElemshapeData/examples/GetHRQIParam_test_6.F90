!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: HRQI length scale for a line element in 2D (xidim < nsd)

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE ElemShapeData_Method, ONLY: GetHRQIParam
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Line2
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
! physical line from (0, 0) to (2, 0): J = [1; 0], element length 2
REAL(DFP), PARAMETER :: xline(2, 2) = RESHAPE([0.0_DFP, 0.0_DFP, &
                                               2.0_DFP, 0.0_DFP], [2, 2])
TYPE(ElemShapeData_) :: line
TYPE(QuadraturePoint_) :: quad

CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=line, quad=quad, nsd=2, xidim=1, &
                           elemType=Line2, refelemCoord=lineCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=line, val=xline, N=line%N, dNdXi=line%dNdXi)

! Test 1: hmax and hmin do not depend on r: hmax = hmin = 2 (length).
! Set gives dNdXt = 0 when xidim < nsd, so r = 0 and h takes the
! zero-direction value; check only that h is finite.
BLOCK
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_FINITE
  REAL(DFP), ALLOCATABLE :: h(:), hmax(:), hmin(:)
  REAL(DFP) :: p(2)

  p = xline(1, :) + 1.0_DFP
  CALL GetHRQIParam(obj=line, h=h, val=NodalVariable(p, Scalar, Space), &
                    hmax=hmax, hmin=hmin)
  CALL OK(ALL(ABS(hmax - 2.0_DFP) .LT. tol), "line 2D: hmax = 2")
  CALL OK(ALL(ABS(hmin - 2.0_DFP) .LT. tol), "line 2D: hmin = 2")
  CALL OK(ALL(IEEE_IS_FINITE(h)), "line 2D: h is finite")
END BLOCK

END PROGRAM main
