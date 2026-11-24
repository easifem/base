!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: HRQI length scale with an unsupported (matrix) val

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE ElemShapeData_Method, ONLY: GetHRQIParam
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])
! physical cell: rectangle [0, 2] x [0, 1], so hmax = 2 and hmin = 1
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)

! Test 1: a matrix val has no direction: r = 0 and h = 0,
! while hmax and hmin do not depend on r
BLOCK
  REAL(DFP), ALLOCATABLE :: h(:), hmax(:), hmin(:), r(:, :)
  REAL(DFP) :: m(2, 2, 4)

  m = 1.0_DFP
  CALL GetHRQIParam(obj=cell, h=h, val=NodalVariable(m, Matrix, Space), &
                    hmax=hmax, hmin=hmin, r=r)
  CALL OK(ALLOCATED(r), "matrix: r is allocated")
  IF (ALLOCATED(r)) THEN
    CALL OK(SIZE(r, 1) .EQ. 2 .AND. SIZE(r, 2) .EQ. cell%nips, &
            "matrix: r has shape (nsd, nips)")
    CALL OK(ALL(ABS(r) .LT. tol), "matrix: r = 0")
  END IF
  CALL OK(ALL(ABS(h) .LT. tol), "matrix: h = 0")
  CALL OK(ALL(ABS(hmax - 2.0_DFP) .LT. tol), "matrix: hmax = 2")
  CALL OK(ALL(ABS(hmin - 1.0_DFP) .LT. tol), "matrix: hmin = 1")
END BLOCK

END PROGRAM main
