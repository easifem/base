!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: HRQI length scale on a UNIT reference element with refLength

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
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! UNIT reference element [0, 1]^2
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, &
                        1.0_DFP, 0.0_DFP, 1.0_DFP, 1.0_DFP, 0.0_DFP, &
                                                   1.0_DFP], [2, 4])
! physical cell: rectangle [0, 2] x [0, 1], so hmax = 2 and hmin = 1
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="UNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="UNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)

! Test 1: UNIT domain, J = diag(2, 1). p = y gives r = (0, 1), so the
! length along r is the cell height 1. With refLength = 1 the physical
! lengths are h = 1, hmax = 2, hmin = 1.
BLOCK
  REAL(DFP), ALLOCATABLE :: h(:), hmax(:), hmin(:)
  REAL(DFP) :: p(4)

  p = xcell(2, :) + 1.0_DFP
  CALL GetHRQIParam(obj=cell, h=h, val=NodalVariable(p, Scalar, Space), &
                    hmax=hmax, hmin=hmin, refLength=1.0_DFP)
  CALL OK(ALL(ABS(h - 1.0_DFP) .LT. tol), "UNIT, refLength = 1: h = 1")
  CALL OK(ALL(ABS(hmax - 2.0_DFP) .LT. tol), &
          "UNIT, refLength = 1: hmax = 2")
  CALL OK(ALL(ABS(hmin - 1.0_DFP) .LT. tol), &
          "UNIT, refLength = 1: hmin = 1")
END BLOCK

! Test 2: without refLength the default factor 2 doubles the lengths
BLOCK
  REAL(DFP), ALLOCATABLE :: h(:)
  REAL(DFP) :: p(4)

  p = xcell(2, :) + 1.0_DFP
  CALL GetHRQIParam(obj=cell, h=h, val=NodalVariable(p, Scalar, Space))
  CALL OK(ALL(ABS(h - 2.0_DFP) .LT. tol), "UNIT, default: h = 2")
END BLOCK

END PROGRAM main
