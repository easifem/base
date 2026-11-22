!> author: Vikas Sharma, Ph. D.
! date: 2026-11-22
! summary: LagrangeElemShapeData with an oversized and undersized coeff
!
! Test 2 passes `coeff` with fewer than `nns` rows and columns.
! This breaks the routine's precondition on purpose (issue #518).
! In a debug build the routine stops with an error message before the
! last check. Without that check it writes outside `coeff`, and the
! last check fails.

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP, 1.0_DFP, &
                                                   -1.0_DFP, &
                                                   1.0_DFP], [2, 4])
TYPE(QuadraturePoint_) :: quad
TYPE(ElemShapeData_) :: ref
INTEGER(I4B), PARAMETER :: order = 2, nns = 9

! Biquadratic quadrangle, 3 x 3 Gauss points, without coeff
CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[3], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=ref, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=order)

! Test 1: coeff larger than (nns, nns) is accepted
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  REAL(DFP) :: coeff(nns + 3, nns + 3)
  LOGICAL(LGT) :: firstCall

  coeff = 0.0_DFP
  firstCall = .TRUE.
  CALL LagrangeElemShapeData(obj=elemsd, quad=quad, nsd=2, xidim=2, &
                             elemType=Quadrangle4, refelemCoord=quadCoord, &
                             domainName="BIUNIT", order=order, &
                             coeff=coeff, firstCall=firstCall)
  CALL OK(ALL(ABS(elemsd%N - ref%N) .LT. tol), "oversized coeff: same N")
  CALL OK(ALL(ABS(elemsd%dNdXi - ref%dNdXi) .LT. tol), &
          "oversized coeff: same dN/dxi")
END BLOCK

! Test 2: coeff(4, 4) for nns = 9 must not be written out of bounds
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  REAL(DFP), PARAMETER :: sentinel = -123.0_DFP
  REAL(DFP) :: buf(nns, nns)
  LOGICAL(LGT) :: firstCall, mask(nns, nns)

  buf = sentinel
  mask = .TRUE.
  mask(1:4, 1:4) = .FALSE.
  firstCall = .TRUE.
  CALL LagrangeElemShapeData(obj=elemsd, quad=quad, nsd=2, xidim=2, &
                             elemType=Quadrangle4, refelemCoord=quadCoord, &
                             domainName="BIUNIT", order=order, &
                             coeff=buf(1:4, 1:4), firstCall=firstCall)
  CALL OK(ALL(PACK(buf, mask) .EQ. sentinel), &
          "undersized coeff: no write outside coeff")
END BLOCK

END PROGRAM main
