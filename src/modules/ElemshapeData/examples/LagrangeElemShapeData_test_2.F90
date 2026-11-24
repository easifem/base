!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Lagrange shape data with stored coefficients (coeff, firstCall)

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
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
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

! Test 1: firstCall = .TRUE. computes coeff, same data as without coeff
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  REAL(DFP) :: coeff(nns, nns)
  LOGICAL(LGT) :: firstCall

  coeff = 0.0_DFP
  firstCall = .TRUE.
  CALL LagrangeElemShapeData(obj=elemsd, quad=quad, nsd=2, xidim=2, &
                             elemType=Quadrangle4, refelemCoord=quadCoord, &
                             domainName="BIUNIT", order=order, &
                             coeff=coeff, firstCall=firstCall)
  CALL OK(elemsd%nns .EQ. nns .AND. elemsd%nips .EQ. 9, &
          "firstCall true: nns = 9, nips = 9")
  CALL OK(ANY(ABS(coeff) .GT. tol), "firstCall true: coeff is filled")
  CALL OK(ALL(ABS(elemsd%N - ref%N) .LT. tol), "firstCall true: same N")
  CALL OK(ALL(ABS(elemsd%dNdXi - ref%dNdXi) .LT. tol), &
          "firstCall true: same dN/dxi")
END BLOCK

! Test 2: firstCall = .FALSE. reuses coeff from a first call
BLOCK
  TYPE(ElemShapeData_) :: elemsd1, elemsd2
  REAL(DFP) :: coeff(nns, nns), coeffCopy(nns, nns)
  LOGICAL(LGT) :: firstCall

  firstCall = .TRUE.
  CALL LagrangeElemShapeData(obj=elemsd1, quad=quad, nsd=2, xidim=2, &
                             elemType=Quadrangle4, refelemCoord=quadCoord, &
                             domainName="BIUNIT", order=order, &
                             coeff=coeff, firstCall=firstCall)
  coeffCopy = coeff
  firstCall = .FALSE.
  CALL LagrangeElemShapeData(obj=elemsd2, quad=quad, nsd=2, xidim=2, &
                             elemType=Quadrangle4, refelemCoord=quadCoord, &
                             domainName="BIUNIT", order=order, &
                             coeff=coeff, firstCall=firstCall)
  CALL OK(ALL(ABS(coeff - coeffCopy) .LT. tol), &
          "firstCall false: coeff unchanged")
  CALL OK(ALL(ABS(elemsd2%N - ref%N) .LT. tol), "firstCall false: same N")
  CALL OK(ALL(ABS(elemsd2%dNdXi - ref%dNdXi) .LT. tol), &
          "firstCall false: same dN/dxi")
END BLOCK

! Test 3: biquadratic quadrangle, partition of unity
CALL OK(ALL(ABS(SUM(ref%N, dim=1) - 1.0_DFP) .LT. tol), &
        "biquadratic: sum of N = 1")
CALL OK(ALL(ABS(SUM(ref%dNdXi(:, 1, :), dim=1)) .LT. tol) .AND. &
        ALL(ABS(SUM(ref%dNdXi(:, 2, :), dim=1)) .LT. tol), &
        "biquadratic: sum of dN/dxi = 0")

END PROGRAM main
