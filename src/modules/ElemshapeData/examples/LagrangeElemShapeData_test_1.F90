!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Lagrange shape functions at quadrature points

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ReferenceLine_
USE BaseType, ONLY: TypeH1
USE BaseType, ONLY: TypeLagrangeInterpolation
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE ReferenceLine_Method, ONLY: ReferenceLine
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])

! Test 1: linear line, 2 Gauss points, N = (1 -+ xi) / 2
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  TYPE(QuadraturePoint_) :: quad
  REAL(DFP) :: xi(2)

  CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                   nips=[2], quadratureType=qpopt%GaussLegendre)
  CALL LagrangeElemShapeData(obj=elemsd, quad=quad, nsd=1, xidim=1, &
                             elemType=Line2, refelemCoord=lineCoord, &
                             domainName="BIUNIT", order=1)
  xi = quad%points(1, 1:2)

  CALL OK(elemsd%nns .EQ. 2 .AND. elemsd%nips .EQ. 2, "line: nns, nips")
  CALL OK(elemsd%nsd .EQ. 1 .AND. elemsd%xidim .EQ. 1, "line: nsd, xidim")
  CALL OK(ALL(ABS(elemsd%N(1, :) - 0.5_DFP * (1.0_DFP - xi)) .LT. tol), &
          "line: N1 = (1 - xi) / 2")
  CALL OK(ALL(ABS(elemsd%N(2, :) - 0.5_DFP * (1.0_DFP + xi)) .LT. tol), &
          "line: N2 = (1 + xi) / 2")
  CALL OK(ALL(ABS(elemsd%dNdXi(1, 1, :) + 0.5_DFP) .LT. tol), &
          "line: dN1/dxi = -1/2")
  CALL OK(ALL(ABS(elemsd%dNdXi(2, 1, :) - 0.5_DFP) .LT. tol), &
          "line: dN2/dxi = 1/2")
  CALL OK(ABS(SUM(elemsd%ws) - 2.0_DFP) .LT. tol, "line: sum of ws = 2")
END BLOCK

! Test 2: quadratic line, partition of unity
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  TYPE(QuadraturePoint_) :: quad

  CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                   nips=[3], quadratureType=qpopt%GaussLegendre)
  CALL LagrangeElemShapeData(obj=elemsd, quad=quad, nsd=1, xidim=1, &
                             elemType=Line2, refelemCoord=lineCoord, &
                             domainName="BIUNIT", order=2)
  CALL OK(elemsd%nns .EQ. 3, "quadratic line: nns = 3")
  CALL OK(ALL(ABS(SUM(elemsd%N, dim=1) - 1.0_DFP) .LT. tol), &
          "quadratic line: sum of N = 1")
  CALL OK(ALL(ABS(SUM(elemsd%dNdXi(:, 1, :), dim=1)) .LT. tol), &
          "quadratic line: sum of dN/dxi = 0")
END BLOCK

! Test 3: bilinear quadrangle, 2 x 2 Gauss points
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  TYPE(QuadraturePoint_) :: quad
  INTEGER(I4B) :: ii

  CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                   nips=[2], quadratureType=qpopt%GaussLegendre)
  CALL LagrangeElemShapeData(obj=elemsd, quad=quad, nsd=2, xidim=2, &
                             elemType=Quadrangle4, refelemCoord=quadCoord, &
                             domainName="BIUNIT", order=1)
  CALL OK(elemsd%nns .EQ. 4 .AND. elemsd%nips .EQ. 4, &
          "quadrangle: nns = 4, nips = 4")
  CALL OK(ALL(ABS(SUM(elemsd%N, dim=1) - 1.0_DFP) .LT. tol), &
          "quadrangle: sum of N = 1")
  DO ii = 1, 2
    CALL OK(ALL(ABS(SUM(elemsd%dNdXi(:, ii, :), dim=1)) .LT. tol), &
            "quadrangle: sum of dN/dxi = 0")
  END DO
  CALL OK(ABS(SUM(elemsd%ws) - 4.0_DFP) .LT. tol, &
          "quadrangle: sum of ws = 4")
  ! N1 = (1 - xi)(1 - eta) / 4 at the first point
  CALL OK(ABS(elemsd%N(1, 1) - 0.25_DFP * (1.0_DFP - quad%points(1, 1)) &
              * (1.0_DFP - quad%points(2, 1))) .LT. tol, &
          "quadrangle: N1 at the first point")
END BLOCK

! Test 4: from a reference element, and with H1 Lagrange type arguments
BLOCK
  TYPE(ElemShapeData_) :: elemsd1, elemsd2, elemsd3
  TYPE(QuadraturePoint_) :: quad
  TYPE(ReferenceLine_) :: refelem

  refelem = ReferenceLine(nsd=1)
  CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                   nips=[2], quadratureType=qpopt%GaussLegendre)
  CALL LagrangeElemShapeData(obj=elemsd1, quad=quad, nsd=1, xidim=1, &
                             elemType=Line2, refelemCoord=lineCoord, &
                             domainName="BIUNIT", order=1)
  CALL LagrangeElemShapeData(obj=elemsd2, quad=quad, refelem=refelem, &
                             order=1)
  CALL Initiate(obj=elemsd3, quad=quad, refelem=refelem, &
                baseContinuity=TypeH1, &
                baseInterpolation=TypeLagrangeInterpolation, order=1)
  CALL OK(ALL(ABS(elemsd2%N - elemsd1%N) .LT. tol), "refelem: same N")
  CALL OK(ALL(ABS(elemsd2%dNdXi - elemsd1%dNdXi) .LT. tol), &
          "refelem: same dN/dxi")
  CALL OK(ALL(ABS(elemsd3%N - elemsd1%N) .LT. tol), "H1 Lagrange: same N")
END BLOCK

END PROGRAM main
