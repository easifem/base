!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Orthogonal (Legendre) shape functions at quadrature points

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ReferenceLine_
USE BaseType, ONLY: TypeH1
USE BaseType, ONLY: TypeOrthogonalInterpolation
USE BaseType, ONLY: poly => TypePolynomialOpt
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: OrthogonalElemShapeData
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

! Test 1: line, order 2, Legendre polynomials P0, P1, P2
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  TYPE(QuadraturePoint_) :: quad
  REAL(DFP) :: xi(3)

  CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                   nips=[3], quadratureType=qpopt%GaussLegendre)
  CALL OrthogonalElemShapeData(obj=elemsd, quad=quad, nsd=1, xidim=1, &
                               elemType=Line2, refelemCoord=lineCoord, &
                               domainName="BIUNIT", order=2, &
                               basisType=poly%Legendre)
  xi = quad%points(1, 1:3)

  CALL OK(elemsd%nns .EQ. 3 .AND. elemsd%nips .EQ. 3, "line: nns, nips")
  CALL OK(ALL(SHAPE(elemsd%dNdXi) .EQ. [3, 1, 3]), "line: shape of dNdXi")
  CALL OK(ALL(ABS(elemsd%N(1, :) - 1.0_DFP) .LT. tol), "line: N1 = 1")
  CALL OK(ALL(ABS(elemsd%N(2, :) - xi) .LT. tol), "line: N2 = xi")
  CALL OK(ALL(ABS(elemsd%N(3, :) - 0.5_DFP * (3.0_DFP * xi**2 - 1.0_DFP)) &
              .LT. tol), "line: N3 = (3 xi^2 - 1) / 2")
  CALL OK(ALL(ABS(elemsd%dNdXi(1, 1, :)) .LT. tol), "line: dN1/dxi = 0")
  CALL OK(ALL(ABS(elemsd%dNdXi(2, 1, :) - 1.0_DFP) .LT. tol), &
          "line: dN2/dxi = 1")
  CALL OK(ALL(ABS(elemsd%dNdXi(3, 1, :) - 3.0_DFP * xi) .LT. tol), &
          "line: dN3/dxi = 3 xi")
  CALL OK(ABS(SUM(elemsd%ws) - 2.0_DFP) .LT. tol, "line: sum of ws = 2")
END BLOCK

! Test 2: line, the mass matrix of the Legendre basis is diagonal
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  TYPE(QuadraturePoint_) :: quad
  REAL(DFP) :: mass(3, 3)

  CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                   nips=[3], quadratureType=qpopt%GaussLegendre)
  CALL OrthogonalElemShapeData(obj=elemsd, quad=quad, nsd=1, xidim=1, &
                               elemType=Line2, refelemCoord=lineCoord, &
                               domainName="BIUNIT", order=2, &
                               basisType=poly%Legendre)
  mass = MassMatrix(elemsd)
  CALL OK(ABS(mass(1, 1) - 2.0_DFP) .LT. tol, "line mass: M11 = 2")
  CALL OK(ABS(mass(2, 2) - 2.0_DFP / 3.0_DFP) .LT. tol, &
          "line mass: M22 = 2/3")
  CALL OK(ABS(mass(3, 3) - 2.0_DFP / 5.0_DFP) .LT. tol, &
          "line mass: M33 = 2/5")
  CALL OK(OffDiagMax(mass) .LT. tol, "line mass: off-diagonal = 0")
END BLOCK

! Test 3: quadrangle, order 1, tensor product Legendre basis
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  TYPE(QuadraturePoint_) :: quad
  REAL(DFP) :: mass(4, 4)
  REAL(DFP) :: trace
  INTEGER(I4B) :: ii

  CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                   nips=[2], quadratureType=qpopt%GaussLegendre)
  CALL OrthogonalElemShapeData(obj=elemsd, quad=quad, nsd=2, xidim=2, &
                               elemType=Quadrangle4, &
                               refelemCoord=quadCoord, &
                               domainName="BIUNIT", order=1, &
                               basisType=poly%Legendre)
  CALL OK(elemsd%nns .EQ. 4 .AND. elemsd%nips .EQ. 4, &
          "quadrangle: nns = 4, nips = 4")
  CALL OK(ALL(SHAPE(elemsd%dNdXi) .EQ. [4, 2, 4]), &
          "quadrangle: shape of dNdXi")
  CALL OK(ABS(SUM(elemsd%ws) - 4.0_DFP) .LT. tol, &
          "quadrangle: sum of ws = 4")

  ! basis {1, xi, eta, xi * eta}: the diagonal is {4, 4/3, 4/3, 4/9}
  mass = MassMatrix(elemsd)
  trace = 0.0_DFP
  DO ii = 1, 4
    trace = trace + mass(ii, ii)
  END DO
  CALL OK(ABS(trace - 64.0_DFP / 9.0_DFP) .LT. tol, &
          "quadrangle mass: trace = 64/9")
  CALL OK(OffDiagMax(mass) .LT. tol, "quadrangle mass: off-diagonal = 0")
END BLOCK

! Test 4: from a reference element, and with H1 orthogonal type arguments
BLOCK
  TYPE(ElemShapeData_) :: elemsd1, elemsd2, elemsd3
  TYPE(QuadraturePoint_) :: quad
  TYPE(ReferenceLine_) :: refelem

  refelem = ReferenceLine(nsd=1)
  CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                   nips=[3], quadratureType=qpopt%GaussLegendre)
  CALL OrthogonalElemShapeData(obj=elemsd1, quad=quad, nsd=1, xidim=1, &
                               elemType=Line2, refelemCoord=lineCoord, &
                               domainName="BIUNIT", order=2, &
                               basisType=poly%Legendre)
  CALL OrthogonalElemShapeData(obj=elemsd2, quad=quad, refelem=refelem, &
                               order=2, basisType=poly%Legendre)
  CALL Initiate(obj=elemsd3, quad=quad, refelem=refelem, &
                baseContinuity=TypeH1, &
                baseInterpolation=TypeOrthogonalInterpolation, order=2, &
                basisType=poly%Legendre)
  CALL OK(ALL(ABS(elemsd2%N - elemsd1%N) .LT. tol), "refelem: same N")
  CALL OK(ALL(ABS(elemsd2%dNdXi - elemsd1%dNdXi) .LT. tol), &
          "refelem: same dN/dxi")
  CALL OK(ALL(ABS(elemsd3%N - elemsd1%N) .LT. tol), &
          "H1 orthogonal: same N")
  CALL OK(ALL(ABS(elemsd3%dNdXi - elemsd1%dNdXi) .LT. tol), &
          "H1 orthogonal: same dN/dxi")
END BLOCK

CONTAINS

! mass(i, j) = sum over quadrature points of ws * N(i) * N(j)
FUNCTION MassMatrix(elemsd) RESULT(ans)
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd
  REAL(DFP) :: ans(elemsd%nns, elemsd%nns)
  INTEGER(I4B) :: ii, jj

  DO jj = 1, elemsd%nns
    DO ii = 1, elemsd%nns
      ans(ii, jj) = SUM(elemsd%ws(1:elemsd%nips) &
                        * elemsd%N(ii, 1:elemsd%nips) &
                        * elemsd%N(jj, 1:elemsd%nips))
    END DO
  END DO
END FUNCTION MassMatrix

! largest absolute off-diagonal entry of a square matrix
FUNCTION OffDiagMax(mat) RESULT(ans)
  REAL(DFP), INTENT(IN) :: mat(:, :)
  REAL(DFP) :: ans
  INTEGER(I4B) :: ii, jj

  ans = 0.0_DFP
  DO jj = 1, SIZE(mat, 2)
    DO ii = 1, SIZE(mat, 1)
      IF (ii .NE. jj) ans = MAX(ans, ABS(mat(ii, jj)))
    END DO
  END DO
END FUNCTION OffDiagMax

END PROGRAM main
