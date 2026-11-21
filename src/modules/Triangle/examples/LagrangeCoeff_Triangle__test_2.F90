!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing the subroutine overloads of LagrangeCoeff_Triangle_

PROGRAM main
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GE_LUMethods, ONLY: GetLU
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: Dubiner_Triangle
USE TriangleInterpolationUtility, ONLY: EquidistancePoint_Triangle
USE TriangleInterpolationUtility, ONLY: LagrangeCoeff_Triangle
USE TriangleInterpolationUtility, ONLY: LagrangeCoeff_Triangle_
USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: order = 2, n = 6
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), ALLOCATABLE :: xij(:, :)
INTEGER(I4B), ALLOCATABLE :: degree(:, :)
REAL(DFP) :: V(n, n), C(n, n)
INTEGER(I4B) :: ii, jj

xij = EquidistancePoint_Triangle(order=order)
degree = LagrangeDegree_Triangle(order=order)
DO jj = 1, n
  DO ii = 1, n
    V(ii, jj) = PRODUCT(xij(:, ii)**degree(jj, :))
  END DO
END DO

C = LagrangeCoeff_Triangle(order=order, xij=xij, basisType=poly%monomial, &
                           refTriangle="UNIT")

! Test 1: one basis function from the nodes
BLOCK
  REAL(DFP) :: ans(n + 2)
  INTEGER(I4B) :: tsize
  LOGICAL(LGT) :: isok

  isok = .TRUE.
  DO ii = 1, n
    CALL LagrangeCoeff_Triangle_(order=order, i=ii, xij=xij, ans=ans, &
                                 tsize=tsize)
    isok = isok .AND. (tsize .EQ. n) .AND. &
           (MAXVAL(ABS(ans(1:tsize) - C(:, ii))) .LT. tol)
  END DO
  CALL OK(isok, "from nodes: tsize and coefficients")
END BLOCK

! Test 2: one basis function from the Vandermonde matrix
BLOCK
  REAL(DFP) :: ans(n)
  INTEGER(I4B) :: tsize
  LOGICAL(LGT) :: isok

  isok = .TRUE.
  DO ii = 1, n
    CALL LagrangeCoeff_Triangle_(order=order, i=ii, v=V, &
                                 isVandermonde=.TRUE., ans=ans, tsize=tsize)
    isok = isok .AND. (tsize .EQ. n) .AND. &
           (MAXVAL(ABS(ans - C(:, ii))) .LT. tol)
  END DO
  CALL OK(isok, "from Vandermonde: tsize and coefficients")
END BLOCK

! Test 3: one basis function from the LU factors
BLOCK
  REAL(DFP) :: ans(n), lu(n, n)
  INTEGER(I4B) :: ipiv(n), info, tsize
  LOGICAL(LGT) :: isok

  lu = V
  CALL GetLU(A=lu, IPIV=ipiv, info=info)

  isok = .TRUE.
  DO ii = 1, n
    CALL LagrangeCoeff_Triangle_(order=order, i=ii, v=lu, ipiv=ipiv, &
                                 ans=ans, tsize=tsize)
    isok = isok .AND. (tsize .EQ. n) .AND. &
           (MAXVAL(ABS(ans - C(:, ii))) .LT. tol)
  END DO
  CALL OK(isok, "from LU factors: tsize and coefficients")
END BLOCK

! Test 4: all basis functions with a given degree table
BLOCK
  REAL(DFP) :: ans(n, n)
  INTEGER(I4B) :: nrow, ncol

  CALL LagrangeCoeff_Triangle_(order=order, xij=xij, &
                               basisType=poly%monomial, refTriangle="UNIT", &
                               degree=degree, ans=ans, nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. n .AND. ncol .EQ. n, "with degree: size")
  CALL OK(MAXVAL(ABS(ans - C)) .LT. tol, "with degree: coefficients")
END BLOCK

! Test 5: orthogonal basis, D * C = I where D(i, j) is Dubiner mode j
! at node i
BLOCK
  REAL(DFP) :: ans(n, n), D(n, n)
  INTEGER(I4B) :: nrow, ncol

  CALL LagrangeCoeff_Triangle_(order=order, xij=xij, &
                               basisType=poly%orthogonal, &
                               refTriangle="UNIT", ans=ans, nrow=nrow, &
                               ncol=ncol)
  CALL OK(nrow .EQ. n .AND. ncol .EQ. n, "orthogonal: size")

  D = Dubiner_Triangle(order=order, xij=xij, refTriangle="UNIT")
  D = MATMUL(D, ans)
  DO ii = 1, n
    D(ii, ii) = D(ii, ii) - 1.0_DFP
  END DO
  CALL OK(MAXVAL(ABS(D)) .LT. tol, "orthogonal: D * C is identity")
END BLOCK

END PROGRAM main
