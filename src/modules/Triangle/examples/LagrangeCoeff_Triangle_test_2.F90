!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing the function overloads of LagrangeCoeff_Triangle that
! return the coefficients of one basis function

PROGRAM main
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GE_LUMethods, ONLY: GetLU
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: EquidistancePoint_Triangle
USE TriangleInterpolationUtility, ONLY: LagrangeCoeff_Triangle
USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: order = 2, n = 6
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), ALLOCATABLE :: xij(:, :)
INTEGER(I4B), ALLOCATABLE :: degree(:, :)
REAL(DFP) :: V(n, n), C(n, n)
INTEGER(I4B) :: ii, jj

! quadratic nodes on the unit triangle and their Vandermonde matrix
xij = EquidistancePoint_Triangle(order=order)
degree = LagrangeDegree_Triangle(order=order)
DO jj = 1, n
  DO ii = 1, n
    V(ii, jj) = PRODUCT(xij(:, ii)**degree(jj, :))
  END DO
END DO

! all coefficients at once; column i belongs to basis function i
C = LagrangeCoeff_Triangle(order=order, xij=xij, basisType=poly%monomial, &
                           refTriangle="UNIT")

! Test 1: coefficients of basis i from the nodes
BLOCK
  REAL(DFP) :: ans(n)
  LOGICAL(LGT) :: isok

  isok = .TRUE.
  DO ii = 1, n
    ans = LagrangeCoeff_Triangle(order=order, i=ii, xij=xij)
    isok = isok .AND. (MAXVAL(ABS(ans - C(:, ii))) .LT. tol)
  END DO
  CALL OK(isok, "coefficients from nodes match column i")
END BLOCK

! Test 2: coefficients of basis i from the Vandermonde matrix
BLOCK
  REAL(DFP) :: ans(n)
  LOGICAL(LGT) :: isok

  isok = .TRUE.
  DO ii = 1, n
    ans = LagrangeCoeff_Triangle(order=order, i=ii, v=V, &
                                 isVandermonde=.TRUE.)
    isok = isok .AND. (MAXVAL(ABS(ans - C(:, ii))) .LT. tol)
  END DO
  CALL OK(isok, "coefficients from Vandermonde match column i")
END BLOCK

! Test 3: coefficients of basis i from the LU factors of V
BLOCK
  REAL(DFP) :: ans(n), lu(n, n)
  INTEGER(I4B) :: ipiv(n), info
  LOGICAL(LGT) :: isok

  lu = V
  CALL GetLU(A=lu, IPIV=ipiv, info=info)

  isok = .TRUE.
  DO ii = 1, n
    ans = LagrangeCoeff_Triangle(order=order, i=ii, v=lu, ipiv=ipiv)
    isok = isok .AND. (MAXVAL(ABS(ans - C(:, ii))) .LT. tol)
  END DO
  CALL OK(isok, "coefficients from LU factors match column i")
END BLOCK

! Test 4: basis i is one at node i and zero at the other nodes
BLOCK
  REAL(DFP) :: ans(n), values(n), delta(n)

  ans = LagrangeCoeff_Triangle(order=order, i=4_I4B, xij=xij)
  values = MATMUL(V, ans)
  delta = 0.0_DFP
  delta(4) = 1.0_DFP
  CALL OK(MAXVAL(ABS(values - delta)) .LT. tol, "delta property")
END BLOCK

END PROGRAM main
