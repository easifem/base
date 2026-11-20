!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing LagrangeCoeff_Tetrahedron for one basis function

PROGRAM main
USE GE_LUMethods, ONLY: GetLU
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
USE TetrahedronInterpolationUtility, ONLY: EquidistancePoint_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: LagrangeCoeff_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: LagrangeCoeff_Tetrahedron_
USE TetrahedronInterpolationUtility, ONLY: LagrangeDegree_Tetrahedron
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: order = 2, n = 10, i = 3
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), ALLOCATABLE :: nodes(:, :)
INTEGER(I4B), ALLOCATABLE :: degree(:, :)
REAL(DFP) :: v(n, n), unitVec(n)
INTEGER(I4B) :: ii, jj

! Quadratic nodes and the monomial Vandermonde matrix v(node, monomial)
nodes = EquidistancePoint_Tetrahedron(order=order)
degree = LagrangeDegree_Tetrahedron(order=order)
DO jj = 1, n
  DO ii = 1, n
    v(ii, jj) = PRODUCT(nodes(:, ii)**degree(jj, :))
  END DO
END DO
unitVec = 0.0_DFP
unitVec(i) = 1.0_DFP

! Test 1: coefficients from the nodes, v * c = e_i
BLOCK
  REAL(DFP) :: c(n)
  c = LagrangeCoeff_Tetrahedron(order=order, i=i, xij=nodes)
  CALL OK(MAXVAL(ABS(MATMUL(v, c) - unitVec)) .LT. tol, &
          "xij overload: basis i is 1 at node i and 0 elsewhere")
END BLOCK

! Test 2: coefficients from a given Vandermonde matrix
BLOCK
  REAL(DFP) :: c(n)
  c = LagrangeCoeff_Tetrahedron(order=order, i=i, v=v, &
                                isVandermonde=.TRUE._LGT)
  CALL OK(MAXVAL(ABS(MATMUL(v, c) - unitVec)) .LT. tol, &
          "Vandermonde overload: v * c = e_i")
END BLOCK

! Test 3: coefficients from the LU factors of the Vandermonde matrix
BLOCK
  REAL(DFP) :: c(n), lu(n, n)
  INTEGER(I4B) :: ipiv(n)
  lu = v
  CALL GetLU(A=lu, IPIV=ipiv)
  c = LagrangeCoeff_Tetrahedron(order=order, i=i, v=lu, ipiv=ipiv)
  CALL OK(MAXVAL(ABS(MATMUL(v, c) - unitVec)) .LT. tol, &
          "LU overload: v * c = e_i")
END BLOCK

! Test 4: subroutine form returns the same column and its size
BLOCK
  REAL(DFP) :: c(n), cSub(n)
  INTEGER(I4B) :: tsize
  c = LagrangeCoeff_Tetrahedron(order=order, i=i, xij=nodes)
  CALL LagrangeCoeff_Tetrahedron_(order=order, i=i, xij=nodes, ans=cSub, &
                                  tsize=tsize)
  CALL OK(tsize .EQ. n, "subroutine form: tsize")
  CALL OK(MAXVAL(ABS(cSub - c)) .LT. tol, "subroutine form: same values")
END BLOCK

! Test 5: column i of the full coefficient matrix is the same column
BLOCK
  REAL(DFP) :: c(n), cAll(n, n)
  c = LagrangeCoeff_Tetrahedron(order=order, i=i, xij=nodes)
  cAll = LagrangeCoeff_Tetrahedron(order=order, xij=nodes)
  CALL OK(MAXVAL(ABS(cAll(:, i) - c)) .LT. tol, &
          "full matrix column i agrees")
END BLOCK

END PROGRAM main
