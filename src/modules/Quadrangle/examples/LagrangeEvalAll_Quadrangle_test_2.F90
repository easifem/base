!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing LagrangeEvalAll_Quadrangle at one point, coefficient
! reuse, basis independence, and the subroutine form with work arrays

PROGRAM main
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE QuadrangleInterpolationUtility, ONLY: EquidistancePoint_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: LagrangeEvalAll_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: LagrangeEvalAll_Quadrangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), ALLOCATABLE :: nodes(:, :)
REAL(DFP) :: coeff(9, 9)

! Quadratic equidistant nodes on the biunit square, VEFC layout
nodes = EquidistancePoint_Quadrangle(order=2_I4B)

! Test 1: at one node the values are a Kronecker delta
BLOCK
  REAL(DFP) :: y(9), e(9)

  y = LagrangeEvalAll_Quadrangle(order=2_I4B, x=nodes(:, 5), xij=nodes, &
                                 coeff=coeff, firstCall=.TRUE.)
  e = 0.0_DFP
  e(5) = 1.0_DFP
  CALL OK(MAXVAL(ABS(y - e)) .LT. tol, "single point: delta at node 5")
END BLOCK

! Test 2: reuse the coefficients with firstCall = .FALSE.
BLOCK
  REAL(DFP) :: y(9)

  y = LagrangeEvalAll_Quadrangle(order=2_I4B, x=[0.3_DFP, -0.2_DFP], &
                                 xij=nodes, coeff=coeff, firstCall=.FALSE.)
  CALL OK(ABS(SUM(y) - 1.0_DFP) .LT. tol, &
          "single point: partition of unity with stored coeff")
END BLOCK

! Test 3: the Lagrange basis does not depend on the auxiliary basis
BLOCK
  REAL(DFP) :: x(2, 2), ym(2, 9), yh(2, 9), yl(2, 9)

  x(:, 1) = [0.3_DFP, -0.2_DFP]
  x(:, 2) = [-0.7_DFP, 0.45_DFP]
  ym = LagrangeEvalAll_Quadrangle(order=2_I4B, x=x, xij=nodes, &
                                  basisType=poly%monomial)
  yh = LagrangeEvalAll_Quadrangle(order=2_I4B, x=x, xij=nodes, &
                                  basisType=poly%hierarchical)
  yl = LagrangeEvalAll_Quadrangle(order=2_I4B, x=x, xij=nodes, &
                                  basisType=poly%legendre)
  CALL OK(MAXVAL(ABS(ym - yh)) .LT. tol, &
          "monomial and hierarchical give the same basis")
  CALL OK(MAXVAL(ABS(ym - yl)) .LT. tol, &
          "monomial and Legendre give the same basis")
END BLOCK

! Test 4: subroutine form with caller supplied coeff and work array
BLOCK
  REAL(DFP) :: ans(9, 9), c(9, 9), xx(9, 9)
  INTEGER(I4B) :: nrow, ncol, ii
  LOGICAL(LGT) :: isok

  CALL LagrangeEvalAll_Quadrangle_(order=2_I4B, x=nodes, xij=nodes, &
                                   ans=ans, nrow=nrow, ncol=ncol, coeff=c, &
                                   xx=xx, firstCall=.TRUE.)
  CALL OK(nrow .EQ. 9 .AND. ncol .EQ. 9, "subroutine form: nrow, ncol")
  DO ii = 1, 9
    ans(ii, ii) = ans(ii, ii) - 1.0_DFP
  END DO
  isok = MAXVAL(ABS(ans)) .LT. tol
  CALL OK(isok, "subroutine form: identity at the nodes")
END BLOCK

END PROGRAM main
