!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing TensorProdBasisGradient_Quadrangle with a different
! basis in each direction

PROGRAM main
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: TensorProdBasisGradient_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: xij(2, 3)

xij(:, 1) = [0.2_DFP, -0.4_DFP]
xij(:, 2) = [-0.3_DFP, 0.5_DFP]
xij(:, 3) = [0.7_DFP, 0.9_DFP]

! Test 1: monomial in x and Legendre in y, p=1, q=2
! Mode (1, 2) is x * P2(y) with P2(y) = (3 y^2 - 1) / 2
BLOCK
  REAL(DFP) :: g(3, 6, 2), dx(3), dy(3)

  g = TensorProdBasisGradient_Quadrangle(p=1_I4B, q=2_I4B, xij=xij, &
                                         basisType1=poly%monomial, &
                                         basisType2=poly%legendre)
  dx = 0.5_DFP * (3.0_DFP * xij(2, :)**2 - 1.0_DFP)
  dy = 3.0_DFP * xij(1, :) * xij(2, :)
  CALL OK(MAXVAL(ABS(g(:, 6, 1) - dx)) .LT. tol, &
          "d/dx of x * P2(y) is P2(y)")
  CALL OK(MAXVAL(ABS(g(:, 6, 2) - dy)) .LT. tol, &
          "d/dy of x * P2(y) is 3 x y")
END BLOCK

! Test 2: the constant mode has a zero gradient
BLOCK
  REAL(DFP) :: g(3, 6, 2)

  g = TensorProdBasisGradient_Quadrangle(p=1_I4B, q=2_I4B, xij=xij, &
                                         basisType1=poly%legendre, &
                                         basisType2=poly%legendre)
  CALL OK(MAXVAL(ABS(g(:, 1, :))) .LT. tol, "constant mode: zero gradient")
END BLOCK

END PROGRAM main
