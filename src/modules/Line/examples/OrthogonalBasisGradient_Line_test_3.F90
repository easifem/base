!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Gradient of the Jacobi basis needs alpha and beta.
!
! Jacobi polynomials with alpha = beta = 0 are the Legendre polynomials,
! so their gradients must agree.

PROGRAM main
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: OrthogonalBasisGradient_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xb(1, 3)

xb(1, :) = [-0.5_DFP, 0.0_DFP, 0.5_DFP]

! Test 1: Jacobi(0, 0) gradient equals Legendre gradient
BLOCK
  REAL(DFP) :: gj(3, 4, 1), gl(3, 4, 1)
  gj = OrthogonalBasisGradient_Line(order=3_I4B, xij=xb, refLine="BIUNIT", &
                                    basisType=poly%Jacobi, alpha=0.0_DFP, &
                                    beta=0.0_DFP)
  gl = OrthogonalBasisGradient_Line(order=3_I4B, xij=xb, refLine="BIUNIT", &
                                    basisType=poly%Legendre)
  CALL OK(MAXVAL(ABS(gj - gl)) < tol, "Jacobi(0,0)' equals Legendre'")
END BLOCK

! Test 2: Jacobi(1, 1) gradient of degree 1 is (alpha + beta + 2) / 2 = 2
BLOCK
  REAL(DFP) :: gj(3, 2, 1)
  gj = OrthogonalBasisGradient_Line(order=1_I4B, xij=xb, refLine="BIUNIT", &
                                    basisType=poly%Jacobi, alpha=1.0_DFP, &
                                    beta=1.0_DFP)
  CALL OK(MAXVAL(ABS(gj(:, 2, 1) - 2.0_DFP)) < tol, &
          "Derivative of P1^(1,1) is 2")
END BLOCK

END PROGRAM main
