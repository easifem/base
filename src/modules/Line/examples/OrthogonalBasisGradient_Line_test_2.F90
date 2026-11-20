!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Gradient of the Legendre basis on the biunit and the unit line.
!
! P2'(x) = 3x. On the unit line the chain rule gives a factor 2.

PROGRAM main
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: OrthogonalBasisGradient_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xb(1, 3), xu(1, 3)

xb(1, :) = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
xu(1, :) = [0.25_DFP, 0.5_DFP, 0.75_DFP]

! Test 1: Legendre gradients on the biunit line
BLOCK
  REAL(DFP) :: g(3, 3, 1)
  g = OrthogonalBasisGradient_Line(order=2_I4B, xij=xb, refLine="BIUNIT", &
                                   basisType=poly%Legendre)
  CALL OK(MAXVAL(ABS(g(:, 1, 1))) < tol, "P0' is zero")
  CALL OK(MAXVAL(ABS(g(:, 2, 1) - 1.0_DFP)) < tol, "P1' is one")
  CALL OK(MAXVAL(ABS(g(:, 3, 1) - 3.0_DFP * xb(1, :))) < tol, "P2' is 3x")
END BLOCK

! Test 2: Legendre gradients on the unit line
BLOCK
  REAL(DFP) :: g(3, 3, 1)
  g = OrthogonalBasisGradient_Line(order=2_I4B, xij=xu, refLine="UNIT", &
                                   basisType=poly%Legendre)
  CALL OK(MAXVAL(ABS(g(:, 3, 1) - 6.0_DFP * xb(1, :))) < tol, &
          "UNIT: P2' scaled by 2")
END BLOCK

END PROGRAM main
