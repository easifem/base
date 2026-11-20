!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Second derivative of the monomial basis on the biunit and the
! unit line.
!
! On the biunit line (x**2)'' = 2 and (x**3)'' = 6x. A unit line point u
! maps to x = 2u - 1, so the chain rule gives a factor 4.

PROGRAM main
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: OrthogonalBasisLaplacian_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xb(1, 3), xu(1, 3)

xb(1, :) = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
xu(1, :) = [0.25_DFP, 0.5_DFP, 0.75_DFP]

! Test 1: cubic monomial basis on the biunit line
BLOCK
  REAL(DFP) :: y(3, 4)
  y = OrthogonalBasisLaplacian_Line(order=3_I4B, xij=xb, &
                                    refLine="BIUNIT", basisType=poly%Monomial)
  CALL OK(MAXVAL(ABS(y(:, 1:2))) < tol, "1 and x have zero second derivative")
  CALL OK(MAXVAL(ABS(y(:, 3) - 2.0_DFP)) < tol, "(x**2)'' is 2")
  CALL OK(MAXVAL(ABS(y(:, 4) - 6.0_DFP * xb(1, :))) < tol, "(x**3)'' is 6x")
END BLOCK

! Test 2: unit line; second derivative of x**2 with respect to u is 8
BLOCK
  REAL(DFP) :: y(3, 3)
  y = OrthogonalBasisLaplacian_Line(order=2_I4B, xij=xu, refLine="UNIT", &
                                    basisType=poly%Monomial)
  CALL OK(MAXVAL(ABS(y(:, 3) - 8.0_DFP)) < tol, "UNIT: factor 4")
END BLOCK

END PROGRAM main
