!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Legendre basis on the biunit and the unit line.
!
! Unit line points 0.25, 0.5, 0.75 map to biunit points -0.5, 0, 0.5.
! P2(x) = (3x**2 - 1)/2.

PROGRAM main
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: OrthogonalBasis_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: xb(1, 3), xu(1, 3), p2(3)

xb(1, :) = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
xu(1, :) = [0.25_DFP, 0.5_DFP, 0.75_DFP]
p2 = [-0.125_DFP, -0.5_DFP, -0.125_DFP]

! Test 1: Legendre basis on the biunit line
BLOCK
  REAL(DFP) :: y(3, 3)
  y = OrthogonalBasis_Line(order=2_I4B, xij=xb, refLine="BIUNIT", &
                           basisType=poly%Legendre)
  CALL OK(MAXVAL(ABS(y(:, 1) - 1.0_DFP)) < tol, "P0 is one")
  CALL OK(MAXVAL(ABS(y(:, 2) - xb(1, :))) < tol, "P1 is x")
  CALL OK(MAXVAL(ABS(y(:, 3) - p2)) < tol, "P2 values")
END BLOCK

! Test 2: unit line points give the same values
BLOCK
  REAL(DFP) :: y(3, 3)
  y = OrthogonalBasis_Line(order=2_I4B, xij=xu, refLine="UNIT", &
                           basisType=poly%Legendre)
  CALL OK(MAXVAL(ABS(y(:, 3) - p2)) < tol, "UNIT: P2 values")
END BLOCK

END PROGRAM main
