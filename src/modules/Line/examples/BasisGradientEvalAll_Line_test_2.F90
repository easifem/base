!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Gradient of monomial and Legendre bases at one point.

PROGRAM main
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: BasisGradientEvalAll_Line
USE LineInterpolationUtility, ONLY: BasisGradientEvalAll_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: x = 0.5_DFP

! Test 1: derivatives of 1, x, x**2, x**3
BLOCK
  REAL(DFP) :: y(4)
  y = BasisGradientEvalAll_Line(order=3_I4B, x=x, refLine="BIUNIT", &
                                basisType=poly%Monomial)
  CALL OK(MAXVAL(ABS(y - [0.0_DFP, 1.0_DFP, 1.0_DFP, 0.75_DFP])) < tol, &
          "Monomial gradients at x = 0.5")
END BLOCK

! Test 2: derivatives of Legendre P0 to P3
BLOCK
  REAL(DFP) :: y(4)
  y = BasisGradientEvalAll_Line(order=3_I4B, x=x, refLine="BIUNIT", &
                                basisType=poly%Legendre)
  CALL OK(MAXVAL(ABS(y - [0.0_DFP, 1.0_DFP, 1.5_DFP, 0.375_DFP])) &
          < 1.0E-10_DFP, "Legendre gradients at x = 0.5")
END BLOCK

! Test 3: subroutine; monomial basis is the default
BLOCK
  REAL(DFP) :: y(4)
  INTEGER(I4B) :: tsize
  CALL BasisGradientEvalAll_Line_(order=3_I4B, x=x, ans=y, tsize=tsize, &
                                  refLine="BIUNIT")
  CALL OK(tsize == 4, "Subroutine writes order+1 values")
  CALL OK(MAXVAL(ABS(y - [0.0_DFP, 1.0_DFP, 1.0_DFP, 0.75_DFP])) < tol, &
          "Subroutine default basis is monomial")
END BLOCK

END PROGRAM main
