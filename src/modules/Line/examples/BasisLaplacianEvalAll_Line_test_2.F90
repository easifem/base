!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Second derivative of the monomial basis at one point.

PROGRAM main
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: BasisLaplacianEvalAll_Line
USE LineInterpolationUtility, ONLY: BasisLaplacianEvalAll_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: x = 0.5_DFP

! Test 1: second derivatives of 1, x, x**2, x**3
BLOCK
  REAL(DFP) :: y(4)
  y = BasisLaplacianEvalAll_Line(order=3_I4B, x=x, refLine="BIUNIT", &
                                 basisType=poly%Monomial)
  CALL OK(MAXVAL(ABS(y - [0.0_DFP, 0.0_DFP, 2.0_DFP, 3.0_DFP])) < tol, &
          "Monomial second derivatives at x = 0.5")
END BLOCK

! Test 2: subroutine; monomial basis is the default
BLOCK
  REAL(DFP) :: y(4)
  INTEGER(I4B) :: tsize
  CALL BasisLaplacianEvalAll_Line_(order=3_I4B, x=x, ans=y, tsize=tsize, &
                                   refLine="BIUNIT")
  CALL OK(tsize == 4, "Subroutine writes order+1 values")
  CALL OK(MAXVAL(ABS(y - [0.0_DFP, 0.0_DFP, 2.0_DFP, 3.0_DFP])) < tol, &
          "Subroutine default basis is monomial")
END BLOCK

! Test 3: order 1 has zero second derivatives
BLOCK
  REAL(DFP) :: y(2)
  y = BasisLaplacianEvalAll_Line(order=1_I4B, x=x, refLine="BIUNIT")
  CALL OK(MAXVAL(ABS(y)) < tol, "Linear basis has zero second derivative")
END BLOCK

END PROGRAM main
