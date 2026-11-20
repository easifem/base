!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Monomial and Legendre basis values at one point.

PROGRAM main
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: BasisEvalAll_Line
USE LineInterpolationUtility, ONLY: BasisEvalAll_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: x = 0.5_DFP

! Test 1: monomials 1, x, x**2, x**3
BLOCK
  REAL(DFP) :: y(4)
  y = BasisEvalAll_Line(order=3_I4B, x=x, refLine="BIUNIT", &
                        basisType=poly%Monomial)
  CALL OK(MAXVAL(ABS(y - [1.0_DFP, 0.5_DFP, 0.25_DFP, 0.125_DFP])) &
          < tol, "Monomial values at x = 0.5")
END BLOCK

! Test 2: Legendre polynomials P0 to P3
BLOCK
  REAL(DFP) :: y(4)
  y = BasisEvalAll_Line(order=3_I4B, x=x, refLine="BIUNIT", &
                        basisType=poly%Legendre)
  CALL OK(MAXVAL(ABS(y - [1.0_DFP, 0.5_DFP, -0.125_DFP, -0.4375_DFP])) &
          < tol, "Legendre values at x = 0.5")
END BLOCK

! Test 3: subroutine; monomial basis is the default
BLOCK
  REAL(DFP) :: y(6)
  INTEGER(I4B) :: tsize
  CALL BasisEvalAll_Line_(order=3_I4B, x=x, ans=y, tsize=tsize, &
                          refLine="BIUNIT")
  CALL OK(tsize == 4, "Subroutine writes order+1 values")
  CALL OK(MAXVAL(ABS(y(1:4) - [1.0_DFP, 0.5_DFP, 0.25_DFP, 0.125_DFP])) &
          < tol, "Subroutine default basis is monomial")
END BLOCK

END PROGRAM main
