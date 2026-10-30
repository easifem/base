!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Return the coefficient of the highest power.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LobattoPolynomialUtility, ONLY: LobattoLeadingCoeff
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: y

y = LobattoLeadingCoeff(n=2_I4B)
CALL OK(ABS(y-(1.5_DFP/SQRT(6.0_DFP))) < 1.0E-11_DFP, &
  "Degree two coefficient or norm")
END PROGRAM main
