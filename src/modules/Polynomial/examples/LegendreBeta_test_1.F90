!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Return a monic recurrence beta coefficient.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LegendrePolynomialUtility, ONLY: LegendreBeta
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: y

y = LegendreBeta(n=2_I4B)
CALL OK(ABS(y-(4.0_DFP/15.0_DFP)) < 1.0E-11_DFP, &
  "Degree two coefficient or norm")
END PROGRAM main
