!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Return weighted squared norms from degree zero through n.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LegendrePolynomialUtility, ONLY: LegendreNormSQR2
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: y(0:2)

y = LegendreNormSQR2(n=2_I4B)
CALL OK(ABS(y(2)-(2.0_DFP/5.0_DFP)) < 1.0E-11_DFP, &
  "Norm array includes degree two")
END PROGRAM main
