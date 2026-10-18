!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromLine2Line_ maps between the unit and biunit line

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: FromLine2Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xb(3), xu(3), ans(3)

xb = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
xu = [0.0_DFP, 0.5_DFP, 1.0_DFP]

! biunit to unit
CALL FromLine2Line_(xin=xb, ans=ans, from="BIUNIT", to="UNIT")
CALL OK(ALL(ABS(ans - xu) .LT. tol), "BIUNIT to UNIT")

! unit to biunit
CALL FromLine2Line_(xin=xu, ans=ans, from="UNIT", to="BIUNIT")
CALL OK(ALL(ABS(ans - xb) .LT. tol), "UNIT to BIUNIT")

! same reference line, values are copied
CALL FromLine2Line_(xin=xb, ans=ans, from="BIUNIT", to="BIUNIT")
CALL OK(ALL(ABS(ans - xb) .LT. tol), "BIUNIT to BIUNIT")

! only the first letter is used, lower case is accepted
CALL FromLine2Line_(xin=xb, ans=ans, from="b", to="u")
CALL OK(ALL(ABS(ans - xu) .LT. tol), "b to u")

END PROGRAM main
