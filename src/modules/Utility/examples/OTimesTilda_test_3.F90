!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: OTimesTilda of four vectors

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ProductUtility, ONLY: OTimesTilda
USE ProductUtility, ONLY: OuterProd
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: a(2), b(2), c(2), d(3), ans(4, 6), ans2(4, 6)
INTEGER(I4B) :: nrow, ncol

a = [1.0_DFP, 2.0_DFP]
b = [3.0_DFP, 4.0_DFP]
c = [5.0_DFP, 6.0_DFP]
d = [7.0_DFP, 8.0_DFP, 9.0_DFP]

ans = 0.0_DFP
CALL OTimesTilda(a=a, b=b, c=c, d=d, ans=ans, nrow=nrow, ncol=ncol, &
                 anscoeff=0.0_DFP, scale=1.0_DFP)

CALL OK(nrow .EQ. 4_I4B, "nrow = SIZE(a) * SIZE(c)")
CALL OK(ncol .EQ. 6_I4B, "ncol = SIZE(b) * SIZE(d)")

! entries computed by hand
CALL OK(ABS(ans(1, 1) - 105.0_DFP) .LT. tol, "ans(1,1) = 1 * 3 * 5 * 7")
CALL OK(ABS(ans(4, 6) - 432.0_DFP) .LT. tol, "ans(4,6) = 2 * 4 * 6 * 9")

! same as the matrix version with (a outer b) and (c outer d)
ans2 = 0.0_DFP
CALL OTimesTilda(a=OuterProd(a, b), b=OuterProd(c, d), ans=ans2, &
                 nrow=nrow, ncol=ncol, anscoeff=0.0_DFP, scale=1.0_DFP)
CALL OK(ALL(ABS(ans - ans2) .LT. tol), "same as the matrix version")

END PROGRAM main
