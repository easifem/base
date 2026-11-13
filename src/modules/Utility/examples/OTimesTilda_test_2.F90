!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: OTimesTilda of two vectors

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ProductUtility, ONLY: OTimesTilda
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: a(2), b(3), ans(7)
INTEGER(I4B) :: tsize

a = [1.0_DFP, 2.0_DFP]
b = [3.0_DFP, 4.0_DFP, 5.0_DFP]

! ans is larger than needed
ans = 1.0_DFP
CALL OTimesTilda(a=a, b=b, ans=ans, tsize=tsize, anscoeff=0.0_DFP, &
                 scale=1.0_DFP)

CALL OK(tsize .EQ. 6_I4B, "tsize = SIZE(a) * SIZE(b)")

! ans((ii-1)*SIZE(b)+jj) = a(ii) * b(jj)
CALL OK(ALL(ABS(ans(1:6) - [3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP, &
                            8.0_DFP, 10.0_DFP]) .LT. tol), &
        "Kronecker product of a and b")
CALL OK(ABS(ans(7) - 1.0_DFP) .LT. tol, "ans(7) is not changed")

! ans = 1 * ans + 1 * (a kron b)
CALL OTimesTilda(a=a, b=b, ans=ans, tsize=tsize, anscoeff=1.0_DFP, &
                 scale=1.0_DFP)
CALL OK(ALL(ABS(ans(1:6) - [6.0_DFP, 8.0_DFP, 10.0_DFP, 12.0_DFP, &
                            16.0_DFP, 20.0_DFP]) .LT. tol), &
        "anscoeff and scale")

END PROGRAM main
