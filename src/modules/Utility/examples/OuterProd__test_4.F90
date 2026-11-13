!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: OuterProd_ of two vectors with sym

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ProductUtility, ONLY: OuterProd
USE ProductUtility, ONLY: OuterProd_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: a(3), b(3), ans(4, 4)
INTEGER(I4B) :: nrow, ncol

a = [1.0_DFP, 2.0_DFP, 3.0_DFP]
b = [4.0_DFP, 5.0_DFP, 6.0_DFP]

! sym = .TRUE.: ans = 0 * ans + 2 * (a x b + b x a) / 2
ans = 1.0_DFP
CALL OuterProd_(a=a, b=b, sym=.TRUE., anscoeff=0.0_DFP, scale=2.0_DFP, &
                ans=ans, nrow=nrow, ncol=ncol)

CALL OK(nrow .EQ. 3_I4B, "nrow = SIZE(a)")
CALL OK(ncol .EQ. 3_I4B, "ncol = SIZE(b)")
CALL OK(ALL(ABS(ans(1:3, 1:3) - (OuterProd(a, b) + OuterProd(b, a))) &
            .LT. tol), "symmetric part")
CALL OK(ABS(ans(1, 2) - 13.0_DFP) .LT. tol, "ans(1,2) = 1*5 + 4*2")

! entries outside ans(1:3, 1:3) are not changed
CALL OK(ALL(ABS(ans(4, :) - 1.0_DFP) .LT. tol) .AND. &
        ALL(ABS(ans(:, 4) - 1.0_DFP) .LT. tol), "rest of ans unchanged")

! sym = .FALSE.: ans = 1 * ans + 1 * (a x b)
CALL OuterProd_(a=a, b=b, sym=.FALSE., anscoeff=1.0_DFP, scale=1.0_DFP, &
                ans=ans, nrow=nrow, ncol=ncol)
CALL OK(ABS(ans(1, 2) - 18.0_DFP) .LT. tol, "ans(1,2) = 13 + 1*5")

END PROGRAM main
