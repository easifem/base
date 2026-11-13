!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: OuterProd_ with sizeA and sizeB

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ProductUtility, ONLY: OuterProd
USE ProductUtility, ONLY: OuterProd_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: a(3), b(4), ans(3, 4)
INTEGER(I4B) :: nrow, ncol

a = [1.0_DFP, 2.0_DFP, 3.0_DFP]
b = [4.0_DFP, 5.0_DFP, 6.0_DFP, 7.0_DFP]

! use only a(1:2) and b(1:3)
ans = 1.0_DFP
CALL OuterProd_(a=a, b=b, sizeA=2_I4B, sizeB=3_I4B, anscoeff=0.0_DFP, &
                scale=1.0_DFP, ans=ans, nrow=nrow, ncol=ncol)

CALL OK(nrow .EQ. 2_I4B, "nrow = sizeA")
CALL OK(ncol .EQ. 3_I4B, "ncol = sizeB")
CALL OK(ALL(ABS(ans(1:2, 1:3) - OuterProd(a(1:2), b(1:3))) .LT. tol), &
        "ans(1:2, 1:3) = a(1:2) x b(1:3)")
CALL OK(ABS(ans(2, 3) - 12.0_DFP) .LT. tol, "ans(2,3) = a(2) * b(3)")

! entries outside ans(1:2, 1:3) are not changed
CALL OK(ALL(ABS(ans(3, :) - 1.0_DFP) .LT. tol) .AND. &
        ALL(ABS(ans(:, 4) - 1.0_DFP) .LT. tol), "rest of ans unchanged")

! ans = 1 * ans + 2 * (a x b)
CALL OuterProd_(a=a, b=b, sizeA=2_I4B, sizeB=3_I4B, anscoeff=1.0_DFP, &
                scale=2.0_DFP, ans=ans, nrow=nrow, ncol=ncol)
CALL OK(ABS(ans(2, 3) - 36.0_DFP) .LT. tol, "anscoeff and scale")

END PROGRAM main
