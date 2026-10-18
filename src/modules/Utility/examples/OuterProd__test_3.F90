!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: OuterProd_ of two matrix columns

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ProductUtility, ONLY: OuterProd
USE ProductUtility, ONLY: OuterProd_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: a(3, 2), b(2, 3), ans(3, 3)
INTEGER(I4B) :: nrow, ncol

! columns of a: [1, 2, 3] and [4, 5, 6]
a = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP], &
            [3, 2])
! columns of b: [1, 2], [3, 4] and [7, 8]
b = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 7.0_DFP, 8.0_DFP], &
            [2, 3])

! ans(i, j) = a(i, 2) * b(j, 3) for i = 1:2, j = 1:2
ans = 1.0_DFP
CALL OuterProd_(a=a, b=b, rowA=2_I4B, rowB=2_I4B, iColA=2_I4B, &
                iColB=3_I4B, anscoeff=0.0_DFP, scale=1.0_DFP, ans=ans, &
                nrow=nrow, ncol=ncol)

CALL OK(nrow .EQ. 2_I4B, "nrow = rowA")
CALL OK(ncol .EQ. 2_I4B, "ncol = rowB")
CALL OK(ALL(ABS(ans(1:2, 1:2) - OuterProd(a(1:2, 2), b(1:2, 3))) &
            .LT. tol), "ans(1:2, 1:2) = a(1:2, 2) x b(1:2, 3)")
CALL OK(ABS(ans(1, 1) - 28.0_DFP) .LT. tol, "ans(1,1) = 4 * 7")
CALL OK(ABS(ans(2, 2) - 40.0_DFP) .LT. tol, "ans(2,2) = 5 * 8")

! entries outside ans(1:2, 1:2) are not changed
CALL OK(ALL(ABS(ans(3, :) - 1.0_DFP) .LT. tol) .AND. &
        ALL(ABS(ans(:, 3) - 1.0_DFP) .LT. tol), "rest of ans unchanged")

END PROGRAM main
