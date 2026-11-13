!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: OuterProd_ sets the sizes when the last operand has no columns
!
! Description:
!   Regression test for bug M-39 in bug-reports/Utility_bug_report.md.
!   In OuterProd_r2r2_ and OuterProd_r1r1r2_, dim1 to dim3 were set only
!   inside the loop over the columns of the last operand, so they were
!   undefined when it had no columns.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ProductUtility, ONLY: OuterProd_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a2(2, 3), b2(2, 0), a(2), b(3), c(4, 0)
REAL(DFP) :: ans(4, 4, 4, 1)
INTEGER(I4B) :: dim1, dim2, dim3, dim4

a2 = 1.0_DFP
a = 1.0_DFP
b = 1.0_DFP

! rank 2 x rank 2, b has no columns
dim1 = -1
dim2 = -1
dim3 = -1
dim4 = -1
CALL OuterProd_(a=a2, b=b2, anscoeff=0.0_DFP, scale=1.0_DFP, &
                ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4)
CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. [2, 3, 2, 0]), &
        "r2r2_: sizes are set")

! rank 1 x rank 1 x rank 2, c has no columns
dim1 = -1
dim2 = -1
dim3 = -1
dim4 = -1
CALL OuterProd_(a=a, b=b, c=c, anscoeff=0.0_DFP, scale=1.0_DFP, &
                ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4)
CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. [2, 3, 4, 0]), &
        "r1r1r2_: sizes are set")

END PROGRAM main
