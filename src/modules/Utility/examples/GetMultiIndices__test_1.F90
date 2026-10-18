!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetMultiIndices_(n, d, ans, nrow, ncol), no allocation

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: GetMultiIndices
USE IntegerUtility, ONLY: GetMultiIndices_
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: ans(3, 6), big(4, 10)
INTEGER(I4B) :: nrow, ncol

! ans has the exact size: (d+1) by Size(n, d)
CALL GetMultiIndices_(2_I4B, 2_I4B, ans, nrow, ncol)
CALL OK(nrow .EQ. 3_I4B, "nrow is d+1")
CALL OK(ncol .EQ. 6_I4B, "ncol is Size(n, d)")
CALL OK(ALL(ans .EQ. GetMultiIndices(2_I4B, 2_I4B)), &
        "same as GetMultiIndices")

! ans can be larger than needed, only ans(1:nrow, 1:ncol) is used
big = -1_I4B
CALL GetMultiIndices_(n=2_I4B, d=2_I4B, ans=big, nrow=nrow, ncol=ncol)
CALL OK(ALL(big(1:nrow, 1:ncol) .EQ. GetMultiIndices(2_I4B, 2_I4B)), &
        "larger ans")
CALL OK(ALL(big(4, :) .EQ. -1_I4B), "extra row is not changed")

END PROGRAM main
