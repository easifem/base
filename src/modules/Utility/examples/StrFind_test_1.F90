!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: StrFind returns the start index of each match

PROGRAM main
USE GlobalData, ONLY: I4B
USE StringUtility, ONLY: StrFind
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=100) :: astr
INTEGER(I4B), ALLOCATABLE :: indices(:)

astr = "/a/b/c"
CALL StrFind(astr, "/", indices)
CALL OK(ALL(indices .EQ. [1_I4B, 3_I4B, 5_I4B]), "/a/b/c")

astr = "/a/b/c/"
CALL StrFind(astr, "/", indices)
CALL OK(SIZE(indices) .EQ. 4_I4B, "/a/b/c/ has four slashes")
CALL OK(indices(4) .EQ. 7_I4B, "last slash at 7")

! overlapping matches
CALL StrFind("aaaa", "aa", indices)
CALL OK(ALL(indices .EQ. [1_I4B, 2_I4B, 3_I4B]), "overlapping")

! no match gives an allocated vector of size 0
astr = "abc"
CALL StrFind(astr, "/", indices)
CALL OK(ALLOCATED(indices), "allocated when there is no match")
CALL OK(SIZE(indices) .EQ. 0_I4B, "size 0 when there is no match")

END PROGRAM main
