!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: PathJoin for two paths (separator / on macOS and Linux)

PROGRAM main
USE GlobalData, ONLY: I4B
USE StringUtility, ONLY: PathJoin
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(:), ALLOCATABLE :: ans

ans = PathJoin("a", "b")
CALL OK(ans .EQ. "a/b", "a + b")
CALL OK(LEN(ans) .EQ. 3_I4B, "length of a/b")

CALL OK(PathJoin("/usr/local", "bin") .EQ. "/usr/local/bin", &
        "absolute path")

! trailing blanks of both arguments are removed
CALL OK(PathJoin("a  ", "b  ") .EQ. "a/b", "trailing blanks")

! a separator is always added, even if one is already there
CALL OK(PathJoin("a/", "b") .EQ. "a//b", "no slash cleaning")
CALL OK(PathJoin("", "b") .EQ. "/b", "empty first path")

END PROGRAM main
