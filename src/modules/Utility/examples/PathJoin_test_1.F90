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

! exactly one separator is put between the paths
CALL OK(PathJoin("a/", "b") .EQ. "a/b", "a/ + b")
CALL OK(PathJoin("a//", "/b") .EQ. "a/b", "a// + /b")
CALL OK(PathJoin("/", "b") .EQ. "/b", "root + b")

! an empty path is skipped
CALL OK(PathJoin("", "b") .EQ. "b", "empty first path")
CALL OK(PathJoin("a", "") .EQ. "a", "empty second path")

END PROGRAM main
