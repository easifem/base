!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: PathDir returns all but the last element of a path

PROGRAM main
USE StringUtility, ONLY: PathDir
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=100) :: astr

astr = " "
CALL OK(PathDir(astr) .EQ. ".", "empty path gives .")

CALL OK(PathDir("/") .EQ. "/", "root")
CALL OK(PathDir("////") .EQ. "/", "only slashes")
CALL OK(PathDir("/a/b/c") .EQ. "/a/b", "/a/b/c")
CALL OK(PathDir("/a/b/c/") .EQ. "/a/b", "trailing slash")
CALL OK(PathDir("/abc") .EQ. "/", "file in root")
CALL OK(PathDir("a/b") .EQ. "a", "relative path")
CALL OK(PathDir("a//b") .EQ. "a", "repeated slashes")
CALL OK(PathDir("abc") .EQ. ".", "no slash gives .")
CALL OK(PathDir("x") .EQ. ".", "single character gives .")

END PROGRAM main
