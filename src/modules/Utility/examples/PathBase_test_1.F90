!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: PathBase returns the last element of a path

PROGRAM main
USE StringUtility, ONLY: PathBase
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=100) :: astr

astr = " "
CALL OK(PathBase(astr) .EQ. ".", "empty path gives .")

CALL OK(PathBase("/") .EQ. "/", "root")
CALL OK(PathBase("////") .EQ. "/", "only slashes")
CALL OK(PathBase("/a/b/c") .EQ. "c", "/a/b/c")
CALL OK(PathBase("/a/b/c/") .EQ. "c", "trailing slash is removed")
CALL OK(PathBase("a/b") .EQ. "b", "relative path")
CALL OK(PathBase("abc") .EQ. "abc", "no slash")
CALL OK(PathBase("x") .EQ. "x", "single character")

! trailing blanks of the argument are ignored
astr = "/home/user/file.txt"
CALL OK(PathBase(astr) .EQ. "file.txt", "padded argument")

END PROGRAM main
