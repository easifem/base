!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: PathJoin for a vector of String (separator / on macOS and Linux)

PROGRAM main
USE GlobalData, ONLY: I4B
USE String_Class, ONLY: String
USE StringUtility, ONLY: PathJoin
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(String) :: paths(3), one(1), empty(0)
CHARACTER(:), ALLOCATABLE :: ans

paths(1) = "usr"
paths(2) = "local"
paths(3) = "bin"

! every part is preceded by a separator
ans = PathJoin(paths)
CALL OK(ans .EQ. "/usr/local/bin", "three parts")

one(1) = "a"
CALL OK(PathJoin(one) .EQ. "/a", "one part")

ans = PathJoin(empty)
CALL OK(LEN(ans) .EQ. 0_I4B, "no parts gives an empty string")

END PROGRAM main
