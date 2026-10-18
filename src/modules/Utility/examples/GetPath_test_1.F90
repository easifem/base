!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetPath returns the directory part of a file name

PROGRAM main
USE GlobalData, ONLY: I4B
USE StringUtility, ONLY: GetPath
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=100) :: path

CALL GetPath("/dir1/dir2/HelloWorld.txt", path)
CALL OK(TRIM(path) .EQ. "/dir1/dir2/", "absolute file name")

CALL GetPath("./HelloWorld.txt", path)
CALL OK(TRIM(path) .EQ. "./", "relative file name")

CALL GetPath("dir/", path)
CALL OK(TRIM(path) .EQ. "dir/", "path ending with slash")

CALL GetPath("HelloWorld.txt", path)
CALL OK(LEN_TRIM(path) .EQ. 0_I4B, "no directory gives blank")

END PROGRAM main
