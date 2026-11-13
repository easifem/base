!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetFileName returns the file name with its extension

PROGRAM main
USE GlobalData, ONLY: I4B
USE StringUtility, ONLY: GetFileName
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=100) :: fname

CALL GetFileName("/dir1/dir2/HelloWorld.txt", fname)
CALL OK(TRIM(fname) .EQ. "HelloWorld.txt", "absolute file name")

CALL GetFileName("./a.txt", fname)
CALL OK(TRIM(fname) .EQ. "a.txt", "relative file name")

CALL GetFileName("HelloWorld.txt", fname)
CALL OK(TRIM(fname) .EQ. "HelloWorld.txt", "no directory")

CALL GetFileName("dir/", fname)
CALL OK(LEN_TRIM(fname) .EQ. 0_I4B, "path ending with slash")

END PROGRAM main
