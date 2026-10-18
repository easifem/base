!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetFileNameExt returns the extension with the dot

PROGRAM main
USE GlobalData, ONLY: I4B
USE StringUtility, ONLY: GetFileNameExt
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=100) :: ext

CALL GetFileNameExt("./HelloWorld.txt", ext)
CALL OK(TRIM(ext) .EQ. ".txt", "relative file name")

CALL GetFileNameExt("/dir1/dir2/HelloWorld.txt", ext)
CALL OK(TRIM(ext) .EQ. ".txt", "absolute file name")

CALL GetFileNameExt("a.tar.gz", ext)
CALL OK(TRIM(ext) .EQ. ".gz", "only the last extension")

! a dot in a directory name is not an extension
CALL GetFileNameExt("dir.d/file", ext)
CALL OK(LEN_TRIM(ext) .EQ. 0_I4B, "dot in directory")

CALL GetFileNameExt("README", ext)
CALL OK(LEN_TRIM(ext) .EQ. 0_I4B, "no extension")

END PROGRAM main
