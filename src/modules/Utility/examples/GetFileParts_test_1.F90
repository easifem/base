!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetFileParts splits a file name into path, name and extension

PROGRAM main
USE GlobalData, ONLY: I4B
USE StringUtility, ONLY: GetFileParts
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=100) :: path, fname, ext

CALL GetFileParts("./HelloWorld.txt", path, fname, ext)
CALL OK(TRIM(path) .EQ. "./", "path of ./HelloWorld.txt")
CALL OK(TRIM(fname) .EQ. "HelloWorld", "name of ./HelloWorld.txt")
CALL OK(TRIM(ext) .EQ. ".txt", "extension of ./HelloWorld.txt")

CALL GetFileParts("/dir1/dir2/HelloWorld.txt", path, fname, ext)
CALL OK(TRIM(path) .EQ. "/dir1/dir2/", "absolute path")
CALL OK(TRIM(fname) .EQ. "HelloWorld", "absolute name")
CALL OK(TRIM(ext) .EQ. ".txt", "absolute extension")

! only the last extension is split off
CALL GetFileParts("archive.tar.gz", path, fname, ext)
CALL OK(LEN_TRIM(path) .EQ. 0_I4B, "no path")
CALL OK(TRIM(fname) .EQ. "archive.tar", "name keeps .tar")
CALL OK(TRIM(ext) .EQ. ".gz", "extension .gz")

CALL GetFileParts("README", path, fname, ext)
CALL OK(TRIM(fname) .EQ. "README", "name without extension")
CALL OK(LEN_TRIM(ext) .EQ. 0_I4B, "no extension")

END PROGRAM main
