!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetExtension returns the text after the last dot

PROGRAM main
USE GlobalData, ONLY: I4B
USE StringUtility, ONLY: GetExtension
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(:), ALLOCATABLE :: ext

! the dot is not included and the case is kept
CALL OK(GetExtension("helloworld.F90") .EQ. "F90", "F90 is kept")
CALL OK(GetExtension("mesh.h5") .EQ. "h5", "h5")
CALL OK(GetExtension("a.tar.gz") .EQ. "gz", "only the last extension")

ext = GetExtension("README")
CALL OK(LEN(ext) .EQ. 0_I4B, "no dot gives an empty string")

ext = GetExtension("file.")
CALL OK(LEN(ext) .EQ. 0_I4B, "dot at the end")

END PROGRAM main
