!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: GetExtension with directories and trailing blanks
!
! Description:
!   Regression test for bug M-27 in bug-reports/Utility_bug_report.md.
!   A dot in a directory name was found, and the trailing blanks of the
!   argument were part of the result.

PROGRAM main
USE GlobalData, ONLY: I4B
USE StringUtility, ONLY: GetExtension
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=40) :: astr
CHARACTER(:), ALLOCATABLE :: ext

! a dot in a directory name is not an extension
ext = GetExtension("dir.d/file")
CALL OK(LEN(ext) .EQ. 0_I4B, "dir.d/file has no extension")
CALL OK(GetExtension("dir.d/file.txt") .EQ. "txt", "dir.d/file.txt")

! trailing blanks are not part of the result
astr = "mesh.h5"
ext = GetExtension(astr)
CALL OK(ext .EQ. "h5" .AND. LEN(ext) .EQ. 2_I4B, "padded argument")

END PROGRAM main
