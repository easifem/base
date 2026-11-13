!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: PathDir and PathBase with a trailing separator
!
! Description:
!   Regression test for bug M-32 in bug-reports/Utility_bug_report.md.
!   PathDir("a/") gave "a" although "a/" has no directory part. The
!   separators are now found with one helper, which on Windows also
!   accepts /.

PROGRAM main
USE StringUtility, ONLY: PathDir
USE StringUtility, ONLY: PathBase
USE Test_Method, ONLY: OK
IMPLICIT NONE

CALL OK(PathDir("a/") .EQ. ".", "PathDir: a/ gives .")
CALL OK(PathDir("ab/") .EQ. ".", "PathDir: ab/ gives .")
CALL OK(PathDir("/a/") .EQ. "/", "PathDir: /a/ gives /")
CALL OK(PathDir("a/b//") .EQ. "a", "PathDir: a/b// gives a")

CALL OK(PathBase("a/") .EQ. "a", "PathBase: a/ gives a")
CALL OK(PathBase("/a//") .EQ. "a", "PathBase: /a// gives a")

END PROGRAM main
