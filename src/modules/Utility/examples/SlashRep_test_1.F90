!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: SlashRep uses / as separator (checks hold on macOS, Linux)

PROGRAM main
USE StringUtility, ONLY: SlashRep
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=20) :: astr
CHARACTER(LEN=1), PARAMETER :: bslash = ACHAR(92)

! on macOS and Linux a backslash is replaced by /
astr = "a"//bslash//"b"//bslash//"c.txt"
CALL SlashRep(astr)
CALL OK(astr .EQ. "a/b/c.txt", "backslashes are replaced")

astr = "a/b/c.txt"
CALL SlashRep(astr)
CALL OK(astr .EQ. "a/b/c.txt", "forward slashes are kept")

astr = "file.txt"
CALL SlashRep(astr)
CALL OK(astr .EQ. "file.txt", "no slash")

END PROGRAM main
