!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FindReplace replaces every match of a pattern in place
!
! Description:
!   Includes the regression test for bug H-10 in
!   bug-reports/Utility_bug_report.md: overlapping matches such as
!   "aa" in "aaa" corrupted the result.

PROGRAM main
USE StringUtility, ONLY: FindReplace
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=40) :: astr
CHARACTER(LEN=10) :: bstr
CHARACTER(LEN=5) :: cstr

astr = "Hello world, how are you"
CALL FindReplace(astr, "you", "you!")
CALL OK(astr .EQ. "Hello world, how are you!", "longer replacement")

bstr = "cat cat"
CALL FindReplace(bstr, "cat", "dog")
CALL OK(bstr .EQ. "dog dog", "same length, two matches")

bstr = "a-b-c"
CALL FindReplace(bstr, "-", "--")
CALL OK(bstr .EQ. "a--b--c", "string grows")

bstr = "a--b--c"
CALL FindReplace(bstr, "--", "-")
CALL OK(bstr .EQ. "a-b-c", "string shrinks")

bstr = "a-b-c"
CALL FindReplace(bstr, "-", "")
CALL OK(bstr .EQ. "abc", "empty replacement")

bstr = "abc"
CALL FindReplace(bstr, "xyz", "123")
CALL OK(bstr .EQ. "abc", "no match")

! the result does not fit in LEN(cstr), so nothing is changed
cstr = "abcde"
CALL FindReplace(cstr, "a", "aa")
CALL OK(cstr .EQ. "abcde", "no room: string is unchanged")

! overlapping matches: scan from the left, skip each match
bstr = "aaa"
CALL FindReplace(bstr, "aa", "b")
CALL OK(bstr .EQ. "ba", "aa -> b in aaa gives ba")

bstr = "aaaa"
CALL FindReplace(bstr, "aa", "b")
CALL OK(bstr .EQ. "bb", "aa -> b in aaaa gives bb")

bstr = "abababa"
CALL FindReplace(bstr, "aba", "x")
CALL OK(bstr .EQ. "xbx", "aba -> x in abababa gives xbx")

! an empty pattern changes nothing
bstr = "aaa"
CALL FindReplace(bstr, "", "b")
CALL OK(bstr .EQ. "aaa", "empty pattern")

END PROGRAM main
