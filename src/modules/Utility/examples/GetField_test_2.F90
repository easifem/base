!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetField with slashes and quoted fields

PROGRAM main
USE StringUtility, ONLY: GetField
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=100) :: astr
CHARACTER(:), ALLOCATABLE :: field

! a slash does not end the field
astr = "dir/file.txt other"
CALL GetField(1, astr, field)
CALL OK(field .EQ. "dir/file.txt", "field with a slash")
CALL GetField(2, astr, field)
CALL OK(field .EQ. "other", "field after a slash field")

astr = "Hello/ world/ how/ are/ you"
CALL GetField(1, astr, field)
CALL OK(field .EQ. "Hello/", "slash at the end of a field")
CALL GetField(5, astr, field)
CALL OK(field .EQ. "you", "last field")

! quotes are removed from a quoted field
astr = "'a b' c"
CALL GetField(1, astr, field)
CALL OK(field .EQ. "a b", "quoted field")
CALL GetField(2, astr, field)
CALL OK(field .EQ. "c", "field after a quoted field")

END PROGRAM main
