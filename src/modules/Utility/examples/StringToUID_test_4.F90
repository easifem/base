!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: StringToUID of long strings, wrap around modulo 1000009

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT64
USE HashingUtility, ONLY: StringToUID
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: m = 1000009_I4B
CHARACTER(LEN=*), PARAMETER :: s1 = "AAAAAAAAAAAAAAAAAAAA"
CHARACTER(LEN=*), PARAMETER :: s2 = "HelloWorldFromEasifemBase"
CHARACTER(LEN=*), PARAMETER :: s3 = "ZzYyXxWwVvUuTtSsRrQqPpOoNn"

! For strings of letters all the weights are positive, so the result
! lies in [0, m).
CALL OK(StringToUID(s1) .GE. 0_I4B, "s1 >= 0")
CALL OK(StringToUID(s1) .LT. m, "s1 < m")
CALL OK(StringToUID(s2) .GE. 0_I4B, "s2 >= 0")
CALL OK(StringToUID(s2) .LT. m, "s2 < m")
CALL OK(StringToUID(s3) .GE. 0_I4B, "s3 >= 0")
CALL OK(StringToUID(s3) .LT. m, "s3 < m")

CALL OK(StringToUID(s1) .EQ. Reference(s1), "s1 matches reference")
CALL OK(StringToUID(s2) .EQ. Reference(s2), "s2 matches reference")
CALL OK(StringToUID(s3) .EQ. Reference(s3), "s3 matches reference")

CONTAINS

! Polynomial rolling hash computed with 64 bit integers
FUNCTION Reference(str) RESULT(ans)
  CHARACTER(LEN=*), INTENT(IN) :: str
  INTEGER(I4B) :: ans

  INTEGER(INT64), PARAMETER :: p = 53_INT64
  INTEGER(INT64), PARAMETER :: mm = 1000009_INT64
  INTEGER(INT64) :: h, pw, w
  INTEGER(I4B) :: ii

  h = 0_INT64
  pw = 1_INT64
  DO ii = 1, LEN_TRIM(str)
    w = INT(ICHAR(str(ii:ii)), INT64) - INT(ICHAR('A'), INT64) + 1_INT64
    h = MOD(h + w * pw, mm)
    pw = MOD(pw * p, mm)
  END DO
  ans = INT(h, I4B)
END FUNCTION Reference

END PROGRAM main
