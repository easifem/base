! Template parameter: wp (working precision)
! Template free identifiers: testline, tests
SUBROUTINE is(got, expected, msg)
  INTEGER(kind=wp), INTENT(in) :: got, expected
  CHARACTER(len=*), INTENT(in), OPTIONAL :: msg
  CHARACTER(len=:), ALLOCATABLE :: testmsg, idmsg
  CHARACTER(len=120) gotmsg, expectedmsg
  LOGICAL good

  IF (PRESENT(msg)) THEN
    ALLOCATE (CHARACTER(len=LEN_TRIM(msg) + 20) :: testmsg, idmsg)
    WRITE (unit=idmsg, fmt='(A,A,A)') 'Failed test: "', TRIM(msg), '"'
    testmsg = TRIM(msg)
  ELSE
    ALLOCATE (CHARACTER(len=30) :: testmsg, idmsg)
    WRITE (unit=idmsg, fmt='(A,I0)') 'Failed test no. ', tests + 1
    testmsg = ""
  END IF
  WRITE (unit=gotmsg, fmt='(A,I0)') '     got: ', got
  WRITE (unit=expectedmsg, fmt='(A,I0)') 'expected: ', expected

  good = got == expected
  CALL testline(good, testmsg, idmsg, gotmsg, expectedmsg)
END
