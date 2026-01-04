! Template parameter: wp (working precision)
! Template free identifiers: testline, tests
SUBROUTINE isabs(got, expected, eps, msg)
  REAL(kind=wp), INTENT(in) :: got, expected
  CHARACTER(len=*), INTENT(in), OPTIONAL :: msg
  REAL(kind=wp), INTENT(in), OPTIONAL :: eps
  CHARACTER(len=:), ALLOCATABLE :: testmsg, idmsg
  CHARACTER(len=120) gotmsg, expectedmsg
  REAL(kind=wp) tolerance
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
  WRITE (unit=gotmsg, fmt='(A,G0)') '     got: ', got
  WRITE (unit=expectedmsg, fmt='(A,G0)') 'expected: ', expected

  IF (PRESENT(eps)) THEN
    tolerance = eps
  ELSE
    tolerance = EPSILON(got)
  END IF
  ! eps = 0.5e-10_wp
  ! Absolute accuracy within the 10 least significant digits
  good = ABS(got - expected) < tolerance
  CALL testline(good, testmsg, idmsg, gotmsg, expectedmsg)
END

!----------------------------------------------------------------------------
!                                                                      IsRel
!----------------------------------------------------------------------------

SUBROUTINE isrel(got, expected, eps, msg)
  REAL(kind=wp), INTENT(in) :: got, expected
  CHARACTER(len=*), INTENT(in), OPTIONAL :: msg
  REAL(kind=wp), INTENT(in), OPTIONAL :: eps
  REAL(kind=wp) tolerance

  ! eps = (abs(a) + abs(b)) * 0.5e-10_wp
  ! Relative accuracy within the 10 most significant digits
  tolerance = (ABS(got) + ABS(expected))
  IF (PRESENT(eps)) THEN
    tolerance = tolerance * eps
  ELSE
    tolerance = tolerance * EPSILON(got)
  END IF
  CALL isabs(got, expected, tolerance, msg)
END

!----------------------------------------------------------------------------
!                                                                     IsNear
!----------------------------------------------------------------------------

SUBROUTINE isnear(got, expected, eps, msg)
  REAL(kind=wp), INTENT(in) :: got, expected
  CHARACTER(len=*), INTENT(in), OPTIONAL :: msg
  REAL(kind=wp), INTENT(in), OPTIONAL :: eps
  CHARACTER(len=:), ALLOCATABLE :: testmsg, idmsg
  CHARACTER(len=120) gotmsg, expectedmsg
  REAL(kind=wp) tolerance
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
  WRITE (unit=gotmsg, fmt='(A,G0)') '     got: ', got
  WRITE (unit=expectedmsg, fmt='(A,G0)') 'expected: ', expected

  IF (PRESENT(eps)) THEN
    tolerance = eps
  ELSE
    tolerance = EPSILON(got) ! minimun eps for which 1 + eps /= 1
  END IF
  ! Relative accuracy around 1.0_wp
  ! Semantics of isnear means using <=, and not <, c.f. epsilon(got)
  good = ABS(got / expected - 1.0_WP) <= tolerance
  CALL testline(good, testmsg, idmsg, gotmsg, expectedmsg)
END
