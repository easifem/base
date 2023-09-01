INTEGER(I4B) :: nc, nr, n, ii, m, jj
LOGICAL(LGT) :: norow, nocol
nc = SIZE(ch)
nr = SIZE(rh)
m = SIZE(val, 1)
n = SIZE(val, 2)

IF (m .EQ. 1) THEN
  ans = MdEncode(val=val(1, :), rh=rh, ch=ch)
  RETURN
END IF

IF (n .EQ. 1) THEN
  ans = MdEncode(val=val(:, 1), rh=rh, ch=ch)
  RETURN
END IF

IF (nc .LT. n) THEN
  nocol = .TRUE.
ELSE
  nocol = .FALSE.
END IF

IF (nr .LT. m) THEN
  norow = .TRUE.
ELSE
  norow = .FALSE.
END IF

IF (nocol .AND. norow) THEN
  ans = MdEncode(val)
  RETURN
END IF

IF (norow .AND. (.NOT. nocol)) THEN
  ! | col-1 | col-2 | col-3 |
  ! | ---- | ----- | ----- |
  ! |  1  |   2   |     3 |

  ans = ivert

  DO ii = 1, n
    ans = ans//ch(ii)%chars()//avert
  END DO
  ans = ans//abr

  ans = ans//ivert
  DO ii = 1, n
    ans = ans//adash//avert
  END DO
  ans = ans//abr

  ans = ans//ivert

  DO ii = 1, m
    DO jj = 1, n
      ans = ans//mdencode(val(ii, jj))//avert
    END DO
    ans = ans//abr
  END DO
  ans = ans//abr
  RETURN
END IF

IF (nocol .AND. (.NOT. norow)) THEN
  ! |       |      |       |       |
  ! | ----- | ---- | ----- | ----- |
  ! | row-1 |  1  |   2   |     3  |
  ! | row-2 |  1  |   2   |     3  |
  ! | row-3 |  1  |   2   |     3 |

  ans = ivert//avert
  DO ii = 1, n
    ans = ans//ablank//avert
  END DO
  ans = ans//abr

  ans = ans//ivert//adash//avert
  DO ii = 1, n
    ans = ans//adash//avert
  END DO
  ans = ans//abr

  DO ii = 1, m
    ans = ans//ivert//rh(ii)%chars()//avert
    DO jj = 1, n
      ans = ans//mdencode(val(ii, jj))//avert
    END DO
    ans = ans//abr
  END DO
  ans = ans//abr
  RETURN
END IF

! |     | col-1 | col-2 | col-3 |
! | ----- | ---- | ----- | ----- |
! | row-1 |  1  |   2   |     3  |
! | row-2 |  1  |   2   |     3  |
! | row-3 |  1  |   2   |     3 |

ans = ivert//avert
DO ii = 1, n
  ans = ans//ch(ii)%chars()//avert
END DO
ans = ans//abr

ans = ans//ivert//adash//avert
DO ii = 1, n
  ans = ans//adash//avert
END DO
ans = ans//abr

DO ii = 1, m
  ans = ans//ivert//rh(ii)%chars()//avert
  DO jj = 1, n
    ans = ans//mdencode(val(ii, jj))//avert
  END DO
  ans = ans//abr
END DO
ans = ans//abr

