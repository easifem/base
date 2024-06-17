
INTEGER(I4B) :: nc, nr, n, ii
nc = SIZE(ch)
nr = SIZE(rh)
n = SIZE(val)

SELECT CASE (nc)
CASE (1)

  IF (nr .EQ. n) THEN

    ans = ivert//avert//ch(1)%chars()//evert//abr
    ans = ans// &
        & ivert//adash//avert//adash//evert//abr

    DO ii = 1, n
      ans = ans// &
          & ivert//rh(ii)%chars()//avert//mdencode(val(ii))//evert//abr
    END DO
    ans = ans//abr

  ELSE

    IF (ch(1)%LEN_TRIM() .EQ. 0_I4B) THEN

      ans = ivert//avert

      DO ii = 1, n
        ans = ans//ablank//evert
      END DO
      ans = ans//abr

      ans = ans//ivert//adash//evert
      DO ii = 1, n
        ans = ans//adash//evert
      END DO
      ans = ans//abr

      ans = ans//ivert//rh(1)%chars()//evert

      DO ii = 1, n
        ans = ans//mdencode(val(ii))//evert
      END DO
      ans = ans//abr
    ELSE

      ans = ivert//ch(1)%chars()//evert//abr
      ans = ans// &
          & ivert//adash//evert//abr

      DO ii = 1, n
        ans = ans// &
            & ivert//mdencode(val(ii))//evert//abr
      END DO
      ans = ans//abr
    END IF

  END IF

CASE default

  IF (nc .EQ. n) THEN

    ans = ivert//avert

    DO ii = 1, n
      ans = ans//ch(ii)%chars()//evert
    END DO
    ans = ans//abr

    ans = ans//ivert//adash//evert
    DO ii = 1, n
      ans = ans//adash//evert
    END DO
    ans = ans//abr

    ans = ans//ivert//rh(1)%chars()//evert

    DO ii = 1, n
      ans = ans//mdencode(val(ii))//evert
    END DO
    ans = ans//abr

  ELSE

    ans = ivert//avert

    DO ii = 1, n
      ans = ans//ablank//evert
    END DO
    ans = ans//abr

    ans = ans//ivert//adash//evert
    DO ii = 1, n
      ans = ans//adash//evert
    END DO
    ans = ans//abr

    ans = ans//ivert//rh(1)%chars()//evert

    DO ii = 1, n
      ans = ans//mdencode(val(ii))//evert
    END DO
    ans = ans//abr

  END IF

END SELECT

