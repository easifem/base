INTEGER(I4B) :: tsize1, tsize2, ii

tsize1 = SIZE(a)
tsize2 = SIZE(b)
tsize = 0

IF (tsize1 .LE. tsize2) THEN
  DO ii = 1, tsize1
    IF (ANY(a(ii) .EQ. b)) THEN
      tsize = tsize + 1
      c(tsize) = a(ii)
    END IF
  END DO
  RETURN
END IF

DO ii = 1, tsize2
  IF (ANY(b(ii) .EQ. a)) THEN
    tsize = tsize + 1
    c(tsize) = b(ii)
  END IF
END DO
