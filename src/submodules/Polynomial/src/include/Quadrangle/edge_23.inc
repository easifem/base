
  ! nodes on edge 23
  ii = p + 1 - ll
  IF (cnt .LT. N) THEN
    DO jj = 2 + ll, q - ll
      cnt = cnt + 1
      temp(1, cnt) = xi(ii, jj)
      temp(2, cnt) = eta(ii, jj)
    END DO
  END IF
