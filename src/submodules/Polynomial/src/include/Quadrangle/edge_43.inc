
  ! nodes on edge 43
  jj = q + 1 - ll
  IF (cnt .LT. N) THEN
    DO ii = 2 + ll, p - ll, +1
      cnt = cnt + 1
      temp(1, cnt) = xi(ii, jj)
      temp(2, cnt) = eta(ii, jj)
    END DO
  END IF
