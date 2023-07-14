
  ! nodes on edge 12
  jj = ll + 1
  IF (cnt .LT. N) THEN
    DO ii = 2 + ll, p - ll
      cnt = cnt + 1
      temp(1, cnt) = xi(ii, jj)
      temp(2, cnt) = eta(ii, jj)
    END DO
  END IF
