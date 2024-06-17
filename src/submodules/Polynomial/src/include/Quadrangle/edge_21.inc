  ! nodes on edge 21
  jj = ll + 1
  IF (cnt .LT. N) THEN
    DO ii = p - ll, 2 + ll, -1
      cnt = cnt + 1
      temp(1, cnt) = xi(ii, jj)
      temp(2, cnt) = eta(ii, jj)
    END DO
  END IF
