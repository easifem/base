  ! v2
  ii = p + 1 - ll
  jj = 1 + ll
  IF (cnt .LT. N) THEN
    cnt = cnt + 1
    temp(1, cnt) = xi(ii, jj)
    temp(2, cnt) = eta(ii, jj)
  END IF
