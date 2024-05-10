! Define internal variables
INTEGER(I4B) :: ii, n

tsize = SIZE(obj)
IF (tsize .LE. 1) RETURN

IF (.NOT. isSorted) CALL QUICKSORT(obj, 1_I4B, tsize)

DO CONCURRENT(ii=1:tsize)
  temp(ii) = obj(ii)
  obj(ii) = 0
END DO

obj(1) = temp(1)

n = 1
DO ii = 2, tsize
  IF (temp(ii) .NE. temp(ii - 1)) THEN
    n = n + 1
    obj(n) = temp(ii)
  END IF
END DO

tsize = n
