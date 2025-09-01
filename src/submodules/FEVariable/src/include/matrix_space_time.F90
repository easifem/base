INTEGER(I4B) :: ii, jj, kk, ll, cnt

obj%len = SIZE(val)
obj%capacity = CAPACITY_EXPAND_FACTOR * obj%len
ALLOCATE (obj%val(obj%capacity))

cnt = 0

DO ll = 1, SIZE(val, 4)
  DO kk = 1, SIZE(val, 3)
    DO jj = 1, SIZE(val, 2)
      DO ii = 1, SIZE(val, 1)
        cnt = cnt + 1
        obj%val(cnt) = val(ii, jj, kk, ll)
      END DO
    END DO
  END DO
END DO

obj%s(1:4) = SHAPE(val)
obj%defineOn = _DEFINEON_
obj%rank = Matrix
obj%varType = SpaceTime
obj%isInit = .TRUE.
