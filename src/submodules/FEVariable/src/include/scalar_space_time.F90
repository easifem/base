INTEGER(I4B) :: ii, jj, kk

obj%len = SIZE(val)
obj%capacity = CAPACITY_EXPAND_FACTOR * obj%len
ALLOCATE (obj%val(obj%capacity))

kk = 0
DO jj = 1, SIZE(val, 2)
  DO ii = 1, SIZE(val, 1)
    kk = kk + 1
    obj%val(kk) = val(ii, jj)
  END DO
END DO

obj%s(1:2) = SHAPE(val)
obj%defineOn = _DEFINEON_
obj%rank = SCALAR
obj%varType = SpaceTime
obj%isInit = .TRUE.
