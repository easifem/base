INTEGER(I4B) :: ii, jj, cnt

obj%len = SIZE(val)
!obj%capacity = CAPACITY_EXPAND_FACTOR * obj%len
obj%capacity = TypeFEVariableOpt%capacityExpandFactor * obj%len
ALLOCATE (obj%val(obj%capacity))

cnt = 0
DO jj = 1, SIZE(val, 2)
  DO ii = 1, SIZE(val, 1)
    cnt = cnt + 1
    obj%val(cnt) = val(ii, jj)
  END DO
END DO

obj%s(1:2) = SHAPE(val)
obj%defineOn = _DEFINEON_
obj%rank = Vector
obj%varType = Space
obj%isInit = .TRUE.
