obj%len = SIZE(val)
obj%capacity = CAPACITY_EXPAND_FACTOR * obj%len
ALLOCATE (obj%val(obj%capacity))
obj%val(1:obj%len) = val
obj%s(1) = SIZE(val)
obj%defineOn = _DEFINEON_
obj%rank = SCALAR
obj%varType = Space
obj%isInit = .TRUE.
