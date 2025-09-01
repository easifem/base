obj%len = 1
obj%capacity = CAPACITY_EXPAND_FACTOR * obj%len
ALLOCATE (obj%val(obj%capacity))
obj%val(1) = val
obj%s(1) = 1
obj%defineOn = _DEFINEON_
obj%rank = Scalar
obj%varType = Constant
obj%isInit = .TRUE.
