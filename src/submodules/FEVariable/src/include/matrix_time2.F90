obj%len = SIZE(val)
obj%capacity = CAPACITY_EXPAND_FACTOR * obj%len
ALLOCATE (obj%val(obj%capacity))

obj%val(1:obj%len) = val(1:obj%len)

obj%s(1:3) = s(1:3)
obj%defineOn = _DEFINEON_
obj%rank = Matrix
obj%varType = Time
obj%isInit = .TRUE.
