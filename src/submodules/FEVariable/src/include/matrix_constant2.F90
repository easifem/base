obj%len = SIZE(val)
obj%capacity = CAPACITY_EXPAND_FACTOR * obj%len
ALLOCATE (obj%val(obj%capacity))

obj%val(1:obj%len) = val(1:obj%len)

obj%s(1:2) = s(1:2)
obj%defineOn = _DEFINEON_
obj%rank = Matrix
obj%varType = Constant
obj%isInit = .TRUE.
