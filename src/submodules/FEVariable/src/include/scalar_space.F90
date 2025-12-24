obj%len = SIZE(val)
obj%s(1) = obj%len
obj%capacity = TypeFEVariableOpt%capacityExpandFactor * obj%len
ALLOCATE (obj%val(obj%capacity))
obj%val(1:obj%len) = val
obj%defineOn = _DEFINEON_
obj%rank = SCALAR
obj%varType = Space
obj%isInit = .TRUE.
