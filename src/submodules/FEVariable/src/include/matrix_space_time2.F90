obj%len = SIZE(val)
obj%capacity = TypeFEVariableOpt%capacityExpandFactor * obj%len
ALLOCATE (obj%val(obj%capacity))

obj%val(1:obj%len) = val(1:obj%len)

obj%s(1:4) = s(1:4)
obj%defineOn = _DEFINEON_
obj%rank = Matrix
obj%varType = SpaceTime
obj%isInit = .TRUE.
