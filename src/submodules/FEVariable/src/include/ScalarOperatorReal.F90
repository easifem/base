SELECT CASE (obj1%vartype)
CASE (constant)
  IF (obj1%defineon .EQ. nodal) THEN
    ans = NodalVariable(obj1%val(1) _OP_ val, &
                        TypeFEVariableScalar, TypeFEVariableConstant)
  ELSE
    ans = QuadratureVariable(obj1%val(1) _OP_ val, &
                             TypeFEVariableScalar, TypeFEVariableConstant)
  END IF
CASE (space)
  IF (obj1%defineon .EQ. nodal) THEN
    ans = NodalVariable(obj1%val(1:obj1%len) _OP_ val, &
                        TypeFEVariableScalar, TypeFEVariableSpace)
  ELSE
    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ val, &
                             TypeFEVariableScalar, TypeFEVariableSpace)
  END IF
CASE (time)
  IF (obj1%defineon .EQ. nodal) THEN
    ans = NodalVariable(obj1%val(1:obj1%len) _OP_ val, &
                        TypeFEVariableScalar, TypeFEVariableTime)
  ELSE
    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ val, &
                             TypeFEVariableScalar, TypeFEVariableTime)
  END IF
CASE (spacetime)
  IF (obj1%defineon .EQ. nodal) THEN
    ans = NodalVariable(obj1%val(1:obj1%len) _OP_ val, &
                   TypeFEVariableScalar, TypeFEVariableSpaceTime, obj1%s(1:2))
  ELSE
    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ val, &
                   TypeFEVariableScalar, TypeFEVariableSpaceTime, obj1%s(1:2))
  END IF
END SELECT
