SELECT CASE (obj1%vartype)
CASE (constant)
  IF (obj1%defineon .EQ. nodal) THEN
    ans = NodalVariable(obj1%val(1:obj1%len) _OP_ val, &
                        TypeFEVariableVector, TypeFEVariableConstant)
  ELSE
    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ val, &
                             TypeFEVariableVector, TypeFEVariableConstant)
  END IF
CASE (space)
  IF (obj1%defineon .EQ. nodal) THEN
    ans = NodalVariable(obj1%val(1:obj1%len) _OP_ val, &
                       TypeFEVariableVector, TypeFEVariableSpace, obj1%s(1:2))
  ELSE
    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ val, &
                       TypeFEVariableVector, TypeFEVariableSpace, obj1%s(1:2))
  END IF
CASE (time)
  IF (obj1%defineon .EQ. nodal) THEN
    ans = NodalVariable(obj1%val(1:obj1%len) _OP_ val, &
                        TypeFEVariableVector, TypeFEVariableTime, obj1%s(1:2))
  ELSE
    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ val, &
                        TypeFEVariableVector, TypeFEVariableTime, obj1%s(1:2))
  END IF
CASE (spacetime)
  IF (obj1%defineon .EQ. nodal) THEN
    ans = NodalVariable(obj1%val(1:obj1%len) _OP_ val, &
                   TypeFEVariableVector, TypeFEVariableSpaceTime, obj1%s(1:3))
  ELSE
    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ val, &
                   TypeFEVariableVector, TypeFEVariableSpaceTime, obj1%s(1:3))
  END IF
END SELECT
