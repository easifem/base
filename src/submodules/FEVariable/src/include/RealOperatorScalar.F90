SELECT CASE (obj1%vartype)
CASE (constant)
  IF (obj1%defineon .EQ. Nodal) THEN
    ans = NodalVariable(val _OP_ obj1%val(1), &
                        TypeFEVariableScalar, TypeFEVariableConstant)
  ELSE
    ans = QuadratureVariable(val _OP_ obj1%val(1), &
                             TypeFEVariableScalar, TypeFEVariableConstant)
  END IF
CASE (space)
  IF (obj1%defineon .EQ. Nodal) THEN
    ans = NodalVariable(val _OP_ obj1%val(1:obj1%len), &
                        TypeFEVariableScalar, TypeFEVariableSpace)
  ELSE
    ans = QuadratureVariable(val _OP_ obj1%val(1:obj1%len), &
                             TypeFEVariableScalar, TypeFEVariableSpace)
  END IF
CASE (time)
  IF (obj1%defineon .EQ. Nodal) THEN
    ans = NodalVariable(val _OP_ obj1%val(1:obj1%len), &
                        TypeFEVariableScalar, TypeFEVariableTime)
  ELSE
    ans = QuadratureVariable(val _OP_ obj1%val(1:obj1%len), &
                             TypeFEVariableScalar, TypeFEVariableTime)
  END IF
CASE (spacetime)
  IF (obj1%defineon .EQ. Nodal) THEN
    ans = NodalVariable(val _OP_ obj1%val(1:obj1%len), &
                   TypeFEVariableScalar, TypeFEVariableSpaceTime, obj1%s(1:2))
  ELSE
    ans = QuadratureVariable(val _OP_ obj1%val(1:obj1%len), &
                   TypeFEVariableScalar, TypeFEVariableSpaceTime, obj1%s(1:2))
  END IF
END SELECT
