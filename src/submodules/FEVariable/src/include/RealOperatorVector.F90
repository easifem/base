SELECT CASE (obj1%vartype)

CASE (constant)

  IF (obj1%defineon .EQ. nodal) THEN
    ans = NodalVariable(val _OP_ obj1%val(1:obj1%len), &
                        TypeFEVariableVector, TypeFEVariableConstant)
  ELSE
    ans = QuadratureVariable(val _OP_ obj1%val(1:obj1%len), &
                             TypeFEVariableVector, TypeFEVariableConstant)
  END IF

CASE (space)

  IF (obj1%defineon .EQ. nodal) THEN
    ans = NodalVariable(val _OP_ obj1%val(1:obj1%len), &
                       TypeFEVariableVector, TypeFEVariableSpace, obj1%s(1:2))
  ELSE
    ans = QuadratureVariable(RESHAPE(val _OP_ obj1%val(1:obj1%len), obj1%s(1:2)), &
                             TypeFEVariableVector, TypeFEVariableSpace)
  END IF

CASE (time)

  IF (obj1%defineon .EQ. nodal) THEN
    ans = NodalVariable(RESHAPE(val _OP_ obj1%val(1:obj1%len), obj1%s(1:2)), &
                        TypeFEVariableVector, TypeFEVariableTime)
  ELSE
    ans = QuadratureVariable(RESHAPE(val _OP_ obj1%val(1:obj1%len), obj1%s(1:2)), &
                             TypeFEVariableVector, TypeFEVariableTime)
  END IF

CASE (spacetime)

  IF (obj1%defineon .EQ. nodal) THEN
    ans = NodalVariable(RESHAPE(val _OP_ obj1%val(1:obj1%len), obj1%s(1:3)), &
                        TypeFEVariableVector, TypeFEVariableSpaceTime)
  ELSE
    ans = QuadratureVariable(RESHAPE(val _OP_ obj1%val(1:obj1%len), obj1%s(1:3)), &
                             TypeFEVariableVector, TypeFEVariableSpaceTime)
  END IF

END SELECT
