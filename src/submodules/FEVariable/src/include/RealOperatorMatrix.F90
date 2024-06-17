SELECT CASE (obj1%vartype)
CASE (constant)
  IF (obj1%defineon .EQ. nodal) THEN
    ans = NodalVariable(val _OP_ obj1%val(1:obj1%len), &
                    typeFEVariableMatrix, typeFEVariableConstant, obj1%s(1:2))
  ELSE
    ans = QuadratureVariable(val _OP_ obj1%val(1:obj1%len), &
                    typeFEVariableMatrix, typeFEVariableConstant, obj1%s(1:2))
  END IF
CASE (space)
  IF (obj1%defineon .EQ. nodal) THEN
    ans = NodalVariable(val _OP_ obj1%val(1:obj1%len), &
                       typeFEVariableMatrix, typeFEVariableSpace, obj1%s(1:3))
  ELSE
    ans = QuadratureVariable(val _OP_ obj1%val(1:obj1%len), &
                       typeFEVariableMatrix, typeFEVariableSpace, obj1%s(1:3))
  END IF
CASE (time)
  IF (obj1%defineon .EQ. nodal) THEN
    ans = NodalVariable(val _OP_ obj1%val(1:obj1%len), &
                        typeFEVariableMatrix, typeFEVariableTime, obj1%s(1:3))
  ELSE
    ans = QuadratureVariable(val _OP_ obj1%val(1:obj1%len), &
                        typeFEVariableMatrix, typeFEVariableTime, obj1%s(1:3))
  END IF
CASE (spacetime)
  IF (obj1%defineon .EQ. nodal) THEN
    ans = NodalVariable(val _OP_ obj1%val(1:obj1%len), &
                   typeFEVariableMatrix, typeFEVariableSpaceTime, obj1%s(1:4))
  ELSE
    ans = QuadratureVariable(val _OP_ obj1%val(1:obj1%len), &
                   typeFEVariableMatrix, typeFEVariableSpaceTime, obj1%s(1:4))
  END IF
END SELECT
