SELECT CASE (obj%vartype)

CASE (constant)

  IF (obj%defineon .EQ. nodal) THEN
    ans = NodalVariable(obj%val(1)**n, &
                        TypeFEVariableScalar, TypeFEVariableConstant)
  ELSE
    ans = QuadratureVariable(obj%val(1)**n, &
                             TypeFEVariableScalar, TypeFEVariableConstant)
  END IF

CASE (space)

  IF (obj%defineon .EQ. nodal) THEN
    ans = NodalVariable(obj%val(1:obj%len)**n, &
                        TypeFEVariableScalar, TypeFEVariableSpace)
  ELSE
    ans = QuadratureVariable(obj%val(1:obj%len)**n, &
                             TypeFEVariableScalar, TypeFEVariableSpace)
  END IF

CASE (time)

  IF (obj%defineon .EQ. nodal) THEN
    ans = NodalVariable(obj%val(1:obj%len)**n, &
                        TypeFEVariableScalar, TypeFEVariableTime)
  ELSE
    ans = QuadratureVariable(obj%val(1:obj%len)**n, &
                             TypeFEVariableScalar, TypeFEVariableTime)
  END IF

CASE (spacetime)

  IF (obj%defineon .EQ. nodal) THEN
    ans = NodalVariable(obj%val(1:obj%len)**n, &
                    TypeFEVariableScalar, TypeFEVariableSpaceTime, obj%s(1:2))
  ELSE
    ans = QuadratureVariable(obj%val(1:obj%len)**n, &
                    TypeFEVariableScalar, TypeFEVariableSpaceTime, obj%s(1:2))
  END IF
END SELECT
