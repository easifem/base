SELECT CASE (obj1%vartype)

CASE (constant)

  SELECT CASE (obj2%vartype)

  CASE (constant)

    IF (obj1%defineon .EQ. Nodal) THEN

      ans = NodalVariable(obj1%val(1) _OP_ obj2%val(1), &
                          TypeFEVariableScalar, TypeFEVariableConstant)

      RETURN

    END IF

    ans = QuadratureVariable(obj1%val(1) _OP_ obj2%val(1), &
                             TypeFEVariableScalar, TypeFEVariableConstant)

  CASE (space)

    IF (obj2%defineon .EQ. Nodal) THEN

      ans = NodalVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                          TypeFEVariableScalar, TypeFEVariableSpace)

      RETURN
    END IF

    ans = QuadratureVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                             TypeFEVariableScalar, TypeFEVariableSpace)

  CASE (time)

    IF (obj2%defineon .EQ. Nodal) THEN
      ans = NodalVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                          TypeFEVariableScalar, TypeFEVariableTime)

      RETURN
    END IF

    ans = QuadratureVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                             TypeFEVariableScalar, TypeFEVariableTime)

  CASE (spacetime)

    IF (obj2%defineon .EQ. Nodal) THEN

      ans = NodalVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                   TypeFEVariableScalar, TypeFEVariableSpaceTime, obj2%s(1:2))

      RETURN
    END IF

    ans = QuadratureVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                   TypeFEVariableScalar, TypeFEVariableSpaceTime, obj2%s(1:2))

  END SELECT

CASE (space)

  SELECT CASE (obj2%vartype)

  CASE (constant)

    IF (obj1%defineon .EQ. Nodal) THEN

      ans = NodalVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1), &
                          TypeFEVariableScalar, TypeFEVariableSpace)

      RETURN
    END IF

    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1), &
                             TypeFEVariableScalar, TypeFEVariableSpace)

  CASE (space)

    IF (obj1%defineon .EQ. Nodal) THEN

      ans = NodalVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                          TypeFEVariableScalar, TypeFEVariableSpace)

      RETURN
    END IF

    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                             TypeFEVariableScalar, TypeFEVariableSpace)

  END SELECT

CASE (time)

  SELECT CASE (obj2%vartype)

  CASE (constant)

    IF (obj1%defineon .EQ. Nodal) THEN

      ans = NodalVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1), &
                          TypeFEVariableScalar, TypeFEVariableTime)

      RETURN
    END IF

    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1), &
                             TypeFEVariableScalar, TypeFEVariableTime)

  CASE (time)

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                          TypeFEVariableScalar, TypeFEVariableTime)
      RETURN
    END IF
    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                             TypeFEVariableScalar, TypeFEVariableTime)

  END SELECT

CASE (spacetime)

  SELECT CASE (obj2%vartype)

  CASE (constant)

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1), &
                   TypeFEVariableScalar, TypeFEVariableSpaceTime, obj1%s(1:2))
      RETURN
    END IF
    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1), &
                   TypeFEVariableScalar, TypeFEVariableSpaceTime, obj1%s(1:2))

  CASE (spacetime)

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                   TypeFEVariableScalar, TypeFEVariableSpaceTime, obj1%s(1:2))
      RETURN
    END IF
    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                   TypeFEVariableScalar, TypeFEVariableSpaceTime, obj1%s(1:2))

  END SELECT

END SELECT
