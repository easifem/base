SELECT CASE (obj1%varType)

CASE (constant)

  SELECT CASE (obj2%varType)

  CASE (constant)

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1), &
                    TypeFEVariableMatrix, TypeFEVariableConstant, obj1%s(1:2))
    ELSE
      ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1), &
                    TypeFEVariableMatrix, TypeFEVariableConstant, obj1%s(1:2))
    END IF

  CASE (space)

    r2 = GET(obj1, TypeFEVariableMatrix, TypeFEVariableConstant)
    CALL Reallocate(r3, SIZE(r2, 1), SIZE(r2, 2), obj2%s(1))
    DO jj = 1, SIZE(r3, 3)
      r3(:, :, jj) = r2 _OP_ obj2%val(jj)
    END DO

    IF (obj2%defineon .EQ. Nodal) THEN
      ans = NodalVariable(r3, TypeFEVariableMatrix, TypeFEVariableSpace)
    ELSE
      ans = QuadratureVariable(r3, TypeFEVariableMatrix, TypeFEVariableSpace)
    END IF

    DEALLOCATE (r2, r3)
  CASE (time)

    r2 = GET(obj1, TypeFEVariableMatrix, TypeFEVariableConstant)
    CALL Reallocate(r3, SIZE(r2, 1), SIZE(r2, 2), obj2%s(1))
    DO jj = 1, SIZE(r3, 3)
      r3(:, :, jj) = r2 _OP_ obj2%val(jj)
    END DO

    IF (obj2%defineon .EQ. Nodal) THEN
      ans = NodalVariable(r3, TypeFEVariableMatrix, TypeFEVariableTime)
    ELSE
      ans = QuadratureVariable(r3, TypeFEVariableMatrix, TypeFEVariableTime)
    END IF

    DEALLOCATE (r2, r3)
  CASE (spacetime)

    r2 = GET(obj2, TypeFEVariableScalar, TypeFEVariableSpaceTime)
    m2 = GET(obj1, TypeFEVariableMatrix, TypeFEVariableConstant)
    CALL Reallocate(r4, SIZE(m2, 1), SIZE(m2, 2), SIZE(r2, 1), SIZE(r2, 2))
    DO kk = 1, SIZE(r4, 4)
      DO jj = 1, SIZE(r4, 3)
        r4(:, :, jj, kk) = m2 _OP_ r2(jj, kk)
      END DO

    END DO

    IF (obj2%defineon .EQ. Nodal) THEN
      ans = NodalVariable(r4, TypeFEVariableMatrix, TypeFEVariableSpaceTime)
    ELSE
      ans = QuadratureVariable(r4, TypeFEVariableMatrix, &
                               TypeFEVariableSpaceTime)
    END IF

    DEALLOCATE (r2, r4, m2)
  END SELECT

CASE (space)

  SELECT CASE (obj1%varType)

  CASE (constant)

    IF (obj1%defineon .EQ. nodal) THEN
      ans = NodalVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1), &
                       TypeFEVariableMatrix, TypeFEVariableSpace, obj1%s(1:3))
    ELSE
      ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1), &
                       TypeFEVariableMatrix, TypeFEVariableSpace, obj1%s(1:3))
    END IF

  CASE (space)

    r3 = GET(obj1, TypeFEVariableMatrix, TypeFEVariableSpace)
    DO jj = 1, SIZE(r3, 3)
      r3(:, :, jj) = r3(:, :, jj) _OP_ obj2%val(jj)
    END DO

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(r3, TypeFEVariableMatrix, TypeFEVariableSpace)
    ELSE
      ans = QuadratureVariable(r3, TypeFEVariableMatrix, TypeFEVariableSpace)
    END IF

    DEALLOCATE (r3)
  END SELECT

CASE (time)

  SELECT CASE (obj1%varType)

  CASE (constant)

    IF (obj1%defineon .EQ. nodal) THEN
      ans = NodalVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1), &
                        TypeFEVariableMatrix, TypeFEVariableTime, obj1%s(1:3))
    ELSE
      ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1), &
                        TypeFEVariableMatrix, TypeFEVariableTime, obj1%s(1:3))
    END IF

  CASE (time)

    r3 = GET(obj1, TypeFEVariableMatrix, TypeFEVariableTime)
    DO jj = 1, SIZE(r3, 3)
      r3(:, :, jj) = r3(:, :, jj) _OP_ obj2%val(jj)
    END DO

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(r3, TypeFEVariableMatrix, TypeFEVariableTime)
    ELSE
      ans = QuadratureVariable(r3, TypeFEVariableMatrix, TypeFEVariableTime)
    END IF

    DEALLOCATE (r3)
  END SELECT

CASE (spacetime)

  SELECT CASE (obj1%varType)

  CASE (constant)

    IF (obj1%defineon .EQ. nodal) THEN
      ans = NodalVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1), &
                   TypeFEVariableMatrix, TypeFEVariableSpaceTime, obj1%s(1:4))
    ELSE
      ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1), &
                   TypeFEVariableMatrix, TypeFEVariableSpaceTime, obj1%s(1:4))
    END IF

  CASE (spacetime)

    r4 = GET(obj1, TypeFEVariableMatrix, TypeFEVariableSpaceTime)
    r2 = GET(obj2, TypeFEVariableScalar, TypeFEVariableSpaceTime)
    DO kk = 1, SIZE(r4, 4)
      DO jj = 1, SIZE(r4, 3)
        r4(:, :, jj, kk) = r4(:, :, jj, kk) _OP_ r2(jj, kk)
      END DO

    END DO

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(r4, TypeFEVariableMatrix, TypeFEVariableSpaceTime)
    ELSE
      ans = QuadratureVariable(r4, TypeFEVariableMatrix, &
                               TypeFEVariableSpaceTime)
    END IF

    DEALLOCATE (r2, r4)
  END SELECT

END SELECT
