SELECT CASE (obj1%vartype)

CASE (constant)

  SELECT CASE (obj2%vartype)

  CASE (constant)

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                          typeFEVariableVector, typeFEVariableConstant)
      RETURN
    END IF

    ans = QuadratureVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                             typeFEVariableVector, typeFEVariableConstant)
  CASE (space)

    IF (obj2%defineon .EQ. nodal) THEN
      ans = NodalVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                       typeFEVariableVector, typeFEVariableSpace, obj2%s(1:2))

      RETURN
    END IF

    ans = QuadratureVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                       typeFEVariableVector, typeFEVariableSpace, obj2%s(1:2))
  CASE (time)

    IF (obj2%defineon .EQ. nodal) THEN
      ans = NodalVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                        typeFEVariableVector, typeFEVariableTime, obj2%s(1:2))
      RETURN
    END IF

    ans = QuadratureVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                        typeFEVariableVector, typeFEVariableTime, obj2%s(1:2))

  CASE (spacetime)

    IF (obj2%defineon .EQ. nodal) THEN
      ans = NodalVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                   typeFEVariableVector, typeFEVariableSpaceTime, obj2%s(1:3))
      RETURN
    END IF

    ans = QuadratureVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                   typeFEVariableVector, typeFEVariableSpaceTime, obj2%s(1:3))

  END SELECT

CASE (space)

  SELECT CASE (obj2%vartype)

  CASE (constant)

    CALL Reallocate(r2, obj2%s(1), obj1%s(1))

    DO jj = 1, SIZE(r2, 2)
      r2(:, jj) = obj1%val(jj) _OP_ obj2%val(1:obj2%len)
    END DO

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(r2, typeFEVariableVector, typeFEVariableSpace)
      RETURN
    END IF

    ans = QuadratureVariable(r2, typeFEVariableVector, typeFEVariableSpace)

  CASE (space)

    r2 = GET(obj2, TypeFEVariableVector, TypeFEVariableSpace)

    DO jj = 1, SIZE(r2, 2)
      r2(:, jj) = obj1%val(jj) _OP_ r2(:, jj)
    END DO

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(r2, typeFEVariableVector, typeFEVariableSpace)
      DEALLOCATE (r2)
      RETURN
    END IF

    ans = QuadratureVariable(r2, typeFEVariableVector, typeFEVariableSpace)
    DEALLOCATE (r2)

  END SELECT

CASE (time)

  SELECT CASE (obj2%vartype)

  CASE (constant)

    CALL Reallocate(r2, obj2%s(1), obj1%s(1))

    DO jj = 1, SIZE(r2, 2)
      r2(:, jj) = obj1%val(jj) _OP_ obj2%val(1:obj2%len)
    END DO

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(r2, typeFEVariableVector, typeFEVariableTime)
      DEALLOCATE (r2)
      RETURN
    END IF

    ans = QuadratureVariable(r2, typeFEVariableVector, typeFEVariableTime)
    DEALLOCATE (r2)

  CASE (time)

    r2 = GET(obj2, TypeFEVariableVector, TypeFEVariableTime)

    DO jj = 1, SIZE(r2, 2)
      r2(:, jj) = obj1%val(jj) _OP_ r2(:, jj)
    END DO

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(r2, typeFEVariableVector, typeFEVariableTime)
      DEALLOCATE (r2)
      RETURN
    END IF

    ans = QuadratureVariable(r2, typeFEVariableVector, typeFEVariableTime)
    DEALLOCATE (r2)

  END SELECT

CASE (spacetime)

  SELECT CASE (obj2%vartype)

  CASE (constant)

    r2 = GET(obj1, typeFEVariableScalar, typeFEVariableSpaceTime)
    CALL Reallocate(r3, obj2%s(1), SIZE(r2, 1), SIZE(r2, 2))

    DO kk = 1, SIZE(r3, 3)
      DO jj = 1, SIZE(r3, 2)
        r3(:, jj, kk) = r2(jj, kk) _OP_ obj2%val(1:obj2%len)
      END DO
    END DO

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(r3, typeFEVariableVector, typeFEVariableSpaceTime)
      DEALLOCATE (r2, r3)

      RETURN
    END IF

    ans = QuadratureVariable(r3, typeFEVariableVector, &
                             typeFEVariableSpaceTime)

    DEALLOCATE (r2, r3)

  CASE (spacetime)
    r2 = GET(obj1, typeFEVariableScalar, typeFEVariableSpaceTime)
    r3 = GET(obj2, typeFEVariableVector, typeFEVariableSpaceTime)

    DO kk = 1, SIZE(r3, 3)
      DO jj = 1, SIZE(r3, 2)
        r3(:, jj, kk) = r2(jj, kk) _OP_ r3(:, jj, kk)
      END DO
    END DO

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(r3, typeFEVariableVector, typeFEVariableSpaceTime)
      DEALLOCATE (r2, r3)
      RETURN
    END IF

    ans = QuadratureVariable(r3, typeFEVariableVector, &
                             typeFEVariableSpaceTime)

    DEALLOCATE (r2, r3)

  END SELECT

END SELECT
