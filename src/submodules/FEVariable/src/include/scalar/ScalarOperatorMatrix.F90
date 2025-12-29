SELECT CASE (obj1%vartype)

CASE (constant)

  SELECT CASE (obj2%vartype)

  CASE (constant)

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                    typeFEVariableMatrix, typeFEVariableConstant, obj2%s(1:2))

      RETURN
    END IF

    ans = QuadratureVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                    typeFEVariableMatrix, typeFEVariableConstant, obj2%s(1:2))

  CASE (space)

    IF (obj2%defineon .EQ. nodal) THEN
      ans = NodalVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                       typeFEVariableMatrix, typeFEVariableSpace, obj2%s(1:3))
      RETURN
    END IF

    ans = QuadratureVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                       typeFEVariableMatrix, typeFEVariableSpace, obj2%s(1:3))

  CASE (time)

    IF (obj2%defineon .EQ. nodal) THEN
      ans = NodalVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                        typeFEVariableMatrix, typeFEVariableTime, obj2%s(1:3))
      RETURN
    END IF

    ans = QuadratureVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                        typeFEVariableMatrix, typeFEVariableTime, obj2%s(1:3))

  CASE (spacetime)

    IF (obj2%defineon .EQ. nodal) THEN
      ans = NodalVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                   typeFEVariableMatrix, typeFEVariableSpaceTime, obj2%s(1:4))
      RETURN
    END IF

    ans = QuadratureVariable(obj1%val(1) _OP_ obj2%val(1:obj2%len), &
                   typeFEVariableMatrix, typeFEVariableSpaceTime, obj2%s(1:4))

  END SELECT

CASE (space)

  SELECT CASE (obj2%vartype)

  CASE (constant)

    r2 = GET(obj2, typeFEVariableMatrix, typeFEVariableConstant)
    CALL Reallocate(r3, SIZE(r2, 1), SIZE(r2, 2), obj1%s(1))

    DO jj = 1, SIZE(r3, 3)
      r3(:, :, jj) = obj1%val(jj) _OP_ r2
    END DO

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(r3, typeFEVariableMatrix, typeFEVariableSpace)
      DEALLOCATE (r2, r3)
      RETURN
    END IF

    ans = QuadratureVariable(r3, typeFEVariableMatrix, typeFEVariableSpace)
    DEALLOCATE (r2, r3)

  CASE (space)

    r3 = GET(obj2, TypeFEVariableMatrix, TypeFEVariableSpace)

    DO jj = 1, SIZE(r3, 3)
      r3(:, :, jj) = obj1%val(jj) _OP_ r3(:, :, jj)
    END DO

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(r3, typeFEVariableMatrix, typeFEVariableSpace)
      DEALLOCATE (r3)
      RETURN
    END IF

    ans = QuadratureVariable(r3, typeFEVariableMatrix, typeFEVariableSpace)
    DEALLOCATE (r3)

  END SELECT

CASE (time)

  SELECT CASE (obj2%vartype)

  CASE (constant)

    r2 = GET(obj2, typeFEVariableMatrix, typeFEVariableConstant)
    CALL Reallocate(r3, SIZE(r2, 1), SIZE(r2, 2), obj1%s(1))

    DO jj = 1, SIZE(r3, 3)
      r3(:, :, jj) = obj1%val(jj) _OP_ r2
    END DO

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(r3, typeFEVariableMatrix, typeFEVariableTime)
      DEALLOCATE (r2, r3)
      RETURN
    END IF

    ans = QuadratureVariable(r3, typeFEVariableMatrix, typeFEVariableTime)
    DEALLOCATE (r2, r3)

  CASE (time)

    r3 = GET(obj2, TypeFEVariableMatrix, TypeFEVariableTime)
    DO jj = 1, SIZE(r3, 3)
      r3(:, :, jj) = obj1%val(jj) _OP_ r3(:, :, jj)
    END DO

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(r3, typeFEVariableMatrix, typeFEVariableTime)
      DEALLOCATE (r3)
      RETURN
    END IF

    ans = QuadratureVariable(r3, typeFEVariableMatrix, typeFEVariableTime)
    DEALLOCATE (r3)

  END SELECT

CASE (spacetime)

  SELECT CASE (obj2%vartype)

  CASE (constant)

    r2 = GET(obj1, typeFEVariableScalar, typeFEVariableSpaceTime)
    m2 = GET(obj2, typeFEVariableMatrix, typeFEVariableConstant)
    CALL Reallocate(r4, SIZE(m2, 1), SIZE(m2, 2), SIZE(r2, 1), SIZE(r2, 2))

    DO kk = 1, SIZE(r4, 4)
      DO jj = 1, SIZE(r4, 3)
        r4(:, :, jj, kk) = r2(jj, kk) _OP_ m2
      END DO
    END DO

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(r4, typeFEVariableMatrix, typeFEVariableSpaceTime)
      DEALLOCATE (r2, m2, r4)
      RETURN
    END IF

    ans = QuadratureVariable(r4, typeFEVariableMatrix, &
                             typeFEVariableSpaceTime)

    DEALLOCATE (r2, m2, r4)

  CASE (spacetime)

    r2 = GET(obj1, typeFEVariableScalar, typeFEVariableSpaceTime)
    r4 = GET(obj2, typeFEVariableMatrix, typeFEVariableSpaceTime)

    DO kk = 1, SIZE(r4, 4)
      DO jj = 1, SIZE(r4, 3)
        r4(:, :, jj, kk) = r2(jj, kk) _OP_ r4(:, :, jj, kk)
      END DO
    END DO

    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(r4, typeFEVariableMatrix, &
                          typeFEVariableSpaceTime)
      DEALLOCATE (r2, r4)
      RETURN
    END IF

    ans = QuadratureVariable(r4, typeFEVariableMatrix, &
                             typeFEVariableSpaceTime)
    DEALLOCATE (r2, r4)

  END SELECT

END SELECT
