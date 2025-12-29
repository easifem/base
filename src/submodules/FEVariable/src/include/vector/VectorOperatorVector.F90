SELECT CASE (obj1%vartype)
CASE (constant)
  SELECT CASE (obj2%vartype)
  CASE (constant)
    IF (obj1%defineon .EQ. nodal) THEN
      ans = NodalVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                          typeFEVariableVector, typeFEVariableConstant)
    ELSE
    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                               typeFEVariableVector, typeFEVariableConstant)
    END IF
  CASE (space)
    r2 = GET(obj2, TypeFEVariableVector, TypeFEVariableSpace)
    DO jj = 1, SIZE(r2, 2)
      r2(:, jj) = obj1%val(1:obj1%len) _OP_ r2(:, jj)
    END DO
    IF (obj2%defineon .EQ. nodal) THEN
      ans = NodalVariable(r2, &
                          typeFEVariableVector, typeFEVariableSpace)
    ELSE
      ans = QuadratureVariable(r2, &
                               typeFEVariableVector, typeFEVariableSpace)
    END IF
    DEALLOCATE (r2)
  CASE (time)
    r2 = GET(obj2, TypeFEVariableVector, TypeFEVariableTime)
    DO jj = 1, SIZE(r2, 2)
      r2(:, jj) = obj1%val(1:obj1%len) _OP_ r2(:, jj)
    END DO
    IF (obj2%defineon .EQ. nodal) THEN
      ans = NodalVariable(r2, &
                          typeFEVariableVector, typeFEVariableTime)
    ELSE
      ans = QuadratureVariable(r2, &
                               typeFEVariableVector, typeFEVariableTime)
    END IF
    DEALLOCATE (r2)
  CASE (spacetime)
    r3 = GET(obj2, TypeFEVariableVector, TypeFEVariableSpaceTime)
    DO kk = 1, SIZE(r3, 3)
      DO jj = 1, SIZE(r3, 2)
        r3(:, jj, kk) = obj1%val(1:obj1%len) _OP_ r3(:, jj, kk)
      END DO
    END DO
    IF (obj2%defineon .EQ. nodal) THEN
      ans = NodalVariable(r3, &
                          typeFEVariableVector, typeFEVariableSpaceTime)
    ELSE
      ans = QuadratureVariable(r3, &
                               typeFEVariableVector, typeFEVariableSpaceTime)
    END IF
    DEALLOCATE (r3)

  END SELECT
CASE (space)
  SELECT CASE (obj2%vartype)
  CASE (constant)
    r2 = GET(obj1, TypeFEVariableVector, TypeFEVariableSpace)
    DO jj = 1, SIZE(r2, 2)
      r2(:, jj) = r2(:, jj) _OP_ obj2%val(1:obj2%len)
    END DO
    IF (obj1%defineon .EQ. nodal) THEN
      ans = NodalVariable(r2, &
                          typeFEVariableVector, typeFEVariableSpace)
    ELSE
      ans = QuadratureVariable(r2, &
                               typeFEVariableVector, typeFEVariableSpace)
    END IF
    DEALLOCATE (r2)
  CASE (space)
    IF (obj1%defineon .EQ. nodal) THEN
      ans = NodalVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                       typeFEVariableVector, typeFEVariableSpace, obj1%s(1:2))
    ELSE
    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                       typeFEVariableVector, typeFEVariableSpace, obj1%s(1:2))
    END IF
  END SELECT
CASE (time)
  SELECT CASE (obj2%vartype)
  CASE (constant)
    r2 = GET(obj1, TypeFEVariableVector, TypeFEVariableTime)
    DO jj = 1, SIZE(r2, 2)
      r2(:, jj) = r2(:, jj) _OP_ obj2%val(1:obj2%len)
    END DO
    IF (obj1%defineon .EQ. nodal) THEN
      ans = NodalVariable(r2, &
                          typeFEVariableVector, typeFEVariableTime)
    ELSE
      ans = QuadratureVariable(r2, &
                               typeFEVariableVector, typeFEVariableTime)
    END IF
  CASE (time)
    IF (obj1%defineon .EQ. nodal) THEN
      ans = NodalVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                        typeFEVariableVector, typeFEVariableTime, obj1%s(1:2))
    ELSE
    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                        typeFEVariableVector, typeFEVariableTime, obj1%s(1:2))
    END IF
  END SELECT
CASE (spacetime)
  SELECT CASE (obj2%vartype)
  CASE (constant)
    r3 = GET(obj1, TypeFEVariableVector, TypeFEVariableSpaceTime)
    DO kk = 1, SIZE(r3, 3)
      DO jj = 1, SIZE(r3, 2)
        r3(:, jj, kk) = r3(:, jj, kk) _OP_ obj2%val(1:obj2%len)
      END DO
    END DO
    IF (obj1%defineon .EQ. nodal) THEN
      ans = NodalVariable(r3, &
                          typeFEVariableVector, typeFEVariableSpaceTime)
    ELSE
      ans = QuadratureVariable(r3, &
                               typeFEVariableVector, typeFEVariableSpaceTime)
    END IF
    DEALLOCATE (r3)

  CASE (spacetime)
    IF (obj1%defineon .EQ. nodal) THEN
      ans = NodalVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                   typeFEVariableVector, typeFEVariableSpaceTime, obj1%s(1:3))
    ELSE
    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                   typeFEVariableVector, typeFEVariableSpaceTime, obj1%s(1:3))
    END IF
  END SELECT

END SELECT
