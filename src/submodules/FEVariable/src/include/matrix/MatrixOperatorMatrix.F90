SELECT CASE (obj1%vartype)
CASE (constant)
  SELECT CASE (obj2%vartype)
  CASE (constant)
    IF (obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                    TypeFEVariableMatrix, TypeFEVariableConstant, obj1%s(1:2))
    ELSE
    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                    TypeFEVariableMatrix, TypeFEVariableConstant, obj1%s(1:2))
    END IF
  CASE (space)
    r2 = GET(obj1, TypeFEVariableMatrix, TypeFEVariableConstant)
    r3 = GET(obj2, TypeFEVariableMatrix, TypeFEVariableSpace)
    DO jj = 1, SIZE(r3, 3)
      r3(:, :, jj) = r2(:, :) _OP_ r3(:, :, jj)
    END DO
    IF (obj2%defineon .EQ. nodal) THEN
      ans = NodalVariable(r3, TypeFEVariableMatrix, TypeFEVariableSpace)
    ELSE
      ans = QuadratureVariable(r3, TypeFEVariableMatrix, TypeFEVariableSpace)
    END IF
    DEALLOCATE (r2, r3)
  CASE (time)
    r2 = GET(obj1, TypeFEVariableMatrix, TypeFEVariableConstant)
    r3 = GET(obj2, TypeFEVariableMatrix, TypeFEVariableTime)
    DO jj = 1, SIZE(r3, 3)
      r3(:, :, jj) = r2(:, :) _OP_ r3(:, :, jj)
    END DO
    IF (obj2%defineon .EQ. nodal) THEN
      ans = NodalVariable(r3, TypeFEVariableMatrix, TypeFEVariableTime)
    ELSE
      ans = QuadratureVariable(r3, TypeFEVariableMatrix, TypeFEVariableTime)
    END IF
    DEALLOCATE (r2, r3)
  CASE (spacetime)
    r2 = GET(obj1, TypeFEVariableMatrix, TypeFEVariableConstant)
    r4 = GET(obj2, TypeFEVariableMatrix, TypeFEVariableSpaceTime)
    DO kk = 1, SIZE(r4, 4)
      DO jj = 1, SIZE(r4, 3)
        r4(:, :, jj, kk) = r2(:, :) _OP_ r4(:, :, jj, kk)
      END DO
    END DO
    IF (obj2%defineon .EQ. nodal) THEN
      ans = NodalVariable(r4, TypeFEVariableMatrix, TypeFEVariableSpaceTime)
    ELSE
      ans = QuadratureVariable(r4, TypeFEVariableMatrix, &
                               TypeFEVariableSpaceTime)
    END IF
    DEALLOCATE (r2, r4)
  END SELECT
CASE (space)
  SELECT CASE (obj2%vartype)
  CASE (constant)
    r3 = GET(obj1, TypeFEVariableMatrix, TypeFEVariableSpace)
    r2 = GET(obj2, TypeFEVariableMatrix, TypeFEVariableConstant)
    DO jj = 1, SIZE(r3, 3)
      r3(:, :, jj) = r3(:, :, jj) _OP_ r2(:, :)
    END DO
    IF (obj1%defineon .EQ. nodal) THEN
      ans = NodalVariable(r3, &
                          TypeFEVariableMatrix, TypeFEVariableSpace)
    ELSE
      ans = QuadratureVariable(r3, &
                               TypeFEVariableMatrix, TypeFEVariableSpace)
    END IF
    DEALLOCATE (r2, r3)
  CASE (space)
    IF (obj1%defineon .EQ. nodal) THEN
      ans = NodalVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                       TypeFEVariableMatrix, TypeFEVariableSpace, obj1%s(1:3))
    ELSE
    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                       TypeFEVariableMatrix, TypeFEVariableSpace, obj1%s(1:3))
    END IF
  END SELECT
CASE (time)
  SELECT CASE (obj2%vartype)
  CASE (constant)
    r3 = GET(obj1, TypeFEVariableMatrix, TypeFEVariableTime)
    r2 = GET(obj2, TypeFEVariableMatrix, TypeFEVariableConstant)
    DO jj = 1, SIZE(r3, 3)
      r3(:, :, jj) = r3(:, :, jj) _OP_ r2(:, :)
    END DO
    IF (obj1%defineon .EQ. nodal) THEN
      ans = NodalVariable(r3, &
                          TypeFEVariableMatrix, TypeFEVariableTime)
    ELSE
      ans = QuadratureVariable(r3, &
                               TypeFEVariableMatrix, TypeFEVariableTime)
    END IF
    DEALLOCATE (r2, r3)
  CASE (time)
    IF (obj1%defineon .EQ. nodal) THEN
      ans = NodalVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                        TypeFEVariableMatrix, TypeFEVariableTime, obj1%s(1:3))
    ELSE
    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                        TypeFEVariableMatrix, TypeFEVariableTime, obj1%s(1:3))
    END IF
  END SELECT
CASE (spacetime)
  SELECT CASE (obj2%vartype)
  CASE (constant)
    r4 = GET(obj1, TypeFEVariableMatrix, TypeFEVariableSpaceTime)
    r2 = GET(obj2, TypeFEVariableMatrix, TypeFEVariableConstant)
    DO kk = 1, SIZE(r4, 4)
      DO jj = 1, SIZE(r4, 3)
        r4(:, :, jj, kk) = r4(:, :, jj, kk) _OP_ r2(:, :)
      END DO
    END DO
    IF (obj1%defineon .EQ. nodal) THEN
      ans = NodalVariable(r4, &
                          TypeFEVariableMatrix, TypeFEVariableSpaceTime)
    ELSE
      ans = QuadratureVariable(r4, &
                               TypeFEVariableMatrix, TypeFEVariableSpaceTime)
    END IF
    DEALLOCATE (r2, r4)
  CASE (spacetime)
    IF (obj1%defineon .EQ. nodal) THEN
      ans = NodalVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                   TypeFEVariableMatrix, TypeFEVariableSpaceTime, obj1%s(1:4))
    ELSE
    ans = QuadratureVariable(obj1%val(1:obj1%len) _OP_ obj2%val(1:obj2%len), &
                   TypeFEVariableMatrix, TypeFEVariableSpaceTime, obj1%s(1:4))
    END IF
  END SELECT
END SELECT
