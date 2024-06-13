! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!----------------------------------------------------------------------------
!                                                             ScalarAddition
!----------------------------------------------------------------------------
!!
!! main
!!
SELECT CASE (obj1%vartype)
!!
!!
!!
!!
CASE (constant)
  !!
  SELECT CASE (obj2%vartype)
  !!
  !! constant = constant _OP_ constant
  !!
  CASE (constant)
    !!
    IF( obj1%defineon .EQ. Nodal ) THEN
      ans = NodalVariable( &
        & RESHAPE(obj1%val(1) _OP_ obj2%val(:), obj2%s(1:2)), &
        & typeFEVariableMatrix, &
        & typeFEVariableConstant)
    ELSE
      ans = QuadratureVariable( &
        & RESHAPE(obj1%val(1) _OP_ obj2%val(:), obj2%s(1:2)), &
        & typeFEVariableMatrix, &
        & typeFEVariableConstant)
    END IF
  !!
  !! space= constant _OP_ space
  !!
  CASE (space)
    !!
    IF( obj2%defineon .EQ. nodal ) THEN
      ans = NodalVariable(&
        & RESHAPE(obj1%val(1) _OP_ obj2%val(:), obj2%s(1:3)), &
        & typeFEVariableMatrix, &
        & typeFEVariableSpace)
    ELSE
      ans = QuadratureVariable(&
        & RESHAPE(obj1%val(1) _OP_ obj2%val(:), obj2%s(1:3)), &
        & typeFEVariableMatrix, &
        & typeFEVariableSpace)
    END IF
  !!
  !! time=constant _OP_ time
  !!
  CASE (time)
    !!
    IF( obj2%defineon .EQ. nodal ) THEN
      ans = NodalVariable(&
        & RESHAPE(obj1%val(1) _OP_ obj2%val(:), obj2%s(1:3)), &
        & typeFEVariableMatrix, &
        & typeFEVariableTime)
    ELSE
      ans = QuadratureVariable(&
        & RESHAPE(obj1%val(1) _OP_ obj2%val(:), obj2%s(1:3)), &
        & typeFEVariableMatrix, &
        & typeFEVariableTime)
    END IF
  !!
  !! spacetime=constant _OP_ spacetime
  !!
  CASE (spacetime)
    !!
    IF( obj2%defineon .EQ. nodal ) THEN
      ans = NodalVariable(&
        & RESHAPE(obj1%val(1) _OP_ obj2%val(:), obj2%s(1:4)), &
        & typeFEVariableMatrix, &
        & typeFEVariableSpaceTime)
      ELSE
        ans = QuadratureVariable(&
          & RESHAPE(obj1%val(1) _OP_ obj2%val(:), obj2%s(1:4)), &
          & typeFEVariableMatrix, &
          & typeFEVariableSpaceTime)
    END IF
    !!
  END SELECT
!!
!!
!!
!!
CASE (space)
  !!
  SELECT CASE (obj2%vartype)
  !!
  !! space=space _OP_ constant
  !!
  CASE (constant)
    !!
    r2 = GET(obj2, typeFEVariableMatrix, typeFEVariableConstant)
    CALL Reallocate(r3, SIZE(r2,1), SIZE(r2,2), obj1%s(1))
    !!
    DO jj = 1, SIZE(r3, 3)
      r3(:, :, jj) = obj1%val(jj) _OP_ r2
    END DO
    !!
    IF( obj1%defineon .EQ. Nodal ) THEN
      ans = NodalVariable(&
        & r3, &
        & typeFEVariableMatrix, &
        & typeFEVariableSpace)
    ELSE
      ans = QuadratureVariable(&
        & r3, &
        & typeFEVariableMatrix, &
        & typeFEVariableSpace)
    END IF
  !!
  !! space=space _OP_ space
  !!
  CASE (space)
    !!
    r3 = GET(obj2, TypeFEVariableMatrix, TypeFEVariableSpace)
    !!
    DO jj = 1, SIZE(r3, 3)
      r3(:, :, jj) = obj1%val(jj) _OP_ r3(:, :, jj)
    END DO
    !!
    IF( obj1%defineon .EQ. Nodal ) THEN
      ans = NodalVariable( &
        & r3, &
        & typeFEVariableMatrix, &
        & typeFEVariableSpace)
    ELSE
      ans = QuadratureVariable( &
        & r3, &
        & typeFEVariableMatrix, &
        & typeFEVariableSpace)
    END IF
  END SELECT
!!
!!
!!
!!
CASE (time)
!!
  SELECT CASE (obj2%vartype)
  !!
  !! time=time _OP_ constant
  !!
  CASE (constant)
    !!
    r2 = GET(obj2, typeFEVariableMatrix, typeFEVariableConstant)
    !!
    CALL Reallocate(r3, SIZE(r2,1), SIZE(r2,2), obj1%s(1))
    !!
    DO jj = 1, SIZE(r3, 3)
      r3(:, :, jj) = obj1%val(jj) _OP_ r2
    END DO
    !!
    IF( obj1%defineon .EQ. Nodal ) THEN
      ans = NodalVariable(&
        & r3, &
        & typeFEVariableMatrix, &
        & typeFEVariableTime)
    ELSE
      ans = QuadratureVariable(&
        & r3, &
        & typeFEVariableMatrix, &
        & typeFEVariableTime)
    END IF
  !!
  !! time=time _OP_ time
  !!
  CASE (time)
    !!
    r3 = GET(obj2, TypeFEVariableMatrix, TypeFEVariableTime)
    !!
    DO jj = 1, SIZE(r3, 3)
      r3(:, :, jj) = obj1%val(jj) _OP_ r3(:, :, jj)
    END DO
    !!
    IF( obj1%defineon .EQ. Nodal ) THEN
      ans = NodalVariable(&
        & r3, &
        & typeFEVariableMatrix, &
        & typeFEVariableTime)
    ELSE
      ans = QuadratureVariable(&
        & r3, &
        & typeFEVariableMatrix, &
        & typeFEVariableTime)
    END IF
  END SELECT
!!
!!
!!
!!
CASE (spacetime)
  !!
  SELECT CASE (obj2%vartype)
  !!
  !! spacetime= spacetime _OP_ constant
  !!
  CASE (constant)
    !!
    r2 = GET(obj1, typeFEVariableScalar, typeFEVariableSpaceTime)
    m2 = GET(obj2, typeFEVariableMatrix, typeFEVariableConstant)
    !!
    CALL Reallocate( r4, SIZE(m2, 1), SIZE(m2,2), SIZE(r2,1), SIZE(r2,2) )
    !!
    DO kk = 1, SIZE(r4, 4)
      DO jj = 1, SIZE(r4, 3)
        r4(:, :, jj, kk) = r2(jj, kk) _OP_ m2
      END DO
    END DO
    !!
    IF(obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(&
        & r4, &
        & typeFEVariableMatrix, &
        & typeFEVariableSpaceTime)
    ELSE
      ans = QuadratureVariable(&
        & r4, &
        & typeFEVariableMatrix, &
        & typeFEVariableSpaceTime)
    END IF
  !!
  !! spacetime=spacetime _OP_ spacetime
  !!
  CASE (spacetime)
    !!
    r2 = GET(obj1, typeFEVariableScalar, typeFEVariableSpaceTime)
    r4 = GET(obj2, typeFEVariableMatrix, typeFEVariableSpaceTime)
    !!
    DO kk = 1, SIZE(r4, 4)
      DO jj = 1, SIZE(r4, 3)
        r4(:, :, jj, kk) = r2(jj, kk) _OP_ r4(:,:,jj,kk)
      END DO
    END DO
    !!
    IF( obj1%defineon .EQ. Nodal ) THEN
      ans = NodalVariable(&
        & r4, &
        & typeFEVariableMatrix, &
        & typeFEVariableSpaceTime)
    ELSE
      ans = QuadratureVariable(&
        & r4, &
        & typeFEVariableMatrix, &
        & typeFEVariableSpaceTime)
    END IF
    !!
  END SELECT
!!
!!
!!
!!
END SELECT
