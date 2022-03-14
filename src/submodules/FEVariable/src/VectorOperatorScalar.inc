! This PROGRAM is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This PROGRAM is free software: you can REDISTRIBUTE it and/or modify
! it under the terms of the GNU General PUBLIC License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This PROGRAM is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General PUBLIC License for more details.
!
! You should have received a copy of the GNU General PUBLIC License
! along WITH this PROGRAM.  IF not, see <https: //www.gnu.org/licenses/>
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
        & obj1%val(:) _OP_ obj2%val(1), &
        & typeFEVariableVector, &
        & typeFEVariableConstant)
    ELSE
      ans = QuadratureVariable( &
        & obj1%val(:) _OP_ obj2%val(1), &
        & typeFEVariableVector, &
        & typeFEVariableConstant)
    END IF
  !!
  !! space= constant _OP_ space
  !!
  CASE (space)
    !!
    CALL Reallocate(r2, obj1%s(1), obj2%s(1))
    !!
    DO jj = 1, SIZE(r2, 2)
      r2(:, jj) = obj1%val(:) _OP_ obj2%val(jj)
    END DO
    !!
    IF( obj2%defineon .EQ. Nodal ) THEN
      ans = NodalVariable(&
        & r2, &
        & typeFEVariableVector, &
        & typeFEVariableSpace)
    ELSE
      ans = QuadratureVariable(&
        & r2, &
        & typeFEVariableVector, &
        & typeFEVariableSpace)
    END IF
  !!
  !! time=constant _OP_ time
  !!
  CASE (time)
    !!
    CALL Reallocate(r2, obj1%s(1), obj2%s(1))
    !!
    DO jj = 1, SIZE(r2, 2)
      r2(:, jj) = obj1%val(:) _OP_ obj2%val(jj)
    END DO
    !!
    IF( obj2%defineon .EQ. Nodal ) THEN
      ans = NodalVariable( &
        & r2, &
        & typeFEVariableVector, &
        & typeFEVariableTime)
    ELSE
      ans = QuadratureVariable( &
        & r2, &
        & typeFEVariableVector, &
        & typeFEVariableTime)
    END IF
  !!
  !! spacetime=constant _OP_ spacetime
  !!
  CASE (spacetime)
    !!
    r2 = GET(obj2, typeFEVariableScalar, typeFEVariableSpaceTime)
    CALL Reallocate( r3, obj1%s(1), SIZE(r2,1), SIZE(r2,2) )
    !!
    DO kk = 1, SIZE(r3, 3)
      DO jj = 1, SIZE(r3, 2)
        r3(:, jj, kk) = obj1%val(:) _OP_ r2(jj, kk)
      END DO
    END DO
    !!
    IF(obj2%defineon .EQ. Nodal) THEN
      ans = NodalVariable(&
        & r3, &
        & typeFEVariableVector, &
        & typeFEVariableSpaceTime)
    ELSE
      ans = QuadratureVariable(&
        & r3, &
        & typeFEVariableVector, &
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
  SELECT CASE (obj1%vartype)
  !!
  !! space=space _OP_ constant
  !!
  CASE (constant)
    !!
    IF( obj1%defineon .EQ. nodal ) THEN
      ans = NodalVariable(&
        & RESHAPE(obj1%val(:) _OP_ obj2%val(1), obj1%s(1:2)), &
        & typeFEVariableVector, &
        & typeFEVariableSpace)
    ELSE
      ans = QuadratureVariable(&
        & RESHAPE(obj1%val(:) _OP_ obj2%val(1), obj1%s(1:2)), &
        & typeFEVariableVector, &
        & typeFEVariableSpace)
    END IF
  !!
  !! space=space _OP_ space
  !!
  CASE (space)
    !!
    r2 = GET(obj1, TypeFEVariableVector, TypeFEVariableSpace)
    !!
    DO jj = 1, SIZE(r2, 2)
      r2(:, jj) = r2(:, jj) _OP_ obj2%val(jj)
    END DO
    !!
    IF( obj1%defineon .EQ. Nodal ) THEN
      ans = NodalVariable( &
        & r2, &
        & typeFEVariableVector, &
        & typeFEVariableSpace)
    ELSE
      ans = QuadratureVariable( &
        & r2, &
        & typeFEVariableVector, &
        & typeFEVariableSpace)
    END IF
  END SELECT
!!
!!
!!
!!
CASE (time)
!!
  SELECT CASE (obj1%vartype)
  !!
  !! time=time _OP_ constant
  !!
  CASE (constant)
    !!
    IF( obj1%defineon .EQ. nodal ) THEN
      ans = NodalVariable(&
        & RESHAPE(obj1%val(:) _OP_ obj2%val(1), obj1%s(1:2)), &
        & typeFEVariableVector, &
        & typeFEVariableTime)
    ELSE
      ans = QuadratureVariable(&
        & RESHAPE(obj1%val(:) _OP_ obj2%val(1), obj1%s(1:2)), &
        & typeFEVariableVector, &
        & typeFEVariableTime)
    END IF
  !!
  !! time=time _OP_ time
  !!
  CASE (time)
    !!
    r2 = GET(obj1, TypeFEVariableVector, TypeFEVariableTime)
    !!
    DO jj = 1, SIZE(r2, 2)
      r2(:, jj) = r2(:, jj) _OP_ obj2%val(jj)
    END DO
    !!
    IF( obj1%defineon .EQ. Nodal ) THEN
      ans = NodalVariable(&
        & r2, &
        & typeFEVariableVector, &
        & typeFEVariableTime)
    ELSE
      ans = QuadratureVariable(&
        & r2, &
        & typeFEVariableVector, &
        & typeFEVariableTime)
    END IF
  END SELECT
!!
!!
!!
!!
CASE (spacetime)
  !!
  SELECT CASE (obj1%vartype)
  !!
  !! spacetime= spacetime _OP_ constant
  !!
  CASE (constant)
    !!
    IF( obj1%defineon .EQ. nodal ) THEN
      ans = NodalVariable(&
        & RESHAPE(obj1%val(:) _OP_ obj2%val(1), obj1%s(1:3)), &
        & typeFEVariableVector, &
        & typeFEVariableSpaceTime)
    ELSE
      ans = QuadratureVariable(&
        & RESHAPE(obj1%val(:) _OP_ obj2%val(1), obj1%s(1:3)), &
        & typeFEVariableVector, &
        & typeFEVariableSpaceTime)
    END IF
  !!
  !! spacetime=spacetime _OP_ spacetime
  !!
  CASE (spacetime)
    !!
    r3 = GET(obj1, typeFEVariableVector, typeFEVariableSpaceTime)
    r2 = GET(obj2, typeFEVariableScalar, typeFEVariableSpaceTime)
    !!
    DO kk = 1, SIZE(r3, 3)
      DO jj = 1, SIZE(r3, 2)
        r3(:, jj, kk) = r3(:,jj,kk) _OP_ r2(jj, kk)
      END DO
    END DO
    !!
    IF( obj1%defineon .EQ. Nodal ) THEN
      ans = NodalVariable(&
        & r3, &
        & typeFEVariableVector, &
        & typeFEVariableSpaceTime)
    ELSE
      ans = QuadratureVariable(&
        & r3, &
        & typeFEVariableVector, &
        & typeFEVariableSpaceTime)
    END IF
    !!
  END SELECT
!!
!!
!!
!!
END SELECT
