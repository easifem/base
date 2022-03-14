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
  !! constant = constant + constant
  !!
  CASE (constant)
    !!
    IF( obj1%defineon .EQ. Nodal ) THEN
      ans = NodalVariable( &
        & obj1%val(1) _OP_ obj2%val(1), &
        & typeFEVariableScalar, &
        & typeFEVariableConstant)
    ELSE
      ans = QuadratureVariable( &
        & obj1%val(1) _OP_ obj2%val(1), &
        & typeFEVariableScalar, &
        & typeFEVariableConstant)
    END IF
  !!
  !! space= constant _OP_ space
  !!
  CASE (space)
  !!
    IF( obj2%defineon .EQ. Nodal ) THEN
      ans = NodalVariable(&
        & obj1%val(1) _OP_ obj2%val(:), &
        & typeFEVariableScalar, &
        & typeFEVariableSpace)
    ELSE
      ans = QuadratureVariable(&
        & obj1%val(1) _OP_ obj2%val(:), &
        & typeFEVariableScalar, &
        & typeFEVariableSpace)
    END IF
  !!
  !! time=constant _OP_ time
  !!
  CASE (time)
  !!
    IF( obj2%defineon .EQ. Nodal) THEN
      ans = NodalVariable(&
        & obj1%val(1) _OP_ obj2%val(:), &
        & typeFEVariableScalar, &
        & typeFEVariableTime)
    ELSE
      ans = QuadratureVariable(&
        & obj1%val(1) _OP_ obj2%val(:), &
        & typeFEVariableScalar, &
        & typeFEVariableTime)
    END IF
  !!
  !! spacetime=constant _OP_ spacetime
  !!
  CASE (spacetime)
  !!
    IF( obj2%defineon .EQ. Nodal ) THEN
      ans = NodalVariable(&
        & RESHAPE(obj1%val(1) _OP_ obj2%val(:), obj2%s(1:2)), &
        & typeFEVariableScalar, &
        & typeFEVariableSpaceTime)
    ELSE
      ans = QuadratureVariable(&
        & RESHAPE(obj1%val(1) _OP_ obj2%val(:), obj2%s(1:2)), &
        & typeFEVariableScalar, &
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
    IF( obj1%defineon .EQ. Nodal ) THEN
      ans = NodalVariable(&
        & obj1%val(:) _OP_ obj2%val(1), &
        & typeFEVariableScalar, &
        & typeFEVariableSpace)
    ELSE
      ans = QuadratureVariable(&
        & obj1%val(:) _OP_ obj2%val(1), &
        & typeFEVariableScalar, &
        & typeFEVariableSpace)
    END IF
  !!
  !! space=space _OP_ space
  !!
  CASE (space)
  !!
    IF( obj1%defineon .EQ. Nodal ) THEN
      ans = NodalVariable( &
        & obj1%val(:) _OP_ obj2%val(:), &
        & typeFEVariableScalar, &
        & typeFEVariableSpace)
    ELSE
      ans = QuadratureVariable( &
        & obj1%val(:) _OP_ obj2%val(:), &
        & typeFEVariableScalar, &
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
    IF( obj1%defineon .EQ. Nodal ) THEN
      ans = NodalVariable( &
        & obj1%val(:) _OP_ obj2%val(1), &
        & typeFEVariableScalar, &
        & typeFEVariableTime)
    ELSE
      ans = QuadratureVariable( &
        & obj1%val(:) _OP_ obj2%val(1), &
        & typeFEVariableScalar, &
        & typeFEVariableTime)
    END IF
  !!
  !! time=time _OP_ time
  !!
  CASE (time)
    !!
    IF( obj1%defineon .EQ. Nodal ) THEN
      ans = NodalVariable(&
        & obj1%val(:) _OP_ obj2%val(:), &
        & typeFEVariableScalar, &
        & typeFEVariableTime)
    ELSE
      ans = QuadratureVariable(&
        & obj1%val(:) _OP_ obj2%val(:), &
        & typeFEVariableScalar, &
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
    IF(obj1%defineon .EQ. Nodal) THEN
      ans = NodalVariable(&
        & RESHAPE(obj1%val(:) _OP_ obj2%val(1), obj1%s(1:2)), &
        & typeFEVariableScalar, &
        & typeFEVariableSpaceTime)
    ELSE
      ans = QuadratureVariable(&
        & RESHAPE(obj1%val(:) _OP_ obj2%val(1), obj1%s(1:2)), &
        & typeFEVariableScalar, &
        & typeFEVariableSpaceTime)
    END IF
  !!
  !! spacetime=spacetime _OP_ spacetime
  !!
  CASE (spacetime)
    !!
    IF( obj1%defineon .EQ. Nodal ) THEN
      ans = NodalVariable(&
        & RESHAPE(obj1%val(:) _OP_ obj2%val(:), obj1%s(1:2)), &
        & typeFEVariableScalar, &
        & typeFEVariableSpaceTime)
    ELSE
      ans = QuadratureVariable(&
        & RESHAPE(obj1%val(:) _OP_ obj2%val(:), obj1%s(1:2)), &
        & typeFEVariableScalar, &
        & typeFEVariableSpaceTime)
    END IF
    !!
  END SELECT
!!
!!
!!
!!
END SELECT
