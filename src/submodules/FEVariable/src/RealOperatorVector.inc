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
  IF( obj1%defineon .EQ. nodal ) THEN
    ans = NodalVariable( &
      & val _OP_ obj1%val(:), &
      & typeFEVariableVector, &
      & typeFEVariableConstant)
  ELSE
    ans = QuadratureVariable( &
      & val _OP_ obj1%val(:), &
      & typeFEVariableVector, &
      & typeFEVariableConstant)
  END IF
!!
!!
!!
!!
CASE (space)
  !!
  IF( obj1%defineon .EQ. nodal ) THEN
    ans = NodalVariable(&
      & RESHAPE(val _OP_ obj1%val(:), obj1%s(1:2)), &
      & typeFEVariableVector, &
      & typeFEVariableSpace)
  ELSE
    ans = QuadratureVariable(&
      & RESHAPE(val _OP_ obj1%val(:), obj1%s(1:2)), &
      & typeFEVariableVector, &
      & typeFEVariableSpace)
  END IF
!!
!!
!!
!!
CASE (time)
  !!
  IF( obj1%defineon .EQ. nodal ) THEN
    ans = NodalVariable( &
      & RESHAPE(val _OP_ obj1%val(:), obj1%s(1:2)), &
      & typeFEVariableVector, &
      & typeFEVariableTime)
  ELSE
    ans = QuadratureVariable( &
      & RESHAPE(val _OP_ obj1%val(:), obj1%s(1:2)), &
      & typeFEVariableVector, &
      & typeFEVariableTime)
  END IF
!!
!!
!!
!!
CASE (spacetime)
  !!
  IF( obj1%defineon .EQ. nodal ) THEN
    ans = NodalVariable(&
      & RESHAPE(val _OP_ obj1%val(:), obj1%s(1:3)), &
      & typeFEVariableVector, &
      & typeFEVariableSpaceTime)
  ELSE
    ans = QuadratureVariable(&
      & RESHAPE(val _OP_ obj1%val(:), obj1%s(1:3)), &
      & typeFEVariableVector, &
      & typeFEVariableSpaceTime)
  END IF
!!
!!
!!
!!
END SELECT
