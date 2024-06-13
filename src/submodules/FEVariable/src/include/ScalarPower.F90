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
!
!!
!! main
!!
SELECT CASE (obj%vartype)
!!
!!
!!
!!
CASE (constant)
  !!
  IF( obj%defineon .EQ. nodal ) THEN
    ans = NodalVariable( &
      & obj%val(1) ** n, &
      & typeFEVariableScalar, &
      & typeFEVariableConstant)
  ELSE
    ans = QuadratureVariable( &
      & obj%val(1) ** n, &
      & typeFEVariableScalar, &
      & typeFEVariableConstant)
  END IF
!!
!!
!!
!!
CASE (space)
  !!
  IF( obj%defineon .EQ. nodal ) THEN
    ans = NodalVariable(&
      & obj%val(:) ** n, &
      & typeFEVariableScalar, &
      & typeFEVariableSpace)
  ELSE
    ans = QuadratureVariable(&
      & obj%val(:) ** n, &
      & typeFEVariableScalar, &
      & typeFEVariableSpace)
  END IF
!!
!!
!!
!!
CASE (time)
  !!
  IF( obj%defineon .EQ. nodal ) THEN
    ans = NodalVariable( &
      & obj%val(:) ** n, &
      & typeFEVariableScalar, &
      & typeFEVariableTime)
  ELSE
    ans = QuadratureVariable( &
      & obj%val(:) ** n, &
      & typeFEVariableScalar, &
      & typeFEVariableTime)
  END IF
!!
!!
!!
!!
CASE (spacetime)
  !!
  IF( obj%defineon .EQ. nodal ) THEN
    ans = NodalVariable(&
      & RESHAPE(obj%val(:) ** n, obj%s(1:2)), &
      & typeFEVariableScalar, &
      & typeFEVariableSpaceTime)
  ELSE
    ans = QuadratureVariable(&
      & RESHAPE(obj%val(:) ** n, obj%s(1:2)), &
      & typeFEVariableScalar, &
      & typeFEVariableSpaceTime)
  END IF
!!
!!
!!
!!
END SELECT
