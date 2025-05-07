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
SELECT CASE (obj%vartype)
CASE (constant)

  IF (obj%defineon .EQ. nodal) THEN
    ans = NodalVariable(_ELEM_METHOD_(obj%val(1)), typeFEVariableScalar, &
                        typeFEVariableConstant)
    RETURN
  END IF

  ans = QuadratureVariable(_ELEM_METHOD_(obj%val(1)), typeFEVariableScalar, &
                           typeFEVariableConstant)
CASE (space)

  IF (obj%defineon .EQ. nodal) THEN
    ans = NodalVariable(_ELEM_METHOD_(obj%val(1:obj%len)), &
                        typeFEVariableScalar, typeFEVariableSpace)
    RETURN
  END IF

  ans = QuadratureVariable(_ELEM_METHOD_(obj%val(1:obj%len)), &
                           typeFEVariableScalar, typeFEVariableSpace)

CASE (time)

  IF (obj%defineon .EQ. nodal) THEN
    ans = NodalVariable(_ELEM_METHOD_(obj%val(1:obj%len)), &
                        typeFEVariableScalar, typeFEVariableTime)
    RETURN
  END IF

  ans = QuadratureVariable(_ELEM_METHOD_(obj%val(1:obj%len)), &
                           typeFEVariableScalar, typeFEVariableTime)

CASE (spacetime)

  IF (obj%defineon .EQ. nodal) THEN
    ans = NodalVariable(_ELEM_METHOD_(obj%val(1:obj%len)), &
                    typeFEVariableScalar, typeFEVariableSpaceTime, obj%s(1:2))
    RETURN
  END IF

  ans = QuadratureVariable(_ELEM_METHOD_(obj%val(1:obj%len)), &
                    typeFEVariableScalar, typeFEVariableSpaceTime, obj%s(1:2))
END SELECT
