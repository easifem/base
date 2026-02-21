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

SUBMODULE(FEVariable_IOMethod) Methods
USE Display_Method, ONLY: Util_Display => Display
USE Display_Method, ONLY: ToString
USE BaseType, ONLY: TypeFEVariableConstant
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: TypeFEVariableTime
USE BaseType, ONLY: TypeFEVariableSpaceTime
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableVector
USE BaseType, ONLY: TypeFEVariableMatrix
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE SafeSizeUtility, ONLY: SafeSize
USE FEVariable_Method, ONLY: GET
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Display

CALL Util_Display(msg, unitno=unitno)

SELECT CASE (obj%rank)

CASE (fevaropt%scalar)

  CALL Util_Display("rank: 0 (Scalar)", unitno=unitno)

  SELECT CASE (obj%varType)
  CASE (fevaropt%constant)
    CALL Util_Display("varType: Constant", unitno=unitno)
    CALL Util_Display( &
      GET(obj, TypeFEVariableScalar, TypeFEVariableConstant), &
      'VALUE: ', unitno=unitno)

  CASE (fevaropt%space)
    CALL Util_Display("varType: Space", unitno=unitno)
    CALL Util_Display(GET(obj, TypeFEVariableScalar, TypeFEVariableSpace), &
                      'VALUE: ', unitno=unitno)

  CASE (fevaropt%time)
    CALL Util_Display("varType: Time", unitno=unitno)
    CALL Util_Display(GET(obj, TypeFEVariableScalar, TypeFEVariableTime), &
                      'VALUE: ', unitno=unitno)

  CASE (fevaropt%spacetime)
    CALL Util_Display("varType: Space & Time", unitno=unitno)
    CALL Util_Display( &
      GET(obj, TypeFEVariableScalar, TypeFEVariableSpaceTime), &
      'VALUE: ', unitno=unitno)

  CASE DEFAULT
    CALL Util_Display("varType: UNKNOWN", unitno=unitno)
  END SELECT

CASE (fevaropt%vector)

  CALL Util_Display("rank: 1 (Vector)", unitno=unitno)

  SELECT CASE (obj%varType)

  CASE (fevaropt%constant)
    CALL Util_Display("varType: Constant", unitno=unitno)
    CALL Util_Display( &
      GET(obj, TypeFEVariableVector, TypeFEVariableConstant), &
      'VALUE: ', unitno=unitno)

  CASE (fevaropt%space)
    CALL Util_Display("varType: Space", unitno=unitno)
    CALL Util_Display( &
      GET(obj, TypeFEVariableVector, TypeFEVariableSpace), &
      'VALUE: ', unitno=unitno)

  CASE (fevaropt%time)
    CALL Util_Display("varType: Time", unitno=unitno)
    CALL Util_Display( &
      GET(obj, TypeFEVariableVector, TypeFEVariableTime), &
      'VALUE: ', unitno=unitno)

  CASE (fevaropt%spacetime)
    CALL Util_Display("varType: Space & Time", unitno=unitno)
    CALL Util_Display( &
      GET(obj, TypeFEVariableVector, TypeFEVariableSpaceTime), &
      'VALUE: ', unitno=unitno)

  CASE DEFAULT
    CALL Util_Display("varType: UNKNOWN", unitno=unitno)

  END SELECT

CASE (fevaropt%matrix)

  CALL Util_Display("rank: 2 (Matrix)", unitno=unitno)

  SELECT CASE (obj%varType)

  CASE (fevaropt%constant)
    CALL Util_Display("varType: Constant", unitno=unitno)
    CALL Util_Display( &
      GET(obj, TypeFEVariableMatrix, TypeFEVariableConstant), &
      'VALUE: ', unitno=unitno)

  CASE (fevaropt%space)
    CALL Util_Display("varType: Space", unitno=unitno)
    CALL Util_Display( &
      GET(obj, TypeFEVariableMatrix, TypeFEVariableSpace), &
      'VALUE: ', unitno=unitno)

  CASE (fevaropt%time)
    CALL Util_Display("varType: Time", unitno=unitno)
    CALL Util_Display(GET(obj, TypeFEVariableMatrix, TypeFEVariableTime), &
                      'VALUE: ', unitno=unitno)

  CASE (fevaropt%spacetime)
    CALL Util_Display("varType: Space & Time", unitno=unitno)
    CALL Util_Display( &
      GET(obj, TypeFEVariableMatrix, TypeFEVariableSpaceTime), &
      'VALUE: ', unitno=unitno)

  CASE DEFAULT
    CALL Util_Display("varType: UNKNOWN", unitno=unitno)

  END SELECT

CASE DEFAULT
  CALL Util_Display("rank: UNKNOWN", unitno=unitno)

END SELECT

CALL Util_Display(obj%s, "s: ", unitno=unitno)
CALL Util_Display(obj%tshape, "tshape: ", unitno=unitno)

IF (obj%defineOn .EQ. fevaropt%nodal) THEN
  CALL Util_Display("defineOn: nodal", unitno=unitno)
ELSEIF (obj%defineOn .EQ. fevaropt%quadrature) THEN
  CALL Util_Display("defineOn: quadrature", unitno=unitno)
END IF

CALL Util_Display(obj%len, "len: ", unitno=unitno)
CALL Util_Display(obj%capacity, "capacity: ", unitno=unitno)
CALL Util_Display(obj%isInit, "isInit: ", unitno=unitno)
CALL Util_Display(SafeSize(obj%val), "Size of obj%val: ", unitno=unitno)

END PROCEDURE fevar_Display

END SUBMODULE Methods
