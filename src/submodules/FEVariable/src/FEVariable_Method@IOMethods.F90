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

SUBMODULE(FEVariable_Method) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Display
!!
!! main
!!
CALL Display(msg, unitno=unitno)
!!
SELECT CASE (obj%rank)
!!
!! rank: SCALAR
!!
CASE (SCALAR)
  CALL Display("# RANK :: 0 (SCALAR)", unitno=unitno)
  !!
  SELECT CASE (obj%vartype)
  CASE (CONSTANT)
    CALL Display("# VarType: CONSTANT", unitno=unitno)
    CALL Display(GET(obj, typeFEVariableScalar, typeFEVariableConstant), &
      & '# VALUE: ', unitno=unitno)
  !!
  CASE (SPACE)
    CALL Display("# VarType: SPACE", unitno=unitno)
    CALL Display(GET(obj, typeFEVariableScalar, typeFEVariableSpace), &
      & '# VALUE: ', unitno=unitno)
  !!
  CASE (TIME)
    CALL Display("# VarType: TIME", unitno=unitno)
    CALL Display(GET(obj, typeFEVariableScalar, typeFEVariableTime), &
      & '# VALUE: ', unitno=unitno)
  !!
  CASE (SPACETIME)
    CALL Display("# VarType: SPACE & TIME", unitno=unitno)
    CALL Display(GET(obj, typeFEVariableScalar, typeFEVariableSpaceTime), &
      & '# VALUE: ', unitno=unitno)
  END SELECT
!!
!! rank: VECTOR
!!
CASE (VECTOR)
  !!
  CALL Display("RANK :: 1 (VECTOR)", unitno=unitno)
  !!
  SELECT CASE (obj%vartype)
  CASE (CONSTANT)
    CALL Display("# VarType: CONSTANT", unitno=unitno)
    CALL Display(GET(obj, typeFEVariableVector, typeFEVariableConstant), &
      & '# VALUE: ', unitno=unitno)
  !!
  CASE (SPACE)
    CALL Display("# VarType: SPACE", unitno=unitno)
    CALL Display(GET(obj, typeFEVariableVector, typeFEVariableSpace), &
      & '# VALUE: ', unitno=unitno)
  !!
  CASE (TIME)
    CALL Display("# VarType: TIME", unitno=unitno)
    CALL Display(GET(obj, typeFEVariableVector, typeFEVariableTime), &
      & '# VALUE: ', unitno=unitno)
  !!
  CASE (SPACETIME)
    CALL Display("# VarType: SPACE & TIME", unitno=unitno)
    CALL Display(GET(obj, typeFEVariableVector, typeFEVariableSpaceTime), &
      & '# VALUE: ', unitno=unitno)
  END SELECT
!!
!! rank: MATRIX
!!
CASE (MATRIX)
  !!
  CALL Display("RANK :: 2 (MATRIX)", unitno=unitno)
  !!
  SELECT CASE (obj%vartype)
  CASE (CONSTANT)
    CALL Display("# VarType: CONSTANT", unitno=unitno)
    CALL Display(GET(obj, typeFEVariableMatrix, typeFEVariableConstant), &
      & '# VALUE: ', unitno=unitno)
  !!
  CASE (SPACE)
    CALL Display("# VarType: SPACE", unitno=unitno)
    CALL Display(GET(obj, typeFEVariableMatrix, typeFEVariableSpace), &
      & '# VALUE: ', unitno=unitno)
  !!
  CASE (TIME)
    CALL Display("# VarType: TIME", unitno=unitno)
    CALL Display(GET(obj, typeFEVariableMatrix, typeFEVariableTime), &
      & '# VALUE: ', unitno=unitno)
  !!
  CASE (SPACETIME)
    CALL Display("# VarType: SPACE & TIME", unitno=unitno)
    CALL Display(GET(obj, typeFEVariableMatrix, typeFEVariableSpaceTime), &
      & '# VALUE: ', unitno=unitno)
  END SELECT
END SELECT
!!
END PROCEDURE fevar_Display

END SUBMODULE IOMethods
