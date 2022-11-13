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

SUBMODULE(ElemshapeData_Method) SUGN3ParamMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              getSUGN3Param
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSUGN3Param_1
REAL(DFP), ALLOCATABLE :: h0(:), nubar(:)
INTEGER(I4B) :: ii
!!
CALL GetHRGNParam(obj=obj, h=h0, val=val, opt=opt)
!!
IF (PRESENT(h)) THEN
  h = QuadratureVariable(h0, TypeFEVariableScalar, &
    & TypeFEVariableSpace)
END IF
!!
CALL GetInterpolation(obj=obj, val=nu, interpol=nubar)
!!
DO ii = 1, SIZE(h0)
  h0(ii) = h0(ii)**2 / nubar(ii)
END DO
!!
tau = QuadratureVariable(h0, TypeFEVariableScalar, &
  & TypeFEVariableSpace)
!!
DEALLOCATE (h0, nubar)
END PROCEDURE elemsd_GetSUGN3Param_1

!----------------------------------------------------------------------------
!                                                             getSUGN3Param
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSUGN3Param_2
  !!
INTEGER(I4B) :: ii
REAL(DFP), ALLOCATABLE :: h0(:, :), nubar(:, :)
  !!
  !! main
  !!
CALL GetHRGNParam(obj=obj, h=h0, val=val, opt=opt)
  !!
IF (PRESENT(h)) THEN
  h = QuadratureVariable( &
    & h0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
END IF
  !!
CALL GetInterpolation(obj=obj, val=nu, interpol=nubar)
  !!
DO ii = 1, SIZE(obj)
  h0(:, ii) = h0(:, ii)**2 / nubar(:, ii)
END DO
  !!
tau = QuadratureVariable( &
  & h0, &
  & TypeFEVariableScalar, &
  & TypeFEVariableSpaceTime)
  !!
IF (ALLOCATED(h0)) DEALLOCATE (h0)
IF (ALLOCATED(nubar)) DEALLOCATE (nubar)
  !!
END PROCEDURE elemsd_GetSUGN3Param_2

!----------------------------------------------------------------------------
!                                                              getSUGN3Param
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSUGN3Param_3
  !!
REAL(DFP), ALLOCATABLE :: h0(:)
INTEGER(I4B) :: ii
  !!
CALL GetHRGNParam(obj=obj, h=h0, val=val, opt=opt)
  !!
IF (PRESENT(h)) THEN
  h = QuadratureVariable(h0, TypeFEVariableScalar, &
    & TypeFEVariableSpace)
END IF
  !!
DO ii = 1, SIZE(h0)
  h0(ii) = h0(ii)**2 / nu
END DO
  !!
tau = QuadratureVariable(h0, TypeFEVariableScalar, &
  & TypeFEVariableSpace)
  !!
DEALLOCATE (h0)
  !!
END PROCEDURE elemsd_GetSUGN3Param_3

!----------------------------------------------------------------------------
!                                                             getSUGN3Param
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetSUGN3Param_4
  !!
INTEGER(I4B) :: ii
REAL(DFP), ALLOCATABLE :: h0(:, :)
  !!
  !! main
  !!
CALL GetHRGNParam(obj=obj, h=h0, val=val, opt=opt)
  !!
IF (PRESENT(h)) THEN
  h = QuadratureVariable( &
    & h0, &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
END IF
  !!
DO ii = 1, SIZE(obj)
  h0(:, ii) = h0(:, ii)**2 / nu
END DO
  !!
tau = QuadratureVariable( &
  & h0, &
  & TypeFEVariableScalar, &
  & TypeFEVariableSpaceTime)
  !!
IF (ALLOCATED(h0)) DEALLOCATE (h0)
  !!
END PROCEDURE elemsd_GetSUGN3Param_4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SUGN3ParamMethods
