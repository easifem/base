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

PUBLIC :: getSUGN3Param

!----------------------------------------------------------------------------
!                                                   getSUGN3Param@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the SUGN3 param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_1(obj, tau, val, nu, h, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
      !! obj can be an instance of [[STElemshapeData_]]
    TYPE(FEVariable_), INTENT(INOUT) :: tau
      !! tau-sugn3 is a scalar and defined on quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
      !! val can be a vector or a scalar
    TYPE(FEVariable_), INTENT(IN) :: nu
      !! kinematic viscosity or diffusivity
      !! scalar and defined on quadrature points
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: h
      !! scalar variable, defined on space-time quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
      !! currently, opt is not used, but it may be used in future
  END SUBROUTINE elemsd_GetSUGN3Param_1
END INTERFACE

INTERFACE getSUGN3Param
  MODULE PROCEDURE elemsd_GetSUGN3Param_1
END INTERFACE getSUGN3Param

!----------------------------------------------------------------------------
!                                                   getSUGN3Param@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 May 2022
! update: 3 May 2022
! summary: Returns the SUGN3 param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_2(obj, tau, val, nu, h, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
      !! obj can be an instance of [[STElemshapeData_]]
    TYPE(FEVariable_), INTENT(INOUT) :: tau
      !! tau-sugn3 is a scalar and defined on quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
      !! val can be a vector or a scalar
    TYPE(FEVariable_), INTENT(IN) :: nu
      !! kinematic viscosity or diffusivity
      !! scalar and defined on quadrature points
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: h
      !! scalar variable, defined on space-time quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
      !! currently, opt is not used, but it may be used in future
  END SUBROUTINE elemsd_GetSUGN3Param_2
END INTERFACE

INTERFACE getSUGN3Param
  MODULE PROCEDURE elemsd_GetSUGN3Param_2
END INTERFACE getSUGN3Param

!----------------------------------------------------------------------------
!                                                   getSUGN3Param@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the SUGN3 param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_3(obj, tau, val, nu, h, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
      !! obj can be an instance of [[STElemshapeData_]]
    TYPE(FEVariable_), INTENT(INOUT) :: tau
      !! tau-sugn3 is a scalar and defined on quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
      !! val can be a vector or a scalar
    REAL(DFP), INTENT(IN) :: nu
      !! kinematic viscosity or diffusivity
      !! scalar and defined on quadrature points
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: h
      !! scalar variable, defined on space-time quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
      !! currently, opt is not used, but it may be used in future
  END SUBROUTINE elemsd_GetSUGN3Param_3
END INTERFACE

INTERFACE getSUGN3Param
  MODULE PROCEDURE elemsd_GetSUGN3Param_3
END INTERFACE getSUGN3Param

!----------------------------------------------------------------------------
!                                                   getSUGN3Param@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 May 2022
! update: 3 May 2022
! summary: Returns the SUGN3 param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_4(obj, tau, val, nu, h, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
      !! obj can be an instance of [[STElemshapeData_]]
    TYPE(FEVariable_), INTENT(INOUT) :: tau
      !! tau-sugn3 is a scalar and defined on quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
      !! val can be a vector or a scalar
    REAL(DFP), INTENT(IN) :: nu
      !! kinematic viscosity or diffusivity
      !! scalar and defined on quadrature points
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: h
      !! scalar variable, defined on space-time quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
      !! currently, opt is not used, but it may be used in future
  END SUBROUTINE elemsd_GetSUGN3Param_4
END INTERFACE

INTERFACE getSUGN3Param
  MODULE PROCEDURE elemsd_GetSUGN3Param_4
END INTERFACE getSUGN3Param

