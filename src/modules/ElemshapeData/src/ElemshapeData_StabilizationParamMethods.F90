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

module ElemshapeData_StabilizationParamMethods
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE
PUBLIC :: GetSUPGParam
PUBLIC :: getSUGN3Param
PUBLIC :: getSUGN3Param_Takizawa2018

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

!----------------------------------------------------------------------------
!                              getSUGN3Param_Takizawa2018@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the SUGN3 param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_Takizawa2018_1(obj, &
    & tau, val, nu, h, hmax, hmin, opt)
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
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmax
      !! scalar variable, defined on space-time quadrature points
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmin
      !! scalar variable, defined on space-time quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
      !! currently, opt is not used, but it may be used in future
  END SUBROUTINE elemsd_GetSUGN3Param_Takizawa2018_1
END INTERFACE

INTERFACE getSUGN3Param_Takizawa2018
  MODULE PROCEDURE elemsd_GetSUGN3Param_Takizawa2018_1
END INTERFACE getSUGN3Param_Takizawa2018

!----------------------------------------------------------------------------
!                                      getSUGN3Param_Takizawa2018@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 May 2022
! update: 3 May 2022
! summary: Returns the SUGN3 param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_Takizawa2018_2(obj, tau, val, &
    & nu, h, hmax, hmin, opt)
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
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmax
      !! scalar variable, defined on space-time quadrature points
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmin
      !! scalar variable, defined on space-time quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
      !! currently, opt is not used, but it may be used in future
  END SUBROUTINE elemsd_GetSUGN3Param_Takizawa2018_2
END INTERFACE

INTERFACE getSUGN3Param_Takizawa2018
  MODULE PROCEDURE elemsd_GetSUGN3Param_Takizawa2018_2
END INTERFACE getSUGN3Param_Takizawa2018

!----------------------------------------------------------------------------
!                                      getSUGN3Param_Takizawa2018@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the SUGN3 param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_Takizawa2018_3(obj, tau, val, &
    & nu, h, hmax, hmin, opt)
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
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmax
      !! scalar variable, defined on space-time quadrature points
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmin
      !! scalar variable, defined on space-time quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
      !! currently, opt is not used, but it may be used in future
  END SUBROUTINE elemsd_GetSUGN3Param_Takizawa2018_3
END INTERFACE

INTERFACE getSUGN3Param_Takizawa2018
  MODULE PROCEDURE elemsd_GetSUGN3Param_Takizawa2018_3
END INTERFACE getSUGN3Param_Takizawa2018

!----------------------------------------------------------------------------
!                                      getSUGN3Param_Takizawa2018@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 May 2022
! update: 3 May 2022
! summary: Returns the SUGN3 param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_Takizawa2018_4(obj, tau, val, &
    & nu, h, hmax, hmin, opt)
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
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmax
      !! scalar variable, defined on space-time quadrature points
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmin
      !! scalar variable, defined on space-time quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
      !! currently, opt is not used, but it may be used in future
  END SUBROUTINE elemsd_GetSUGN3Param_Takizawa2018_4
END INTERFACE

INTERFACE getSUGN3Param_Takizawa2018
  MODULE PROCEDURE elemsd_GetSUGN3Param_Takizawa2018_4
END INTERFACE getSUGN3Param_Takizawa2018

!----------------------------------------------------------------------------
!                                                   GetSUPGParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the SUPG param
!
!# Introduction
!
!- `tau` stabilization parameter, instance of [[FEVariable_]],
!- defined on quadrature, changes in space
!- `c` convective velocity, instance of [[FEVariable_]], vector, defined on
!- nodes or quadrature points
!- `val` instance of [[FEVariable_]], can be vector or scalar, defined on
!- nodes or quadrature points
!- `nu` instance of [[FEVariable_]], scalar, defined on nodes or quadrature
!- `k` instance of [[FEVariable_]], scalar, optional, defined on nodes/
!- quadrature points
!- `phi`, porosity, [[FEVariable_]], scalar, optional, defined on nodes/quads
!-

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUPGParam1(obj, tau, c, val, nu, k, &
    & phi, dt, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! It can be an instance of ElemshapeData_ or STElemshapeData_
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! Stabilization parameter, [[FEVariable_]], Defined on Quadrature points
    TYPE(FEVariable_), INTENT(IN) :: c
    !! Convective velocity => Vector variable
    TYPE(FEVariable_), INTENT(IN) :: val
    !! solution, it can be scalar or vector variable
    TYPE(FEVariable_), INTENT(IN) :: nu
    !! diffusivity
    !! In case of NSE it should be mu/rho
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: k
    !! permeability
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: phi
    !! porosity
    REAL(DFP), OPTIONAL, INTENT(IN) :: dt
    !! time step size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! default=1
    !! opt=1, then `ans(ii) = SQRT(1.0_DFP / (t12**2 + t3**2 + t4**2))`
    !! opt=2, then `ans(ii) = 1.0_DFP / (t12 + t3 + t4)`
  END SUBROUTINE elemsd_GetSUPGParam1
END INTERFACE

INTERFACE GetSUPGParam
  MODULE PROCEDURE elemsd_GetSUPGParam1
END INTERFACE GetSUPGParam

!----------------------------------------------------------------------------
!                                                   GetSUPGParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the SUPG param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUPGParam2(obj, tau, c, val, nu, k, &
    & phi, dt, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! Stabilization parameter
    !! Quadrature type
    !! SpaceTime
    TYPE(FEVariable_), INTENT(IN) :: c
    !! convective velocity
    !! vector FEVariable
    TYPE(FEVariable_), INTENT(IN) :: val
    !! solution
    !! scalar or vector FEVariable
    TYPE(FEVariable_), INTENT(IN) :: nu
    !! kinematic diffusivity
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: k
    !! permeability
    !! Scalar FEVariable
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: phi
    !! porosity
    !! Scalar FEVariable
    REAL(DFP), OPTIONAL, INTENT(IN) :: dt
    !! time step size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! default=1
    !! opt=1, then `ans(ii) = SQRT(1.0_DFP / (t12**2 + t3**2 + t4**2))`
    !! opt=2, then `ans(ii) = 1.0_DFP / (t12 + t3 + t4)`
  END SUBROUTINE elemsd_GetSUPGParam2
END INTERFACE

INTERFACE GetSUPGParam
  MODULE PROCEDURE elemsd_GetSUPGParam2
END INTERFACE GetSUPGParam

!----------------------------------------------------------------------------
!                                                   GetSUPGParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the SUPG param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUPGParam3(obj, tau, c, val, nu, k, &
    & phi, dt, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! It can be an instance of ElemshapeData_ or STElemshapeData_
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! Stabilization parameter
    !! Quadrature FEVariable
    !! varType=Space
    TYPE(FEVariable_), INTENT(IN) :: c
    !! Convective velocity => Vector variable
    TYPE(FEVariable_), INTENT(IN) :: val
    !! solution, it can be scalar or vector variable
    REAL(DFP), INTENT(IN) :: nu
    !! In case of NSE it should be mu/rho
    !! diffusivity
    REAL(DFP), OPTIONAL, INTENT(IN) :: k
    !! permeability
    REAL(DFP), OPTIONAL, INTENT(IN) :: phi
    !! porosity
    REAL(DFP), OPTIONAL, INTENT(IN) :: dt
    !! time step size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! default=1
    !! opt=1, then `ans(ii) = SQRT(1.0_DFP / (t12**2 + t3**2 + t4**2))`
    !! opt=2, then `ans(ii) = 1.0_DFP / (t12 + t3 + t4)`
  END SUBROUTINE elemsd_GetSUPGParam3
END INTERFACE

INTERFACE GetSUPGParam
  MODULE PROCEDURE elemsd_GetSUPGParam3
END INTERFACE GetSUPGParam

!----------------------------------------------------------------------------
!                                                   GetSUPGParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the SUPG param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUPGParam4(obj, tau, c, val, nu, k, &
    & phi, dt, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! Stabilization parameter
    !! Quadrature type
    !! SpaceTime
    TYPE(FEVariable_), INTENT(IN) :: c
    !! convective velocity
    !! vector FEVariable
    TYPE(FEVariable_), INTENT(IN) :: val
    !! solution
    !! scalar or vector FEVariable
    REAL(DFP), INTENT(IN) :: nu
    !! kinematic diffusivity
    REAL(DFP), OPTIONAL, INTENT(IN) :: k
    !! permeability
    !! Scalar FEVariable
    REAL(DFP), OPTIONAL, INTENT(IN) :: phi
    !! porosity
    !! Scalar FEVariable
    REAL(DFP), OPTIONAL, INTENT(IN) :: dt
    !! time step size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! default=1
    !! opt=1, then `ans(ii) = SQRT(1.0_DFP / (t12**2 + t3**2 + t4**2))`
    !! opt=2, then `ans(ii) = 1.0_DFP / (t12 + t3 + t4)`
  END SUBROUTINE elemsd_GetSUPGParam4
END INTERFACE

INTERFACE GetSUPGParam
  MODULE PROCEDURE elemsd_GetSUPGParam4
END INTERFACE GetSUPGParam

end module ElemshapeData_StabilizationParamMethods
