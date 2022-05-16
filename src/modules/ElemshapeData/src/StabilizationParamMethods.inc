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

PUBLIC :: getUnitNormal
PUBLIC :: GetSUPGParam
PUBLIC :: GetHRGNParam
PUBLIC :: getSUGN3Param
PUBLIC :: GetHRQIParam
PUBLIC :: getSUGN3Param_Takizawa2018

!----------------------------------------------------------------------------
!                                                    getUnitNormal@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine can be used in SUPG formulation
!
!# Introduction
!
! This routine can be used in the SUPG formulation:
!
! $$
! \frac{\nabla \vert \phi \vert}{\Vert \nabla \vert \phi \vert \Vert}
! $$

INTERFACE
  MODULE PURE SUBROUTINE getUnitNormal_1(obj, R, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: R(:, :)
    !! unit vector defined over quadrature points
    REAL(DFP), INTENT(IN) :: val(:)
    !! spatial nodal values of scalar
  END SUBROUTINE getUnitNormal_1
END INTERFACE

INTERFACE getUnitNormal
  MODULE PROCEDURE getUnitNormal_1
END INTERFACE getUnitNormal

!----------------------------------------------------------------------------
!                                                    getUnitNormal@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine can be used in SUPG formulation
!
!# Introduction
!
! This routine can be used in the SUPG formulation, here
! val is spatial nodal values of a vector.
!
! $$
! {\bf r}=\frac{\nabla\Vert{\bf v}\Vert}{\left|\nabla\Vert{\bf v}\Vert\right|}
! $$

INTERFACE
  MODULE PURE SUBROUTINE getUnitNormal_2(obj, R, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: R(:, :)
    !! unit vector defined over quadrature points
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! spatial nodal values of velocity
  END SUBROUTINE getUnitNormal_2
END INTERFACE

INTERFACE getUnitNormal
  MODULE PROCEDURE getUnitNormal_2
END INTERFACE getUnitNormal

!----------------------------------------------------------------------------
!                                                    getUnitNormal@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine can be used in SUPG formulation
!
!# Introduction
!
! - This routine can be used in the SUPG formulation.
! - `val` is the [[FEVariable_]], it can be vector or scalar
!
! $$
! \frac{\nabla \vert \phi \vert}{\Vert \nabla \vert \phi \vert \Vert}
! $$
!
! $$
! {\bf r}=\frac{\nabla\Vert{\bf v}\Vert}{\left|\nabla\Vert{\bf v}\Vert\right|}
! $$
!


INTERFACE
  MODULE PURE SUBROUTINE getUnitNormal_3(obj, R, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: R(:, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    !! it can be scalar or vector fe variable
  END SUBROUTINE getUnitNormal_3
END INTERFACE

INTERFACE getUnitNormal
  MODULE PROCEDURE getUnitNormal_3
END INTERFACE getUnitNormal

!----------------------------------------------------------------------------
!                                                   GetHRGNParam@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the HRGN param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRGNParam1(obj, h, val, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
      !! obj can be an instance of [[STElemshapeData_]]
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: h( : )
      !! h is a scalar, and defined on quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
      !! val can be a vector or a scalar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
      !! currently, opt is not used, but it may be used in future
  END SUBROUTINE elemsd_GetHRGNParam1
END INTERFACE

INTERFACE GetHRGNParam
  MODULE PROCEDURE elemsd_GetHRGNParam1
END INTERFACE GetHRGNParam

!----------------------------------------------------------------------------
!                                                   GetHRGNParam@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the HRGN param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRGNParam2(obj, h, val, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
      !! obj can be an instance of [[STElemshapeData_]]
    TYPE(FEVariable_), INTENT(INOUT) :: h
      !! h is a scalar, and defined on quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
      !! val can be a vector or a scalar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
      !! currently, opt is not used, but it may be used in future
  END SUBROUTINE elemsd_GetHRGNParam2
END INTERFACE

INTERFACE GetHRGNParam
  MODULE PROCEDURE elemsd_GetHRGNParam2
END INTERFACE GetHRGNParam

!----------------------------------------------------------------------------
!                                                   GetHRGNParam@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the HRGN param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRGNParam3(obj, h, val, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
      !! Spacetime shape function data, obj(ipt) denotes data at
      !! ipt quadrature point in time domain
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: h( :, : )
      !! scalar variable, defined on space-time quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
      !! it can be a scalar, defined on space or space-time quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  END SUBROUTINE elemsd_GetHRGNParam3
END INTERFACE

INTERFACE GetHRGNParam
  MODULE PROCEDURE elemsd_GetHRGNParam3
END INTERFACE GetHRGNParam

!----------------------------------------------------------------------------
!                                                   GetHRGNParam@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the HRGN param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRGNParam4(obj, h, val, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
      !! Spacetime shape function data, obj(ipt) denotes data at
      !! ipt quadrature point in time domain
    TYPE(FEVariable_), INTENT(INOUT) :: h
      !! scalar variable, defined on space-time quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
      !! it can be a scalar, defined on space or space-time quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  END SUBROUTINE elemsd_GetHRGNParam4
END INTERFACE

INTERFACE GetHRGNParam
  MODULE PROCEDURE elemsd_GetHRGNParam4
END INTERFACE GetHRGNParam


!----------------------------------------------------------------------------
!                                                   GetHRQIParam@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the HRQI param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRQIParam1(obj, h, val, hmax, hmin, &
    & r, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
      !! obj can be an instance of [[STElemshapeData_]]
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: h( : )
      !! h is a scalar, and defined on quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
      !! val can be a vector or a scalar
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: hmax( : )
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: hmin( : )
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: r( :, : )
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
      !! currently, opt is not used, but it may be used in future
  END SUBROUTINE elemsd_GetHRQIParam1
END INTERFACE

INTERFACE GetHRQIParam
  MODULE PROCEDURE elemsd_GetHRQIParam1
END INTERFACE GetHRQIParam

!----------------------------------------------------------------------------
!                                                   GetHRQIParam@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the HRQI param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRQIParam2(obj, h, val, hmax, &
    & hmin, r, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
      !! obj can be an instance of [[STElemshapeData_]]
    TYPE(FEVariable_), INTENT(INOUT) :: h
      !! h is a scalar, and defined on quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
      !! val can be a vector or a scalar
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmax, hmin, r
      !! h is a scalar, and defined on quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
      !! currently, opt is not used, but it may be used in future
  END SUBROUTINE elemsd_GetHRQIParam2
END INTERFACE

INTERFACE GetHRQIParam
  MODULE PROCEDURE elemsd_GetHRQIParam2
END INTERFACE GetHRQIParam

!----------------------------------------------------------------------------
!                                                   GetHRQIParam@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the HRQI param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRQIParam3(obj, h, val, hmax, &
    & hmin, r, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
      !! Spacetime shape function data, obj(ipt) denotes data at
      !! ipt quadrature point in time domain
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: h( :, : )
      !! scalar variable, defined on space-time quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
      !! it can be a scalar, defined on space or space-time quadrature points
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: hmax( :, : )
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: hmin( :, : )
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: r( :, :, : )
      !! scalar variable, defined on space-time quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  END SUBROUTINE elemsd_GetHRQIParam3
END INTERFACE

INTERFACE GetHRQIParam
  MODULE PROCEDURE elemsd_GetHRQIParam3
END INTERFACE GetHRQIParam

!----------------------------------------------------------------------------
!                                                   GetHRQIParam@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the HRQI param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRQIParam4(obj, h, val, hmax, &
    & hmin, r, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
      !! Spacetime shape function data, obj(ipt) denotes data at
      !! ipt quadrature point in time domain
    TYPE(FEVariable_), INTENT(INOUT) :: h
      !! scalar variable, defined on space-time quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
      !! it can be a scalar, defined on space or space-time quadrature points
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmax, hmin, r
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  END SUBROUTINE elemsd_GetHRQIParam4
END INTERFACE

INTERFACE GetHRQIParam
  MODULE PROCEDURE elemsd_GetHRQIParam4
END INTERFACE GetHRQIParam

!----------------------------------------------------------------------------
!                                                   getSUGN3Param@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
! date: 3 May 2022
! update: 3 May 2022
! summary: Returns the SUGN3 param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_2(obj, tau, val, nu, h, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj( : )
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

!> authors: Vikas Sharma, Ph. D.
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
    REAL( DFP ), INTENT(IN) :: nu
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

!> authors: Vikas Sharma, Ph. D.
! date: 3 May 2022
! update: 3 May 2022
! summary: Returns the SUGN3 param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_4(obj, tau, val, nu, h, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj( : )
      !! obj can be an instance of [[STElemshapeData_]]
    TYPE(FEVariable_), INTENT(INOUT) :: tau
      !! tau-sugn3 is a scalar and defined on quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
      !! val can be a vector or a scalar
    REAL( DFP ), INTENT(IN) :: nu
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

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
! date: 3 May 2022
! update: 3 May 2022
! summary: Returns the SUGN3 param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_Takizawa2018_2(obj, tau, val, &
    & nu, h, hmax, hmin, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj( : )
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

!> authors: Vikas Sharma, Ph. D.
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
    REAL( DFP ), INTENT(IN) :: nu
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

!> authors: Vikas Sharma, Ph. D.
! date: 3 May 2022
! update: 3 May 2022
! summary: Returns the SUGN3 param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_Takizawa2018_4(obj, tau, val, &
    & nu, h, hmax, hmin, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj( : )
      !! obj can be an instance of [[STElemshapeData_]]
    TYPE(FEVariable_), INTENT(INOUT) :: tau
      !! tau-sugn3 is a scalar and defined on quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
      !! val can be a vector or a scalar
    REAL( DFP ), INTENT(IN) :: nu
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

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the SUPG param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUPGParam1(obj, tau, c, val, nu, k, &
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
    TYPE(FEVariable_), INTENT(IN) :: nu
    !! diffusivity
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: k
    !! permeability
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: phi
    !! porosity
    !! In case of NSE it should be mu/rho
    REAL(DFP), OPTIONAL, INTENT(IN) :: dt
    !! time step size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! futuristic options
  END SUBROUTINE elemsd_GetSUPGParam1
END INTERFACE

INTERFACE GetSUPGParam
  MODULE PROCEDURE elemsd_GetSUPGParam1
END INTERFACE GetSUPGParam

!----------------------------------------------------------------------------
!                                                   GetSUPGParam@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
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
    !! futuristic options
  END SUBROUTINE elemsd_GetSUPGParam2
END INTERFACE

INTERFACE GetSUPGParam
  MODULE PROCEDURE elemsd_GetSUPGParam2
END INTERFACE GetSUPGParam

!----------------------------------------------------------------------------
!                                                   GetSUPGParam@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
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
    REAL( DFP ), INTENT(IN) :: nu
    !! In case of NSE it should be mu/rho
    !! diffusivity
    REAL( DFP ), OPTIONAL, INTENT(IN) :: k
    !! permeability
    REAL( DFP ), OPTIONAL, INTENT(IN) :: phi
    !! porosity
    REAL(DFP), OPTIONAL, INTENT(IN) :: dt
    !! time step size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! futuristic options
  END SUBROUTINE elemsd_GetSUPGParam3
END INTERFACE

INTERFACE GetSUPGParam
  MODULE PROCEDURE elemsd_GetSUPGParam3
END INTERFACE GetSUPGParam

!----------------------------------------------------------------------------
!                                                   GetSUPGParam@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
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
    REAL( DFP ), INTENT(IN) :: nu
    !! kinematic diffusivity
    REAL( DFP ), OPTIONAL, INTENT(IN) :: k
    !! permeability
    !! Scalar FEVariable
    REAL( DFP ), OPTIONAL, INTENT(IN) :: phi
    !! porosity
    !! Scalar FEVariable
    REAL(DFP), OPTIONAL, INTENT(IN) :: dt
    !! time step size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! futuristic options
  END SUBROUTINE elemsd_GetSUPGParam4
END INTERFACE

INTERFACE GetSUPGParam
  MODULE PROCEDURE elemsd_GetSUPGParam4
END INTERFACE GetSUPGParam

