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

PUBLIC :: GetDBEStabParam

!----------------------------------------------------------------------------
!                                    GetDBEStabParam@STDBEStabParamMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the stabilization parameter for STDBE kernels
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

INTERFACE
  MODULE SUBROUTINE elemsd_GetDBEStabParam1(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, opt, L0, c4, dt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! It can be an instance of ElemshapeData_ or STElemshapeData_
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! Stabilization parameter, [[FEVariable_]], Defined on Quadrature points
    TYPE(FEVariable_), INTENT(INOUT) :: h
    !! Stabilization parameter, [[FEVariable_]], Defined on Quadrature points
    TYPE(FEVariable_), INTENT(INOUT) :: nulsic
    !! Stabilization parameter, [[FEVariable_]], Defined on Quadrature points
    TYPE(FEVariable_), INTENT(IN) :: c
    !! Convective velocity => Vector variable
    TYPE(FEVariable_), INTENT(IN) :: val
    !! solution, it can be scalar or vector variable
    TYPE(FEVariable_), INTENT(IN) :: nu
    !! diffusivity
    !! In case of NSE it should be mu/rho
    TYPE(FEVariable_), INTENT(IN) :: k
    !! permeability
    TYPE(FEVariable_), INTENT(IN) :: phi
    !! porosity
    INTEGER(I4B), INTENT(IN) :: opt
    !! option for stabilization parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: L0
    !! Characteristic length
    REAL(DFP), OPTIONAL, INTENT(IN) :: c4
    !!
    REAL(DFP), OPTIONAL, INTENT(IN) :: dt
    !! time step size
  END SUBROUTINE elemsd_GetDBEStabParam1
END INTERFACE

INTERFACE GetDBEStabParam
  MODULE PROCEDURE elemsd_GetDBEStabParam1
END INTERFACE GetDBEStabParam

!----------------------------------------------------------------------------
!                                    GetDBEStabParam@STDBEStabParamMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the SUPG param

INTERFACE
  MODULE SUBROUTINE elemsd_GetDBEStabParam2(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, opt, L0, c4, dt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! Stabilization parameter
    !! Quadrature type
    !! SpaceTime
    TYPE(FEVariable_), INTENT(INOUT) :: h
    !! Stabilization parameter
    !! Quadrature type
    !! SpaceTime
    TYPE(FEVariable_), INTENT(INOUT) :: nulsic
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
    TYPE(FEVariable_), INTENT(IN) :: k
    !! permeability
    !! Scalar FEVariable
    TYPE(FEVariable_), INTENT(IN) :: phi
    !! porosity
    !! Scalar FEVariable
    INTEGER(I4B), INTENT(IN) :: opt
    !! 1-8, 11-18
    REAL(DFP), OPTIONAL, INTENT(IN) :: L0
    !! Characteristic length
    REAL(DFP), OPTIONAL, INTENT(IN) :: c4
    !! Characteristic length
    REAL(DFP), OPTIONAL, INTENT(IN) :: dt
    !! time step size
  END SUBROUTINE elemsd_GetDBEStabParam2
END INTERFACE

INTERFACE GetDBEStabParam
  MODULE PROCEDURE elemsd_GetDBEStabParam2
END INTERFACE GetDBEStabParam

!----------------------------------------------------------------------------
!                                    GetDBEStabParam@STDBEStabParamMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the SUPG param

INTERFACE
  MODULE SUBROUTINE elemsd_GetDBEStabParam3(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, opt, L0, c4, dt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! It can be an instance of ElemshapeData_ or STElemshapeData_
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! Stabilization parameter
    TYPE(FEVariable_), INTENT(INOUT) :: h
    !! Stabilization parameter
    TYPE(FEVariable_), INTENT(INOUT) :: nulsic
    !! Stabilization parameter
    TYPE(FEVariable_), INTENT(IN) :: c
    !! Convective velocity => Vector variable
    TYPE(FEVariable_), INTENT(IN) :: val
    !! solution, it can be scalar or vector variable
    REAL(DFP), INTENT(IN) :: nu
    !! In case of NSE it should be mu/rho
    !! diffusivity
    REAL(DFP), INTENT(IN) :: k
    !! permeability
    REAL(DFP), INTENT(IN) :: phi
    !! porosity
    INTEGER(I4B), INTENT(IN) :: opt
    !! option for stabilization parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: L0
    !! Characteristic length
    REAL(DFP), OPTIONAL, INTENT(IN) :: c4
    !!
    REAL(DFP), OPTIONAL, INTENT(IN) :: dt
    !! time step size
  END SUBROUTINE elemsd_GetDBEStabParam3
END INTERFACE

INTERFACE GetDBEStabParam
  MODULE PROCEDURE elemsd_GetDBEStabParam3
END INTERFACE GetDBEStabParam

!----------------------------------------------------------------------------
!                                    GetDBEStabParam@STDBEStabParamMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the SUPG param

INTERFACE
  MODULE SUBROUTINE elemsd_GetDBEStabParam4(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, opt, L0, c4, dt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! Space-time shape data
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! Stabilization parameter
    TYPE(FEVariable_), INTENT(INOUT) :: h
    !! Direction dependent element length
    TYPE(FEVariable_), INTENT(INOUT) :: nulsic
    !! Stabilization parameter
    TYPE(FEVariable_), INTENT(IN) :: c
    !! Convective velocity
    TYPE(FEVariable_), INTENT(IN) :: val
    !! Solution
    REAL(DFP), INTENT(IN) :: nu
    !! Kinematic diffusivity
    REAL(DFP), INTENT(IN) :: k
    !! Permeability
    REAL(DFP), INTENT(IN) :: phi
    !! Porosity
    INTEGER(I4B), INTENT(IN) :: opt
    !! Option
    REAL(DFP), OPTIONAL, INTENT(IN) :: L0
    !! Characteristic length
    REAL(DFP), OPTIONAL, INTENT(IN) :: c4
    !!
    REAL(DFP), OPTIONAL, INTENT(IN) :: dt
    !! Time step size
  END SUBROUTINE elemsd_GetDBEStabParam4
END INTERFACE

INTERFACE GetDBEStabParam
  MODULE PROCEDURE elemsd_GetDBEStabParam4
END INTERFACE GetDBEStabParam

