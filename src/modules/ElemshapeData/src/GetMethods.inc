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
! This routine can be used in the SUPG formulation
!
!  $$\nabla \vert phi \vert / \vert (\nabla \vert phi \vert) \vert$$

INTERFACE
  MODULE PURE SUBROUTINE getUnitNormal_1(obj, R, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: R(:, :)
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
! \nabla \Vert mathbf{v}  \Vert / \Vert (\nabla \Vert mathbf{v} \Vert) \Vert
! $$

INTERFACE
  MODULE PURE SUBROUTINE getUnitNormal_2(obj, R, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: R(:, :)
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
! This routine can be used in the SUPG formulation, here
! val is spatial nodal values of a vector.
!
! $$
! \nabla \Vert mathbf{v}  \Vert / \Vert
! (\nabla \Vert mathbf{v} \Vert) \Vert
! $$

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
!                                                   GetSUPGParam@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the SUPG param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUPGParam(obj, tau, c, val, nu, dt, opt)
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
    !! In case of NSE it should be mu/rho
    REAL(DFP), OPTIONAL, INTENT(IN) :: dt
    !! time step size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! futuristic options
  END SUBROUTINE elemsd_GetSUPGParam
END INTERFACE

INTERFACE GetSUPGParam
  MODULE PROCEDURE elemsd_GetSUPGParam
END INTERFACE GetSUPGParam

!----------------------------------------------------------------------------
!                                                   GetSUPGParam@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the SUPG param

INTERFACE
  MODULE PURE SUBROUTINE stelemsd_GetSUPGParam(obj, tau, c, val, nu, dt, opt)
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
    !! Scalar FEVariable
    REAL(DFP), OPTIONAL, INTENT(IN) :: dt
    !! time step size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! futuristic options
  END SUBROUTINE stelemsd_GetSUPGParam
END INTERFACE

INTERFACE GetSUPGParam
  MODULE PROCEDURE stelemsd_GetSUPGParam
END INTERFACE GetSUPGParam
