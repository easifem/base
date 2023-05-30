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

module ElemshapeData_HRQIParamMethods
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: GetHRQIParam

!----------------------------------------------------------------------------
!                                                   GetHRQIParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the HRQI param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRQIParam1(obj, h, val, hmax, hmin, &
    & r, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! obj can be an instance of [[STElemshapeData_]]
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: h(:)
    !! h is a scalar, and defined on quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! val can be a vector or a scalar
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: hmax(:)
    !! maximum directional length, size(hmax) = nips
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: hmin(:)
    !! minimum directional length, size(hmin) = nips
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: r(:, :)
    !! unit normal, shape(r) = (nsd, nips)
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

!> author: Vikas Sharma, Ph. D.
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

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the HRQI param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRQIParam3(obj, h, val, hmax, &
    & hmin, r, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
      !! Spacetime shape function data, obj(ipt) denotes data at
      !! ipt quadrature point in time domain
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: h(:, :)
      !! scalar variable, defined on space-time quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
      !! it can be a scalar, defined on space or space-time quadrature points
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: hmax(:, :)
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: hmin(:, :)
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: r(:, :, :)
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

!> author: Vikas Sharma, Ph. D.
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

end module ElemshapeData_HRQIParamMethods
