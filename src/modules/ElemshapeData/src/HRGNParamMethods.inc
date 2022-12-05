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

PUBLIC :: GetHRGNParam

!----------------------------------------------------------------------------
!                                                   GetHRGNParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the HRGN param
!
!# Introduction
!
! In this method `h` is oneD real-vector defined at quadrature points

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRGNParam1(obj, h, val, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
      !! obj can be an instance of [[STElemshapeData_]]
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: h(:)
      !! h is a scalar field and defined on quadrature points
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

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the HRGN param
!
!# Introduction
!
!- This routine is same as `elemsd_GetHRGNParam1`
!- Here, `h` is an [[FEVariable_]]  (scalar and quadVariable)
!- This routine calls `elemsd_GetHRGNParam1` and then convert
!- the result in to [[FEVariable_]].

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

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the HRGN param

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRGNParam3(obj, h, val, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
      !! Spacetime shape function data, obj(ipt) denotes data at
      !! ipt quadrature point in time domain
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: h(:, :)
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

!> author: Vikas Sharma, Ph. D.
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

