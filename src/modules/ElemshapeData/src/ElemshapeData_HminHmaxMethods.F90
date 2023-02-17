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

module ElemshapeData_HminHmaxMethods
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: GetHminHmax

!----------------------------------------------------------------------------
!                                                   GetHminHmax@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns Hmin and Hmax

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHminHmax1(obj, hmax, hmin)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! obj can be an instance of [[STElemshapeData_]]
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: hmax(:)
    !! maximum directional length, size(hmax) = nips
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: hmin(:)
    !! minimum directional length, size(hmin) = nips
  END SUBROUTINE elemsd_GetHminHmax1
END INTERFACE

INTERFACE GetHminHmax
  MODULE PROCEDURE elemsd_GetHminHmax1
END INTERFACE GetHminHmax

!----------------------------------------------------------------------------
!                                                   GetHminHmax@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns Hmin and Hmax

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHminHmax2(obj, hmax, hmin, G)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! obj can be an instance of [[STElemshapeData_]]
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: hmax(:)
    !! maximum directional length, size(hmax) = nips
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: hmin(:)
    !! minimum directional length, size(hmin) = nips
    REAL(DFP), INTENT(IN) :: G(:, :, :)
    !! shape(G) = [nsd, nsd, nips]
  END SUBROUTINE elemsd_GetHminHmax2
END INTERFACE

INTERFACE GetHminHmax
  MODULE PROCEDURE elemsd_GetHminHmax2
END INTERFACE GetHminHmax

!----------------------------------------------------------------------------
!                                                   GetHminHmax@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns Hmin and Hmax

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHminHmax3(obj, hmax, hmin)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! obj can be an instance of [[STElemshapeData_]]
    TYPE(FEVariable_), INTENT(INOUT) :: hmax
    !! maximum directional length, size(hmax) = nips
    TYPE(FEVariable_), INTENT(INOUT) :: hmin
    !! minimum directional length, size(hmin) = nips
  END SUBROUTINE elemsd_GetHminHmax3
END INTERFACE

INTERFACE GetHminHmax
  MODULE PROCEDURE elemsd_GetHminHmax3
END INTERFACE GetHminHmax

!----------------------------------------------------------------------------
!                                                   GetHminHmax@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns Hmin and Hmax

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHminHmax6(obj, hmax, hmin, G)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! obj can be an instance of [[STElemshapeData_]]
    TYPE(FEVariable_), INTENT(INOUT) :: hmax
    !! maximum directional length, size(hmax) = nips
    TYPE(FEVariable_), INTENT(INOUT) :: hmin
    !! minimum directional length, size(hmin) = nips
    REAL(DFP), INTENT(IN) :: G(:, :, :)
    !! shape=[nsd, nsd, nips]
  END SUBROUTINE elemsd_GetHminHmax6
END INTERFACE

INTERFACE GetHminHmax
  MODULE PROCEDURE elemsd_GetHminHmax6
END INTERFACE GetHminHmax

!----------------------------------------------------------------------------
!                                                   GetHminHmax@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the HRQI param

INTERFACE
  MODULE SUBROUTINE elemsd_GetHminHmax4(obj, hmax, hmin)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! Spacetime shape function data, obj(ipt) denotes data at
    !! ipt quadrature point in time domain
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: hmax(:, :)
    !! shape(hmax) = [nips, nipt]
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: hmin(:, :)
    !! shape(hmin) = [nips, nipt]
  END SUBROUTINE elemsd_GetHminHmax4
END INTERFACE

INTERFACE GetHminHmax
  MODULE PROCEDURE elemsd_GetHminHmax4
END INTERFACE GetHminHmax

!----------------------------------------------------------------------------
!                                                   GetHminHmax@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the HRQI param

INTERFACE
  MODULE SUBROUTINE elemsd_GetHminHmax7(obj, hmax, hmin, G)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! Spacetime shape function data, obj(ipt) denotes data at
    !! ipt quadrature point in time domain
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: hmax(:, :)
    !! shape(hmax) = [nips, nipt]
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: hmin(:, :)
    !! shape(hmin) = [nips, nipt]
    REAL(DFP), INTENT(IN) :: G(:, :, :, :)
    !! shape = [nsd, nsd, nips, nipt]
  END SUBROUTINE elemsd_GetHminHmax7
END INTERFACE

INTERFACE GetHminHmax
  MODULE PROCEDURE elemsd_GetHminHmax7
END INTERFACE GetHminHmax

!----------------------------------------------------------------------------
!                                                   GetHminHmax@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the HRQI param

INTERFACE
  MODULE SUBROUTINE elemsd_GetHminHmax5(obj, hmax, hmin)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! Spacetime shape function data, obj(ipt) denotes data at
    !! ipt quadrature point in time domain
    !! it can be a scalar, defined on space or space-time quadrature points
    TYPE(FEVariable_), INTENT(INOUT) :: hmax, hmin
    !! SpaceTime, Quadrature
  END SUBROUTINE elemsd_GetHminHmax5
END INTERFACE

INTERFACE GetHminHmax
  MODULE PROCEDURE elemsd_GetHminHmax5
END INTERFACE GetHminHmax

!----------------------------------------------------------------------------
!                                                   GetHminHmax@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! update: 2021-11-27
! summary: Returns the HRQI param

INTERFACE
  MODULE SUBROUTINE elemsd_GetHminHmax8(obj, hmax, hmin, G)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! Spacetime shape function data, obj(ipt) denotes data at
    !! ipt quadrature point in time domain
    !! it can be a scalar, defined on space or space-time quadrature points
    TYPE(FEVariable_), INTENT(INOUT) :: hmax, hmin
    !! SpaceTime, Quadrature
    REAL(DFP), INTENT(IN) :: G(:, :, :, :)
    !! shape = [nsd, nsd, nips, nipt]
  END SUBROUTINE elemsd_GetHminHmax8
END INTERFACE

INTERFACE GetHminHmax
  MODULE PROCEDURE elemsd_GetHminHmax8
END INTERFACE GetHminHmax

end module ElemshapeData_HminHmaxMethods
