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

PUBLIC :: getNormal

!----------------------------------------------------------------------------
!                                                                 GetNormal
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Jan 2022
! update: 28 Jan 2022
! summary: This routine returns the normal vector stored in [[ElemShapeData_]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getNormal_1(obj, normal, nsd)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: normal(:, :)
    !! normal(1:3, 1:nip) = obj%normal
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nsd
  END SUBROUTINE elemsd_getNormal_1
END INTERFACE

INTERFACE getNormal
  MODULE PROCEDURE elemsd_getNormal_1
END INTERFACE getNormal

!----------------------------------------------------------------------------
!                                                                 GetNormal
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Jan 2022
! update: 28 Jan 2022
! summary: This routine returns the normal vector stored in [[ElemShapeData_]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getNormal_2(obj, normal, nsd)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: normal
    !! normal(1:3, 1:nip) = obj%normal
    !! Quadrature, Vector, Space
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nsd
  END SUBROUTINE elemsd_getNormal_2
END INTERFACE

INTERFACE getNormal
  MODULE PROCEDURE elemsd_getNormal_2
END INTERFACE getNormal

!----------------------------------------------------------------------------
!                                                                 GetNormal
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Jan 2022
! update: 28 Jan 2022
! summary: This routine returns the normal vector stored in [[ElemShapeData_]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getNormal_3(obj, normal, nsd)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    TYPE(FEVariable_), INTENT(INOUT) :: normal
    !! normal(1:3, 1:nip) = obj%normal
    !! Quadrature, Vector, SpaceTime
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nsd
  END SUBROUTINE elemsd_getNormal_3
END INTERFACE

INTERFACE getNormal
  MODULE PROCEDURE elemsd_getNormal_3
END INTERFACE getNormal
