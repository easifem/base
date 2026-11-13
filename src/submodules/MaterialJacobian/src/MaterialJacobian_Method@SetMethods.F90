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
! along with this program.  If not, see <https://www.gnu.org/licenses/>

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Setters of [[MaterialJacobian_]]

SUBMODULE(MaterialJacobian_Method) SetMethods
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                            SetMeasureTypes
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_SetMeasureTypes
IF (PRESENT(stressType)) obj%stressType = stressType
IF (PRESENT(strainType)) obj%strainType = strainType
IF (PRESENT(rateType)) obj%rateType = rateType
END PROCEDURE MaterialJacobian_SetMeasureTypes

END SUBMODULE SetMethods
