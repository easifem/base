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

SUBMODULE(FEVariable_Method) PowerMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Power
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_power
SELECT CASE (obj%rank)
!!
CASE (SCALAR)
#include "./include/ScalarPower.F90"
!!
CASE (VECTOR)
#include "./include/VectorPower.F90"
!!
CASE (MATRIX)
#include "./include/MatrixPower.F90"
!!
END SELECT
!!
END PROCEDURE fevar_power

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE PowerMethods
