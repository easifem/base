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

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: This module contains methods for [[RealMatrix_]] data type

MODULE RealMatrix_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

#include "./IOMethods.inc"
#include "./ConstructorMethods.inc"
#include "./GetValuesMethods.inc"
#include "./SetValuesMethods.inc"
#include "./MatmulMethods.inc"
#include "./LapackMethods.inc"
#include "./IterativeSolverMethods.inc"

END MODULE RealMatrix_Method