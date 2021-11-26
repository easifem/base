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
! date: 6 March 2021
! summary: This module contains method to construct finite element matrices

MODULE FEMatrix_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

#include "./MassMatrix.inc"
#include "./DiffusionMatrix.inc"
#include "./StiffnessMatrix.inc"
#include "./NitscheMatrix.inc"
#include "./ConvectiveMatrix.inc"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEMatrix_Method
