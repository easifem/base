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

MODULE FEVariable_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

#include "./IOMethods.inc"
#include "./QuadratureConstructorMethods.inc"
#include "./NodalConstructorMethods.inc"
#include "./GetMethods.inc"
#include "./Addition.inc"
#include "./Subtraction.inc"
#include "./Multiplication.inc"
#include "./Division.inc"
#include "./Power.inc"
#include "./SQRT.inc"
#include "./ABS.inc"
#include "./DOT_PRODUCT.inc"
#include "./NORM2.inc"
#include "./EQUAL.inc"

!----------------------------------------------------------------------------
!                                                   Addition@OperatorMethods
!----------------------------------------------------------------------------

END MODULE FEVariable_Method
