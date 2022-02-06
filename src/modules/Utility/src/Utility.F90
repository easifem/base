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

MODULE Utility
USE GlobalData
USE Display_Method
USE ErrorHandling
USE ISO_C_BINDING
IMPLICIT NONE

PRIVATE
INTEGER(I4B), PARAMETER :: NPAR_ARTH = 16, NPAR2_ARTH = 8
INTEGER(I4B), PARAMETER :: NPAR_GEOP = 4, NPAR2_GEOP = 2
INTEGER(I4B), PARAMETER :: NPAR_CUMSUM = 16
INTEGER(I4B), PARAMETER :: NPAR_CUMPROD = 8
INTEGER(I4B), PARAMETER :: NPAR_POLY = 8
INTEGER(I4B), PARAMETER :: NPAR_POLYTERM = 8

#include "AppendMethods.inc"
#include "ApproxMethods.inc"
#include "AssertMethods.inc"
#include "FunctionalFortran.inc"
#include "OnesMethods.inc"
#include "EyeMethods.inc"
#include "DiagMethods.inc"
#include "HashingMethods.inc"
#include "InputMethods.inc"
#include "InvMethods.inc"
#include "MatmulMethods.inc"
#include "ContrationMethods.inc"
#include "MiscMethods.inc"
#include "ProductMethods.inc"
#include "ReallocateMethods.inc"
#include "SortMethods.inc"
#include "StringMethods.inc"
#include "SwapMethods.inc"
#include "ConvertMethods.inc"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Utility
