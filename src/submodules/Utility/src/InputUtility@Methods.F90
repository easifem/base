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

!> author: Vikas Sharma, Ph. D.
! date: 	3 April 2021
! summary: 	This method contains the input method

SUBMODULE(InputUtility) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Input
!----------------------------------------------------------------------------

MODULE PROCEDURE input_Int8
#include "./Input/Input1.inc"
END PROCEDURE input_Int8
MODULE PROCEDURE input_Int16
#include "./Input/Input1.inc"
END PROCEDURE input_Int16
MODULE PROCEDURE input_Int32
#include "./Input/Input1.inc"
END PROCEDURE input_Int32
MODULE PROCEDURE input_Int64
#include "./Input/Input1.inc"
END PROCEDURE input_Int64

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

MODULE PROCEDURE input_Real32
#include "./Input/Input1.inc"
END PROCEDURE input_Real32
MODULE PROCEDURE input_Real64
#include "./Input/Input1.inc"
END PROCEDURE input_Real64

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

MODULE PROCEDURE input_Int8Vec
#include "./Input/Input1.inc"
END PROCEDURE input_Int8Vec
MODULE PROCEDURE input_Int16Vec
#include "./Input/Input1.inc"
END PROCEDURE input_Int16Vec
MODULE PROCEDURE input_Int32Vec
#include "./Input/Input1.inc"
END PROCEDURE input_Int32Vec
MODULE PROCEDURE input_Int64Vec
#include "./Input/Input1.inc"
END PROCEDURE input_Int64Vec

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

MODULE PROCEDURE input_Real32vec
#include "./Input/Input1.inc"
END PROCEDURE input_Real32vec
MODULE PROCEDURE input_Real64vec
#include "./Input/Input1.inc"
END PROCEDURE input_Real64vec

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

MODULE PROCEDURE input_Int8Array
#include "./Input/Input1.inc"
END PROCEDURE input_Int8Array
MODULE PROCEDURE input_Int16Array
#include "./Input/Input1.inc"
END PROCEDURE input_Int16Array
MODULE PROCEDURE input_Int32Array
#include "./Input/Input1.inc"
END PROCEDURE input_Int32Array
MODULE PROCEDURE input_Int64Array
#include "./Input/Input1.inc"
END PROCEDURE input_Int64Array

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

MODULE PROCEDURE input_Real32Array
#include "./Input/Input1.inc"
END PROCEDURE input_Real32Array
MODULE PROCEDURE input_Real64Array
#include "./Input/Input1.inc"
END PROCEDURE input_Real64Array

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

MODULE PROCEDURE input_String
#include "./Input/Input1.inc"
END PROCEDURE input_String

! MODULE PROCEDURE input_StringVec
! #include "./Input/Input1.inc"
! END PROCEDURE input_StringVec

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

MODULE PROCEDURE input_logical
#include "./Input/Input1.inc"
END PROCEDURE input_logical

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

MODULE PROCEDURE input_logicalvec
#include "./Input/Input1.inc"
END PROCEDURE input_logicalvec

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

MODULE PROCEDURE input_logicalArray
#include "./Input/Input1.inc"
END PROCEDURE input_logicalArray

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
