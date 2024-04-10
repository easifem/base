! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

MODULE is_i8_mod
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: wp => INT8
USE, NON_INTRINSIC :: test_base, ONLY: testline, tests
CONTAINS
INCLUDE "is_i.inc"
END MODULE is_i8_mod

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE is_i16_mod
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: wp => INT16
USE, NON_INTRINSIC :: test_base, ONLY: testline, tests
CONTAINS
INCLUDE "is_i.inc"
END MODULE is_i16_mod

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE is_i32_mod
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: wp => INT32
USE, NON_INTRINSIC :: test_base, ONLY: testline, tests
CONTAINS
INCLUDE "is_i.inc"
END MODULE is_i32_mod

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE is_i64_mod
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: wp => INT64
USE, NON_INTRINSIC :: test_base, ONLY: testline, tests
CONTAINS
INCLUDE "is_i.inc"
END MODULE is_i64_mod

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE is_i
USE is_i8_mod, ONLY: is_i8 => is
USE is_i16_mod, ONLY: is_i16 => is
USE is_i32_mod, ONLY: is_i32 => is
USE is_i64_mod, ONLY: is_i64 => is
INTERFACE is
  MODULE PROCEDURE is_i8, is_i16, is_i32, is_i64
END INTERFACE
END MODULE is_i

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE is_r32_mod
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: wp => REAL32
USE, NON_INTRINSIC :: test_base, ONLY: testline, tests
CONTAINS
INCLUDE "is_r.inc"
END MODULE is_r32_mod

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE is_r64_mod
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: wp => REAL64
USE, NON_INTRINSIC :: test_base, ONLY: testline, tests
CONTAINS
INCLUDE "is_r.inc"
END MODULE is_r64_mod

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE is_r128_mod
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: wp => REAL128
USE, NON_INTRINSIC :: test_base, ONLY: testline, tests
CONTAINS
INCLUDE "is_r.inc"
END MODULE is_r128_mod

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE is_r
USE is_r32_mod, ONLY: isrel_r32 => isrel, isabs_r32 => isabs, &
                        & isnear_r32 => isnear
USE is_r64_mod, ONLY: isrel_r64 => isrel, isabs_r64 => isabs, &
                        & isnear_r64 => isnear
USE is_r128_mod, ONLY: isrel_r128 => isrel, isabs_r128 => isabs, &
                        & isnear_r128 => isnear
INTERFACE isrel
  MODULE PROCEDURE isrel_r32, isrel_r64, isrel_r128
END INTERFACE

INTERFACE isabs
  MODULE PROCEDURE isabs_r32, isabs_r64, isabs_r128
END INTERFACE

INTERFACE isnear
  MODULE PROCEDURE isnear_r32, isnear_r64, isnear_r128
END INTERFACE

END MODULE is_r
