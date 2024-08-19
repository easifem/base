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

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 22 July 2022
! summary: PENF string-to-number (and viceversa) facility.

MODULE PENF_STRINGIFY
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: stderr => ERROR_UNIT
USE PENF_B_SIZE
USE PENF_GLOBAL_PARAMETERS_VARIABLES
IMPLICIT NONE
PRIVATE
SAVE
PUBLIC :: STR_ASCII, STR_UCS4
PUBLIC :: STR, STRZ, CTON
PUBLIC :: BSTR, BCTON

!----------------------------------------------------------------------------
!                                                                 STR_ASCII
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 22 July 2022
! summary: Convert string of any kind to ASCII string.

INTERFACE STR_ASCII
  MODULE PROCEDURE str_ascii_default
#if defined _ASCII_SUPPORTED && defined _ASCII_NEQ_DEFAULT
  MODULE PROCEDURE str_ascii_ascii
#endif
#ifdef _UCS4_SUPPORTED
  MODULE PROCEDURE STR_ASCII_UCS4
#endif
END INTERFACE STR_ASCII

!----------------------------------------------------------------------------
!                                                                 STR_UCS4
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 22 July 2022
! summary:         Convert string of any kind to UCS4 string.

INTERFACE STR_UCS4
  MODULE PROCEDURE str_ucs4_default
#if defined _ASCII_SUPPORTED && defined _ASCII_NEQ_DEFAULT
  MODULE PROCEDURE str_ucs4_ascii
#endif
#ifdef _UCS4_SUPPORTED
  MODULE PROCEDURE str_ucs4_ucs4
#endif
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      STR
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 22 July 2022
! summary: Convert number (real and integer) to string (number to string type
! casting).

INTERFACE STR
  MODULE PROCEDURE &
    & strf_R8P, str_R8P, &
    & strf_R4P, str_R4P, &
    & strf_I8P, str_I8P, &
    & strf_I4P, str_I4P, &
    & strf_I2P, str_I2P, &
    & strf_I1P, str_I1P, &
    & str_bol, &
    & str_a_R8P, &
    & str_a_R4P, &
    & str_a_I8P, &
    & str_a_I4P, &
    & str_a_I2P, &
    & str_a_I1P
#ifdef _R16P
  MODULE PROCEDURE strf_R16P, str_R16P, str_a_R16P
#endif
END INTERFACE STR

!----------------------------------------------------------------------------
!                                                                 STRZ
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 22 July 2022
! summary: Convert integer, to string, prefixing with the right number of
! zeros (integer to string type casting with zero padding).

INTERFACE STRZ
  MODULE PROCEDURE strz_I8P, strz_I4P, strz_I2P, strz_I1P
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     CTON
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 22 July 2022
! summary: Convert string to number (real and integer, string to number type
! casting).

INTERFACE CTON
  MODULE PROCEDURE &
    ctor_R8P, &
    ctor_R4P, &
    ctoi_I8P, &
    ctoi_I4P, &
    ctoi_I2P, &
    ctoi_I1P
#if defined _R16P
  MODULE PROCEDURE ctor_R16P
#endif
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      BSTR
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 22 July 2022
! summary:         Convert number (real and integer) to bit-string (number to
! bit-string type casting).

INTERFACE BSTR
  MODULE PROCEDURE            &
    & bstr_R8P,  &
    & bstr_R4P,  &
    & bstr_I8P,  &
    & bstr_I4P,  &
    & bstr_I2P,  &
    & bstr_I1P

#if defined _R16P
  MODULE PROCEDURE bstr_R16P
#endif
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     BCTON
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 22 July 2022
! summary: Convert bit-string to number (real and integer, bit-string to
! number type casting).
INTERFACE BCTON
  MODULE PROCEDURE             &
    & bctor_R8P,  &
    & bctor_R4P,  &
    & bctoi_I8P,  &
    & bctoi_I4P,  &
    & bctoi_I2P,  &
    & bctoi_I1P
#if defined _R16P
  MODULE PROCEDURE bctor_R16P
#endif

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

#include "./STR_ASCII.inc"
#include "./STR_UCS4.inc"
#include "./STR.inc"
#include "./COMPACT_REAL_STRING.inc"
#include "./STRZ.inc"
#include "./CTOA.inc"
#include "./BSTR.inc"
#include "./BCTON.inc"

ENDMODULE PENF_STRINGIFY
