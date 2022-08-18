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

SUBMODULE(MdEncode_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                   MdEncode_Method@Methods
!----------------------------------------------------------------------------

MODULE PROCEDURE MdEncode_1
  !!
  SELECT TYPE( val )
  TYPE IS( REAL(Real32) )
    ans = TOSTRING( val )
  TYPE IS( REAL(Real64) )
    ans = TOSTRING( val )
  TYPE IS( INTEGER( Int8 ) )
    ans = TOSTRING( val )
  TYPE IS( INTEGER( Int16 ) )
    ans = TOSTRING( val )
  TYPE IS( INTEGER( Int32 ) )
    ans = TOSTRING( val )
  TYPE IS( INTEGER( Int64 ) )
    ans = TOSTRING( val )
  TYPE IS( CHARACTER( LEN = * ) )
    ans = TRIM( val )
  TYPE IS( String )
    ans = TRIM( val )
  END SELECT
  !!
END PROCEDURE MdEncode_1
!----------------------------------------------------------------------------
!                                                   MdEncode_Method@Methods
!----------------------------------------------------------------------------

MODULE PROCEDURE MdEncode_2
  INTEGER( I4B ) :: ii, n
  !!
  n = SIZE( val )
  ans = " | "
  DO ii = 1, n
    ans = ans // " | "
  END DO
  ans = ans // CHAR_LF
  !!
  ans = ans // " | "
  DO ii = 1, n
    ans = ans // " --- | "
  END DO
  ans = ans // CHAR_LF
  !!
  SELECT TYPE( val )
  TYPE IS( REAL(Real32) )
#include "./inc/MdEncode_2.inc"
  TYPE IS( REAL(Real64) )
#include "./inc/MdEncode_2.inc"
  TYPE IS( INTEGER( Int8 ) )
#include "./inc/MdEncode_2.inc"
  TYPE IS( INTEGER( Int16 ) )
#include "./inc/MdEncode_2.inc"
  TYPE IS( INTEGER( Int32 ) )
#include "./inc/MdEncode_2.inc"
  TYPE IS( INTEGER( Int64 ) )
#include "./inc/MdEncode_2.inc"
  TYPE IS( CHARACTER( LEN = * ) )
    ans = ans // " | "
    DO ii = 1, n
      ans = ans // TRIM( val( ii ) ) // " | "
    END DO
    ans = ans // CHAR_LF
  TYPE IS( String )
    ans = ans // " | "
    DO ii = 1, n
      ans = ans // TRIM( val( ii ) ) // " | "
    END DO
    ans = ans // CHAR_LF
  END SELECT
  !!
END PROCEDURE MdEncode_2

!----------------------------------------------------------------------------
!                                                   MdEncode_Method@Methods
!----------------------------------------------------------------------------

MODULE PROCEDURE MdEncode_3
  INTEGER( I4B ) :: ii, jj,  m, n
  !!
  m = SIZE( val, 1 )
  n = SIZE( val, 2 )
  ans = " | "
  DO ii = 1, n
    ans = ans // " | "
  END DO
  ans = ans // CHAR_LF
  !!
  ans = ans // " | "
  DO ii = 1, n
    ans = ans // " --- | "
  END DO
  ans = ans // CHAR_LF
  !!
  SELECT TYPE( val )
  TYPE IS( REAL(Real32) )
#include "./inc/MdEncode_3.inc"
  TYPE IS( REAL(Real64) )
#include "./inc/MdEncode_3.inc"
  TYPE IS( INTEGER( Int8 ) )
#include "./inc/MdEncode_3.inc"
  TYPE IS( INTEGER( Int16 ) )
#include "./inc/MdEncode_3.inc"
  TYPE IS( INTEGER( Int32 ) )
#include "./inc/MdEncode_3.inc"
  TYPE IS( INTEGER( Int64 ) )
#include "./inc/MdEncode_3.inc"
  TYPE IS( CHARACTER( LEN = * ) )
#include "./inc/MdEncode_3b.inc"
  TYPE IS( String )
#include "./inc/MdEncode_3b.inc"
  END SELECT
  !!
END PROCEDURE MdEncode_3


END SUBMODULE Methods