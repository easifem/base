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
!
! `FNV_1_HASH` and  `FNV_1A_Hash` are translations to Fortran 2008 of the
! `FNV-1` and `FNV-1a` hash functions of Glenn Fowler, Landon Curt Noll,
! and Phong Vo, that has been released into the public domain. Permission
! has been granted, by Landon Curt Noll, for the use of these algorithms
! in the Fortran Standard Library. A description of these functions is
! available at https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function.

SUBMODULE(Hashing32) fnvMethods
IMPLICIT NONE
INTEGER(INT_HASH), PARAMETER :: OFFSET_BASIS = INT(z'811C9DC5', INT_HASH)
INTEGER(INT_HASH), PARAMETER :: PRIME = INT(z'01000193', INT_HASH)
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Int8_fnv_1
  INTEGER(Int64) :: ii
  !!
  ans = OFFSET_BASIS
  !!
  DO ii = 1_int64, SIZE(key, kind=int64)
  ans = ans * prime
  IF (little_endian) THEN
      ans = IEOR(ans, &
      TRANSFER([key(ii), 0_int8, 0_int8, 0_int8], &
      & 0_int_hash))
  ELSE
      ans = IEOR(ans, &
      TRANSFER([0_int8, 0_int8, 0_int8, key(ii)], &
      & 0_int_hash))
  END IF
  END DO
END PROCEDURE Int8_fnv_1

MODULE PROCEDURE Int16_fnv_1
  ans = Int8_fnv_1(TRANSFER(key, 0_int8, &
    & BYTES_INT16 * SIZE(key, kind=Int64)))
END PROCEDURE Int16_fnv_1

MODULE PROCEDURE Int32_fnv_1
  ans = Int8_fnv_1(TRANSFER(key, 0_int8, &
    & BYTES_INT32 * SIZE(key, kind=Int64)))
END PROCEDURE Int32_fnv_1

MODULE PROCEDURE Int64_fnv_1
  ans = Int8_fnv_1(TRANSFER(key, 0_int8, &
    & BYTES_INT64 * SIZE(key, kind=Int64)))
END PROCEDURE Int64_fnv_1

MODULE PROCEDURE Char_fnv_1
  ans = Int8_fnv_1(TRANSFER(key, 0_int8, &
    & BYTES_CHAR * LEN(key, kind=Int64)))
END PROCEDURE Char_fnv_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Int8_fnv_1a
  INTEGER( Int64 ) :: ii
  !!
  ans = OFFSET_BASIS
  !!
  DO ii = 1_Int64, SIZE(key, kind=int64)
    IF(little_endian) THEN
      ans = IEOR(ans, TRANSFER([key(ii), 0_Int8, 0_Int8, 0_Int8], &
        & 0_Int_hash))
    ELSE
      ans = IEOR(ans, &
        & TRANSFER([0_Int8, 0_Int8, 0_Int8, key(ii)], &
        & 0_int_hash))
    END IF
    ans = ans * prime
  END DO
  !!
END PROCEDURE Int8_fnv_1a

MODULE PROCEDURE Int16_fnv_1a
  ans = Int8_fnv_1a(TRANSFER(key, 0_Int8, &
    & BYTES_INT16 * SIZE(key, kind=Int64)))
END PROCEDURE Int16_fnv_1a

MODULE PROCEDURE Int32_fnv_1a
  ans = Int8_fnv_1a(TRANSFER(key, 0_Int8, &
    & BYTES_INT32 * SIZE(key, kind=Int64)))
END PROCEDURE Int32_fnv_1a

MODULE PROCEDURE Int64_fnv_1a
  ans = Int8_fnv_1a(TRANSFER(key, 0_Int8, &
    & BYTES_INT64 * SIZE(key, kind=Int64)))
END PROCEDURE Int64_fnv_1a

MODULE PROCEDURE Char_fnv_1a
  ans = Int8_fnv_1a(TRANSFER(key, 0_Int8, &
    & BYTES_CHAR * LEN(key, kind=Int64)))
END PROCEDURE Char_fnv_1a

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE fnvMethods
