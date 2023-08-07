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
! This code is taken from
!
! https://github.com/fortran-lang/stdlib/blob/master/src/stdlib_hash_32bit.fypp
!
! `FNV_1_HASH` and  `FNV_1A_Hash` are translations to Fortran 2008 of the `FNV-1` 
! and `FNV-1a` hash functions of Glenn Fowler, Landon Curt Noll, and Phong Vo, that 
! has been released into the public domain. 

MODULE Hashing32

USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: CHARACTER_STORAGE_SIZE
USE GlobalData, ONLY: I4B, LGT, dp, INT8, INT16, INT32, INT64
USE GlobalData, ONLY: BITS_INT8 => BIInt8, BITS_INT16 => BIInt16, &
  & BITS_INT32 => BIInt32, BITS_INT64 => BIInt64, &
  & BYTES_INT8 => BYInt8, &
  & BYTES_INT16 => BYInt16, &
  & BYTES_INT32 => BYInt32, &
  & BYTES_INT64 => BYInt64
IMPLICIT NONE
PRIVATE

INTEGER(I4B), PARAMETER, PUBLIC :: INT_HASH = INT32
!! The number of bits in the output hash
INTEGER(I4B), PARAMETER :: POW32_OVER_PHI = INT(z'9E3779B9', INT32)
!! pow32_over_phi is the odd integer that most closely approximates
!! 2**32/phi, where phi is the golden ratio 1.618...
INTEGER(I4B), PARAMETER :: BITS_CHAR = CHARACTER_STORAGE_SIZE
INTEGER(I4B), PARAMETER :: BYTES_CHAR = BITS_CHAR / BITS_INT8

! Dealing with different endians
LOGICAL(LGT), PARAMETER, PUBLIC :: little_endian = &
  & (1 == TRANSFER([1_INT8, 0_INT8], 0_INT16))

PUBLIC :: fibonacci_hash, odd_random_integer, universal_mult_hash
PUBLIC :: fnv_1_hash
PUBLIC :: fnv_1a_hash
PUBLIC :: nmhash32
PUBLIC :: nmhash32x
PUBLIC :: new_water_hash_seed
PUBLIC :: water_hash
PUBLIC :: new_nmhash32_seed
PUBLIC :: new_nmhash32x_seed

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 July 2022
! summary: FNV_1 hash function for rank 1 array keys of integers

INTERFACE fnv_1_hash
  MODULE PURE FUNCTION Int8_fnv_1(key) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: key(:)
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int8_fnv_1

  MODULE PURE FUNCTION Int16_fnv_1(key) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: key(:)
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int16_fnv_1

  MODULE PURE FUNCTION Int32_fnv_1(key) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: key(:)
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int32_fnv_1

  MODULE PURE FUNCTION Int64_fnv_1(key) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: key(:)
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int64_fnv_1

  MODULE PURE FUNCTION Char_fnv_1(key) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT_HASH) :: ans
  END FUNCTION Char_fnv_1
END INTERFACE fnv_1_hash

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE fnv_1a_hash
  MODULE PURE FUNCTION Int8_fnv_1a(key) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: key(:)
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int8_fnv_1a

  MODULE PURE FUNCTION Int16_fnv_1a(key) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: key(:)
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int16_fnv_1a

  MODULE PURE FUNCTION Int32_fnv_1a(key) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: key(:)
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int32_fnv_1a

  MODULE PURE FUNCTION Int64_fnv_1a(key) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: key(:)
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int64_fnv_1a

  MODULE PURE FUNCTION Char_fnv_1a(key) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT_HASH) :: ans
  END FUNCTION Char_fnv_1a
END INTERFACE fnv_1a_hash

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE nmhash32
  MODULE PURE FUNCTION Int8_nmhash32(key, seed) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: key(0:)
    INTEGER(INT32), INTENT(IN) :: seed
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int8_nmhash32
  MODULE PURE FUNCTION Int16_nmhash32(key, seed) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: key(0:)
    INTEGER(INT32), INTENT(IN) :: seed
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int16_nmhash32
  MODULE PURE FUNCTION Int32_nmhash32(key, seed) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: key(0:)
    INTEGER(INT32), INTENT(IN) :: seed
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int32_nmhash32
  MODULE PURE FUNCTION Int64_nmhash32(key, seed) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: key(0:)
    INTEGER(INT32), INTENT(IN) :: seed
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int64_nmhash32
  MODULE PURE FUNCTION Char_nmhash32(key, seed) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT32), INTENT(IN) :: seed
    INTEGER(INT_HASH) :: ans
  END FUNCTION Char_nmhash32
END INTERFACE nmhash32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE nmhash32x
  MODULE PURE FUNCTION Int8_nmhash32x(key, seed) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: key(0:)
    INTEGER(INT32), INTENT(IN) :: seed
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int8_nmhash32x
  MODULE PURE FUNCTION Int16_nmhash32x(key, seed) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: key(0:)
    INTEGER(INT32), INTENT(IN) :: seed
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int16_nmhash32x
  MODULE PURE FUNCTION Int32_nmhash32x(key, seed) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: key(0:)
    INTEGER(INT32), INTENT(IN) :: seed
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int32_nmhash32x
  MODULE PURE FUNCTION Int64_nmhash32x(key, seed) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: key(0:)
    INTEGER(INT32), INTENT(IN) :: seed
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int64_nmhash32x
  MODULE PURE FUNCTION Char_nmhash32x(key, seed) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT32), INTENT(IN) :: seed
    INTEGER(INT_HASH) :: ans
  END FUNCTION Char_nmhash32x
END INTERFACE nmhash32x

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE water_hash
  MODULE PURE FUNCTION Int8_water_hash(key, seed) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: key(0:)
    INTEGER(INT64), INTENT(IN) :: seed
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int8_water_hash
  MODULE PURE FUNCTION Int16_water_hash(key, seed) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: key(0:)
    INTEGER(INT64), INTENT(IN) :: seed
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int16_water_hash
  MODULE PURE FUNCTION Int32_water_hash(key, seed) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: key(0:)
    INTEGER(INT64), INTENT(IN) :: seed
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int32_water_hash
  MODULE PURE FUNCTION Int64_water_hash(key, seed) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: key(0:)
    INTEGER(INT64), INTENT(IN) :: seed
    INTEGER(INT_HASH) :: ans
  END FUNCTION Int64_water_hash
  MODULE PURE FUNCTION Char_water_hash(key, seed) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT64), INTENT(IN) :: seed
    INTEGER(INT_HASH) :: ans
  END FUNCTION Char_water_hash
END INTERFACE water_hash

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE new_water_hash_seed(seed)
    INTEGER(INT64), INTENT(INOUT) :: seed
  END SUBROUTINE new_water_hash_seed
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE new_nmhash32_seed(seed)
    INTEGER(INT_HASH), INTENT(INOUT) :: seed
  END SUBROUTINE new_nmhash32_seed
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE new_nmhash32x_seed(seed)
    INTEGER(INT_HASH), INTENT(INOUT) :: seed
  END SUBROUTINE new_nmhash32x_seed
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!> author: Vikas Sharma, Ph. D.
! date: 25 July 2023
! summary: Maps the 32 bit integer `key` to an unsigned integer value with
! only `nbits` bits where `nbits` is less than 32

ELEMENTAL FUNCTION fibonacci_hash(key, nbits) RESULT(sample)
  INTEGER(INT32), INTENT(IN) :: key
  INTEGER, INTENT(IN) :: nbits
  INTEGER(INT32) :: sample
  sample = ISHFT(key * pow32_over_phi, -32 + nbits)
END FUNCTION fibonacci_hash

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 July 2022
! summary: Universal mult hash
!
!# Introduction
!
! Uses the "random" odd 32 bit integer `seed` to map the 32 bit integer
! `key` to an unsigned integer value with only `nbits` bits where `nbits` is
! less than 32

ELEMENTAL FUNCTION universal_mult_hash(key, seed, nbits) RESULT(sample)
  INTEGER(INT32), INTENT(IN) :: key
  INTEGER(INT32), INTENT(IN) :: seed
  INTEGER, INTENT(IN) :: nbits
  INTEGER(INT32) :: sample
  sample = ISHFT(key * seed, -32 + nbits)
END FUNCTION universal_mult_hash

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 July 2022
! summary:
!
!# Introduction
!
! Returns a 32 bit pseudo random integer, `harvest`, distributed uniformly
! over the odd integers of the `Int32` kind.

SUBROUTINE odd_random_integer(harvest)
  INTEGER(INT32), INTENT(OUT) :: harvest
  REAL(dp) :: sample
  CALL RANDOM_NUMBER(sample)
  harvest = INT(FLOOR(sample * 2_INT64**32, INT64) - 2_INT64**31, &
    & INT32)
  harvest = ISHFT(harvest, 1) + 1_INT32
END SUBROUTINE odd_random_integer

END MODULE Hashing32
