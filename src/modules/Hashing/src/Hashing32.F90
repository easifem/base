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

MODULE Hashing32

USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: CHARACTER_STORAGE_SIZE
USE GlobalData, ONLY: I4B, LGT, dp, Int8, Int16, Int32, Int64
USE GlobalData, ONLY: BITS_INT8 => BIInt8, BITS_INT16=>BIInt16, &
  & BITS_INT32 => BIInt32, BITS_INT64 => BIInt64, &
  & BYTES_INT8 => BYInt8, &
  & BYTES_INT16 => BYInt16, &
  & BYTES_INT32 => BYInt32, &
  & BYTES_INT64 => BYInt64
IMPLICIT NONE
PRIVATE

INTEGER( I4B ), PARAMETER, PUBLIC :: INT_HASH = Int32
!! The number of bits in the output hash
INTEGER( I4B ), PARAMETER :: POW32_OVER_PHI = int(z'9E3779B9', Int32)
!! pow32_over_phi is the odd integer that most closely approximates
!! 2**32/phi, where phi is the golden ratio 1.618...
INTEGER( I4B ), PARAMETER :: BITS_CHAR = CHARACTER_STORAGE_SIZE
INTEGER( I4B ), PARAMETER :: BYTES_CHAR = BITS_CHAR / BITS_INT8

! Dealing with different endians
LOGICAL( LGT ), PARAMETER, PUBLIC :: little_endian = &
  & (1 == TRANSFER([1_Int8, 0_Int8], 0_Int16))

PUBLIC :: fibonacci_hash, odd_random_integer, universal_mult_hash

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2022
! summary: FNV_1 hash function for rank 1 array keys of integers

INTERFACE
MODULE PURE FUNCTION Int8_fnv_1( key ) RESULT( ans )
  INTEGER( Int8 ), INTENT( IN ) :: key( : )
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int8_fnv_1

MODULE PURE FUNCTION Int16_fnv_1( key ) RESULT( ans )
  INTEGER( Int16 ), INTENT( IN ) :: key( : )
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int16_fnv_1

MODULE PURE FUNCTION Int32_fnv_1( key ) RESULT( ans )
  INTEGER( Int32 ), INTENT( IN ) :: key( : )
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int32_fnv_1

MODULE PURE FUNCTION Int64_fnv_1( key ) RESULT( ans )
  INTEGER( Int64 ), INTENT( IN ) :: key( : )
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int64_fnv_1

MODULE PURE FUNCTION Char_fnv_1( key ) RESULT( ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: key
  INTEGER( INT_HASH ) :: ans
END FUNCTION Char_fnv_1
END INTERFACE

INTERFACE fnv_1_hash
  MODULE PROCEDURE Int8_fnv_1, Int16_fnv_1, Int32_fnv_1, &
    & Int64_fnv_1, Char_fnv_1
END INTERFACE fnv_1_hash

PUBLIC :: fnv_1_hash

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


INTERFACE
MODULE PURE FUNCTION Int8_fnv_1a( key ) RESULT( ans )
  INTEGER( Int8 ), INTENT( IN ) :: key( : )
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int8_fnv_1a

MODULE PURE FUNCTION Int16_fnv_1a( key ) RESULT( ans )
  INTEGER( Int16 ), INTENT( IN ) :: key( : )
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int16_fnv_1a

MODULE PURE FUNCTION Int32_fnv_1a( key ) RESULT( ans )
  INTEGER( Int32 ), INTENT( IN ) :: key( : )
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int32_fnv_1a

MODULE PURE FUNCTION Int64_fnv_1a( key ) RESULT( ans )
  INTEGER( Int64 ), INTENT( IN ) :: key( : )
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int64_fnv_1a

MODULE PURE FUNCTION Char_fnv_1a( key ) RESULT( ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: key
  INTEGER( INT_HASH ) :: ans
END FUNCTION Char_fnv_1a
END INTERFACE

INTERFACE fnv_1a_hash
  MODULE PROCEDURE Int8_fnv_1a, Int16_fnv_1a, Int32_fnv_1a, &
    & Int64_fnv_1a, Char_fnv_1a
END INTERFACE fnv_1a_hash

PUBLIC :: fnv_1a_hash

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Int8_nmhash32( key, seed ) RESULT( ans )
  INTEGER( Int8 ), INTENT( IN ) :: key(0:)
  INTEGER( Int32 ), INTENT( IN ) :: seed
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int8_nmhash32
MODULE PURE FUNCTION Int16_nmhash32( key, seed ) RESULT( ans )
  INTEGER( Int16 ), INTENT( IN ) :: key(0:)
  INTEGER( Int32 ), INTENT( IN ) :: seed
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int16_nmhash32
MODULE PURE FUNCTION Int32_nmhash32( key, seed ) RESULT( ans )
  INTEGER( Int32 ), INTENT( IN ) :: key(0:)
  INTEGER( Int32 ), INTENT( IN ) :: seed
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int32_nmhash32
MODULE PURE FUNCTION Int64_nmhash32( key, seed ) RESULT( ans )
  INTEGER( Int64 ), INTENT( IN ) :: key(0:)
  INTEGER( Int32 ), INTENT( IN ) :: seed
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int64_nmhash32
MODULE PURE FUNCTION Char_nmhash32( key, seed ) RESULT( ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: key
  INTEGER( Int32 ), INTENT( IN ) :: seed
  INTEGER( INT_HASH ) :: ans
END FUNCTION Char_nmhash32
END INTERFACE

INTERFACE nmhash32
  MODULE PROCEDURE Int8_nmhash32, Int16_nmhash32, Int32_nmhash32, &
    & Int64_nmhash32, Char_nmhash32
END INTERFACE nmhash32

PUBLIC :: nmhash32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Int8_nmhash32x( key, seed ) RESULT( ans )
  INTEGER( Int8 ), INTENT( IN ) :: key(0:)
  INTEGER( Int32 ), INTENT( IN ) :: seed
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int8_nmhash32x
MODULE PURE FUNCTION Int16_nmhash32x( key, seed ) RESULT( ans )
  INTEGER( Int16 ), INTENT( IN ) :: key(0:)
  INTEGER( Int32 ), INTENT( IN ) :: seed
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int16_nmhash32x
MODULE PURE FUNCTION Int32_nmhash32x( key, seed ) RESULT( ans )
  INTEGER( Int32 ), INTENT( IN ) :: key(0:)
  INTEGER( Int32 ), INTENT( IN ) :: seed
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int32_nmhash32x
MODULE PURE FUNCTION Int64_nmhash32x( key, seed ) RESULT( ans )
  INTEGER( Int64 ), INTENT( IN ) :: key(0:)
  INTEGER( Int32 ), INTENT( IN ) :: seed
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int64_nmhash32x
MODULE PURE FUNCTION Char_nmhash32x( key, seed ) RESULT( ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: key
  INTEGER( Int32 ), INTENT( IN ) :: seed
  INTEGER( INT_HASH ) :: ans
END FUNCTION Char_nmhash32x
END INTERFACE

INTERFACE nmhash32x
  MODULE PROCEDURE Int8_nmhash32x, Int16_nmhash32x, Int32_nmhash32x, &
    & Int64_nmhash32x, Char_nmhash32x
END INTERFACE nmhash32x

PUBLIC :: nmhash32x

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Int8_water_hash( key, seed ) RESULT( ans )
  INTEGER( Int8 ), INTENT( IN ) :: key(0:)
  INTEGER( Int64 ), INTENT( IN ) :: seed
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int8_water_hash
MODULE PURE FUNCTION Int16_water_hash( key, seed ) RESULT( ans )
  INTEGER( Int16 ), INTENT( IN ) :: key(0:)
  INTEGER( Int64 ), INTENT( IN ) :: seed
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int16_water_hash
MODULE PURE FUNCTION Int32_water_hash( key, seed ) RESULT( ans )
  INTEGER( Int32 ), INTENT( IN ) :: key(0:)
  INTEGER( Int64 ), INTENT( IN ) :: seed
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int32_water_hash
MODULE PURE FUNCTION Int64_water_hash( key, seed ) RESULT( ans )
  INTEGER( Int64 ), INTENT( IN ) :: key(0:)
  INTEGER( Int64 ), INTENT( IN ) :: seed
  INTEGER( INT_HASH ) :: ans
END FUNCTION Int64_water_hash
MODULE PURE FUNCTION Char_water_hash( key, seed ) RESULT( ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: key
  INTEGER( Int64 ), INTENT( IN ) :: seed
  INTEGER( INT_HASH ) :: ans
END FUNCTION Char_water_hash
END INTERFACE

INTERFACE water_hash
  MODULE PROCEDURE Int8_water_hash, Int16_water_hash, Int32_water_hash, &
    & Int64_water_hash, Char_water_hash
END INTERFACE water_hash

PUBLIC :: water_hash

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE new_water_hash_seed(seed)
    INTEGER(Int64), INTENT(INOUT) :: seed
  END SUBROUTINE new_water_hash_seed
END INTERFACE

PUBLIC :: new_water_hash_seed

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE new_nmhash32_seed(seed)
    INTEGER(INT_HASH), INTENT(INOUT) :: seed
  END SUBROUTINE new_nmhash32_seed
END INTERFACE

PUBLIC :: new_nmhash32_seed

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE new_nmhash32x_seed(seed)
    INTEGER(INT_HASH), INTENT(INOUT) :: seed
  END SUBROUTINE new_nmhash32x_seed
END INTERFACE

PUBLIC :: new_nmhash32x_seed

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2023
! summary: Maps the 32 bit integer `key` to an unsigned integer value with
! only `nbits` bits where `nbits` is less than 32

ELEMENTAL FUNCTION fibonacci_hash(key, nbits) result(sample)
  INTEGER(Int32), INTENT(IN) :: key
  INTEGER, INTENT(IN) :: nbits
  INTEGER(Int32) :: sample
  sample = ISHFT(key * pow32_over_phi, -32 + nbits)
END FUNCTION fibonacci_hash

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2022
! summary: Universal mult hash
!
!# Introduction
!
! Uses the "random" odd 32 bit integer `seed` to map the 32 bit integer
! `key` to an unsigned integer value with only `nbits` bits where `nbits` is
! less than 32

ELEMENTAL FUNCTION universal_mult_hash(key, seed, nbits) result(sample)
  INTEGER(Int32), INTENT(IN) :: key
  INTEGER(Int32), INTENT(IN) :: seed
  INTEGER, INTENT(IN) :: nbits
  INTEGER(Int32) :: sample
  sample = ishft(key * seed, -32 + nbits)
END FUNCTION universal_mult_hash

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2022
! summary:
!
!# Introduction
!
! Returns a 32 bit pseudo random integer, `harvest`, distributed uniformly
! over the odd integers of the `Int32` kind.

SUBROUTINE odd_random_integer(harvest)
  INTEGER(Int32), INTENT(OUT) :: harvest
  REAL(dp) :: sample
  CALL RANDOM_NUMBER(sample)
  harvest = INT(FLOOR(sample * 2_Int64**32, Int64) - 2_Int64**31, &
    & Int32)
  harvest = ISHFT(harvest, 1) + 1_Int32
END SUBROUTINE odd_random_integer

end module Hashing32
