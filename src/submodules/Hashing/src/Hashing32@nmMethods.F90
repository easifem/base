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
! date: 25 July 2021
! summary:
!
!# Introduction
!
! Reference: https://github.com/fortran-lang/stdlib/tree/master/src
!
! `NM_HASH32` and `NM_HASH32X` are translations to Fortran 2008 and signed
! two's complement arithmetic of the `nmhash32` and `nmhash32x` scalar
! algorithms of James Z. M. Gao, copyright 2021. James Z. M. Gao's original
! C++ code, `nmhash.h`, is available at the URL:
! https://github.com/gzm55/hash-garage/blob/
! a8913138bdb3b7539c202edee30a7f0794bbd835/nmhash.h
!
! under the BSD 2-Clause License:
!
! https://github.com/gzm55/hash-garage/blob/
! a8913138bdb3b7539c202edee30a7f0794bbd835/LICENSE
!
! The algorithms come in multiple versions, depending on whether the
! vectorized instructions SSE2 or AVX2 are available. As neither instruction
! is available in portable Fortran 2008, the algorithms that do not use these
! instructions are used.
!
! The BSD 2-Clause license is as follows:
!
! BSD 2-Clause License
!
! Copyright (c) 2021, water hash algorithm. James Z.M. Gao
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! 1. Redistributions of source code must retain the above copyright notice,
!    this list of conditions and the following disclaimer.
!
! 2. Redistributions in binary form must reproduce the above copyright notice,
!    this list of conditions and the following disclaimer in the documentation
!    and/or other materials provided with the distribution.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
! CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
! SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
! CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
! ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.

SUBMODULE(Hashing32) nmMethods
IMPLICIT NONE
! Primes from XXH
INTEGER(INT32), PARAMETER :: nmh_prime32_1 = INT(Z'9E3779B1', INT32)
INTEGER(INT32), PARAMETER :: nmh_prime32_2 = INT(Z'85EBCA77', INT32)
INTEGER(INT32), PARAMETER :: nmh_prime32_3 = INT(Z'C2B2AE3D', INT32)
INTEGER(INT32), PARAMETER :: nmh_prime32_4 = INT(Z'27D4EB2F', INT32)
INTEGER(INT32), PARAMETER :: nmh_m1 = INT(z'F0D9649B', INT32)
INTEGER(INT32), PARAMETER :: nmh_m2 = INT(z'29A7935D', INT32)
INTEGER(INT32), PARAMETER :: nmh_m3 = INT(z'55D35831', INT32)
INTEGER(INT32), PARAMETER :: nmh_m1_v(0:31) = nmh_m1
INTEGER(INT32), PARAMETER :: nmh_m2_v(0:31) = nmh_m2
INTEGER(INT32), PARAMETER :: nmh_m3_v(0:31) = nmh_m3
LOGICAL(LGT), PARAMETER :: nmh_short32_without_seed2 = .FALSE.
LOGICAL(LGT), PARAMETER :: nmh_short32_with_seed2 = .TRUE.
INTEGER(INT32), PARAMETER :: init_size = 32
! Pseudorandom secrets taken directly from FARSH.
INTEGER(INT32), PARAMETER :: nmh_acc_init(0:init_size - 1) = [ &
  & INT(z'B8FE6C39', INT32), INT(z'23A44BBE', INT32), &
  & INT(z'7C01812C', INT32), INT(z'F721AD1C', INT32), &
  & INT(z'DED46DE9', INT32), INT(z'839097DB', INT32), &
  & INT(z'7240A4A4', INT32), INT(z'B7B3671F', INT32), &
  & INT(z'CB79E64E', INT32), INT(z'CCC0E578', INT32), &
  & INT(z'825AD07D', INT32), INT(z'CCFF7221', INT32), &
  & INT(z'B8084674', INT32), INT(z'F743248E', INT32), &
  & INT(z'E03590E6', INT32), INT(z'813A264C', INT32), &
  & INT(z'3C2852BB', INT32), INT(z'91C300CB', INT32), &
  & INT(z'88D0658B', INT32), INT(z'1B532EA3', INT32), &
  & INT(z'71644897', INT32), INT(z'A20DF94E', INT32), &
  & INT(z'3819EF46', INT32), INT(z'A9DEACD8', INT32), &
  & INT(z'A8FA763F', INT32), INT(z'E39C343F', INT32), &
  & INT(z'F9DCBBC7', INT32), INT(z'C70B4F1D', INT32), &
  & INT(z'8A51E04B', INT32), INT(z'CDB45931', INT32), &
  & INT(z'C89F7EC9', INT32), INT(z'D9787364', INT32)]

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Int8_nmhash32x
INTEGER(INT64) :: len
INTEGER(INT32) :: seed2
INTEGER(INT32) :: u32
INTEGER(INT16) :: u16(0:1)

len = SIZE(key, kind=INT64)
IF (len <= 8) THEN
  IF (len > 4) THEN
    ans = nmhash32x_5to8(key, seed)
    RETURN
  ELSE ! 0 to 4 bytes
    SELECT CASE (len)
    CASE (0)
      seed2 = seed + nmh_prime32_2
      u32 = 0
    CASE (1)
      seed2 = seed + nmh_prime32_2 + ISHFT(1_INT32, 24) + &
              ISHFT(1_INT32, 1)
      IF (little_endian) THEN
        u32 = TRANSFER([key(0), 0_INT8, 0_INT8, 0_INT8], &
        & 0_INT32)
      ELSE
        u32 = TRANSFER([0_INT8, 0_INT8, 0_INT8, key(0)], &
        & 0_INT32)
      END IF
    CASE (2)
      seed2 = seed + nmh_prime32_2 + ISHFT(2_INT32, 24) + &
              ISHFT(2_INT32, 1)
      IF (little_endian) THEN
        u32 = TRANSFER([nmh_readle16(key), 0_INT16], 0_INT32)
      ELSE
        u32 = TRANSFER([0_INT16, nmh_readle16(key)], 0_INT32)
      END IF
    CASE (3)
      seed2 = seed + nmh_prime32_2 + ISHFT(3_INT32, 24) + &
              ISHFT(3_INT32, 1)
      IF (little_endian) THEN
        u16(1) = TRANSFER([key(2), 0_INT8], 0_INT16)
        u16(0) = nmh_readle16(key)
      ELSE
        u16(0) = TRANSFER([0_INT8, key(2)], 0_INT16)
        u16(1) = nmh_readle16(key)
      END IF
      u32 = TRANSFER(u16, 0_INT32)
    CASE (4)
      seed2 = seed + nmh_prime32_1
      u32 = nmh_readle32(key)
    CASE default
      ans = 0
      RETURN
    END SELECT
    ans = nmhash32x_0to4(u32, seed2)
    RETURN
  END IF
END IF
IF (len < 256) THEN
  ans = nmhash32x_9to255(key, seed)
  RETURN
END IF
ans = nmhash32x_avalanche32(nmhash32_long(key, seed))
END PROCEDURE Int8_nmhash32x

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Int8_nmhash32
  !! NMHASH32 ans function for rank 1 array keys of kind INT8
INTEGER(INT64) :: len
INTEGER(INT32) :: u32
INTEGER(INT16) :: u16(0:1)
INTEGER(INT32) :: x, y
INTEGER(INT32) :: new_seed
  !!
len = SIZE(key, kind=INT64)
IF (len <= 32) THEN
  IF (len > 8) THEN
    ans = nmhash32_9to32(key, seed)
    RETURN
  ELSE IF (len > 4) THEN
    x = nmh_readle32(key)
    y = IEOR(nmh_readle32(key(len - 4:)), nmh_prime32_4 + 2 + seed)
    x = x + y
    x = IEOR(x, ISHFT(x, len + 7))
    ans = nmhash32_0to8(x, ISHFTC(y, 5))
    RETURN
  ELSE
    SELECT CASE (len)
    CASE (0)
      new_seed = seed + nmh_prime32_2
      u32 = 0
    CASE (1)
      new_seed = seed + nmh_prime32_2 + ISHFT(1_INT32, 24) + &
                 2_INT32
      IF (little_endian) THEN
        u32 = TRANSFER([key(0), 0_INT8, 0_INT8, 0_INT8], &
                       0_INT32)
      ELSE
        u32 = TRANSFER([0_INT8, 0_INT8, 0_INT8, key(0)], &
                       0_INT32)
      END IF
    CASE (2)
      new_seed = seed + nmh_prime32_2 + ISHFT(2_INT32, 24) + &
                 4_INT32
      IF (little_endian) THEN
        u32 = TRANSFER([nmh_readle16(key), 0_INT16], 0_INT32)
      ELSE
        u32 = TRANSFER([0_INT16, nmh_readle16(key)], 0_INT32)
      END IF
    CASE (3)
      new_seed = seed + nmh_prime32_2 + ISHFT(3_INT32, 24) + &
                 6_INT32
      IF (little_endian) THEN
        u16(1) = TRANSFER([key(2), 0_INT8], 0_INT16)
        u16(0) = nmh_readle16(key)
      ELSE
        u16(0) = TRANSFER([0_INT8, key(2)], 0_INT16)
        u16(1) = nmh_readle16(key)
      END IF
      u32 = TRANSFER(u16, 0_INT32)
    CASE (4)
      new_seed = seed + nmh_prime32_3
      u32 = nmh_readle32(key)
    CASE default
      ans = 0
      RETURN
    END SELECT
    ans = nmhash32_0to8(u32 + new_seed, ISHFTC(new_seed, 5))
    RETURN
  END IF
ELSE IF (len < 256_INT64) THEN
  ans = nmhash32_33to255(key, seed)
  RETURN
ELSE
  ans = nmhash32_avalanche32(nmhash32_long(key, seed))
  RETURN
END IF
  !!
END PROCEDURE Int8_nmhash32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Int16_nmhash32
!! NMHASH32 hash function for rank 1 array keys of kind Int16
ans = Int8_nmhash32(TRANSFER(key, 0_INT8, &
  & bytes_Int16 * SIZE(key, kind=INT64)), seed)
END PROCEDURE Int16_nmhash32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Int32_nmhash32
ans = Int8_nmhash32(TRANSFER(key, 0_INT8, &
  & bytes_Int32 * SIZE(key, kind=INT64)), seed)
END PROCEDURE Int32_nmhash32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Int64_nmhash32
ans = Int8_nmhash32(TRANSFER(key, 0_INT8, &
  & bytes_Int64 * SIZE(key, kind=INT64)), seed)
END PROCEDURE Int64_nmhash32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Char_nmhash32
ans = Int8_nmhash32(TRANSFER(key, 0_INT8, &
  & bytes_char * LEN(key, kind=INT64)), seed)
END PROCEDURE Char_nmhash32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Int16_nmhash32x
ans = Int8_nmhash32x(TRANSFER(key, 0_INT8, &
  & bytes_Int16 * SIZE(key, kind=INT64)), seed)
END PROCEDURE Int16_nmhash32x

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Int32_nmhash32x
ans = Int8_nmhash32x(TRANSFER(key, 0_INT8, &
  & bytes_Int32 * SIZE(key, kind=INT64)), seed)
END PROCEDURE Int32_nmhash32x

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Int64_nmhash32x
ans = Int8_nmhash32x(TRANSFER(key, 0_INT8, &
    & bytes_Int64 * SIZE(key, kind=INT64)), seed)
END PROCEDURE Int64_nmhash32x

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Char_nmhash32x
ans = Int8_nmhash32x(TRANSFER(key, 0_INT8, &
    & bytes_char * LEN(key, kind=INT64)), seed)
END PROCEDURE Char_nmhash32x

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE new_nmhash32_seed
! Random SEED generator for NMHASH32
INTEGER(INT32) :: old_seed
REAL(dp) :: sample
old_seed = seed
find_seed: DO
  CALL RANDOM_NUMBER(sample)
  seed = INT(FLOOR(sample * 2_INT64**32, INT64) - 2_INT64**31, &
             INT32)
  IF (seed /= old_seed) RETURN
END DO find_seed
END PROCEDURE new_nmhash32_seed

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE new_nmhash32x_seed
INTEGER(INT32) :: old_seed
REAL(dp) :: sample

old_seed = seed
find_seed: DO
  CALL RANDOM_NUMBER(sample)
  seed = INT(FLOOR(sample * 2_INT64**32, INT64) - 2_INT64**31, &
             INT32)
  IF (seed /= old_seed) RETURN
END DO find_seed
END PROCEDURE new_nmhash32x_seed

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION nmh_readle32(p) RESULT(v)
  INTEGER(INT32) :: v
  INTEGER(INT8), INTENT(in) :: p(:)

  IF (little_endian) THEN
    v = TRANSFER(p(1:4), 0_INT32)
  ELSE
    v = TRANSFER([p(4), p(3), p(2), p(1)], 0_INT32)
  END IF

END FUNCTION nmh_readle32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION nmh_readle16(p) RESULT(v)
  INTEGER(INT16) :: v
  INTEGER(INT8), INTENT(in) :: p(:)

  IF (little_endian) THEN
    v = TRANSFER(p(1:2), 0_INT16)
  ELSE
    v = TRANSFER([p(2), p(1)], 0_INT16)
  END IF

END FUNCTION nmh_readle16

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION nmhash32_0to8(x, seed) RESULT(vx32)
  INTEGER(INT32), INTENT(in) :: x
  INTEGER(INT32), INTENT(in) :: seed
  INTEGER(INT32) :: vx32
  ! base mixer: [-6 -12 776bf593 -19 11 3fb39c65 -15 -9 e9139917 -11 16]
  ! = 0.027071104091278835
  INTEGER(INT32), PARAMETER :: m1 = INT(z'776BF593', INT32)
  INTEGER(INT32), PARAMETER :: m2 = INT(z'3FB39C65', INT32)
  INTEGER(INT32), PARAMETER :: m3 = INT(z'E9139917', INT32)

  INTEGER(INT16) :: vx16(2)

  vx32 = x
  vx32 = IEOR(vx32, IEOR(ISHFT(vx32, -12), ISHFT(vx32, -6)))
  vx16 = TRANSFER(vx32, 0_INT16, 2)
  vx16 = vx16 * TRANSFER(m1, 0_INT16, 2)
  vx32 = TRANSFER(vx16, 0_INT32)
  vx32 = IEOR(vx32, IEOR(ISHFT(vx32, 11), ISHFT(vx32, -19)))
  vx16 = TRANSFER(vx32, 0_INT16, 2)
  vx16 = vx16 * TRANSFER(m2, 0_INT16, 2)
  vx32 = TRANSFER(vx16, 0_INT32)
  vx32 = IEOR(vx32, seed)
  vx32 = IEOR(vx32, IEOR(ISHFT(vx32, -15), ISHFT(vx32, -9)))
  vx16 = TRANSFER(vx32, 0_INT16, 2)
  vx16 = vx16 * TRANSFER(m3, 0_INT16, 2)
  vx32 = TRANSFER(vx16, 0_INT32)
  vx32 = IEOR(vx32, IEOR(ISHFT(vx32, 16), ISHFT(vx32, -11)))

END FUNCTION nmhash32_0to8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION nmhash32_9to255(p, seed, full_avalanche) RESULT(RESULT)
  INTEGER(INT8), INTENT(in) :: p(0:)
  INTEGER(INT32), INTENT(in) :: seed
  LOGICAL, INTENT(in) :: full_avalanche
  INTEGER(INT32) :: RESULT

  INTEGER(INT32) :: xu32(0:3), yu32(0:3)
  INTEGER(INT16) :: xu16(0:1)
  !       Due to an issue with Intel OneAPI ifort 2021 (see
  !       https://community.intel.com/t5/Intel-Fortran-Compiler/
  ! Intrinsic-transfer-with-a-provided-size-un-expected-behavior/m-p/
  ! 1343313#M158733
  !       ), it is not possible to define the following variables as a
  ! PARAMETER.
  !        INTEGER(int16), PARAMETER :: &
  !            nmh_m1_16(0:1) = transfer( nmh_m1, 0_int16, 2 ),  &
  !            nmh_m2_16(0:1) = transfer( nmh_m2, 0_int16, 2 ),  &
  !            nmh_m3_16(0:1) = transfer( nmh_m3, 0_int16, 2 )
  INTEGER(INT16) :: nmh_m1_16(0:1), nmh_m2_16(0:1), nmh_m3_16(0:1)
  INTEGER(INT32) :: s1
  INTEGER(INT64) :: length
  INTEGER(INT32) :: length32(0:1)
  INTEGER(INT64) :: i, j, r

  nmh_m1_16(0:1) = TRANSFER(nmh_m1, 0_INT16, 2)
  nmh_m2_16(0:1) = TRANSFER(nmh_m2, 0_INT16, 2)
  nmh_m3_16(0:1) = TRANSFER(nmh_m3, 0_INT16, 2)

  ! base mixer: [f0d9649b  5 -13 29a7935d -9 11 55d35831 -20 -10 ] =
  ! 0.93495901789135362

  RESULT = 0
  length = SIZE(p, kind=INT64)
  length32 = TRANSFER(length, 0_INT32, 2)
  IF (little_endian) THEN
    s1 = seed + length32(0)
  ELSE
    s1 = seed + length32(1)
  END IF
  xu32(0) = nmh_prime32_1
  xu32(1) = nmh_prime32_2
  xu32(2) = nmh_prime32_3
  xu32(3) = nmh_prime32_4
  yu32(:) = s1

  IF (full_avalanche) THEN
    ! 33 to 255 bytes
    r = (length - 1) / 32
    DO i = 0, r - 1
      DO j = 0, 3
        xu32(j) = IEOR(xu32(j), nmh_readle32(p(i * 32 + j * 4:)))
        yu32(j) = IEOR(yu32(j), &
                       nmh_readle32(p(i * 32 + j * 4 + 16:)))
        xu32(j) = xu32(j) + yu32(j)
        xu16 = TRANSFER(xu32(j), 0_INT16, 2)
        xu16 = xu16 * nmh_m1_16
        xu32(j) = TRANSFER(xu16, 0_INT32)
        xu32(j) = IEOR(xu32(j), &
                       IEOR(ISHFT(xu32(j), 5), &
                            ISHFT(xu32(j), -13)))
        xu16 = TRANSFER(xu32(j), 0_INT16, 2)
        xu16 = xu16 * nmh_m2_16
        xu32(j) = TRANSFER(xu16, 0_INT32)
        xu32(j) = IEOR(xu32(j), yu32(j))
        xu32(j) = IEOR(xu32(j), &
                       IEOR(ISHFT(xu32(j), 11), &
                            ISHFT(xu32(j), -9)))
        xu16 = TRANSFER(xu32(j), 0_INT16, 2)
        xu16 = xu16 * nmh_m3_16
        xu32(j) = TRANSFER(xu16, 0_INT32)
        xu32(j) = IEOR(xu32(j), &
                       IEOR(ISHFT(xu32(j), -10), &
                            ISHFT(xu32(j), -20)))
      END DO
    END DO
    DO j = 0, 3
      xu32(j) = IEOR(xu32(j), &
                     nmh_readle32(p(length - 32 + j * 4:)))
      yu32(j) = IEOR(yu32(j), &
                     nmh_readle32(p(length - 16 + j * 4:)))
    END DO
  ELSE
    ! 9 to 32 bytes
    xu32(0) = IEOR(xu32(0), nmh_readle32(p(0:)))
    xu32(1) = IEOR(xu32(1), nmh_readle32(p(ISHFT(ISHFT(length, -4), 3):)))
    xu32(2) = IEOR(xu32(2), nmh_readle32(p(length - 8:)))
    xu32(3) = IEOR(xu32(3), &
                   nmh_readle32(p(length - 8 - ISHFT(ISHFT(length, -4), 3):)))
    yu32(0) = IEOR(yu32(0), nmh_readle32(p(4:)))
    yu32(1) = IEOR(yu32(1), &
                   nmh_readle32(p(ISHFT(ISHFT(length, -4), 3) + 4:)))
    yu32(2) = IEOR(yu32(2), nmh_readle32(p(length - 8 + 4:)))
    yu32(3) = IEOR(yu32(3), &
                   nmh_readle32(p(length - 8 - &
                                  ISHFT(ISHFT(length, -4), 3) + 4:)))
  END IF
  DO j = 0, 3
    xu32(j) = xu32(j) + yu32(j)
    yu32(j) = IEOR(yu32(j), IEOR(ISHFT(yu32(j), 17), &
                                 ISHFT(yu32(j), -6)))
    xu16 = TRANSFER(xu32(j), 0_INT16, 2)
    xu16 = xu16 * nmh_m1_16
    xu32(j) = TRANSFER(xu16, 0_INT32)
    xu32(j) = IEOR(xu32(j), IEOR(ISHFT(xu32(j), 5), &
                                 ISHFT(xu32(j), -13)))
    xu16 = TRANSFER(xu32(j), 0_INT16, 2)
    xu16 = xu16 * nmh_m2_16
    xu32(j) = TRANSFER(xu16, 0_INT32)
    xu32(j) = IEOR(xu32(j), yu32(j))
    xu32(j) = IEOR(xu32(j), IEOR(ISHFT(xu32(j), 11), &
                                 ISHFT(xu32(j), -9)))
    xu16 = TRANSFER(xu32(j), 0_INT16, 2)
    xu16 = xu16 * nmh_m3_16
    xu32(j) = TRANSFER(xu16, 0_INT32)
    xu32(j) = IEOR(xu32(j), IEOR(ISHFT(xu32(j), -10), &
                                 ISHFT(xu32(j), -20)))
  END DO
  xu32(0) = IEOR(xu32(0), nmh_prime32_1)
  xu32(1) = IEOR(xu32(1), nmh_prime32_2)
  xu32(2) = IEOR(xu32(2), nmh_prime32_3)
  xu32(3) = IEOR(xu32(3), nmh_prime32_4)
  DO j = 1, 3
    xu32(0) = xu32(0) + xu32(j)
  END DO
  xu32(0) = IEOR(xu32(0), s1 + ISHFT(s1, -5))
  xu16 = TRANSFER(xu32(0), 0_INT16, 2)
  xu16 = xu16 * nmh_m3_16
  xu32(0) = TRANSFER(xu16, 0_INT32)
  xu32(0) = IEOR(xu32(0), &
                 IEOR(ISHFT(xu32(0), -10), ISHFT(xu32(0), -20)))
  RESULT = xu32(0)

END FUNCTION nmhash32_9to255

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION nmhash32_9to32(p, seed) RESULT(ans)
  INTEGER(INT8), INTENT(in) :: p(0:)
  INTEGER(INT32), INTENT(in) :: seed
  INTEGER(INT32) :: ans
  ans = nmhash32_9to255(p, seed, .FALSE.)
END FUNCTION nmhash32_9to32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION nmhash32_33to255(p, seed) RESULT(ans)
  INTEGER(INT8), INTENT(in) :: p(0:)
  INTEGER(INT32), INTENT(in) :: seed
  INTEGER(INT32) :: ans
  ans = nmhash32_9to255(p, seed, .TRUE.)
END FUNCTION nmhash32_33to255

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE nmhash32_long_round(accx, accy, p)
  INTEGER(INT32), INTENT(inout) :: accx(0:)
  INTEGER(INT32), INTENT(inout) :: accy(0:)
  INTEGER(INT8), INTENT(in) :: p(0:)

  INTEGER(INT64), PARAMETER :: nbgroups = init_size
  INTEGER(INT64) :: i
  INTEGER(INT16) :: dummy1(0:1)
  INTEGER(INT16) :: dummy2(0:1)

  DO i = 0, nbgroups - 1
    accx(i) = IEOR(accx(i), nmh_readle32(p(i * 4:)))
    accy(i) = IEOR(accy(i), nmh_readle32(p(i * 4 + nbgroups * 4:)))
    accx(i) = accx(i) + accy(i)
    accy(i) = IEOR(accy(i), ISHFT(accx(i), -1))
    dummy1 = TRANSFER(accx(i), 0_INT16, 2)
    dummy2 = TRANSFER(nmh_m1_v(i), 0_INT16, 2)
    dummy1 = dummy1 * dummy2
    accx(i) = TRANSFER(dummy1, 0_INT32)
    accx(i) = IEOR(accx(i), IEOR(ISHFT(accx(i), 5), &
                                 ISHFT(accx(i), -13)))
    dummy1 = TRANSFER(accx(i), 0_INT16, 2)
    dummy2 = TRANSFER(nmh_m2_v(i), 0_INT16, 2)
    dummy1 = dummy1 * dummy2
    accx(i) = TRANSFER(dummy1, 0_INT32)
    accx(i) = IEOR(accx(i), accy(i))
    accx(i) = IEOR(accx(i), IEOR(ISHFT(accx(i), 11), &
                                 ISHFT(accx(i), -9)))
    dummy1 = TRANSFER(accx(i), 0_INT16, 2)
    dummy2 = TRANSFER(nmh_m3_v(i), 0_INT16, 2)
    dummy1 = dummy1 * dummy2
    accx(i) = TRANSFER(dummy1, 0_INT32)
    accx(i) = IEOR(accx(i), IEOR(ISHFT(accx(i), -10), &
                                 ISHFT(accx(i), -20)))
  END DO

END SUBROUTINE nmhash32_long_round

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION nmhash32_long(p, seed) RESULT(sum)
  INTEGER(INT32) :: sum
  INTEGER(INT8), INTENT(in) :: p(0:)
  INTEGER(INT32), INTENT(in) :: seed
  !!
  INTEGER(INT32) :: accx(0:SIZE(nmh_acc_init) - 1)
  INTEGER(INT32) :: accy(0:SIZE(nmh_acc_init) - 1)
  INTEGER(INT64) :: nbrounds
  INTEGER(INT64) :: len
  INTEGER(INT32) :: len32(0:1)
  INTEGER(INT64) :: i
  !!
  len = SIZE(p, kind=INT64)
  nbrounds = (len - 1) / (4 * SIZE(accx, kind=INT64) * 2)
  sum = 0
  !!
  !  Init
  DO i = 0_INT64, SIZE(nmh_acc_init, kind=INT64) - 1
    accx(i) = nmh_acc_init(i)
    accy(i) = seed
  END DO
  !!
  ! init
  DO i = 0_INT64, nbrounds - 1
    CALL nmhash32_long_round(accx, accy, &
      & p(i * 8 * SIZE(accx, kind=INT64):))
  END DO
  CALL nmhash32_long_round(accx, accy, &
      & p(len - 8 * SIZE(accx, kind=INT64):))
  !!
  ! merge acc
  DO i = 0, SIZE(accx, kind=INT64) - 1
    accx(i) = IEOR(accx(i), nmh_acc_init(i))
    sum = sum + accx(i)
  END DO
  !!
  len32 = TRANSFER(len, 0_INT32, 2)
  IF (little_endian) THEN
    sum = sum + len32(1)
    sum = IEOR(sum, len32(0))
  ELSE
    sum = sum + len32(0)
    sum = IEOR(sum, len32(1))
  END IF
  !!
END FUNCTION nmhash32_long

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION nmhash32_avalanche32(x) RESULT(u32)
  INTEGER(INT32) :: u32
  INTEGER(INT32), INTENT(in) :: x
  !!
  INTEGER(INT16) :: u16(0:1)
  INTEGER(INT32), PARAMETER :: m1 = INT(z'CCE5196D', INT32)
  INTEGER(INT32), PARAMETER :: m2 = INT(z'464BE229', INT32)
  ! Due to an issue with Intel OneAPI ifort 2021 (see
  ! https://community.intel.com/t5/Intel-Fortran-Compiler/
  ! Intrinsic-transfer-with-a-provided-size-un-expected-behavior/m-p/
  ! 1343313#M158733
  ! ), it is not possible to define the following variables as a PARAMETER.
  !INTEGER(int16), PARAMETER:: m1_16(0:1) = transfer(m1, 0_int16, 2)
  !INTEGER(int16), PARAMETER:: m2_16(0:1) = transfer(m2, 0_int16, 2)
  INTEGER(INT16) :: m1_16(0:1), m2_16(0:1)
  ! [-21 -8 cce5196d 12 -7 464be229 -21 -8] = 3.2267098842182733
  !!
  m1_16(0:1) = TRANSFER(m1, 0_INT16, 2)
  m2_16(0:1) = TRANSFER(m2, 0_INT16, 2)
  !!
  u32 = x
  u32 = IEOR(u32, IEOR(ISHFT(u32, -8), ISHFT(u32, -21)))
  u16 = TRANSFER(u32, 0_INT16, 2)
  u16(0) = u16(0) * m1_16(0)
  u16(1) = u16(1) * m1_16(1)
  u32 = TRANSFER(u16, 0_INT32)
  u32 = IEOR(u32, IEOR(ISHFT(u32, 12), ISHFT(u32, -7)))
  u16 = TRANSFER(u32, 0_INT16, 2)
  u16(0) = u16(0) * m2_16(0)
  u16(1) = u16(1) * m2_16(1)
  u32 = TRANSFER(u16, 0_INT32)
  u32 = IEOR(u32, IEOR(ISHFT(u32, -8), ISHFT(u32, -21)))
  !!
END FUNCTION nmhash32_avalanche32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION nmhash32x_0to4(x, seed) RESULT(hash)
  INTEGER(INT32), INTENT(in) :: x
  INTEGER(INT32), INTENT(in) :: seed
  INTEGER(INT32) :: hash
  !!
  !! [bdab1ea9 18 a7896a1b 12 83796a2d 16] = 0.092922873297662509
  !!
  hash = x
  hash = IEOR(hash, seed)
  hash = hash * INT(z'BDAB1EA9', INT32)
  hash = hash + ISHFTC(seed, 31)
  hash = IEOR(hash, ISHFT(hash, -18))
  hash = hash * INT(z'A7896A1B', INT32)
  hash = IEOR(hash, ISHFT(hash, -12))
  hash = hash * INT(z'83796A2D', INT32)
  hash = IEOR(hash, ISHFT(hash, -16))
  !!
END FUNCTION nmhash32x_0to4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION nmhash32x_5to8(p, seed) RESULT(x)
  INTEGER(INT8), INTENT(in) :: p(0:)
  INTEGER(INT32), INTENT(in) :: seed
  INTEGER(INT32) :: x
  !!
  !! internal variables
  !!
  INTEGER(INT64) :: len
  INTEGER(INT32) :: y
  !
  ! 5 to 9 bytes
  ! mixer: [11049a7d 23 bcccdc7b 12 065e9dad 12] = 0.16577596555667246
  !
  len = SIZE(p, kind=INT64)
  x = IEOR(nmh_readle32(p), nmh_prime32_3)
  y = IEOR(nmh_readle32(p(len - 4:)), seed)
  x = x + y
  x = IEOR(x, ISHFT(x, -len))
  x = x * INT(z'11049A7D', INT32)
  x = IEOR(x, ISHFT(x, -23))
  x = x * INT(z'BCCCDC7B', INT32)
  x = IEOR(x, ISHFTC(y, 3))
  x = IEOR(x, ISHFT(x, -12))
  x = x * INT(z'065E9DAD', INT32)
  x = IEOR(x, ISHFT(x, -12))
END FUNCTION nmhash32x_5to8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION nmhash32x_9to255(p, seed) RESULT(x)
  INTEGER(INT8), INTENT(in) :: p(0:)
  INTEGER(INT32), INTENT(in) :: seed
  INTEGER(INT32) :: x
  !!
  !! internal variables
  !!
  INTEGER(INT64) :: len
  INTEGER(INT32) :: len32(0:1), len_base
  INTEGER(INT32) :: y
  INTEGER(INT32) :: a, b
  INTEGER(INT64) :: i, r
  !!
  ! - at least 9 bytes
  ! - base mixer: [11049a7d 23 bcccdc7b 12 065e9dad 12] = 0.16577596555667246
  ! - tail mixer: [16 a52fb2cd 15 551e4d49 16] = 0.17162579707098322
  !!
  len = SIZE(p, kind=INT64)
  len32 = TRANSFER(len, 0_INT32, 2)
  IF (little_endian) THEN
    len_base = len32(0)
  ELSE
    len_base = len32(1)
  END IF
  x = nmh_prime32_3
  y = seed
  a = nmh_prime32_4
  b = seed
  r = (len - 1) / 16
  !!
  DO i = 0, r - 1
    x = IEOR(x, nmh_readle32(p(i * 16 + 0:)))
    y = IEOR(y, nmh_readle32(p(i * 16 + 4:)))
    x = IEOR(x, y)
    x = x * INT(z'11049A7D', INT32)
    x = IEOR(x, ISHFT(x, -23))
    x = x * INT(z'BCCCDC7B', INT32)
    y = ISHFTC(y, 4)
    x = IEOR(x, y)
    x = IEOR(x, ISHFT(x, -12))
    x = x * INT(z'065E9DAD', INT32)
    x = IEOR(x, ISHFT(x, -12))

    a = IEOR(a, nmh_readle32(p(i * 16 + 8:)))
    b = IEOR(b, nmh_readle32(p(i * 16 + 12:)))
    a = IEOR(a, b)
    a = a * INT(z'11049A7D', INT32)
    a = IEOR(a, ISHFT(a, -23))
    a = a * INT(z'BCCCDC7B', INT32)
    b = ISHFTC(b, 3)
    a = IEOR(a, b)
    a = IEOR(a, ISHFT(a, -12))
    a = a * INT(z'065E9DAD', INT32)
    a = IEOR(a, ISHFT(a, -12))
  END DO
  !!
  IF (IAND(len_base - 1_INT32, 8_INT32) /= 0) THEN
    IF (IAND(len_base - 1_INT32, 4_INT32) /= 0) THEN
      a = IEOR(a, nmh_readle32(p(r * 16 + 0:)))
      b = IEOR(b, nmh_readle32(p(r * 16 + 4:)))
      a = IEOR(a, b)
      a = a * INT(z'11049A7D', INT32)
      a = IEOR(a, ISHFT(a, -23))
      a = a * INT(z'BCCCDC7B', INT32)
      a = IEOR(a, ISHFTC(b, 4))
      a = IEOR(a, ISHFT(a, -12))
      a = a * INT(z'065E9DAD', INT32)
    ELSE
      a = IEOR(a, nmh_readle32(p(r * 16:)) + b)
      a = IEOR(a, ISHFT(a, -16))
      a = a * INT(z'A52FB2CD', INT32)
      a = IEOR(a, ISHFT(a, -15))
      a = a * INT(z'551E4D49', INT32)
    END IF
    x = IEOR(x, nmh_readle32(p(len - 8:)))
    y = IEOR(y, nmh_readle32(p(len - 4:)))
    x = IEOR(x, y)
    x = x * INT(z'11049A7D', INT32)
    x = IEOR(x, ISHFT(x, -23))
    x = x * INT(z'BCCCDC7B', INT32); 
    x = IEOR(x, ISHFTC(y, 3))
    x = IEOR(x, ISHFT(x, -12))
    x = x * INT(z'065E9DAD', INT32)
  ELSE
    IF (IAND(len_base - 1_INT32, 4_INT32) /= 0) THEN
      a = IEOR(a, nmh_readle32(p(r * 16:)) + b)
      a = IEOR(a, ISHFT(a, -16))
      a = a * INT(z'A52FB2CD', INT32)
      a = IEOR(a, ISHFT(a, -15))
      a = a * INT(z'551E4D49', INT32)
    END IF
    x = IEOR(x, nmh_readle32(p(len - 4:)) + y)
    x = IEOR(x, ISHFT(x, -16))
    x = x * INT(z'A52FB2CD', INT32)
    x = IEOR(x, ISHFT(x, -15))
    x = x * INT(z'551E4D49', INT32)
  END IF
  !!
  x = IEOR(x, len_base)
  x = IEOR(x, ISHFTC(a, 27)) ! rotate one lane to pass Diff test
  x = IEOR(x, ISHFT(x, -14))
  x = x * INT(z'141CC535', INT32)
  !!
END FUNCTION nmhash32x_9to255

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION nmhash32x_avalanche32(x) RESULT(hash)
  INTEGER(INT32) :: hash
  INTEGER(INT32), INTENT(in) :: x
  ! Mixer with 2 mul from skeeto/hash-prospector:
  ! [15 d168aaad 15 af723597 15] = 0.15983776156606694
  hash = x
  hash = IEOR(hash, ISHFT(hash, -15))
  hash = hash * INT(z'D168AAAD', INT32)
  hash = IEOR(hash, ISHFT(hash, -15))
  hash = hash * INT(z'AF723597', INT32)
  hash = IEOR(hash, ISHFT(hash, -15))
END FUNCTION nmhash32x_avalanche32

END SUBMODULE nmMethods
