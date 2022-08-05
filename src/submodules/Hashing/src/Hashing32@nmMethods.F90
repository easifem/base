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
INTEGER(Int32), PARAMETER :: nmh_prime32_1 = INT(Z'9E3779B1', Int32)
INTEGER(Int32), PARAMETER :: nmh_prime32_2 = INT(Z'85EBCA77', Int32)
INTEGER(Int32), PARAMETER :: nmh_prime32_3 = INT(Z'C2B2AE3D', Int32)
INTEGER(Int32), PARAMETER :: nmh_prime32_4 = INT(Z'27D4EB2F', Int32)
INTEGER(Int32), PARAMETER :: nmh_m1 = INT(z'F0D9649B', Int32)
INTEGER(Int32), PARAMETER :: nmh_m2 = INT(z'29A7935D', Int32)
INTEGER(Int32), PARAMETER :: nmh_m3 = INT(z'55D35831', Int32)
INTEGER(Int32), PARAMETER :: nmh_m1_v(0:31) = nmh_m1
INTEGER(Int32), PARAMETER :: nmh_m2_v(0:31) = nmh_m2
INTEGER(Int32), PARAMETER :: nmh_m3_v(0:31) = nmh_m3
LOGICAL(LGT), PARAMETER :: nmh_short32_without_seed2 = .false.
LOGICAL(LGT), PARAMETER :: nmh_short32_with_seed2 = .true.
INTEGER(Int32), PARAMETER :: init_size = 32
! Pseudorandom secrets taken directly from FARSH.
INTEGER(Int32), PARAMETER :: nmh_acc_init(0:init_size - 1) = [ &
  & INT(z'B8FE6C39', Int32), INT(z'23A44BBE', Int32), &
  & INT(z'7C01812C', Int32), INT(z'F721AD1C', Int32), &
  & INT(z'DED46DE9', Int32), INT(z'839097DB', Int32), &
  & INT(z'7240A4A4', Int32), INT(z'B7B3671F', Int32), &
  & INT(z'CB79E64E', Int32), INT(z'CCC0E578', Int32), &
  & INT(z'825AD07D', Int32), INT(z'CCFF7221', Int32), &
  & INT(z'B8084674', Int32), INT(z'F743248E', Int32), &
  & INT(z'E03590E6', Int32), INT(z'813A264C', Int32), &
  & INT(z'3C2852BB', Int32), INT(z'91C300CB', Int32), &
  & INT(z'88D0658B', Int32), INT(z'1B532EA3', Int32), &
  & INT(z'71644897', Int32), INT(z'A20DF94E', Int32), &
  & INT(z'3819EF46', Int32), INT(z'A9DEACD8', Int32), &
  & INT(z'A8FA763F', Int32), INT(z'E39C343F', Int32), &
  & INT(z'F9DCBBC7', Int32), INT(z'C70B4F1D', Int32), &
  & INT(z'8A51E04B', Int32), INT(z'CDB45931', Int32), &
  & INT(z'C89F7EC9', Int32), INT(z'D9787364', Int32)]

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Int8_nmhash32x
  INTEGER(Int64) :: len
  INTEGER(Int32) :: seed2
  INTEGER(Int32) :: u32
  INTEGER(Int16) :: u16(0:1)

  len = SIZE(key, kind=Int64)
  if (len <= 8) then
    if (len > 4) then
      ans = nmhash32x_5to8(key, seed)
      return
    else ! 0 to 4 bytes
      select case (len)
      case (0)
        seed2 = seed + nmh_prime32_2
        u32 = 0
      case (1)
        seed2 = seed + nmh_prime32_2 + ishft(1_int32, 24) + &
        ishft(1_int32, 1)
        if (little_endian) then
          u32 = transfer([key(0), 0_int8, 0_int8, 0_int8], &
          & 0_int32)
        else
          u32 = transfer([0_int8, 0_int8, 0_int8, key(0)], &
          & 0_int32)
        end if
      case (2)
        seed2 = seed + nmh_prime32_2 + ishft(2_int32, 24) + &
        ishft(2_int32, 1)
        if (little_endian) then
          u32 = transfer([nmh_readle16(key), 0_int16], 0_int32)
        else
          u32 = transfer([0_int16, nmh_readle16(key)], 0_int32)
        end if
      case (3)
        seed2 = seed + nmh_prime32_2 + ishft(3_int32, 24) + &
        ishft(3_int32, 1)
        if (little_endian) then
          u16(1) = transfer([key(2), 0_int8], 0_int16)
          u16(0) = nmh_readle16(key)
        else
          u16(0) = transfer([0_int8, key(2)], 0_int16)
          u16(1) = nmh_readle16(key)
        end if
        u32 = transfer(u16, 0_int32)
      case (4)
        seed2 = seed + nmh_prime32_1
        u32 = nmh_readle32(key)
      case default
        ans = 0
        return
      end select
      ans = nmhash32x_0to4(u32, seed2)
      return
    end if
  end if
  if (len < 256) then
    ans = nmhash32x_9to255(key, seed)
    return
  end if
  ans = nmhash32x_avalanche32(nmhash32_long(key, seed))
END PROCEDURE Int8_nmhash32x

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Int8_nmhash32
  !! NMHASH32 ans function for rank 1 array keys of kind INT8
  INTEGER(Int64) :: len
  INTEGER(Int32) :: u32
  INTEGER(Int16) :: u16(0:1)
  INTEGER(Int32) :: x, y
  INTEGER(Int32) :: new_seed
  !!
  len = size(key, kind=Int64)
  if (len <= 32) then
    if (len > 8) then
      ans = nmhash32_9to32(key, seed)
      return
    else if (len > 4) then
      x = nmh_readle32(key)
      y = ieor(nmh_readle32(key(len - 4:)), nmh_prime32_4 + 2 + seed)
      x = x + y
      x = ieor(x, ishft(x, len + 7))
      ans = nmhash32_0to8(x, ishftc(y, 5))
      return
    else
      select case (len)
      case (0)
        new_seed = seed + nmh_prime32_2
        u32 = 0
      case (1)
        new_seed = seed + nmh_prime32_2 + ishft(1_Int32, 24) + &
        2_Int32
        if (little_endian) then
          u32 = transfer([key(0), 0_Int8, 0_Int8, 0_Int8], &
          0_Int32)
        else
          u32 = transfer([0_Int8, 0_Int8, 0_Int8, key(0)], &
          0_Int32)
        end if
      case (2)
        new_seed = seed + nmh_prime32_2 + ishft(2_Int32, 24) + &
        4_Int32
        if (little_endian) then
          u32 = transfer([nmh_readle16(key), 0_Int16], 0_Int32)
        else
          u32 = transfer([0_Int16, nmh_readle16(key)], 0_Int32)
        end if
      case (3)
        new_seed = seed + nmh_prime32_2 + ishft(3_Int32, 24) + &
        6_Int32
        if (little_endian) then
          u16(1) = transfer([key(2), 0_Int8], 0_Int16)
          u16(0) = nmh_readle16(key)
        else
          u16(0) = transfer([0_Int8, key(2)], 0_Int16)
          u16(1) = nmh_readle16(key)
        end if
        u32 = transfer(u16, 0_Int32)
      case (4)
        new_seed = seed + nmh_prime32_3
        u32 = nmh_readle32(key)
      case default
        ans = 0
        return
      end select
      ans = nmhash32_0to8(u32 + new_seed, ishftc(new_seed, 5))
      return
    end if
  else if (len < 256_Int64) then
    ans = nmhash32_33to255(key, seed)
    return
  else
    ans = nmhash32_avalanche32(nmhash32_long(key, seed))
    return
  end if
  !!
END PROCEDURE Int8_nmhash32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Int16_nmhash32
!! NMHASH32 hash function for rank 1 array keys of kind Int16
  ans = Int8_nmhash32(transfer(key, 0_Int8, &
    & bytes_Int16 * size(key, kind=Int64)), seed)
END PROCEDURE Int16_nmhash32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Int32_nmhash32
  ans = Int8_nmhash32(transfer(key, 0_Int8, &
    & bytes_Int32 * size(key, kind=Int64)), seed)
END PROCEDURE Int32_nmhash32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Int64_nmhash32
  ans = Int8_nmhash32(transfer(key, 0_Int8, &
    & bytes_Int64 * size(key, kind=Int64)), seed)
END PROCEDURE Int64_nmhash32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Char_nmhash32
  ans = Int8_nmhash32(transfer(key, 0_Int8, &
    & bytes_char * len(key, kind=Int64)), seed)
END PROCEDURE Char_nmhash32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Int16_nmhash32x
  ans = Int8_nmhash32x(transfer(key, 0_Int8, &
    & bytes_Int16 * size(key, kind=Int64)), seed)
END PROCEDURE Int16_nmhash32x

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Int32_nmhash32x
  ans = Int8_nmhash32x(transfer(key, 0_Int8, &
    & bytes_Int32 * size(key, kind=Int64)), seed)
END PROCEDURE Int32_nmhash32x

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Int64_nmhash32x
  ans = Int8_nmhash32x(transfer(key, 0_Int8, &
      & bytes_Int64 * size(key, kind=Int64)), seed)
END PROCEDURE Int64_nmhash32x

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Char_nmhash32x
  ans = Int8_nmhash32x(transfer(key, 0_Int8, &
      & bytes_char * len(key, kind=Int64)), seed)
END PROCEDURE Char_nmhash32x

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE new_nmhash32_seed
  ! Random SEED generator for NMHASH32
  INTEGER(Int32) :: old_seed
  real(dp) :: sample
  old_seed = seed
  find_seed: do
    call random_number(sample)
    seed = INT(floor(sample * 2_int64**32, int64) - 2_int64**31, &
      Int32)
    if (seed /= old_seed) return
  end do find_seed
END PROCEDURE new_nmhash32_seed

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE new_nmhash32x_seed
  INTEGER(Int32) :: old_seed
  real(dp) :: sample

  old_seed = seed
  find_seed: do
    call random_number(sample)
    seed = INT(floor(sample * 2_int64**32, int64) - 2_int64**31, &
              Int32)
    if (seed /= old_seed) return
  end do find_seed
END PROCEDURE new_nmhash32x_seed

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function nmh_readle32(p) result(v)
  INTEGER(Int32) :: v
  INTEGER(int8), intent(in) :: p(:)

  if (little_endian) then
    v = transfer(p(1:4), 0_int32)
  else
    v = transfer([p(4), p(3), p(2), p(1)], 0_int32)
  end if

end function nmh_readle32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function nmh_readle16(p) result(v)
  INTEGER(int16) :: v
  INTEGER(int8), intent(in) :: p(:)

  if (little_endian) then
    v = transfer(p(1:2), 0_int16)
  else
    v = transfer([p(2), p(1)], 0_int16)
  end if

end function nmh_readle16

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function nmhash32_0to8(x, seed) result(vx32)
  INTEGER(Int32), intent(in) :: x
  INTEGER(Int32), intent(in) :: seed
  INTEGER(Int32) :: vx32
  ! base mixer: [-6 -12 776bf593 -19 11 3fb39c65 -15 -9 e9139917 -11 16]
  ! = 0.027071104091278835
  INTEGER(Int32), PARAMETER :: m1 = INT(z'776BF593', Int32)
  INTEGER(Int32), PARAMETER :: m2 = INT(z'3FB39C65', Int32)
  INTEGER(Int32), PARAMETER :: m3 = INT(z'E9139917', Int32)

  INTEGER(int16) :: vx16(2)

  vx32 = x
  vx32 = ieor(vx32, ieor(ishft(vx32, -12), ishft(vx32, -6)))
  vx16 = transfer(vx32, 0_int16, 2)
  vx16 = vx16 * transfer(m1, 0_int16, 2)
  vx32 = transfer(vx16, 0_int32)
  vx32 = ieor(vx32, ieor(ishft(vx32, 11), ishft(vx32, -19)))
  vx16 = transfer(vx32, 0_int16, 2)
  vx16 = vx16 * transfer(m2, 0_int16, 2)
  vx32 = transfer(vx16, 0_int32)
  vx32 = ieor(vx32, seed)
  vx32 = ieor(vx32, ieor(ishft(vx32, -15), ishft(vx32, -9)))
  vx16 = transfer(vx32, 0_int16, 2)
  vx16 = vx16 * transfer(m3, 0_int16, 2)
  vx32 = transfer(vx16, 0_int32)
  vx32 = ieor(vx32, ieor(ishft(vx32, 16), ishft(vx32, -11)))

end function nmhash32_0to8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function nmhash32_9to255(p, seed, full_avalanche) result(result)
  INTEGER(int8), intent(in) :: p(0:)
  INTEGER(Int32), intent(in) :: seed
  LOGICAL, intent(in) :: full_avalanche
  INTEGER(Int32) :: result

  INTEGER(Int32) :: xu32(0:3), yu32(0:3)
  INTEGER(int16) :: xu16(0:1)
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
  INTEGER(int16) :: nmh_m1_16(0:1), nmh_m2_16(0:1), nmh_m3_16(0:1)
  INTEGER(Int32) :: s1
  INTEGER(int64) :: length
  INTEGER(Int32) :: length32(0:1)
  INTEGER(int64) :: i, j, r

  nmh_m1_16(0:1) = transfer(nmh_m1, 0_int16, 2)
  nmh_m2_16(0:1) = transfer(nmh_m2, 0_int16, 2)
  nmh_m3_16(0:1) = transfer(nmh_m3, 0_int16, 2)

  ! base mixer: [f0d9649b  5 -13 29a7935d -9 11 55d35831 -20 -10 ] =
  ! 0.93495901789135362

  result = 0
  length = size(p, kind=int64)
  length32 = transfer(length, 0_int32, 2)
  if (little_endian) then
    s1 = seed + length32(0)
  else
    s1 = seed + length32(1)
  end if
  xu32(0) = nmh_prime32_1
  xu32(1) = nmh_prime32_2
  xu32(2) = nmh_prime32_3
  xu32(3) = nmh_prime32_4
  yu32(:) = s1

  if (full_avalanche) then
    ! 33 to 255 bytes
    r = (length - 1) / 32
    do i = 0, r - 1
      do j = 0, 3
        xu32(j) = ieor(xu32(j), nmh_readle32(p(i * 32 + j * 4:)))
        yu32(j) = ieor(yu32(j), &
                       nmh_readle32(p(i * 32 + j * 4 + 16:)))
        xu32(j) = xu32(j) + yu32(j)
        xu16 = transfer(xu32(j), 0_int16, 2)
        xu16 = xu16 * nmh_m1_16
        xu32(j) = transfer(xu16, 0_int32)
        xu32(j) = ieor(xu32(j), &
                      ieor(ishft(xu32(j), 5), &
                            ishft(xu32(j), -13)))
        xu16 = transfer(xu32(j), 0_int16, 2)
        xu16 = xu16 * nmh_m2_16
        xu32(j) = transfer(xu16, 0_int32)
        xu32(j) = ieor(xu32(j), yu32(j))
        xu32(j) = ieor(xu32(j), &
                      ieor(ishft(xu32(j), 11), &
                            ishft(xu32(j), -9)))
        xu16 = transfer(xu32(j), 0_int16, 2)
        xu16 = xu16 * nmh_m3_16
        xu32(j) = transfer(xu16, 0_int32)
        xu32(j) = ieor(xu32(j), &
                      ieor(ishft(xu32(j), -10), &
                            ishft(xu32(j), -20)))
      end do
    end do
    do j = 0, 3
      xu32(j) = ieor(xu32(j), &
                     nmh_readle32(p(length - 32 + j * 4:)))
      yu32(j) = ieor(yu32(j), &
                     nmh_readle32(p(length - 16 + j * 4:)))
    end do
  else
    ! 9 to 32 bytes
    xu32(0) = ieor(xu32(0), nmh_readle32(p(0:)))
    xu32(1) = ieor(xu32(1), nmh_readle32(p(ishft(ishft(length, -4), 3):)))
    xu32(2) = ieor(xu32(2), nmh_readle32(p(length - 8:)))
    xu32(3) = ieor(xu32(3), &
                  nmh_readle32(p(length - 8 - ishft(ishft(length, -4), 3):)))
    yu32(0) = ieor(yu32(0), nmh_readle32(p(4:)))
    yu32(1) = ieor(yu32(1), &
                  nmh_readle32(p(ishft(ishft(length, -4), 3) + 4:)))
    yu32(2) = ieor(yu32(2), nmh_readle32(p(length - 8 + 4:)))
    yu32(3) = ieor(yu32(3), &
                  nmh_readle32(p(length - 8 - &
                                  ishft(ishft(length, -4), 3) + 4:)))
  end if
  do j = 0, 3
    xu32(j) = xu32(j) + yu32(j)
    yu32(j) = ieor(yu32(j), ieor(ishft(yu32(j), 17), &
                                ishft(yu32(j), -6)))
    xu16 = transfer(xu32(j), 0_int16, 2)
    xu16 = xu16 * nmh_m1_16
    xu32(j) = transfer(xu16, 0_int32)
    xu32(j) = ieor(xu32(j), ieor(ishft(xu32(j), 5), &
                                ishft(xu32(j), -13)))
    xu16 = transfer(xu32(j), 0_int16, 2)
    xu16 = xu16 * nmh_m2_16
    xu32(j) = transfer(xu16, 0_int32)
    xu32(j) = ieor(xu32(j), yu32(j))
    xu32(j) = ieor(xu32(j), ieor(ishft(xu32(j), 11), &
                                ishft(xu32(j), -9)))
    xu16 = transfer(xu32(j), 0_int16, 2)
    xu16 = xu16 * nmh_m3_16
    xu32(j) = transfer(xu16, 0_int32)
    xu32(j) = ieor(xu32(j), ieor(ishft(xu32(j), -10), &
                                ishft(xu32(j), -20)))
  end do
  xu32(0) = ieor(xu32(0), nmh_prime32_1)
  xu32(1) = ieor(xu32(1), nmh_prime32_2)
  xu32(2) = ieor(xu32(2), nmh_prime32_3)
  xu32(3) = ieor(xu32(3), nmh_prime32_4)
  do j = 1, 3
    xu32(0) = xu32(0) + xu32(j)
  end do
  xu32(0) = ieor(xu32(0), s1 + ishft(s1, -5))
  xu16 = transfer(xu32(0), 0_int16, 2)
  xu16 = xu16 * nmh_m3_16
  xu32(0) = transfer(xu16, 0_int32)
  xu32(0) = ieor(xu32(0), &
    ieor(ishft(xu32(0), -10), ishft(xu32(0), -20)))
  result = xu32(0)

end function nmhash32_9to255

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function nmhash32_9to32(p, seed) result(ans)
  INTEGER(int8), intent(in) :: p(0:)
  INTEGER(Int32), intent(in) :: seed
  INTEGER(Int32) :: ans
  ans = nmhash32_9to255(p, seed, .false.)
end function nmhash32_9to32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function nmhash32_33to255(p, seed) result(ans)
  INTEGER(int8), intent(in) :: p(0:)
  INTEGER(Int32), intent(in) :: seed
  INTEGER(Int32) :: ans
  ans = nmhash32_9to255(p, seed, .true.)
end function nmhash32_33to255

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure subroutine nmhash32_long_round(accx, accy, p)
  INTEGER(Int32), intent(inout) :: accx(0:)
  INTEGER(Int32), intent(inout) :: accy(0:)
  INTEGER(int8), intent(in) :: p(0:)

  INTEGER(int64), PARAMETER :: nbgroups = init_size
  INTEGER(int64) :: i
  INTEGER(int16) :: dummy1(0:1)
  INTEGER(int16) :: dummy2(0:1)

  do i = 0, nbgroups - 1
    accx(i) = ieor(accx(i), nmh_readle32(p(i * 4:)))
    accy(i) = ieor(accy(i), nmh_readle32(p(i * 4 + nbgroups * 4:)))
    accx(i) = accx(i) + accy(i)
    accy(i) = ieor(accy(i), ishft(accx(i), -1))
    dummy1 = transfer(accx(i), 0_int16, 2)
    dummy2 = transfer(nmh_m1_v(i), 0_int16, 2)
    dummy1 = dummy1 * dummy2
    accx(i) = transfer(dummy1, 0_int32)
    accx(i) = ieor(accx(i), ieor(ishft(accx(i), 5), &
                                ishft(accx(i), -13)))
    dummy1 = transfer(accx(i), 0_int16, 2)
    dummy2 = transfer(nmh_m2_v(i), 0_int16, 2)
    dummy1 = dummy1 * dummy2
    accx(i) = transfer(dummy1, 0_int32)
    accx(i) = ieor(accx(i), accy(i))
    accx(i) = ieor(accx(i), ieor(ishft(accx(i), 11), &
                                ishft(accx(i), -9)))
    dummy1 = transfer(accx(i), 0_int16, 2)
    dummy2 = transfer(nmh_m3_v(i), 0_int16, 2)
    dummy1 = dummy1 * dummy2
    accx(i) = transfer(dummy1, 0_int32)
    accx(i) = ieor(accx(i), ieor(ishft(accx(i), -10), &
                                ishft(accx(i), -20)))
  end do

end subroutine nmhash32_long_round

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function nmhash32_long(p, seed) result(sum)
  INTEGER(Int32) :: sum
  INTEGER(int8), intent(in) :: p(0:)
  INTEGER(Int32), intent(in) :: seed
  !!
  INTEGER(Int32) :: accx(0:size(nmh_acc_init) - 1)
  INTEGER(Int32) :: accy(0:size(nmh_acc_init) - 1)
  INTEGER(int64) :: nbrounds
  INTEGER(int64) :: len
  INTEGER(Int32) :: len32(0:1)
  INTEGER(int64) :: i
  !!
  len = size(p, kind=int64)
  nbrounds = (len - 1) / (4 * size(accx, kind=int64) * 2)
  sum = 0
  !!
  !  Init
  do i = 0_int64, size(nmh_acc_init, kind=int64) - 1
    accx(i) = nmh_acc_init(i)
    accy(i) = seed
  end do
  !!
  ! init
  do i = 0_int64, nbrounds - 1
    call nmhash32_long_round(accx, accy, &
      & p(i * 8 * size(accx, kind=int64):))
  end do
  call nmhash32_long_round(accx, accy, &
      & p(len - 8 * size(accx, kind=int64):))
  !!
  ! merge acc
  do i = 0, size(accx, kind=int64) - 1
    accx(i) = ieor(accx(i), nmh_acc_init(i))
    sum = sum + accx(i)
  end do
  !!
  len32 = transfer(len, 0_int32, 2)
  if (little_endian) then
    sum = sum + len32(1)
    sum = ieor(sum, len32(0))
  else
    sum = sum + len32(0)
    sum = ieor(sum, len32(1))
  end if
  !!
end function nmhash32_long

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function nmhash32_avalanche32(x) result(u32)
  INTEGER(Int32) :: u32
  INTEGER(Int32), intent(in) :: x
  !!
  INTEGER(int16) :: u16(0:1)
  INTEGER(Int32), PARAMETER :: m1 = INT(z'CCE5196D', Int32)
  INTEGER(Int32), PARAMETER :: m2 = INT(z'464BE229', Int32)
  ! Due to an issue with Intel OneAPI ifort 2021 (see
  ! https://community.intel.com/t5/Intel-Fortran-Compiler/
  ! Intrinsic-transfer-with-a-provided-size-un-expected-behavior/m-p/
  ! 1343313#M158733
  ! ), it is not possible to define the following variables as a PARAMETER.
  !INTEGER(int16), PARAMETER:: m1_16(0:1) = transfer(m1, 0_int16, 2)
  !INTEGER(int16), PARAMETER:: m2_16(0:1) = transfer(m2, 0_int16, 2)
  INTEGER(int16) :: m1_16(0:1), m2_16(0:1)
  ! [-21 -8 cce5196d 12 -7 464be229 -21 -8] = 3.2267098842182733
  !!
  m1_16(0:1) = transfer(m1, 0_int16, 2)
  m2_16(0:1) = transfer(m2, 0_int16, 2)
  !!
  u32 = x
  u32 = ieor(u32, ieor(ishft(u32, -8), ishft(u32, -21)))
  u16 = transfer(u32, 0_int16, 2)
  u16(0) = u16(0) * m1_16(0)
  u16(1) = u16(1) * m1_16(1)
  u32 = transfer(u16, 0_int32)
  u32 = ieor(u32, ieor(ishft(u32, 12), ishft(u32, -7)))
  u16 = transfer(u32, 0_int16, 2)
  u16(0) = u16(0) * m2_16(0)
  u16(1) = u16(1) * m2_16(1)
  u32 = transfer(u16, 0_int32)
  u32 = ieor(u32, ieor(ishft(u32, -8), ishft(u32, -21)))
  !!
end function nmhash32_avalanche32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function nmhash32x_0to4(x, seed) result(hash)
  INTEGER(Int32), intent(in) :: x
  INTEGER(Int32), intent(in) :: seed
  INTEGER(Int32) :: hash
  !!
  !! [bdab1ea9 18 a7896a1b 12 83796a2d 16] = 0.092922873297662509
  !!
  hash = x
  hash = ieor(hash, seed)
  hash = hash * INT(z'BDAB1EA9', Int32)
  hash = hash + ishftc(seed, 31)
  hash = ieor(hash, ishft(hash, -18))
  hash = hash * INT(z'A7896A1B', Int32)
  hash = ieor(hash, ishft(hash, -12))
  hash = hash * INT(z'83796A2D', Int32)
  hash = ieor(hash, ishft(hash, -16))
  !!
end function nmhash32x_0to4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function nmhash32x_5to8(p, seed) result(x)
  INTEGER(int8), intent(in) :: p(0:)
  INTEGER(Int32), intent(in) :: seed
  INTEGER(Int32) :: x
  !!
  !! internal variables
  !!
  INTEGER(int64) :: len
  INTEGER(Int32) :: y
  !
  ! 5 to 9 bytes
  ! mixer: [11049a7d 23 bcccdc7b 12 065e9dad 12] = 0.16577596555667246
  !
  len = size(p, kind=int64)
  x = ieor(nmh_readle32(p), nmh_prime32_3)
  y = ieor(nmh_readle32(p(len - 4:)), seed)
  x = x + y
  x = ieor(x, ishft(x, -len))
  x = x * INT(z'11049A7D', Int32)
  x = ieor(x, ishft(x, -23))
  x = x * INT(z'BCCCDC7B', Int32)
  x = ieor(x, ishftc(y, 3))
  x = ieor(x, ishft(x, -12))
  x = x * INT(z'065E9DAD', Int32)
  x = ieor(x, ishft(x, -12))
end function nmhash32x_5to8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function nmhash32x_9to255(p, seed) result(x)
  INTEGER(int8), intent(in) :: p(0:)
  INTEGER(Int32), intent(in) :: seed
  INTEGER(Int32) :: x
  !!
  !! internal variables
  !!
  INTEGER(int64) :: len
  INTEGER(Int32) :: len32(0:1), len_base
  INTEGER(Int32) :: y
  INTEGER(Int32) :: a, b
  INTEGER(int64) :: i, r
  !!
  ! - at least 9 bytes
  ! - base mixer: [11049a7d 23 bcccdc7b 12 065e9dad 12] = 0.16577596555667246
  ! - tail mixer: [16 a52fb2cd 15 551e4d49 16] = 0.17162579707098322
  !!
  len = size(p, kind=int64)
  len32 = transfer(len, 0_int32, 2)
  if (little_endian) then
    len_base = len32(0)
  else
    len_base = len32(1)
  end if
  x = nmh_prime32_3
  y = seed
  a = nmh_prime32_4
  b = seed
  r = (len - 1) / 16
  !!
  do i = 0, r - 1
    x = ieor(x, nmh_readle32(p(i * 16 + 0:)))
    y = ieor(y, nmh_readle32(p(i * 16 + 4:)))
    x = ieor(x, y)
    x = x * INT(z'11049A7D', Int32)
    x = ieor(x, ishft(x, -23))
    x = x * INT(z'BCCCDC7B', Int32)
    y = ishftc(y, 4)
    x = ieor(x, y)
    x = ieor(x, ishft(x, -12))
    x = x * INT(z'065E9DAD', Int32)
    x = ieor(x, ishft(x, -12))

    a = ieor(a, nmh_readle32(p(i * 16 + 8:)))
    b = ieor(b, nmh_readle32(p(i * 16 + 12:)))
    a = ieor(a, b)
    a = a * INT(z'11049A7D', Int32)
    a = ieor(a, ishft(a, -23))
    a = a * INT(z'BCCCDC7B', Int32)
    b = ishftc(b, 3)
    a = ieor(a, b)
    a = ieor(a, ishft(a, -12))
    a = a * INT(z'065E9DAD', Int32)
    a = ieor(a, ishft(a, -12))
  end do
  !!
  if (iand(len_base - 1_int32, 8_int32) /= 0) then
    if (iand(len_base - 1_int32, 4_int32) /= 0) then
      a = ieor(a, nmh_readle32(p(r * 16 + 0:)))
      b = ieor(b, nmh_readle32(p(r * 16 + 4:)))
      a = ieor(a, b)
      a = a * INT(z'11049A7D', Int32)
      a = ieor(a, ishft(a, -23))
      a = a * INT(z'BCCCDC7B', Int32)
      a = ieor(a, ishftc(b, 4))
      a = ieor(a, ishft(a, -12))
      a = a * INT(z'065E9DAD', Int32)
    else
      a = ieor(a, nmh_readle32(p(r * 16:)) + b)
      a = ieor(a, ishft(a, -16))
      a = a * INT(z'A52FB2CD', Int32)
      a = ieor(a, ishft(a, -15))
      a = a * INT(z'551E4D49', Int32)
    end if
    x = ieor(x, nmh_readle32(p(len - 8:)))
    y = ieor(y, nmh_readle32(p(len - 4:)))
    x = ieor(x, y)
    x = x * INT(z'11049A7D', Int32)
    x = ieor(x, ishft(x, -23))
    x = x * INT(z'BCCCDC7B', Int32);
    x = ieor(x, ishftc(y, 3))
    x = ieor(x, ishft(x, -12))
    x = x * INT(z'065E9DAD', Int32)
  else
    if (iand(len_base - 1_int32, 4_int32) /= 0) then
      a = ieor(a, nmh_readle32(p(r * 16:)) + b)
      a = ieor(a, ishft(a, -16))
      a = a * INT(z'A52FB2CD', Int32)
      a = ieor(a, ishft(a, -15))
      a = a * INT(z'551E4D49', Int32)
    end if
    x = ieor(x, nmh_readle32(p(len - 4:)) + y)
    x = ieor(x, ishft(x, -16))
    x = x * INT(z'A52FB2CD', Int32)
    x = ieor(x, ishft(x, -15))
    x = x * INT(z'551E4D49', Int32)
  end if
  !!
  x = ieor(x, len_base)
  x = ieor(x, ishftc(a, 27)) ! rotate one lane to pass Diff test
  x = ieor(x, ishft(x, -14))
  x = x * INT(z'141CC535', Int32)
  !!
end function nmhash32x_9to255

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function nmhash32x_avalanche32(x) result(hash)
  INTEGER(Int32) :: hash
  INTEGER(Int32), intent(in) :: x
  ! Mixer with 2 mul from skeeto/hash-prospector:
  ! [15 d168aaad 15 af723597 15] = 0.15983776156606694
  hash = x
  hash = ieor(hash, ishft(hash, -15))
  hash = hash * INT(z'D168AAAD', Int32)
  hash = ieor(hash, ishft(hash, -15))
  hash = hash * INT(z'AF723597', Int32)
  hash = ieor(hash, ishft(hash, -15))
end function nmhash32x_avalanche32

END SUBMODULE nmMethods
