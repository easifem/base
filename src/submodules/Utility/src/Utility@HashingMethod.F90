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

SUBMODULE( Utility ) HashingMethod
IMPLICIT NONE
CONTAINS
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! C code from the refer to
! https://cp-algorithms.com/string/string-hashing.html
! long long compute_hash(string const& s) {
!     const int p = 31;
!     const int m = 1e9 + 9;
!     long long hash_value = 0;
!     long long p_pow = 1;
!     for (char c : s) {
!         hash_value = (hash_value + (c - 'a' + 1) * p_pow) % m;
!         p_pow = (p_pow * p) % m;
!     }
!     return hash_value;
! }

MODULE PROCEDURE StringToUID_PolyRoll
  INTEGER( I4B ), PARAMETER :: p = 53
  INTEGER( I4B ), PARAMETER :: m = 1e6 + 9
  INTEGER( I4B ) :: p_pow, ii, aa

  p_pow = 1
  ans = 0

  DO ii = 1, LEN_TRIM( charVar )
    ans = MOD( (ans + (ICHAR(charVar(ii:ii)) - ICHAR('A') + 1) * p_pow ), m )
    p_pow = MOD( (p_pow * p), m )
  END DO
END PROCEDURE StringToUID_PolyRoll

END SUBMODULE HashingMethod