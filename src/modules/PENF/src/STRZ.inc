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

elemental function strz_I8P(n, nz_pad) result(str)
  !< Converting integer to string, prefixing with the right number of zeros.
  !<
  !<```fortran
  !< use penf
  !< print "(A)", strz(n=1_I8P)
  !<```
  !=> 0000000000000000001 <<<
  !<
  !<```fortran
  !< use penf
  !< print "(A)", strz(n=1_I8P, nz_pad=5)
  !<```
  !=> 00001 <<<
  integer(I8P), intent(in) :: n      !< Integer to be converted.
  integer(I4P), intent(in), optional :: nz_pad !< Number of zeros padding.
  character(DI8P) :: str    !< Returned string containing input number plus padding zeros.

  write (str, FI8PZP) n                              ! Casting of n to string.
  str = str(2:)                                      ! Leaving out the sign.
  if (present(nz_pad)) str = str(DI8P - nz_pad:DI8P - 1) ! Leaving out the extra zeros padding
end function strz_I8P

elemental function strz_I4P(n, nz_pad) result(str)
  !< Convert integer to string, prefixing with the right number of zeros.
  !<
  !<```fortran
  !< use penf
  !< print "(A)", strz(n=1_I4P)
  !<```
  !=> 0000000001 <<<
  !<
  !<```fortran
  !< use penf
  !< print "(A)", strz(n=1_I4P, nz_pad=5)
  !<```
  !=> 00001 <<<
  integer(I4P), intent(in) :: n      !< Integer to be converted.
  integer(I4P), intent(in), optional :: nz_pad !< Number of zeros padding.
  character(DI4P) :: str    !< Returned string containing input number plus padding zeros.

  write (str, FI4PZP) n                              ! Casting of n to string.
  str = str(2:)                                      ! Leaving out the sign.
  if (present(nz_pad)) str = str(DI4P - nz_pad:DI4P - 1) ! Leaving out the extra zeros padding
end function strz_I4P

elemental function strz_I2P(n, nz_pad) result(str)
  !< Convert integer to string, prefixing with the right number of zeros.
  !<
  !<```fortran
  !< use penf
  !< print "(A)", strz(n=1_I2P)
  !<```
  !=> 00001 <<<
  !<
  !<```fortran
  !< use penf
  !< print "(A)", strz(n=1_I2P, nz_pad=3)
  !<```
  !=> 001 <<<
  integer(I2P), intent(in) :: n      !< Integer to be converted.
  integer(I4P), intent(in), optional :: nz_pad !< Number of zeros padding.
  character(DI2P) :: str    !< Returned string containing input number plus padding zeros.

  write (str, FI2PZP) n                              ! Casting of n to string.
  str = str(2:)                                      ! Leaving out the sign.
  if (present(nz_pad)) str = str(DI2P - nz_pad:DI2P - 1) ! Leaving out the extra zeros padding
end function strz_I2P

elemental function strz_I1P(n, nz_pad) result(str)
  !< Convert integer to string, prefixing with the right number of zeros.
  !<
  !<```fortran
  !< use penf
  !< print "(A)", strz(n=1_I1P)
  !<```
  !=> 001 <<<
  !<
  !<```fortran
  !< use penf
  !< print "(A)", strz(n=1_I1P, nz_pad=3)
  !<```
  !=> 001 <<<
  integer(I1P), intent(in) :: n      !< Integer to be converted.
  integer(I4P), intent(in), optional :: nz_pad !< Number of zeros padding.
  character(DI1P) :: str    !< Returned string containing input number plus padding zeros.

  write (str, FI1PZP) n                              ! Casting of n to string.
  str = str(2:)                                      ! Leaving out the sign.
  if (present(nz_pad)) str = str(DI1P - nz_pad:DI1P - 1) ! Leaving out the extra zeros padding
end function strz_I1P
