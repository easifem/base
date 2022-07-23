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

elemental function bstr_R16P(n) result(bstr)
  !< Convert real to string of bits.
  !<
  !< @note It is assumed that R16P is represented by means of 128 bits, but this is not ensured in all architectures.
  !<
  !<```fortran
  !< use penf
  !< character(128) :: b
  !< b = bstr(n=1._R16P)
  !< print "(A)", b(17:)
  !<```
  !=> 0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001111111100111111 <<<
  real(R16P), intent(in) :: n          !< Real to be converted.
  character(128) :: bstr       !< Returned bit-string containing input number.
  integer(I1P) :: buffer(16) !< Transfer buffer.

  buffer = transfer(n, buffer)
  write (bstr, '(16B8.8)') buffer
end function bstr_R16P

elemental function bstr_R8P(n) result(bstr)
  !< Convert real to string of bits.
  !<
  !< @note It is assumed that R8P is represented by means of 64 bits, but this is not ensured in all architectures.
  !<
  !<```fortran
  !< use penf
  !< print "(A)", bstr(n=1._R8P)
  !<```
  !=> 0000000000000000000000000000000000000000000000001111000000111111 <<<
  real(R8P), intent(in) :: n         !< Real to be converted.
  character(64) :: bstr      !< Returned bit-string containing input number.
  integer(I1P) :: buffer(8) !< Transfer buffer.

  buffer = transfer(n, buffer)
  write (bstr, '(8B8.8)') buffer
end function bstr_R8P

elemental function bstr_R4P(n) result(bstr)
  !< Convert real to string of bits.
  !<
  !< @note It is assumed that R4P is represented by means of 32 bits, but this is not ensured in all architectures.
  !<
  !<```fortran
  !< use penf
  !< print "(A)", bstr(n=1._R4P)
  !<```
  !=> 00000000000000001000000000111111 <<<
  real(R4P), intent(in) :: n         !< Real to be converted.
  character(32) :: bstr      !< Returned bit-string containing input number.
  integer(I1P) :: buffer(4) !< Transfer buffer.

  buffer = transfer(n, buffer)
  write (bstr, '(4B8.8)') buffer
end function bstr_R4P

elemental function bstr_I8P(n) result(bstr)
  !< Convert integer to string of bits.
  !<
  !< @note It is assumed that I8P is represented by means of 64 bits, but this is not ensured in all architectures.
  !<
  !<```fortran
  !< use penf
  !< print "(A)", bstr(n=1_I8P)
  !<```
  !=> 0000000000000000000000000000000000000000000000000000000000000001 <<<
  integer(I8P), intent(in) :: n    !< Real to be converted.
  character(64) :: bstr !< Returned bit-string containing input number.

  write (bstr, '(B64.64)') n
end function bstr_I8P

elemental function bstr_I4P(n) result(bstr)
  !< Convert integer to string of bits.
  !<
  !< @note It is assumed that I4P is represented by means of 32 bits, but this is not ensured in all architectures.
  !<
  !<```fortran
  !< use penf
  !< print "(A)", bstr(n=1_I4P)
  !<```
  !=> 00000000000000000000000000000001 <<<
  integer(I4P), intent(in) :: n    !< Real to be converted.
  character(32) :: bstr !< Returned bit-string containing input number.

  write (bstr, '(B32.32)') n
end function bstr_I4P

elemental function bstr_I2P(n) result(bstr)
  !< Convert integer to string of bits.
  !<
  !< @note It is assumed that I2P is represented by means of 16 bits, but this is not ensured in all architectures.
  !<
  !<```fortran
  !< use penf
  !< print "(A)", bstr(n=1_I2P)
  !<```
  !=> 0000000000000001 <<<
  integer(I2P), intent(in) :: n    !< Real to be converted.
  character(16) :: bstr !< Returned bit-string containing input number.

  write (bstr, '(B16.16)') n
end function bstr_I2P

elemental function bstr_I1P(n) result(bstr)
  !< Convert integer to string of bits.
  !<
  !< @note It is assumed that I1P is represented by means of 8 bits, but this is not ensured in all architectures.
  !<
  !<```fortran
  !< use penf
  !< print "(A)", bstr(n=1_I1P)
  !<```
  !=> 00000001 <<<
  integer(I1P), intent(in) :: n    !< Real to be converted.
  character(8) :: bstr !< Returned bit-string containing input number.

  write (bstr, '(B8.8)') n
end function bstr_I1P
