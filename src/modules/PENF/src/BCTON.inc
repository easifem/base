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

elemental function bctor_R16P(bstr, knd) result(n)
  !< Convert bit-string to real.
  !<
  !<```fortran
  !< use penf
  !< print FR16P, bcton('00000000000000000000000000000000000000000000000000000000000000000000000000000'//&
  !<                    '000000000000000000000000000000000001111111100111111', knd=1._R16P)
  !<```
  !=> 0.100000000000000000000000000000000E+0001 <<<
  character(*), intent(in) :: bstr       !< String containing input number.
  real(R16P), intent(in) :: knd        !< Number kind.
  real(R16P) :: n          !< Number returned.
  integer(I1P) :: buffer(16) !< Transfer buffer.

  read (bstr, '(16B8.8)') buffer
  n = transfer(buffer, n)
end function bctor_R16P

elemental function bctor_R8P(bstr, knd) result(n)
  !< Convert bit-string to real.
  !<
  !<```fortran
  !< use penf
  !< print FR8P, bcton('0000000000000000000000000000000000000000000000001111000000111111', knd=1._R8P)
  !<```
  !=> 0.100000000000000E+001 <<<
  character(*), intent(in) :: bstr      !< String containing input number.
  real(R8P), intent(in) :: knd       !< Number kind.
  real(R8P) :: n         !< Number returned.
  integer(I1P) :: buffer(8) !< Transfer buffer.

  read (bstr, '(8B8.8)') buffer
  n = transfer(buffer, n)
end function bctor_R8P

elemental function bctor_R4P(bstr, knd) result(n)
  !< Convert bit-string to real.
  !<
  !<```fortran
  !< use penf
  !< print FR4P, bcton('00000000000000001000000000111111', knd=1._R4P)
  !<```
  !=> 0.100000E+01 <<<
  character(*), intent(in) :: bstr      !< String containing input number.
  real(R4P), intent(in) :: knd       !< Number kind.
  real(R4P) :: n         !< Number returned.
  integer(I1P) :: buffer(4) !< Transfer buffer.

  read (bstr, '(4B8.8)') buffer
  n = transfer(buffer, n)
end function bctor_R4P

elemental function bctoi_I8P(bstr, knd) result(n)
  !< Convert bit-string to integer.
  !<
  !<```fortran
  !< use penf
  !< print FI8P, bcton('0000000000000000000000000000000000000000000000000000000000000001', knd=1_I8P)
  !<```
  !=> 1 <<<
  character(*), intent(in) :: bstr !< String containing input number.
  integer(I8P), intent(in) :: knd  !< Number kind.
  integer(I8P) :: n    !< Number returned.

read(bstr,'(B'//trim(str(bit_size(knd), .true.))//'.'//trim(str(bit_size(knd), .true.))//')') n
end function bctoi_I8P

elemental function bctoi_I4P(bstr, knd) result(n)
  !< Convert bit-string to integer.
  !<
  !<```fortran
  !< use penf
  !< print FI4P, bcton('00000000000000000000000000000001', knd=1_I4P)
  !<```
  !=> 1 <<<
  character(*), intent(in) :: bstr !< String containing input number.
  integer(I4P), intent(in) :: knd  !< Number kind.
  integer(I4P) :: n    !< Number returned.

read(bstr,'(B'//trim(str(bit_size(knd), .true.))//'.'//trim(str(bit_size(knd), .true.))//')') n
end function bctoi_I4P

elemental function bctoi_I2P(bstr, knd) result(n)
  !< Convert bit-string to integer.
  !<
  !<```fortran
  !< use penf
  !< print FI2P, bcton('0000000000000001', knd=1_I2P)
  !<```
  !=> 1 <<<
  character(*), intent(in) :: bstr !< String containing input number.
  integer(I2P), intent(in) :: knd  !< Number kind.
  integer(I2P) :: n    !< Number returned.

read(bstr,'(B'//trim(str(bit_size(knd), .true.))//'.'//trim(str(bit_size(knd), .true.))//')') n
end function bctoi_I2P

elemental function bctoi_I1P(bstr, knd) result(n)
  !< Convert bit-string to integer.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, bcton('00000001', knd=1_I1P)
  !<```
  !=> 1 <<<
  character(*), intent(in) :: bstr !< String containing input number.
  integer(I1P), intent(in) :: knd  !< Number kind.
  integer(I1P) :: n    !< Number returned.

read(bstr,'(B'//trim(str(bit_size(knd), .true.))//'.'//trim(str(bit_size(knd), .true.))//')') n
end function bctoi_I1P
