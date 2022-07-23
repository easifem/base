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

!< PENF bit/byte size functions.

module penf_b_size
!< PENF bit/byte size functions.
use penf_global_parameters_variables

implicit none
private
save
public :: bit_size, byte_size

interface bit_size
  !< Overloading of the intrinsic *bit_size* function for computing the number of bits of (also) real and character variables.
  module procedure &
#if defined _R16P
    bit_size_R16P, &
#endif
    bit_size_R8P, &
    bit_size_R4P, &
    bit_size_chr
end interface

interface byte_size
  !< Compute the number of bytes of a variable.
  module procedure &
    byte_size_I8P, &
    byte_size_I4P, &
    byte_size_I2P, &
    byte_size_I1P, &
#if defined _R16P
    byte_size_R16P, &
#endif
    byte_size_R8P, &
    byte_size_R4P, &
    byte_size_chr
end interface

contains
elemental function bit_size_R16P(i) result(bits)
  !< Compute the number of bits of a real variable.
  !<
  !<```fortran
  !< use penf
  !< print FI2P, bit_size(1._R16P)
  !<```
  !=> 128 <<<
  real(R16P), intent(in) :: i       !< Real variable whose number of bits must be computed.
  integer(I2P) :: bits    !< Number of bits of r.
  integer(I1P) :: mold(1) !< "Molding" dummy variable for bits counting.

  bits = size(transfer(i, mold), dim=1, kind=I2P) * 8_I2P
end function bit_size_R16P

elemental function bit_size_R8P(i) result(bits)
  !< Compute the number of bits of a real variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, bit_size(1._R8P)
  !<```
  !=> 64 <<<
  real(R8P), intent(in) :: i       !< Real variable whose number of bits must be computed.
  integer(I1P) :: bits    !< Number of bits of r.
  integer(I1P) :: mold(1) !< "Molding" dummy variable for bits counting.

  bits = size(transfer(i, mold), dim=1, kind=I1P) * 8_I1P
end function bit_size_R8P

elemental function bit_size_R4P(i) result(bits)
  !< Compute the number of bits of a real variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, bit_size(1._R4P)
  !<```
  !=> 32 <<<
  real(R4P), intent(in) :: i       !< Real variable whose number of bits must be computed.
  integer(I1P) :: bits    !< Number of bits of r.
  integer(I1P) :: mold(1) !< "Molding" dummy variable for bits counting.

  bits = size(transfer(i, mold), dim=1, kind=I1P) * 8_I1P
end function bit_size_R4P

elemental function bit_size_chr(i) result(bits)
  !< Compute the number of bits of a character variable.
  !<
  !<```fortran
  !< use penf
  !< print FI4P, bit_size('ab')
  !<```
  !=> 16 <<<
  character(*), intent(IN) :: i       !< Character variable whose number of bits must be computed.
  integer(I4P) :: bits    !< Number of bits of c.
  integer(I1P) :: mold(1) !< "Molding" dummy variable for bits counting.

  bits = size(transfer(i, mold), dim=1, kind=I4P) * 8_I4P
end function bit_size_chr

elemental function byte_size_R16P(i) result(bytes)
  !< Compute the number of bytes of a real variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, byte_size(1._R16P)
  !<```
  !=> 16 <<<
  real(R16P), intent(in) :: i     !< Real variable whose number of bytes must be computed.
  integer(I1P) :: bytes !< Number of bytes of r.

  bytes = bit_size(i) / 8_I1P
end function byte_size_R16P

elemental function byte_size_R8P(i) result(bytes)
  !< Compute the number of bytes of a real variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, byte_size(1._R8P)
  !<```
  !=> 8 <<<
  real(R8P), intent(in) :: i     !< Real variable whose number of bytes must be computed.
  integer(I1P) :: bytes !< Number of bytes of r.

  bytes = bit_size(i) / 8_I1P
end function byte_size_R8P

elemental function byte_size_R4P(i) result(bytes)
  !< Compute the number of bytes of a real variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, byte_size(1._R4P)
  !<```
  !=> 4 <<<
  real(R4P), intent(in) :: i     !< Real variable whose number of bytes must be computed.
  integer(I1P) :: bytes !< Number of bytes of r.

  bytes = bit_size(i) / 8_I1P
end function byte_size_R4P

elemental function byte_size_chr(i) result(bytes)
  !< Compute the number of bytes of a character variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, byte_size('ab')
  !<```
  !=> 2 <<<
  character(*), intent(in) :: i     !< Character variable whose number of bytes must be computed.
  integer(I4P) :: bytes !< Number of bytes of c.

  bytes = bit_size(i) / 8_I4P
end function byte_size_chr

elemental function byte_size_I8P(i) result(bytes)
  !< Compute the number of bytes of an integer variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, byte_size(1_I8P)
  !<```
  !=> 8 <<<
  integer(I8P), intent(in) :: i     !< Integer variable whose number of bytes must be computed.
  integer(I1P) :: bytes !< Number of bytes of i.

  bytes = bit_size(i) / 8_I1P
end function byte_size_I8P

elemental function byte_size_I4P(i) result(bytes)
  !< Compute the number of bytes of an integer variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, byte_size(1_I4P)
  !<```
  !=> 4 <<<
  integer(I4P), intent(in) :: i     !< Integer variable whose number of bytes must be computed.
  integer(I1P) :: bytes !< Number of bytes of i.

  bytes = bit_size(i) / 8_I1P
end function byte_size_I4P

elemental function byte_size_I2P(i) result(bytes)
  !< Compute the number of bytes of an integer variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, byte_size(1_I2P)
  !<```
  !=> 2 <<<
  integer(I2P), intent(in) :: i     !< Integer variable whose number of bytes must be computed.
  integer(I1P) :: bytes !< Number of bytes of i.

  bytes = bit_size(i) / 8_I1P
end function byte_size_I2P

elemental function byte_size_I1P(i) result(bytes)
  !< Compute the number of bytes of an integer variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, byte_size(1_I1P)
  !<```
  !=> 1 <<<
  integer(I1P), intent(in) :: i     !< Integer variable whose number of bytes must be computed.
  integer(I1P) :: bytes !< Number of bytes of i.

  bytes = bit_size(i) / 8_I1P
end function byte_size_I1P
endmodule penf_b_size
