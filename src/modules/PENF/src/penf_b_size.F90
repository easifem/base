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

MODULE penf_b_size
!< PENF bit/byte size functions.
USE penf_global_parameters_variables

IMPLICIT NONE
PRIVATE
SAVE
PUBLIC :: bit_size, byte_size

INTERFACE bit_size
  !< Overloading of the intrinsic *bit_size* function for computing the number of bits of (also) real and character variables.
  MODULE PROCEDURE &
#if defined _R16P
    bit_size_R16P, &
#endif
    bit_size_R8P, &
    bit_size_R4P, &
    bit_size_chr
END INTERFACE

INTERFACE byte_size
  !< Compute the number of bytes of a variable.
  MODULE PROCEDURE &
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
END INTERFACE

CONTAINS
ELEMENTAL FUNCTION bit_size_R16P(i) RESULT(bits)
  !< Compute the number of bits of a real variable.
  !<
  !<```fortran
  !< use penf
  !< print FI2P, bit_size(1._R16P)
  !<```
  !=> 128 <<<
  REAL(R16P), INTENT(in) :: i !< Real variable whose number of bits must be computed.
  INTEGER(I2P) :: bits !< Number of bits of r.
  INTEGER(I1P) :: mold(1) !< "Molding" dummy variable for bits counting.

  bits = SIZE(TRANSFER(i, mold), dim=1, kind=I2P) * 8_I2P
END FUNCTION bit_size_R16P

ELEMENTAL FUNCTION bit_size_R8P(i) RESULT(bits)
  !< Compute the number of bits of a real variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, bit_size(1._R8P)
  !<```
  !=> 64 <<<
  REAL(R8P), INTENT(in) :: i !< Real variable whose number of bits must be computed.
  INTEGER(I1P) :: bits !< Number of bits of r.
  INTEGER(I1P) :: mold(1) !< "Molding" dummy variable for bits counting.

  bits = SIZE(TRANSFER(i, mold), dim=1, kind=I1P) * 8_I1P
END FUNCTION bit_size_R8P

ELEMENTAL FUNCTION bit_size_R4P(i) RESULT(bits)
  !< Compute the number of bits of a real variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, bit_size(1._R4P)
  !<```
  !=> 32 <<<
  REAL(R4P), INTENT(in) :: i !< Real variable whose number of bits must be computed.
  INTEGER(I1P) :: bits !< Number of bits of r.
  INTEGER(I1P) :: mold(1) !< "Molding" dummy variable for bits counting.

  bits = SIZE(TRANSFER(i, mold), dim=1, kind=I1P) * 8_I1P
END FUNCTION bit_size_R4P

ELEMENTAL FUNCTION bit_size_chr(i) RESULT(bits)
  !< Compute the number of bits of a character variable.
  !<
  !<```fortran
  !< use penf
  !< print FI4P, bit_size('ab')
  !<```
  !=> 16 <<<
  CHARACTER(*), INTENT(IN) :: i !< Character variable whose number of bits must be computed.
  INTEGER(I4P) :: bits !< Number of bits of c.
  INTEGER(I1P) :: mold(1) !< "Molding" dummy variable for bits counting.

  bits = SIZE(TRANSFER(i, mold), dim=1, kind=I4P) * 8_I4P
END FUNCTION bit_size_chr

ELEMENTAL FUNCTION byte_size_R16P(i) RESULT(bytes)
  !< Compute the number of bytes of a real variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, byte_size(1._R16P)
  !<```
  !=> 16 <<<
  REAL(R16P), INTENT(in) :: i !< Real variable whose number of bytes must be computed.
  INTEGER(I1P) :: bytes !< Number of bytes of r.

  bytes = BIT_SIZE(i) / 8_I1P
END FUNCTION byte_size_R16P

ELEMENTAL FUNCTION byte_size_R8P(i) RESULT(bytes)
  !< Compute the number of bytes of a real variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, byte_size(1._R8P)
  !<```
  !=> 8 <<<
  REAL(R8P), INTENT(in) :: i !< Real variable whose number of bytes must be computed.
  INTEGER(I1P) :: bytes !< Number of bytes of r.

  bytes = BIT_SIZE(i) / 8_I1P
END FUNCTION byte_size_R8P

ELEMENTAL FUNCTION byte_size_R4P(i) RESULT(bytes)
  !< Compute the number of bytes of a real variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, byte_size(1._R4P)
  !<```
  !=> 4 <<<
  REAL(R4P), INTENT(in) :: i !< Real variable whose number of bytes must be computed.
  INTEGER(I1P) :: bytes !< Number of bytes of r.

  bytes = BIT_SIZE(i) / 8_I1P
END FUNCTION byte_size_R4P

ELEMENTAL FUNCTION byte_size_chr(i) RESULT(bytes)
  !< Compute the number of bytes of a character variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, byte_size('ab')
  !<```
  !=> 2 <<<
  CHARACTER(*), INTENT(in) :: i !< Character variable whose number of bytes must be computed.
  INTEGER(I4P) :: bytes !< Number of bytes of c.

  bytes = BIT_SIZE(i) / 8_I4P
END FUNCTION byte_size_chr

ELEMENTAL FUNCTION byte_size_I8P(i) RESULT(bytes)
  !< Compute the number of bytes of an integer variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, byte_size(1_I8P)
  !<```
  !=> 8 <<<
  INTEGER(I8P), INTENT(in) :: i !< Integer variable whose number of bytes must be computed.
  INTEGER(I1P) :: bytes !< Number of bytes of i.

  bytes = BIT_SIZE(i) / 8_I1P
END FUNCTION byte_size_I8P

ELEMENTAL FUNCTION byte_size_I4P(i) RESULT(bytes)
  !< Compute the number of bytes of an integer variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, byte_size(1_I4P)
  !<```
  !=> 4 <<<
  INTEGER(I4P), INTENT(in) :: i !< Integer variable whose number of bytes must be computed.
  INTEGER(I1P) :: bytes !< Number of bytes of i.

  bytes = BIT_SIZE(i) / 8_I1P
END FUNCTION byte_size_I4P

ELEMENTAL FUNCTION byte_size_I2P(i) RESULT(bytes)
  !< Compute the number of bytes of an integer variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, byte_size(1_I2P)
  !<```
  !=> 2 <<<
  INTEGER(I2P), INTENT(in) :: i !< Integer variable whose number of bytes must be computed.
  INTEGER(I1P) :: bytes !< Number of bytes of i.

  bytes = BIT_SIZE(i) / 8_I1P
END FUNCTION byte_size_I2P

ELEMENTAL FUNCTION byte_size_I1P(i) RESULT(bytes)
  !< Compute the number of bytes of an integer variable.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, byte_size(1_I1P)
  !<```
  !=> 1 <<<
  INTEGER(I1P), INTENT(in) :: i !< Integer variable whose number of bytes must be computed.
  INTEGER(I1P) :: bytes !< Number of bytes of i.

  bytes = BIT_SIZE(i) / 8_I1P
END FUNCTION byte_size_I1P
endmodule penf_b_size
