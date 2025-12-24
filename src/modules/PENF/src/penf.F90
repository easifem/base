!< Portability Environment for Fortran poor people.

MODULE penf
!< Portability Environment for Fortran poor people.
USE penf_global_parameters_variables
#ifdef __INTEL_COMPILER
USE penf_b_size
#else
USE penf_b_size, ONLY: bit_size, byte_size
#endif
USE penf_stringify, ONLY: str_ascii, str_ucs4, str, strz, cton, bstr, bcton

IMPLICIT NONE
PRIVATE
SAVE
! global parameters and variables
PUBLIC :: endianL, endianB, endian, is_initialized
PUBLIC :: ASCII, UCS4, CK
public :: R16P, FR16P, DR16P, MinR16P, MaxR16P, BIR16P, BYR16P, smallR16P, ZeroR16P
PUBLIC :: R8P, FR8P, DR8P, MinR8P, MaxR8P, BIR8P, BYR8P, smallR8P, ZeroR8P
PUBLIC :: R4P, FR4P, DR4P, MinR4P, MaxR4P, BIR4P, BYR4P, smallR4P, ZeroR4P
PUBLIC :: R_P, FR_P, DR_P, MinR_P, MaxR_P, BIR_P, BYR_P, smallR_P, ZeroR_P
PUBLIC :: I8P, FI8P, DI8P, MinI8P, MaxI8P, BII8P, BYI8P
PUBLIC :: I4P, FI4P, DI4P, MinI4P, MaxI4P, BII4P, BYI4P
PUBLIC :: I2P, FI2P, DI2P, MinI2P, MaxI2P, BII2P, BYI2P
PUBLIC :: I1P, FI1P, DI1P, MinI1P, MaxI1P, BII1P, BYI1P
PUBLIC :: I_P, FI_P, DI_P, MinI_P, MaxI_P, BII_P, BYI_P
PUBLIC :: CHARACTER_KINDS_LIST, REAL_KINDS_LIST, REAL_FORMATS_LIST
PUBLIC :: INTEGER_KINDS_LIST, INTEGER_FORMATS_LIST
! bit/byte size functions
PUBLIC :: bit_size, byte_size
! stringify facility
PUBLIC :: str_ascii, str_ucs4
PUBLIC :: str, strz, cton
PUBLIC :: bstr, bcton
! miscellanea facility
PUBLIC :: check_endian
PUBLIC :: digit
PUBLIC :: penf_Init
PUBLIC :: penf_print

INTEGER, PROTECTED :: endian = endianL !< Bit ordering: Little endian (endianL), or Big endian (endianB).
LOGICAL, PROTECTED :: is_initialized = .FALSE. !< Check the initialization of some variables that must be initialized.

#ifdef __GFORTRAN__
! work-around for strange gfortran bug...
INTERFACE bit_size
  !< Overloading of the intrinsic *bit_size* function for computing the number of bits of (also) real and character variables.
END INTERFACE
#endif

INTERFACE digit
  !< Compute the number of digits in decimal base of the input integer.
  MODULE PROCEDURE digit_I8, digit_I4, digit_I2, digit_I1
END INTERFACE

CONTAINS
! public procedures
SUBROUTINE check_endian()
  !< Check the type of bit ordering (big or little endian) of the running architecture.
  !<
  !> @note The result is stored into the *endian* global variable.
  !<
  !<```fortran
  !< use penf
  !< call check_endian
  !< print *, endian
  !<```
  !=> 1 <<<
  IF (is_little_endian()) THEN
    endian = endianL
  ELSE
    endian = endianB
  END IF
CONTAINS
  PURE FUNCTION is_little_endian() RESULT(is_little)
    !< Check if the type of the bit ordering of the running architecture is little endian.
    LOGICAL :: is_little !< Logical output: true is the running architecture uses little endian ordering, false otherwise.
    INTEGER(I1P) :: int1(1:4) !< One byte integer array for casting 4 bytes integer.

    int1 = TRANSFER(1_I4P, int1)
    is_little = (int1(1) == 1_I1P)
  END FUNCTION is_little_endian
END SUBROUTINE check_endian

SUBROUTINE penf_init()
  !< Initialize PENF's variables that are not initialized into the definition specification.
  !<
  !<```fortran
  !< use penf
  !< call penf_init
  !< print FI1P, BYR4P
  !<```
  !=> 4 <<<

  CALL check_endian
  is_initialized = .TRUE.
END SUBROUTINE penf_init

SUBROUTINE penf_print(unit, pref, iostat, iomsg)
  !< Print to the specified unit the PENF's environment data.
  !<
  !<```fortran
  !< use penf
  !< integer :: u
  !< open(newunit=u, status='scratch')
  !< call penf_print(u)
  !< close(u)
  !< print "(A)", 'done'
  !<```
  !=> done <<<
  INTEGER(I4P), INTENT(in) :: unit !< Logic unit.
  CHARACTER(*), INTENT(in), OPTIONAL :: pref !< Prefixing string.
  INTEGER(I4P), INTENT(out), OPTIONAL :: iostat !< IO error.
  CHARACTER(*), INTENT(out), OPTIONAL :: iomsg !< IO error message.
  CHARACTER(len=:), ALLOCATABLE :: prefd !< Prefixing string.
  INTEGER(I4P) :: iostatd !< IO error.
  CHARACTER(500) :: iomsgd !< Temporary variable for IO error message.

  IF (.NOT. is_initialized) CALL penf_init
  prefd = ''; IF (PRESENT(pref)) prefd = pref
  IF (endian == endianL) THEN
     write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)prefd//'This architecture has LITTLE Endian bit ordering'
  ELSE
     write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)prefd//'This architecture has BIG Endian bit ordering'
  END IF
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'Character kind:'
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  ASCII: '//str(n=ASCII)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  UCS4:  '//str(n=UCS4)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  CK:    '//str(n=CK)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'Reals kind, format and characters number:'
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R16P: '//str(n=R16P)//','//FR16P//','//str(n=DR16P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R8P:  '//str(n=R8P )//','//FR8P //','//str(n=DR8P )
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R4P:  '//str(n=R4P )//','//FR4P //','//str(n=DR4P )
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R_P:  '//str(n=R_P )//','//FR_P //','//str(n=DR_P )
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'Integers kind, format and characters number:'
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I8P:  '//str(n=I8P)//','//FI8P //','//str(n=DI8P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I4P:  '//str(n=I4P)//','//FI4P //','//str(n=DI4P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I2P:  '//str(n=I2P)//','//FI2P //','//str(n=DI2P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I1P:  '//str(n=I1P)//','//FI1P //','//str(n=DI1P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'Reals minimum and maximum values:'
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R16P: '//str(n=MinR16P)//','//str(n=MaxR16P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R8P:  '//str(n=MinR8P )//','//str(n=MaxR8P )
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R4P:  '//str(n=MinR4P )//','//str(n=MaxR4P )
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R_P:  '//str(n=MinR_P )//','//str(n=MaxR_P )
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'Integergs minimum and maximum values:'
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I8P:  '//str(n=MinI8P )//','//str(n=MaxI8P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I4P:  '//str(n=MinI4P )//','//str(n=MaxI4P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I2P:  '//str(n=MinI2P )//','//str(n=MaxI2P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I1P:  '//str(n=MinI1P )//','//str(n=MaxI1P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'Reals bits/bytes sizes:'
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R16P: '//str(n=BIR16P)//'/'//str(n=BYR16P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R8P:  '//str(n=BIR8P )//'/'//str(n=BYR8P )
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R4P:  '//str(n=BIR4P )//'/'//str(n=BYR4P )
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R_P:  '//str(n=BIR_P )//'/'//str(n=BYR_P )
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'Integers bits/bytes sizes:'
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I8P:  '//str(n=BII8P)//'/'//str(n=BYI8P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I4P:  '//str(n=BII4P)//'/'//str(n=BYI4P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I2P:  '//str(n=BII2P)//'/'//str(n=BYI2P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I1P:  '//str(n=BII1P)//'/'//str(n=BYI1P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'Smallest reals'
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  smallR16P: '//str(smallR16P, .true.)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  smallR8P:  '//str(smallR8P,  .true.)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  smallR4P:  '//str(smallR4P,  .true.)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  smallR_P:  '//str(smallR_P,  .true.)
 write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'Machine zero'
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  ZeroR16P: '//str(ZeroR16P, .true.)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  ZeroR8P:  '//str(ZeroR8P,  .true.)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  ZeroR4P:  '//str(ZeroR4P,  .true.)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  ZeroR_P:  '//str(ZeroR_P,  .true.)
  IF (PRESENT(iostat)) iostat = iostatd
  IF (PRESENT(iomsg)) iomsg = iomsgd
END SUBROUTINE penf_print

! private procedures
ELEMENTAL FUNCTION digit_I8(n) RESULT(digit)
  !< Compute the number of digits in decimal base of the input integer.
  !<
  !<```fortran
  !< use penf
  !< print FI4P, digit(100_I8P)
  !<```
  !=> 3 <<<
  INTEGER(I8P), INTENT(in) :: n !< Input integer.
  CHARACTER(DI8P) :: str !< Returned string containing input number plus padding zeros.
  INTEGER(I4P) :: digit !< Number of digits.

  WRITE (str, FI8P) ABS(n) ! Casting of n to string.
  digit = LEN_TRIM(ADJUSTL(str)) ! Calculating the digits number of n.
END FUNCTION digit_I8

ELEMENTAL FUNCTION digit_I4(n) RESULT(digit)
  !< Compute the number of digits in decimal base of the input integer.
  !<
  !<```fortran
  !< use penf
  !< print FI4P, digit(100_I4P)
  !<```
  !=> 3 <<<
  INTEGER(I4P), INTENT(in) :: n !< Input integer.
  CHARACTER(DI4P) :: str !< Returned string containing input number plus padding zeros.
  INTEGER(I4P) :: digit !< Number of digits.

  WRITE (str, FI4P) ABS(n) ! Casting of n to string.
  digit = LEN_TRIM(ADJUSTL(str)) ! Calculating the digits number of n.
END FUNCTION digit_I4

ELEMENTAL FUNCTION digit_I2(n) RESULT(digit)
  !< Compute the number of digits in decimal base of the input integer.
  !<
  !<```fortran
  !< use penf
  !< print FI4P, digit(100_I2P)
  !<```
  !=> 3 <<<
  INTEGER(I2P), INTENT(in) :: n !< Input integer.
  CHARACTER(DI2P) :: str !< Returned string containing input number plus padding zeros.
  INTEGER(I4P) :: digit !< Number of digits.

  WRITE (str, FI2P) ABS(n) ! Casting of n to string.
  digit = LEN_TRIM(ADJUSTL(str)) ! Calculating the digits number of n.
END FUNCTION digit_I2

ELEMENTAL FUNCTION digit_I1(n) RESULT(digit)
  !< Compute the number of digits in decimal base of the input integer.
  !<
  !<```fortran
  !< use penf
  !< print FI4P, digit(100_I1P)
  !<```
  !=> 3 <<<
  INTEGER(I1P), INTENT(in) :: n !< Input integer.
  CHARACTER(DI1P) :: str !< Returned string containing input number plus padding zeros.
  INTEGER(I4P) :: digit !< Number of digits.

  WRITE (str, FI1P) ABS(n) ! Casting of n to string.
  digit = LEN_TRIM(ADJUSTL(str)) ! Calculating the digits number of n.
END FUNCTION digit_I1
endmodule penf
