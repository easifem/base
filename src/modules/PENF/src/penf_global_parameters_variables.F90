!< PENF global parameters and variables.

MODULE penf_global_parameters_variables
!< PENF global parameters and variables.
!<
!< @note All module defined entities are public.

IMPLICIT NONE
PUBLIC
SAVE

INTEGER, PARAMETER :: endianL = 1 !< Little endian parameter.
INTEGER, PARAMETER :: endianB = 0 !< Big endian parameter.

! portable kind parameters
#ifdef _ASCII_SUPPORTED
INTEGER, PARAMETER :: ASCII = SELECTED_CHAR_KIND('ascii') !< ASCII character set kind.
#else
INTEGER, PARAMETER :: ASCII = SELECTED_CHAR_KIND('default') !< ASCII character set kind defined as default set.
#endif
#ifdef _UCS4_SUPPORTED
INTEGER, PARAMETER :: UCS4 = SELECTED_CHAR_KIND('iso_10646') !< Unicode character set kind.
#else
INTEGER, PARAMETER :: UCS4 = SELECTED_CHAR_KIND('default') !< Unicode character set kind defined as default set.
#endif
#if defined _CK_IS_DEFAULT
INTEGER, PARAMETER :: CK = SELECTED_CHAR_KIND('default') !< Default kind character.
#elif defined _CK_IS_ASCII
INTEGER, PARAMETER :: CK = ASCII !< Default kind character.
#elif defined _CK_IS_UCS4
INTEGER, PARAMETER :: CK = UCS4 !< Default kind character.
#else
INTEGER, PARAMETER :: CK = SELECTED_CHAR_KIND('default') !< Default kind character.
#endif

#if defined _R16P
INTEGER, PARAMETER :: R16P = SELECTED_REAL_KIND(33, 4931) !< 33 digits, range \([10^{-4931}, 10^{+4931} - 1]\); 128 bits.
#else
INTEGER, PARAMETER :: R16P = SELECTED_REAL_KIND(15, 307) !< 15 digits, range \([10^{-307} , 10^{+307}  - 1]\); 64 bits.
#endif
INTEGER, PARAMETER :: R8P = SELECTED_REAL_KIND(15, 307) !< 15 digits, range \([10^{-307} , 10^{+307}  - 1]\); 64 bits.
INTEGER, PARAMETER :: R4P = SELECTED_REAL_KIND(6, 37) !< 6  digits, range \([10^{-37}  , 10^{+37}   - 1]\); 32 bits.
#if defined _R16P
#if defined _R_P_IS_R16P
INTEGER, PARAMETER :: R_P = R16P !< Default real precision.
#endif
#endif
#if defined _R_P_IS_R8P
INTEGER, PARAMETER :: R_P = R8P !< Default real precision.
#elif defined _R_P_IS_R4P
INTEGER, PARAMETER :: R_P = R4P !< Default real precision.
#else
INTEGER, PARAMETER :: R_P = R8P !< Default real precision.
#endif

INTEGER, PARAMETER :: I8P = SELECTED_INT_KIND(18) !< Range \([-2^{63},+2^{63} - 1]\), 19 digits plus sign; 64 bits.
INTEGER, PARAMETER :: I4P = SELECTED_INT_KIND(9) !< Range \([-2^{31},+2^{31} - 1]\), 10 digits plus sign; 32 bits.
INTEGER, PARAMETER :: I2P = SELECTED_INT_KIND(4) !< Range \([-2^{15},+2^{15} - 1]\), 5  digits plus sign; 16 bits.
INTEGER, PARAMETER :: I1P = SELECTED_INT_KIND(2) !< Range \([-2^{7} ,+2^{7}  - 1]\), 3  digits plus sign; 8  bits.
INTEGER, PARAMETER :: I_P = I4P !< Default integer precision.

! format parameters
#if defined _R16P
CHARACTER(*), PARAMETER :: FR16P = '(E42.33E4)' !< Output format for kind=R16P real.
#else
CHARACTER(*), PARAMETER :: FR16P = '(E23.15E3)' !< Output format for kind=R8P real.
#endif
CHARACTER(*), PARAMETER :: FR8P = '(E23.15E3)' !< Output format for kind=R8P real.
CHARACTER(*), PARAMETER :: FR4P = '(E13.6E2)' !< Output format for kind=R4P real.
#if defined _R16P
#if defined _R_P_IS_R16P
CHARACTER(*), PARAMETER :: FR_P = FR16P !< Output format for kind=R_P real.
#endif
#endif
#if defined _R_P_IS_R8P
CHARACTER(*), PARAMETER :: FR_P = FR8P !< Output format for kind=R_P real.
#elif defined _R_P_IS_R4P
CHARACTER(*), PARAMETER :: FR_P = FR4P !< Output format for kind=R_P real.
#else
CHARACTER(*), PARAMETER :: FR_P = FR8P !< Output format for kind=R_P real.
#endif

CHARACTER(*), PARAMETER :: FI8P = '(I20)' !< Output format for kind=I8P integer.
CHARACTER(*), PARAMETER :: FI8PZP = '(I20.19)' !< Output format for kind=I8P integer with zero prefixing.
CHARACTER(*), PARAMETER :: FI4P = '(I11)' !< Output format for kind=I4P integer.
CHARACTER(*), PARAMETER :: FI4PZP = '(I11.10)' !< Output format for kind=I4P integer with zero prefixing.
CHARACTER(*), PARAMETER :: FI2P = '(I6)' !< Output format for kind=I2P integer.
CHARACTER(*), PARAMETER :: FI2PZP = '(I6.5)' !< Output format for kind=I2P integer with zero prefixing.
CHARACTER(*), PARAMETER :: FI1P = '(I4)' !< Output format for kind=I1P integer.
CHARACTER(*), PARAMETER :: FI1PZP = '(I4.3)' !< Output format for kind=I1P integer with zero prefixing.
CHARACTER(*), PARAMETER :: FI_P = FI4P !< Output format for kind=I_P integer.
CHARACTER(*), PARAMETER :: FI_PZP = FI4PZP !< Output format for kind=I_P integer with zero prefixing.

! length (number of digits) of formatted numbers
#if defined _R16P
INTEGER, PARAMETER :: DR16P = 42 !< Number of digits of output format FR16P.
#else
INTEGER, PARAMETER :: DR16P = 23 !< Number of digits of output format FR8P.
#endif
INTEGER, PARAMETER :: DR8P = 23 !< Number of digits of output format FR8P.
INTEGER, PARAMETER :: DR4P = 13 !< Number of digits of output format FR4P.
#if defined _R16P
#if defined _R_P_IS_R16P
INTEGER, PARAMETER :: DR_P = DR16P !< Number of digits of output format FR_P.
#endif
#endif
#if defined _R_P_IS_R8P
INTEGER, PARAMETER :: DR_P = DR8P !< Number of digits of output format FR_P.
#elif defined _R_P_IS_R4P
INTEGER, PARAMETER :: DR_P = DR4P !< Number of digits of output format FR_P.
#else
INTEGER, PARAMETER :: DR_P = DR8P !< Number of digits of output format FR_P.
#endif

INTEGER, PARAMETER :: DI8P = 20 !< Number of digits of output format I8P.
INTEGER, PARAMETER :: DI4P = 11 !< Number of digits of output format I4P.
INTEGER, PARAMETER :: DI2P = 6 !< Number of digits of output format I2P.
INTEGER, PARAMETER :: DI1P = 4 !< Number of digits of output format I1P.
INTEGER, PARAMETER :: DI_P = DI4P !< Number of digits of output format I_P.

! list of kinds
INTEGER, PARAMETER :: CHARACTER_KINDS_LIST(1:3) = [ASCII, UCS4, CK] !< List of character kinds.
#if defined _R16P
INTEGER, PARAMETER :: REAL_KINDS_LIST(1:4) = [R16P, R8P, R4P, R_P] !< List of real kinds.
#else
INTEGER, PARAMETER :: REAL_KINDS_LIST(1:3) = [R8P, R4P, R_P] !< List of real kinds.
#endif
#if defined _R16P
character(*), parameter :: REAL_FORMATS_LIST(1:4)    = [FR16P, FR8P, FR4P//' ', FR_P]           !< List of real formats.
#else
CHARACTER(*), PARAMETER :: REAL_FORMATS_LIST(1:3) = [FR8P, FR4P//' ', FR_P] !< List of real formats.
#endif
INTEGER, PARAMETER :: INTEGER_KINDS_LIST(1:5) = [I8P, I4P, I2P, I1P, I_P] !< List of integer kinds.
character(*), parameter :: INTEGER_FORMATS_LIST(1:5) = [FI8P, FI4P, FI2P//' ', FI1P//' ', FI_P] !< List of integer formats.

! minimum and maximum (representable) values
#if defined _R16P
REAL(R16P), PARAMETER :: MinR16P = -HUGE(1._R16P) !< Minimum value of kind=R16P real.
REAL(R16P), PARAMETER :: MaxR16P = HUGE(1._R16P) !< Maximum value of kind=R16P real.
#else
REAL(R8P), PARAMETER :: MinR16P = -HUGE(1._R8P) !< Minimum value of kind=R8P real.
REAL(R8P), PARAMETER :: MaxR16P = HUGE(1._R8P) !< Maximum value of kind=R8P real.
#endif
REAL(R8P), PARAMETER :: MinR8P = -HUGE(1._R8P) !< Minimum value of kind=R8P real.
REAL(R8P), PARAMETER :: MaxR8P = HUGE(1._R8P) !< Maximum value of kind=R8P real.
REAL(R4P), PARAMETER :: MinR4P = -HUGE(1._R4P) !< Minimum value of kind=R4P real.
REAL(R4P), PARAMETER :: MaxR4P = HUGE(1._R4P) !< Maximum value of kind=R4P real.
REAL(R_P), PARAMETER :: MinR_P = -HUGE(1._R_P) !< Minimum value of kind=R_P real.
REAL(R_P), PARAMETER :: MaxR_P = HUGE(1._R_P) !< Maximum value of kind=R_P real.
INTEGER(I8P), PARAMETER :: MinI8P = -HUGE(1_I8P) !< Minimum value of kind=I8P integer.
INTEGER(I4P), PARAMETER :: MinI4P = -HUGE(1_I4P) !< Minimum value of kind=I4P integer.
INTEGER(I2P), PARAMETER :: MinI2P = -HUGE(1_I2P) !< Minimum value of kind=I2P integer.
INTEGER(I1P), PARAMETER :: MinI1P = -HUGE(1_I1P) !< Minimum value of kind=I1P integer.
INTEGER(I_P), PARAMETER :: MinI_P = -HUGE(1_I_P) !< Minimum value of kind=I_P integer.
INTEGER(I8P), PARAMETER :: MaxI8P = HUGE(1_I8P) !< Maximum value of kind=I8P integer.
INTEGER(I4P), PARAMETER :: MaxI4P = HUGE(1_I4P) !< Maximum value of kind=I4P integer.
INTEGER(I2P), PARAMETER :: MaxI2P = HUGE(1_I2P) !< Maximum value of kind=I2P integer.
INTEGER(I1P), PARAMETER :: MaxI1P = HUGE(1_I1P) !< Maximum value of kind=I1P integer.
INTEGER(I_P), PARAMETER :: MaxI_P = HUGE(1_I_P) !< Maximum value of kind=I_P integer.

! real smallest (representable) values
#if defined _R16P
REAL(R16P), PARAMETER :: smallR16P = TINY(1._R16P) !< Smallest representable value of kind=R16P real.
#else
REAL(R8P), PARAMETER :: smallR16P = TINY(1._R8P) !< Smallest representable value of kind=R8P real.
#endif
REAL(R8P), PARAMETER :: smallR8P = TINY(1._R8P) !< Smallest representable value of kind=R8P real.
REAL(R4P), PARAMETER :: smallR4P = TINY(1._R4P) !< Smallest representable value of kind=R4P real.
REAL(R_P), PARAMETER :: smallR_P = TINY(1._R_P) !< Smallest representable value of kind=R_P real.

! smallest real representable difference by the running calculator
#if defined _R16P
REAL(R16P), PARAMETER :: ZeroR16P = NEAREST(1._R16P, 1._R16P) - &
                         NEAREST(1._R16P, -1._R16P) !< Smallest representable difference of kind=R16P real.
#else
REAL(R8P), PARAMETER :: ZeroR16P = 0._R8P !nearest(1._R8P, 1._R8P) - &
!nearest(1._R8P,-1._R8P)   !< Smallest representable difference of kind=R8P real.
#endif
REAL(R8P), PARAMETER :: ZeroR8P = 0._R8P !nearest(1._R8P, 1._R8P) - &
!nearest(1._R8P,-1._R8P)   !< Smallest representable difference of kind=R8P real.
REAL(R4P), PARAMETER :: ZeroR4P = 0._R4P !nearest(1._R4P, 1._R4P) - &
!nearest(1._R4P,-1._R4P)   !< Smallest representable difference of kind=R4P real.
REAL(R_P), PARAMETER :: ZeroR_P = 0._R_P !nearest(1._R_P, 1._R_P) - &
!nearest(1._R_P,-1._R_P)   !< Smallest representable difference of kind=R_P real.

! bits/bytes memory requirements
#if defined _R16P
INTEGER(I2P), PARAMETER :: BIR16P = STORAGE_SIZE(MaxR16P) !< Number of bits of kind=R16P real.
#else
INTEGER(I1P), PARAMETER :: BIR16P = STORAGE_SIZE(MaxR8P) !< Number of bits of kind=R8P real.
#endif
INTEGER(I1P), PARAMETER :: BIR8P = STORAGE_SIZE(MaxR8P) !< Number of bits of kind=R8P real.
INTEGER(I1P), PARAMETER :: BIR4P = STORAGE_SIZE(MaxR4P) !< Number of bits of kind=R4P real.
INTEGER(I1P), PARAMETER :: BIR_P = STORAGE_SIZE(MaxR_P) !< Number of bits of kind=R_P real.
#if defined _R16P
INTEGER(I2P), PARAMETER :: BYR16P = BIR16P / 8_I2P !< Number of bytes of kind=R16P real.
#else
INTEGER(I1P), PARAMETER :: BYR16P = BIR8P / 8_I1P !< Number of bytes of kind=R8P real.
#endif
INTEGER(I1P), PARAMETER :: BYR8P = BIR8P / 8_I1P !< Number of bytes of kind=R8P real.
INTEGER(I1P), PARAMETER :: BYR4P = BIR4P / 8_I1P !< Number of bytes of kind=R4P real.
INTEGER(I1P), PARAMETER :: BYR_P = BIR_P / 8_I1P !< Number of bytes of kind=R_P real.
INTEGER(I8P), PARAMETER :: BII8P = STORAGE_SIZE(MaxI8P) !< Number of bits of kind=I8P integer.
INTEGER(I4P), PARAMETER :: BII4P = STORAGE_SIZE(MaxI4P) !< Number of bits of kind=I4P integer.
INTEGER(I2P), PARAMETER :: BII2P = STORAGE_SIZE(MaxI2P) !< Number of bits of kind=I2P integer.
INTEGER(I1P), PARAMETER :: BII1P = STORAGE_SIZE(MaxI1P) !< Number of bits of kind=I1P integer.
INTEGER(I_P), PARAMETER :: BII_P = STORAGE_SIZE(MaxI_P) !< Number of bits of kind=I_P integer.
INTEGER(I8P), PARAMETER :: BYI8P = BII8P / 8_I8P !< Number of bytes of kind=I8P integer.
INTEGER(I4P), PARAMETER :: BYI4P = BII4P / 8_I4P !< Number of bytes of kind=I4P integer.
INTEGER(I2P), PARAMETER :: BYI2P = BII2P / 8_I2P !< Number of bytes of kind=I2P integer.
INTEGER(I1P), PARAMETER :: BYI1P = BII1P / 8_I1P !< Number of bytes of kind=I1P integer.
INTEGER(I_P), PARAMETER :: BYI_P = BII_P / 8_I_P !< Number of bytes of kind=I_P integer.
endmodule penf_global_parameters_variables
