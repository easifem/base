!< BeFoR64, Base64 encoding/decoding library for FoRtran poor people.

MODULE befor64
!< BeFoR64, Base64 encoding/decoding library for FoRtran poor people.
USE penf
USE befor64_pack_data_m

IMPLICIT NONE
PRIVATE
PUBLIC :: is_b64_initialized, b64_init
PUBLIC :: b64_encode, b64_encode_up
PUBLIC :: b64_decode, b64_decode_up
PUBLIC :: pack_data

LOGICAL :: is_b64_initialized = .FALSE. !< Flag for checking the initialization of the library.
character(64) :: base64="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" !< Base64 alphabet.

INTERFACE b64_encode
  !< Encode numbers (integer and real) to base64.
  !<
  !< This is an interface for encoding integer and real numbers of any kinds into a base64 string. This interface can encode both
  !< scalar and array.
  !<
  !< @warning The encoded string is returned as varying length character string, `character(len=:), allocatable:: string`, thus the
  !< compiler must support such a Fortran (2003) feature.
  !<
  !< @note Before start to encode anything the library must be initialized. The procedure `b64_init` must be called at first. The
  !< global variable `is_b64_initialized` can be used to check the status of the initialization.
  !<
  !<### Usage
  !< For a practical example see the `autotest` procedure.
  !<
  !<#### Scalar encoding
  !<```ortran
  !<character(len=:), allocatable:: code64 ! base64 encoded string
  !<...
  !<call b64_encode(n=12._R8P,code=code64)
  !<```
  !<
  !<#### Array encoding
  !<```ortran
  !<character(len=:), allocatable:: code64 ! base64 encoded string
  !<...
  !<call b64_encode(n=[12_I4P,1_I4P],code=code64)
  !<```
  !<
  !< @note If you want to encode heterogenous data (e.g. integer and real numbers), you must use the auxiliary `pack_data`
  !< procedure.
  !<
  !< @warning The encoding of array of strings is admitted only if each string of the array has the same length.
  MODULE PROCEDURE &
#if defined _R16P
    b64_encode_R16, b64_encode_R16_a, &
#endif
    b64_encode_R8, b64_encode_R8_a, &
    b64_encode_R4, b64_encode_R4_a, &
    b64_encode_I8, b64_encode_I8_a, &
    b64_encode_I4, b64_encode_I4_a, &
    b64_encode_I2, b64_encode_I2_a, &
    b64_encode_I1, b64_encode_I1_a, &
    b64_encode_string, b64_encode_string_a
END INTERFACE

INTERFACE b64_encode_up
  !< Encode unlimited polymorphic variable to base64.
  !<
  !< This is an interface for encoding both scalar and array.
  !<
  !< @warning The encoded string is returned as varying length character string, `character(len=:), allocatable:: string`, thus the
  !< compiler must support such a Fortran (2003) feature.
  !<
  !< @note Before start to encode anything the library must be initialized. The procedure `b64_init` must be called at first. The
  !< global variable `is_b64_initialized` can be used to check the status of the initialization.
  !<
  !<### Usage
  !< For a practical example see the `autotest` procedure.
  !<
  !<#### Scalar encoding
  !<```ortran
  !<character(len=:), allocatable:: code64 ! base64 encoded string
  !<...
  !<call b64_encode_up(up=12._R8P,code=code64)
  !<```
  !<
  !<#### Array encoding
  !<```ortran
  !<character(len=:), allocatable:: code64 ! base64 encoded string
  !<...
  !<call b64_encode_up(up=[12_I4P,1_I4P],code=code64)
  !<```
  !<
  !< @note If you want to encode heterogenous data (e.g. integer and real numbers), you must use the auxiliary `pack_data`
  !< procedure.
  !<
  !< @warning The encoding of array of strings is admitted only if each string of the array has the same length.
  MODULE PROCEDURE b64_encode_up, b64_encode_up_a
END INTERFACE

INTERFACE b64_decode
  !< Decode numbers (integer and real) from base64.
  !<
  !< This is an interface for decoding integer and real numbers of any kinds from a base64 string. This interface can decode both
  !< scalar and array.
  !<
  !< @note Before start to decode anything the library must be initialized. The procedure `b64_init` must be called at first. The
  !< global variable `is_b64_initialized` can be used to check the status of the initialization.
  !<
  !<### Usage
  !< For a practical example see the `autotest` procedure.
  !<
  !<#### Scalar decoding
  !<```ortran
  !<real(R8P):: decoded ! scalar to be decoded
  !<...
  !<call b64_decode(code='AAAAAAAA8D8=',n=decoded)
  !<```
  !<
  !<#### Array decoding
  !<```ortran
  !<integer(I8P):: decoded(1:4) ! array to be decoded
  !<...
  !<call b64_decode(code='FwAAAAAAAABEAQAAAAAAABBwhAEAAAAAAgAAAAAAAAA=',n=decoded)
  !<```
  !<
  !< @note If you want to decode heterogenous data (e.g. integer and real numbers), you must use the auxiliary `pack_data`
  !< procedure.
  !<
  !< @warning The decoding of array of strings is admitted only if each string of the array has the same length.
  MODULE PROCEDURE &
#if defined _R16P
    b64_decode_R16, b64_decode_R16_a, &
#endif
    b64_decode_R8, b64_decode_R8_a, &
    b64_decode_R4, b64_decode_R4_a, &
    b64_decode_I8, b64_decode_I8_a, &
    b64_decode_I4, b64_decode_I4_a, &
    b64_decode_I2, b64_decode_I2_a, &
    b64_decode_I1, b64_decode_I1_a, &
    b64_decode_string, b64_decode_string_a
END INTERFACE

INTERFACE b64_decode_up
  !< Decode unlimited polymorphic variable from base64.
  !<
  !< This is an interface for decoding both scalar and array.
  !<
  !< @note Before start to decode anything the library must be initialized. The procedure `b64_init` must be called at first. The
  !< global variable `is_b64_initialized` can be used to check the status of the initialization.
  !<
  !<### Usage
  !< For a practical example see the `autotest` procedure.
  !<
  !<#### Scalar decoding
  !<```ortran
  !<real(R8P):: decoded ! scalar to be decoded
  !<...
  !<call b64_decode_up(code='AAAAAAAA8D8=',up=decoded)
  !<```
  !<
  !<#### Array decoding
  !<```ortran
  !<integer(I8P):: decoded(1:4) ! array to be decoded
  !<...
  !<call b64_decode_up(code='FwAAAAAAAABEAQAAAAAAABBwhAEAAAAAAgAAAAAAAAA=',up=decoded)
  !<```
  !<
  !< @note If you want to decode heterogenous data (e.g. integer and real numbers), you must use the auxiliary `pack_data`
  !< procedure.
  !<
  !< @warning The decoding of array of strings is admitted only if each string of the array has the same length.
  MODULE PROCEDURE b64_decode_up, b64_decode_up_a
END INTERFACE

CONTAINS
SUBROUTINE b64_init()
  !< Initialize the BeFoR64 library.
  !<
  !< @note This procedure **must** be called before encoding/decoding anything!
  !<
  !<```fortran
  !< use befor64
  !< call b64_init
  !< print "(L1)", is_b64_initialized
  !<```
  !=> T <<<

  IF (.NOT. is_initialized) CALL penf_init
  is_b64_initialized = .TRUE.
END SUBROUTINE b64_init

PURE SUBROUTINE encode_bits(bits, padd, code)
  !< Encode a bits stream (must be multiple of 24 bits) into base64 charcaters code (of length multiple of 4).
  !<
  !< The bits stream are encoded in chunks of 24 bits as the following example (in little endian order)
  !<```
  !< +--first octet--+-second octet--+--third octet--+
  !< |7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|
  !< +-----------+---+-------+-------+---+-----------+
  !< |5 4 3 2 1 0|5 4 3 2 1 0|5 4 3 2 1 0|5 4 3 2 1 0|
  !< +--1.index--+--2.index--+--3.index--+--4.index--+
  !<```
  !< @note The 4 indexes are stored into 4 elements 8 bits array, thus 2 bits of each array element are not used.
  !<
  !< @note The number of paddings must be computed outside this procedure, into the calling scope.
  !<
  !< @warning This procedure is the backend of encoding, thus it must be never called outside the module.
  INTEGER(I1P), INTENT(in) :: bits(1:) !< Bits to be encoded.
  INTEGER(I4P), INTENT(in) :: padd !< Number of padding characters ('=').
  CHARACTER(*), INTENT(out) :: code !< Characters code.
  INTEGER(I1P) :: sixb(1:4) !< 6 bits slices (stored into 8 bits integer) of 24 bits input.
  INTEGER(I8P) :: c !< Counter.
  INTEGER(I8P) :: e !< Counter.
  INTEGER(I8P) :: Nb !< Length of bits array.

  Nb = SIZE(bits, dim=1, kind=I8P)
  c = 1_I8P
  DO e = 1_I8P, Nb, 3_I8P ! loop over array elements: 3 bytes (24 bits) scanning
    sixb = 0_I1P
    CALL MVBITS(bits(e), 2, 6, sixb(1), 0)
    CALL MVBITS(bits(e), 0, 2, sixb(2), 4)
    IF (e + 1 <= Nb) THEN
      CALL MVBITS(bits(e + 1), 4, 4, sixb(2), 0)
      CALL MVBITS(bits(e + 1), 0, 4, sixb(3), 2)
    END IF
    IF (e + 2 <= Nb) THEN
      CALL MVBITS(bits(e + 2), 6, 2, sixb(3), 0)
      CALL MVBITS(bits(e + 2), 0, 6, sixb(4), 0)
    END IF
    sixb = sixb + 1_I1P
    code(c:c) = base64(sixb(1):sixb(1))
    code(c + 1:c + 1) = base64(sixb(2):sixb(2))
    code(c + 2:c + 2) = base64(sixb(3):sixb(3))
    code(c + 3:c + 3) = base64(sixb(4):sixb(4))
    c = c + 4_I8P
  END DO
  IF (padd > 0) code(LEN(code) - padd + 1:) = REPEAT('=', padd)
END SUBROUTINE encode_bits

PURE SUBROUTINE decode_bits(code, bits)
  !< Decode a base64 string into a sequence of bits stream.
  !<
  !< The base64 string must be parsed with a strike of 4 characters and converted into a 3 bytes stream. Considering the base64 code
  !< `QUJD` the decoding process must do
  !<```
  !< +-b64 char--+-b64 char--+-b64 char--+-b64 char--+
  !< |      Q    |      U    |      J    |      D    |
  !< +-b64 index-+-b64 index-+-b64 index-+-b64 index-+
  !< !      16   |      20   |      9    |      3    |
  !< +-6 bits----+-6 bits----+-6 bits----+-6 bits----+
  !< |0 1 0 0 0 0|0 1 0 1 0 0|0 0 1 0 0 1|0 0 0 0 1 1|
  !< +-----------+---+-------+-------+---+-----------+
  !< |0 1 0 0 0 0 0 1|0 1 0 0 0 0 1 0|0 1 0 0 0 0 1 1|
  !< +-----8 bits----+-----8 bits----+-----8 bits----+
  !<```
  !< @note The bits pattern is returned as a 1-byte element array, the dimension of witch must be computed outside this procedure.
  !<
  !< @warning This procedure is the backend of decoding, thus it must be never called outside the module.
  CHARACTER(*), INTENT(in) :: code !< Characters code.
  INTEGER(I1P), INTENT(out) :: bits(1:) !< Bits decoded.
  INTEGER(I1P) :: sixb(1:4) !< 6 bits slices (stored into 8 bits integer) of 24 bits input.
  INTEGER(I8P) :: c !< Counter.
  INTEGER(I8P) :: e !< Counter.
  INTEGER(I8P) :: Nb !< Length of bits array.

  Nb = SIZE(bits, dim=1, kind=I8P)
  e = 1_I8P
  DO c = 1_I8P, LEN(code), 4_I8P ! loop over code characters: 3 bytes (24 bits) scanning
    sixb = 0_I1P
    sixb(1) = INDEX(base64, code(c:c)) - 1
    sixb(2) = INDEX(base64, code(c + 1:c + 1)) - 1
    sixb(3) = INDEX(base64, code(c + 2:c + 2)) - 1
    sixb(4) = INDEX(base64, code(c + 3:c + 3)) - 1
    CALL MVBITS(sixb(1), 0, 6, bits(e), 2); CALL MVBITS(sixb(2), 4, 2, bits(e), 0)
    IF (e + 1 <= Nb) THEN
      CALL MVBITS(sixb(2), 0, 4, bits(e + 1), 4); CALL MVBITS(sixb(3), 2, 4, bits(e + 1), 0)
    END IF
    IF (e + 2 <= Nb) THEN
      CALL MVBITS(sixb(3), 0, 2, bits(e + 2), 6); CALL MVBITS(sixb(4), 0, 6, bits(e + 2), 0)
    END IF
    e = e + 3_I8P
  END DO
END SUBROUTINE decode_bits

SUBROUTINE b64_encode_up(up, code)
  !< Encode an unlimited polymorphic scalar to base64.
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(len=:), allocatable :: code64
  !< call b64_encode_up(up=1._R8P, code=code64)
  !< print "(A)", code64
  !<```
  !=> AAAAAAAA8D8= <<<
  CLASS(*), INTENT(in) :: up !< Unlimited polymorphic variable to be encoded.
  CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: code !< Encoded scalar.

  SELECT TYPE (up)
  TYPE is (REAL(R8P))
    CALL b64_encode_R8(n=up, code=code)
  TYPE is (REAL(R4P))
    CALL b64_encode_R4(n=up, code=code)
  TYPE is (INTEGER(I8P))
    CALL b64_encode_I8(n=up, code=code)
  TYPE is (INTEGER(I4P))
    CALL b64_encode_I4(n=up, code=code)
  TYPE is (INTEGER(I2P))
    CALL b64_encode_I2(n=up, code=code)
  TYPE is (INTEGER(I1P))
    CALL b64_encode_I1(n=up, code=code)
  TYPE is (CHARACTER(*))
    CALL b64_encode_string(s=up, code=code)
  END SELECT
END SUBROUTINE b64_encode_up

PURE SUBROUTINE b64_encode_up_a(up, code)
  !< Encode an unlimited polymorphic array to base64.
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(len=:), allocatable :: code64
  !< call b64_encode_up(up=[0._R4P,-32.12_R4P], code=code64)
  !< print "(A)", code64
  !<```
  !=> AAAAAOF6AMI= <<<
  CLASS(*), INTENT(in) :: up(1:) !< Unlimited polymorphic variable to be encoded.
  CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: code !< Encoded array.

  SELECT TYPE (up)
  TYPE is (REAL(R8P))
    CALL b64_encode_R8_a(n=up, code=code)
  TYPE is (REAL(R4P))
    CALL b64_encode_R4_a(n=up, code=code)
  TYPE is (INTEGER(I8P))
    CALL b64_encode_I8_a(n=up, code=code)
  TYPE is (INTEGER(I4P))
    CALL b64_encode_I4_a(n=up, code=code)
  TYPE is (INTEGER(I2P))
    CALL b64_encode_I2_a(n=up, code=code)
  TYPE is (INTEGER(I1P))
    CALL b64_encode_I1_a(n=up, code=code)
  TYPE is (CHARACTER(*))
    CALL b64_encode_string_a(s=up, code=code)
  END SELECT
END SUBROUTINE b64_encode_up_a

SUBROUTINE b64_decode_up(code, up)
  !< Decode an unlimited polymorphic scalar from base64.
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< integer(I4P) :: scalar_I4
  !< call b64_decode_up(code='5wcAAA==',up=scalar_I4)
  !< print "(L1)", scalar_I4==2023_I4P
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: code !< Encoded scalar.
  CLASS(*), INTENT(out) :: up !< Unlimited polymorphic variable to be decoded.

  SELECT TYPE (up)
  TYPE is (REAL(R8P))
    CALL b64_decode_R8(code=code, n=up)
  TYPE is (REAL(R4P))
    CALL b64_decode_R4(code=code, n=up)
  TYPE is (INTEGER(I8P))
    CALL b64_decode_I8(code=code, n=up)
  TYPE is (INTEGER(I4P))
    CALL b64_decode_I4(code=code, n=up)
  TYPE is (INTEGER(I2P))
    CALL b64_decode_I2(code=code, n=up)
  TYPE is (INTEGER(I1P))
    CALL b64_decode_I1(code=code, n=up)
  TYPE is (CHARACTER(*))
    CALL b64_decode_string(code=code, s=up)
  END SELECT
END SUBROUTINE b64_decode_up

SUBROUTINE b64_decode_up_a(code, up)
  !< Decode an unlimited polymorphic array from base64.
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< integer(I8P) :: array_I8(1:4)
  !< call b64_decode_up(code='FwAAAAAAAABEAQAAAAAAABBwhAEAAAAAAgAAAAAAAAA=', up=array_I8)
  !< print "(L1)", str(n=array_I8)==str(n=[23_I8P,324_I8P,25456656_I8P,2_I8P])
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: code !< Encoded array.
  CLASS(*), INTENT(out) :: up(1:) !< Unlimited polymorphic variable to be decoded.

  SELECT TYPE (up)
  TYPE is (REAL(R8P))
    CALL b64_decode_R8_a(code=code, n=up)
  TYPE is (REAL(R4P))
    CALL b64_decode_R4_a(code=code, n=up)
  TYPE is (INTEGER(I8P))
    CALL b64_decode_I8_a(code=code, n=up)
  TYPE is (INTEGER(I4P))
    CALL b64_decode_I4_a(code=code, n=up)
  TYPE is (INTEGER(I2P))
    CALL b64_decode_I2_a(code=code, n=up)
  TYPE is (INTEGER(I1P))
    CALL b64_decode_I1_a(code=code, n=up)
  TYPE is (CHARACTER(*))
    CALL b64_decode_string_a(code=code, s=up)
  END SELECT
END SUBROUTINE b64_decode_up_a

PURE SUBROUTINE b64_encode_R16(n, code)
  !< Encode scalar number to base64 (R16P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(len=:), allocatable :: code64
  !< call b64_encode(n=134.231_R16P, code=code64)
  !< print "(A)", code64
  !<```
  !=> CKwcWmTHYEA= <<<
  REAL(R16P), INTENT(in) :: n !< Number to be encoded.
  CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: code !< Encoded scalar.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.
  INTEGER(I4P) :: padd !< Number of padding characters ('=').

  ALLOCATE (nI1P(1:((BYR16P + 2) / 3) * 3)); nI1P = 0_I1P
  code = REPEAT(' ', ((BYR16P + 2) / 3) * 4)
  nI1P = TRANSFER(n, nI1P)
#if defined _R16P
  padd = MOD((BYR16P), 3_I2P); IF (padd > 0_I4P) padd = 3_I4P - padd
#else
  padd = MOD((BYR16P), 3_I1P); IF (padd > 0_I4P) padd = 3_I4P - padd
#endif
  CALL encode_bits(bits=nI1P, padd=padd, code=code)
END SUBROUTINE b64_encode_R16

PURE SUBROUTINE b64_encode_R8(n, code)
  !< Encode scalar number to base64 (R8P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(len=:), allocatable :: code64
  !< call b64_encode(n=1._R8P, code=code64)
  !< print "(A)", code64
  !<```
  !=> AAAAAAAA8D8= <<<
  REAL(R8P), INTENT(in) :: n !< Number to be encoded.
  CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: code !< Encoded scalar.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.
  INTEGER(I4P) :: padd !< Number of padding characters ('=').

  ALLOCATE (nI1P(1:((BYR8P + 2) / 3) * 3)); nI1P = 0_I1P
  code = REPEAT(' ', ((BYR8P + 2) / 3) * 4)
  nI1P = TRANSFER(n, nI1P)
  padd = MOD((BYR8P), 3_I1P); IF (padd > 0_I4P) padd = 3_I4P - padd
  CALL encode_bits(bits=nI1P, padd=padd, code=code)
END SUBROUTINE b64_encode_R8

PURE SUBROUTINE b64_encode_R4(n, code)
  !< Encode scalar number to base64 (R4P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(len=:), allocatable :: code64
  !< call b64_encode(n=0._R4P, code=code64)
  !< print "(A)", code64
  !<```
  !=> AAAAAA== <<<
  REAL(R4P), INTENT(in) :: n !< Number to be encoded.
  CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: code !< Encoded scalar.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.
  INTEGER(I4P) :: padd !< Number of padding characters ('=').

  ALLOCATE (nI1P(1:((BYR4P + 2) / 3) * 3)); nI1P = 0_I1P
  code = REPEAT(' ', ((BYR4P + 2) / 3) * 4)
  nI1P = TRANSFER(n, nI1P)
  padd = MOD((BYR4P), 3_I1P); IF (padd > 0_I4P) padd = 3_I4P - padd
  CALL encode_bits(bits=nI1P, padd=padd, code=code)
END SUBROUTINE b64_encode_R4

PURE SUBROUTINE b64_encode_I8(n, code)
  !< Encode scalar number to base64 (I8P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(len=:), allocatable :: code64
  !< call b64_encode(n=23_I8P, code=code64)
  !< print "(A)", code64
  !<```
  !=> FwAAAAAAAAA= <<<
  INTEGER(I8P), INTENT(in) :: n !< Number to be encoded.
  CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: code !< Encoded scalar.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.
  INTEGER(I4P) :: padd !< Number of padding characters ('=').

  ALLOCATE (nI1P(1:((BYI8P + 2) / 3) * 3)); nI1P = 0_I1P
  code = REPEAT(' ', ((BYI8P + 2) / 3) * 4)
  nI1P = TRANSFER(n, nI1P)
  padd = MOD((BYI8P), 3_I8P); IF (padd > 0_I4P) padd = 3_I4P - padd
  CALL encode_bits(bits=nI1P, padd=padd, code=code)
END SUBROUTINE b64_encode_I8

PURE SUBROUTINE b64_encode_I4(n, code)
  !< Encode scalar number to base64 (I4P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(len=:), allocatable :: code64
  !< call b64_encode(n=2023_I4P, code=code64)
  !< print "(A)", code64
  !<```
  !=> 5wcAAA== <<<
  INTEGER(I4P), INTENT(in) :: n !< Number to be encoded.
  CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: code !< Encoded scalar.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.
  INTEGER(I4P) :: padd !< Number of padding characters ('=').

  ALLOCATE (nI1P(1:((BYI4P + 2) / 3) * 3)); nI1P = 0_I1P
  code = REPEAT(' ', ((BYI4P + 2) / 3) * 4)
  nI1P = TRANSFER(n, nI1P)
  padd = MOD((BYI4P), 3_I4P); IF (padd > 0_I4P) padd = 3_I4P - padd
  CALL encode_bits(bits=nI1P, padd=padd, code=code)
END SUBROUTINE b64_encode_I4

PURE SUBROUTINE b64_encode_I2(n, code)
  !< Encode scalar number to base64 (I2P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(len=:), allocatable :: code64
  !< call b64_encode(n=-203_I2P, code=code64)
  !< print "(A)", code64
  !<```
  !=> Nf8= <<<
  INTEGER(I2P), INTENT(in) :: n !< Number to be encoded.
  CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: code !< Encoded scalar.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.
  INTEGER(I4P) :: padd !< Number of padding characters ('=').

  ALLOCATE (nI1P(1:((BYI2P + 2) / 3) * 3)); nI1P = 0_I1P
  code = REPEAT(' ', ((BYI2P + 2) / 3) * 4)
  nI1P = TRANSFER(n, nI1P)
  padd = MOD((BYI2P), 3_I2P); IF (padd > 0_I4P) padd = 3_I4P - padd
  CALL encode_bits(bits=nI1P, padd=padd, code=code)
END SUBROUTINE b64_encode_I2

PURE SUBROUTINE b64_encode_I1(n, code)
  !< Encode scalar number to base64 (I1P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(len=:), allocatable :: code64
  !< call b64_encode(n=120_I1P, code=code64)
  !< print "(A)", code64
  !<```
  !=> eA== <<<
  INTEGER(I1P), INTENT(in) :: n !< Number to be encoded.
  CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: code !< Encoded scalar.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.
  INTEGER(I4P) :: padd !< Number of padding characters ('=').

  ALLOCATE (nI1P(1:((BYI1P + 2) / 3) * 3)); nI1P = 0_I1P
  code = REPEAT(' ', ((BYI1P + 2) / 3) * 4)
  nI1P = TRANSFER(n, nI1P)
  padd = MOD((BYI1P), 3_I1P); IF (padd > 0_I4P) padd = 3_I4P - padd
  CALL encode_bits(bits=nI1P, padd=padd, code=code)
END SUBROUTINE b64_encode_I1

PURE SUBROUTINE b64_encode_string(s, code)
  !< Encode scalar string to base64.
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(len=:), allocatable :: code64
  !< call b64_encode(s='hello', code=code64)
  !< print "(A)", code64
  !<```
  !=> aGVsbG8= <<<
  CHARACTER(*), INTENT(in) :: s !< String to be encoded.
  CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: code !< Encoded scalar.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.
  INTEGER(I4P) :: padd !< Number of padding characters ('=').
  INTEGER(I4P) :: BYCHS !< Bytes of character string.

  BYCHS = byte_size(s)
  ALLOCATE (nI1P(1:((BYCHS + 2) / 3) * 3)); nI1P = 0_I1P
  code = REPEAT(' ', ((BYCHS + 2) / 3) * 4)
  nI1P = TRANSFER(s, nI1P)
  padd = MOD((BYCHS), 3_I4P); IF (padd > 0_I4P) padd = 3_I4P - padd
  CALL encode_bits(bits=nI1P, padd=padd, code=code)
END SUBROUTINE b64_encode_string

PURE SUBROUTINE b64_encode_R16_a(n, code)
  !< Encode array numbers to base64 (R16P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(len=:), allocatable :: code64
  !< call b64_encode(n=[121._R16P,2.32_R16P], code=code64)
  !< print "(A)", code64
  !<```
  !=> AAAAAABAXkCPwvUoXI8CQA== <<<
  REAL(R16P), INTENT(in) :: n(1:) !< Array of numbers to be encoded.
  CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: code !< Encoded array.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.
  INTEGER(I4P) :: padd !< Number of padding characters ('=').
  INTEGER(I8P) :: ns !< Size of n.

  ns = SIZE(n, dim=1)
  ALLOCATE (nI1P(1:((ns * BYR16P + 2) / 3) * 3)); nI1P = 0_I1P
  code = REPEAT(' ', ((ns * BYR16P + 2) / 3) * 4)
  nI1P = TRANSFER(n, nI1P)
  padd = MOD((ns * BYR16P), 3_I8P); IF (padd > 0_I4P) padd = 3_I4P - padd
  CALL encode_bits(bits=nI1P, padd=padd, code=code)
END SUBROUTINE b64_encode_R16_a

PURE SUBROUTINE b64_encode_R8_a(n, code)
  !< Encode array numbers to base64 (R8P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(len=:), allocatable :: code64
  !< call b64_encode(n=[1._R8P,2._R8P], code=code64)
  !< print "(A)", code64
  !<```
  !=> AAAAAAAA8D8AAAAAAAAAQA== <<<
  REAL(R8P), INTENT(in) :: n(1:) !< Array of numbers to be encoded.
  CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: code !< Encoded array.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.
  INTEGER(I4P) :: padd !< Number of padding characters ('=').
  INTEGER(I8P) :: ns !< Size of n.

  ns = SIZE(n, dim=1)
  ALLOCATE (nI1P(1:((ns * BYR8P + 2) / 3) * 3)); nI1P = 0_I1P
  code = REPEAT(' ', ((ns * BYR8P + 2) / 3) * 4)
  nI1P = TRANSFER(n, nI1P)
  padd = MOD((ns * BYR8P), 3_I8P); IF (padd > 0_I4P) padd = 3_I4P - padd
  CALL encode_bits(bits=nI1P, padd=padd, code=code)
END SUBROUTINE b64_encode_R8_a

PURE SUBROUTINE b64_encode_R4_a(n, code)
  !< Encode array numbers to base64 (R4P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(len=:), allocatable :: code64
  !< call b64_encode(n=[0._R4P,-32.12_R4P], code=code64)
  !< print "(A)", code64
  !<```
  !=> AAAAAOF6AMI= <<<
  REAL(R4P), INTENT(in) :: n(1:) !< Array of numbers to be encoded.
  CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: code !< Encoded array.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.
  INTEGER(I4P) :: padd !< Number of padding characters ('=').
  INTEGER(I8P) :: ns !< Size of n.

  ns = SIZE(n, dim=1)
  ALLOCATE (nI1P(1:((ns * BYR4P + 2) / 3) * 3)); nI1P = 0_I1P
  code = REPEAT(' ', ((ns * BYR4P + 2) / 3) * 4)
  nI1P = TRANSFER(n, nI1P)
  padd = MOD((ns * BYR4P), 3_I8P); IF (padd > 0_I4P) padd = 3_I4P - padd
  CALL encode_bits(bits=nI1P, padd=padd, code=code)
END SUBROUTINE b64_encode_R4_a

PURE SUBROUTINE b64_encode_I8_a(n, code)
  !< Encode array numbers to base64 (I8P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(len=:), allocatable :: code64
  !< call b64_encode(n=[23_I8P,324_I8P,25456656_I8P,2_I8P], code=code64)
  !< print "(A)", code64
  !<```
  !=> FwAAAAAAAABEAQAAAAAAABBwhAEAAAAAAgAAAAAAAAA= <<<
  INTEGER(I8P), INTENT(in) :: n(1:) !< Array of numbers to be encoded.
  CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: code !< Encoded array.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.
  INTEGER(I4P) :: padd !< Number of padding characters ('=').
  INTEGER(I8P) :: ns !< Size of n.

  ns = SIZE(n, dim=1)
  ALLOCATE (nI1P(1:((ns * BYI8P + 2) / 3) * 3)); nI1P = 0_I1P
  code = REPEAT(' ', ((ns * BYI8P + 2) / 3) * 4)
  nI1P = TRANSFER(n, nI1P)
  padd = MOD((ns * BYI8P), 3_I8P); IF (padd > 0_I4P) padd = 3_I4P - padd
  CALL encode_bits(bits=nI1P, padd=padd, code=code)
END SUBROUTINE b64_encode_I8_a

PURE SUBROUTINE b64_encode_I4_a(n, code)
  !< Encode array numbers to base64 (I4P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(len=:), allocatable :: code64
  !< call b64_encode(n=[2023_I4P,-24_I4P], code=code64)
  !< print "(A)", code64
  !<```
  !=> 5wcAAOj///8= <<<
  INTEGER(I4P), INTENT(in) :: n(1:) !< Array of numbers to be encoded.
  CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: code !< Encoded array.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.
  INTEGER(I4P) :: padd !< Number of padding characters ('=').
  INTEGER(I8P) :: ns !< Size of n.

  ns = SIZE(n, dim=1)
  ALLOCATE (nI1P(1:((ns * BYI4P + 2) / 3) * 3)); nI1P = 0_I1P
  code = REPEAT(' ', ((ns * BYI4P + 2) / 3) * 4)
  nI1P = TRANSFER(n, nI1P)
  padd = MOD((ns * BYI4P), 3_I8P); IF (padd > 0_I4P) padd = 3_I4P - padd
  CALL encode_bits(bits=nI1P, padd=padd, code=code)
END SUBROUTINE b64_encode_I4_a

PURE SUBROUTINE b64_encode_I2_a(n, code)
  !< Encode array numbers to base64 (I2P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(len=:), allocatable :: code64
  !< call b64_encode(n=[-203_I2P,-10_I2P], code=code64)
  !< print "(A)", code64
  !<```
  !=> Nf/2/w== <<<
  INTEGER(I2P), INTENT(in) :: n(1:) !< Array of numbers to be encoded.
  CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: code !< Encoded array.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.
  INTEGER(I4P) :: padd !< Number of padding characters ('=').
  INTEGER(I8P) :: ns !< Size of n.

  ns = SIZE(n, dim=1)
  ALLOCATE (nI1P(1:((ns * BYI2P + 2) / 3) * 3)); nI1P = 0_I1P
  code = REPEAT(' ', ((ns * BYI2P + 2) / 3) * 4)
  nI1P = TRANSFER(n, nI1P)
  padd = MOD((ns * BYI2P), 3_I8P); IF (padd > 0_I4P) padd = 3_I4P - padd
  CALL encode_bits(bits=nI1P, padd=padd, code=code)
END SUBROUTINE b64_encode_I2_a

PURE SUBROUTINE b64_encode_I1_a(n, code)
  !< Encode array numbers to base64 (I1P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(len=:), allocatable :: code64
  !< call b64_encode(n=[120_I1P,-1_I1P], code=code64)
  !< print "(A)", code64
  !<```
  !=> eP8= <<<
  INTEGER(I1P), INTENT(in) :: n(1:) !< Array of numbers to be encoded.
  CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: code !< Encoded array.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.
  INTEGER(I4P) :: padd !< Number of padding characters ('=').
  INTEGER(I8P) :: ns !< Size of n.

  ns = SIZE(n, dim=1)
  ALLOCATE (nI1P(1:((ns * BYI1P + 2) / 3) * 3)); nI1P = 0_I1P
  code = REPEAT(' ', ((ns * BYI1P + 2) / 3) * 4)
  nI1P = TRANSFER(n, nI1P)
  padd = MOD((ns * BYI1P), 3_I8P); IF (padd > 0_I4P) padd = 3_I4P - padd
  CALL encode_bits(bits=nI1P, padd=padd, code=code)
END SUBROUTINE b64_encode_I1_a

PURE SUBROUTINE b64_encode_string_a(s, code)
  !< Encode array string to base64.
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(len=:), allocatable :: code64
  !< call b64_encode(s=['hello','world'], code=code64)
  !< print "(A)", code64
  !<```
  !=> aGVsbG93b3JsZA== <<<
  CHARACTER(*), INTENT(in) :: s(1:) !< String to be encoded.
  CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: code !< Encoded scalar.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.
  INTEGER(I4P) :: padd !< Number of padding characters ('=').
  INTEGER(I4P) :: BYCHS !< Bytes of character string.

  BYCHS = byte_size(s(1)) * SIZE(s, dim=1)
  ALLOCATE (nI1P(1:((BYCHS + 2) / 3) * 3)); nI1P = 0_I1P
  code = REPEAT(' ', ((BYCHS + 2) / 3) * 4)
  nI1P = TRANSFER(s, nI1P)
  padd = MOD((BYCHS), 3_I4P); IF (padd > 0_I4P) padd = 3_I4P - padd
  CALL encode_bits(bits=nI1P, padd=padd, code=code)
END SUBROUTINE b64_encode_string_a

ELEMENTAL SUBROUTINE b64_decode_R16(code, n)
  !< Decode a base64 code into a scalar number (R16P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< real(R16P) :: scalar_R16
  !< call b64_decode(code='CKwcWmTHYEA=',n=scalar_R16)
  !< print "(L1)", scalar_R16==134.231_R16P
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: code !< Encoded scalar.
  REAL(R16P), INTENT(out) :: n !< Number to be decoded.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.

  ALLOCATE (nI1P(1:BYR16P)); nI1P = 0_I1P
  CALL decode_bits(code=code, bits=nI1P)
  n = TRANSFER(nI1P, n)
END SUBROUTINE b64_decode_R16

ELEMENTAL SUBROUTINE b64_decode_R8(code, n)
  !< Decode a base64 code into a scalar number (R8P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< real(R8P) :: scalar_R8
  !< call b64_decode(code='AAAAAAAA8D8=',n=scalar_R8)
  !< print "(L1)", scalar_R8==1._R8P
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: code !< Encoded scalar.
  REAL(R8P), INTENT(out) :: n !< Number to be decoded.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.

  ALLOCATE (nI1P(1:BYR8P)); nI1P = 0_I1P
  CALL decode_bits(code=code, bits=nI1P)
  n = TRANSFER(nI1P, n)
END SUBROUTINE b64_decode_R8

ELEMENTAL SUBROUTINE b64_decode_R4(code, n)
  !< Decode a base64 code into a scalar number (R4P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< real(R4P) :: scalar_R4
  !< call b64_decode(code='AAAAAA==',n=scalar_R4)
  !< print "(L1)", scalar_R4==0._R4P
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: code !< Encoded scalar.
  REAL(R4P), INTENT(out) :: n !< Number to be decoded.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.

  ALLOCATE (nI1P(1:BYR4P)); nI1P = 0_I1P
  CALL decode_bits(code=code, bits=nI1P)
  n = TRANSFER(nI1P, n)
END SUBROUTINE b64_decode_R4

ELEMENTAL SUBROUTINE b64_decode_I8(code, n)
  !< Decode a base64 code into a scalar number (I8P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< integer(I8P) :: scalar_I8
  !< call b64_decode(code='FwAAAAAAAAA=',n=scalar_I8)
  !< print "(L1)", scalar_I8==23_I8P
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: code !< Encoded scalar.
  INTEGER(I8P), INTENT(out) :: n !< Number to be decoded.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.

  ALLOCATE (nI1P(1:BYI8P)); nI1P = 0_I1P
  CALL decode_bits(code=code, bits=nI1P)
  n = TRANSFER(nI1P, n)
END SUBROUTINE b64_decode_I8

ELEMENTAL SUBROUTINE b64_decode_I4(code, n)
  !< Decode a base64 code into a scalar number (I4P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< integer(I4P) :: scalar_I4
  !< call b64_decode(code='5wcAAA==',n=scalar_I4)
  !< print "(L1)", scalar_I4==2023_I4P
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: code !< Encoded scalar.
  INTEGER(I4P), INTENT(out) :: n !< Number to be decoded.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.

  ALLOCATE (nI1P(1:BYI4P)); nI1P = 0_I1P
  CALL decode_bits(code=code, bits=nI1P)
  n = TRANSFER(nI1P, n)
END SUBROUTINE b64_decode_I4

ELEMENTAL SUBROUTINE b64_decode_I2(code, n)
  !< Decode a base64 code into a scalar number (I2P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< integer(I2P) :: scalar_I2
  !< call b64_decode(code='Nf8=',n=scalar_I2)
  !< print "(L1)", scalar_I2==-203_I2P
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: code !< Encoded scalar.
  INTEGER(I2P), INTENT(out) :: n !< Number to be decoded.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.

  ALLOCATE (nI1P(1:BYI2P)); nI1P = 0_I1P
  CALL decode_bits(code=code, bits=nI1P)
  n = TRANSFER(nI1P, n)
END SUBROUTINE b64_decode_I2

ELEMENTAL SUBROUTINE b64_decode_I1(code, n)
  !< Decode a base64 code into a scalar number (I1P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< integer(I1P) :: scalar_I1
  !< call b64_decode(code='eA==',n=scalar_I1)
  !< print "(L1)", scalar_I1==120_I1P
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: code !< Encoded scalar.
  INTEGER(I1P), INTENT(out) :: n !< Number to be decoded.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.

  ALLOCATE (nI1P(1:BYI1P)); nI1P = 0_I1P
  CALL decode_bits(code=code, bits=nI1P)
  n = TRANSFER(nI1P, n)
END SUBROUTINE b64_decode_I1

ELEMENTAL SUBROUTINE b64_decode_string(code, s)
  !< Decode a base64 code into a scalar string.
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(:), allocatable :: code64
  !< code64 = repeat(' ',5)
  !< call b64_decode(code='aGVsbG8=',s=code64)
  !< print "(L1)", code64=='hello'
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: code !< Encoded scalar.
  CHARACTER(*), INTENT(out) :: s !< String to be decoded.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.

  ALLOCATE (nI1P(1:byte_size(s))); nI1P = 0_I1P
  CALL decode_bits(code=code, bits=nI1P)
  s = TRANSFER(nI1P, s)
END SUBROUTINE b64_decode_string

PURE SUBROUTINE b64_decode_R16_a(code, n)
  !< Decode a base64 code into an array numbers (R16P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< real(R16P) :: array_R16(1:2)
  !< call b64_decode(code='AAAAAABAXkCPwvUoXI8CQA==',n=array_R16)
  !< print "(L1)", str(n=array_R16)==str(n=[121._R16P,2.32_R16P])
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: code !< Encoded array.
  REAL(R16P), INTENT(out) :: n(1:) !< Array of numbers to be decoded.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.

  ALLOCATE (nI1P(1:SIZE(n, dim=1) * BYR16P)); nI1P = 0_I1P
  CALL decode_bits(code=code, bits=nI1P)
  n = TRANSFER(nI1P, n)
END SUBROUTINE b64_decode_R16_a

PURE SUBROUTINE b64_decode_R8_a(code, n)
  !< Decode a base64 code into an array numbers (R8P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< real(R8P) :: array_R8(1:2)
  !< call b64_decode(code='AAAAAAAA8D8AAAAAAAAAQA==',n=array_R8)
  !< print "(L1)", str(n=array_R8)==str(n=[1._R8P,2._R8P])
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: code !< Encoded array.
  REAL(R8P), INTENT(out) :: n(1:) !< Array of numbers to be decoded.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.

  ALLOCATE (nI1P(1:SIZE(n, dim=1) * BYR8P)); nI1P = 0_I1P
  CALL decode_bits(code=code, bits=nI1P)
  n = TRANSFER(nI1P, n)
END SUBROUTINE b64_decode_R8_a

PURE SUBROUTINE b64_decode_R4_a(code, n)
  !< Decode a base64 code into an array numbers (R4P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< real(R4P) :: array_R4(1:2)
  !< call b64_decode(code='AAAAAOF6AMI=',n=array_R4)
  !< print "(L1)", str(n=array_R4)==str(n=[0._R4P,-32.12_R4P])
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: code !< Encoded array.
  REAL(R4P), INTENT(out) :: n(1:) !< Array of numbers to be decoded.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.

  ALLOCATE (nI1P(1:SIZE(n, dim=1) * BYR4P)); nI1P = 0_I1P
  CALL decode_bits(code=code, bits=nI1P)
  n = TRANSFER(nI1P, n)
END SUBROUTINE b64_decode_R4_a

PURE SUBROUTINE b64_decode_I8_a(code, n)
  !< Decode a base64 code into an array numbers (I8P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< integer(I8P) :: array_I8(1:4)
  !< call b64_decode(code='FwAAAAAAAABEAQAAAAAAABBwhAEAAAAAAgAAAAAAAAA=',n=array_I8)
  !< print "(L1)", str(n=array_I8)==str(n=[23_I8P,324_I8P,25456656_I8P,2_I8P])
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: code !< Encoded array.
  INTEGER(I8P), INTENT(out) :: n(1:) !< Array of numbers to be decoded.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.

  ALLOCATE (nI1P(1:SIZE(n, dim=1) * BYI8P)); nI1P = 0_I1P
  CALL decode_bits(code=code, bits=nI1P)
  n = TRANSFER(nI1P, n)
END SUBROUTINE b64_decode_I8_a

PURE SUBROUTINE b64_decode_I4_a(code, n)
  !< Decode a base64 code into an array numbers (I4P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< integer(I4P) :: array_I4(1:2)
  !< call b64_decode(code='5wcAAOj///8=',n=array_I4)
  !< print "(L1)", str(n=array_I4)==str(n=[2023_I4P,-24_I4P])
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: code !< Encoded array.
  INTEGER(I4P), INTENT(out) :: n(1:) !< Array of numbers to be decoded.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.

  ALLOCATE (nI1P(1:SIZE(n, dim=1) * BYI4P)); nI1P = 0_I1P
  CALL decode_bits(code=code, bits=nI1P)
  n = TRANSFER(nI1P, n)
END SUBROUTINE b64_decode_I4_a

PURE SUBROUTINE b64_decode_I2_a(code, n)
  !< Decode a base64 code into an array numbers (I2P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< integer(I2P) :: array_I2(1:2)
  !< call b64_decode(code='Nf/2/w==',n=array_I2)
  !< print "(L1)", str(n=array_I2)==str(n=[-203_I2P,-10_I2P])
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: code !< Encoded array.
  INTEGER(I2P), INTENT(out) :: n(1:) !< Array of numbers to be decoded.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.

  ALLOCATE (nI1P(1:SIZE(n, dim=1) * BYI2P)); nI1P = 0_I1P
  CALL decode_bits(code=code, bits=nI1P)
  n = TRANSFER(nI1P, n)
END SUBROUTINE b64_decode_I2_a

PURE SUBROUTINE b64_decode_I1_a(code, n)
  !< Decode a base64 code into an array numbers (I1P).
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< integer(I1P) :: array_I1(1:2)
  !< call b64_decode(code='eP8=',n=array_I1)
  !< print "(L1)", str(n=array_I1)==str(n=[120_I1P,-1_I1P])
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: code !< Encoded array.
  INTEGER(I1P), INTENT(out) :: n(1:) !< Array of numbers to be decoded.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.

  ALLOCATE (nI1P(1:SIZE(n, dim=1) * BYI1P)); nI1P = 0_I1P
  CALL decode_bits(code=code, bits=nI1P)
  n = TRANSFER(nI1P, n)
END SUBROUTINE b64_decode_I1_a

PURE SUBROUTINE b64_decode_string_a(code, s)
  !< Decode a base64 code into an array of strings.
  !<
  !<```fortran
  !< use befor64
  !< use penf
  !< character(5) :: array_s(1:2)
  !< call b64_decode(code='aGVsbG93b3JsZA==',s=array_s)
  !< print "(L1)", array_s(1)//array_s(2)=='helloworld'
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: code !< Encoded scalar.
  CHARACTER(*), INTENT(out) :: s(1:) !< String to be decoded.
  INTEGER(I1P), ALLOCATABLE :: nI1P(:) !< One byte integer array containing n.

  ALLOCATE (nI1P(1:byte_size(s(1)) * SIZE(s, dim=1))); nI1P = 0_I1P
  CALL decode_bits(code=code, bits=nI1P)
  s = TRANSFER(nI1P, s)
END SUBROUTINE b64_decode_string_a
endmodule befor64
