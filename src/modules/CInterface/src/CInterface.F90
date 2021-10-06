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

MODULE CInterface
USE GlobalData
USE StringiFor, ONLY: String
USE, INTRINSIC :: ISO_C_Binding, C_PTR=>C_PTR, &
  & C_CHAR_PTR => C_PTR, C_CONST_CHAR_PTR => C_PTR, &
  & C_void_ptr => C_PTR, C_CONST_VOID_PTR => C_PTR
IMPLICIT NONE
PRIVATE

PUBLIC :: C_CHAR_PTR, C_VOID_PTR, C_CONST_CHAR_PTR, C_CONST_VOID_PTR

INTEGER( I4B ), PUBLIC, PARAMETER :: C_ENUM = C_INT
  !! a C enum may not always be a standard C int
CHARACTER(LEN=1,KIND=C_CHAR), PUBLIC, PARAMETER :: NUL = C_NULL_CHAR
  !! C string terminator alais using the 3-letter ASCII name.
  !! The C_ prefix is not used because it is just an ASCII character.
  !! In C, "char" is distinct from "signed char", unlike integers.
  !! The plain "char" type is specific for text/string values, whereas
  !! "signed char" should indicate 1-byte integer data.
  !! Most ISO-C systems have wide chars "wchar_t", but Fortran compilers
  !! have limited support for different character kinds. UTF encoding
  !! adds more complexity. This should be updated as Fortran compilers
  !! include support for more character types.
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UNSIGNED = C_INT
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UNSIGNED_SHORT = C_SHORT
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UNSIGNED_LONG = C_LONG
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UNSIGNED_LONG_LONG = C_LONG_LONG
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UNSIGNED_CHAR = C_SIGNED_CHAR
INTEGER( I4B ), PUBLIC, PARAMETER :: C_SSIZE_T = C_SIZE_T
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UINT8_T = C_INT8_T
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UINT16_T = C_INT16_T
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UINT32_T = C_INT32_T
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UINT64_T = C_INT64_T
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UINT_LEAST8_T = C_INT_LEAST8_T
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UINT_LEAST16_T = C_INT_LEAST16_T
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UINT_LEAST32_T = C_INT_LEAST32_T
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UINT_LEAST64_T = C_INT_LEAST64_T
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UINT_FAST8_T = C_INT_FAST8_T
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UINT_FAST16_T = C_INT_FAST16_T
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UINT_FAST32_T = C_INT_FAST32_T
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UINT_FAST64_T = C_INT_FAST64_T
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UINTMAX_T = C_INTMAX_T
INTEGER( I4B ), PUBLIC, PARAMETER :: C_SHORT_INT = C_SHORT
INTEGER( I4B ), PUBLIC, PARAMETER :: C_LONG_INT = C_LONG
INTEGER( I4B ), PUBLIC, PARAMETER :: C_LONG_LONG_INT = C_LONG_LONG
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UNSIGNED_INT = C_UNSIGNED
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UNSIGNED_SHORT_INT = C_SHORT
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UNSIGNED_LONG_INT = C_LONG
INTEGER( I4B ), PUBLIC, PARAMETER :: C_UNSIGNED_LONG_LONG_INT = C_LONG_LONG

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Copy N bytes of SRC to DEST, no aliasing or overlapping allowed.
!
!# Introduction
!
! Copy N bytes of SRC to DEST, no aliasing or overlapping allowed.
!
!### CInterface
!
!```c
! extern void *memcpy (void *dest, const void *src, size_t n);
!```

INTERFACE
FUNCTION C_MEMCPY(dest, src, n) RESULT(result) BIND(C,name="memcpy")
  IMPORT C_void_ptr, C_SIZE_T
  TYPE(C_VOID_PTR) :: result
  TYPE(C_VOID_PTR), VALUE, INTENT(IN) :: dest
    !! target=intent(out)
  TYPE(C_VOID_PTR), VALUE, INTENT(IN) :: src
    !! target=INTENT(IN)
  INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: n
END FUNCTION C_MEMCPY
END INTERFACE

PUBLIC :: C_MEMCPY

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Copy N bytes of SRC to DEST, guaranteeing correct behavior for overlapping strings.
!
!# Introduction
! Copy N bytes of SRC to DEST, guaranteeing correct behavior for overlapping strings.
!
!
!### CInterface
!
!```c
! extern void *memmove (void *dest, const void *src, size_t n)
!```

INTERFACE
FUNCTION C_memmove(dest, src, n) result(result) bind(C,name="memmove")
  IMPORT C_void_ptr, C_SIZE_T
  TYPE(C_void_ptr) :: result
  TYPE(C_void_ptr), VALUE, INTENT(IN) :: dest ! target=intent(out)
  TYPE(C_void_ptr), VALUE, INTENT(IN) :: src
  INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: n
END FUNCTION C_memmove
END INTERFACE

PUBLIC :: C_memmove

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Set N bytes of S to C.
!
!# Introduction
! Set N bytes of S to C.
!
!### CInterface
!
!```c
!extern void *memset (void *s, int c, size_t n)
!```

INTERFACE
FUNCTION C_memset(s, c, n) RESULT(result) BIND(C,name="memset")
  IMPORT :: C_void_ptr, C_int, C_SIZE_T
  TYPE(C_void_ptr) :: result
  TYPE(C_void_ptr), VALUE, intent(in) :: s ! target=intent(out)
  INTEGER(C_int), VALUE, intent(in) :: c
  INTEGER(C_SIZE_T), VALUE, intent(in) :: n
END FUNCTION C_memset
END INTERFACE

PUBLIC :: C_memset

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Compare N bytes of S1 and S2.
!
!# Introduction
! Compare N bytes of S1 and S2.
!
!### CInterface
!
!```c
!extern int memcmp (const void *s1, const void *s2, size_t n)
!```

INTERFACE
PURE FUNCTION C_memcmp(s1, s2, n) RESULT(result) BIND(C,name="memcmp")
  IMPORT :: C_int, C_void_ptr, C_SIZE_T
  INTEGER(C_int) :: result
  TYPE(C_void_ptr), VALUE, INTENT( IN ) :: s1
  TYPE(C_void_ptr), VALUE, INTENT( IN ) :: s2
  INTEGER(C_SIZE_T), VALUE, INTENT( IN ) :: n
END FUNCTION C_memcmp
END INTERFACE

PUBLIC :: C_memcmp

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Search N bytes of S for C.
!
!# Introduction
! Search N bytes of S for C.
!
!### CInterface
!
!```c
!extern void *memchr (const void *s, int c, size_t n)
!```

INTERFACE
PURE FUNCTION C_memchr(s, c, n) RESULT(result) BIND(C,name="memchr")
  IMPORT :: C_void_ptr, C_int, C_SIZE_T
  TYPE(C_void_ptr) :: result
  TYPE(C_void_ptr), VALUE, INTENT(IN) :: s
  INTEGER(C_int), VALUE, INTENT(IN) :: c
  INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: n
END FUNCTION C_memchr
END INTERFACE

PUBLIC :: C_memchr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Copy SRC to DEST.
!
!# Introduction
! Copy SRC to DEST.
!
!### CInterface
!
!```c
!extern char *strcpy (char *dest, const char *src)
!```

INTERFACE
FUNCTION C_strcpy(dest, src) RESULT(result) BIND(C,name="strcpy")
  IMPORT :: C_CHAR_PTR, C_SIZE_T
  TYPE(C_CHAR_PTR) :: result
  TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: dest ! target=intent(out)
  TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: src
END FUNCTION C_strcpy
END INTERFACE

PUBLIC :: C_strcpy

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Copy no more than N characters of SRC to DEST.
!
!# Introduction
! Copy no more than N characters of SRC to DEST.
!
!
!### CInterface
!
!```c
!extern char *strncpy (char *dest, const char *src, size_t n)
!```

INTERFACE
    function C_strncpy(dest, src, n) result(result) bind(C,name="strncpy")
      import C_CHAR_PTR, C_SIZE_T
      type(C_CHAR_PTR) :: result
      type(C_CHAR_PTR), value, intent(in) :: dest ! target=intent(out)
      type(C_CHAR_PTR), value, intent(in) :: src
      integer(C_SIZE_T), value, intent(in) :: n
    end function C_strncpy
END INTERFACE

PUBLIC :: C_strncpy

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Append SRC onto DEST.
!
!# Introduction
! Append SRC onto DEST.
!
!
!### CInterface
!
!```c
!extern char *strcat (char *dest, const char *src)
!```

INTERFACE
FUNCTION C_strcat(dest, src) result(result) bind(C,name="strcat")
  IMPORT :: C_CHAR_PTR, C_SIZE_T
  TYPE(C_CHAR_PTR) :: result
  TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: dest
    !! target=intent(out)
  TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: src
END FUNCTION C_strcat
END INTERFACE

PUBLIC :: C_strcat

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Append no more than N characters from SRC onto DEST.
!
!# Introduction
! Append no more than N characters from SRC onto DEST.
!
!### CInterface
!
!```c
!extern char *strncat (char *dest, const char *src, size_t n)
!```

INTERFACE
FUNCTION C_strncat(dest, src, n) RESULT(result) BIND(C,name="strncat")
  IMPORT :: C_CHAR_PTR, C_SIZE_T
  TYPE(C_CHAR_PTR) :: result
  TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: dest
    !! target=intent(out)
  TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: src
  INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: n
END FUNCTION C_strncat
END INTERFACE

PUBLIC :: C_strncat

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Compare S1 and S2.
!
!# Introduction
! Compare S1 and S2.
!
!### CInterface
!
!```c
!extern int strcmp (const char *s1, const char *s2)
!```

INTERFACE
PURE FUNCTION C_strcmp(s1, s2) RESULT(result) BIND(C,name="strcmp")
  IMPORT :: C_int, C_CHAR_PTR, C_SIZE_T
  INTEGER(C_int) :: result
  TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: s1
  TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: s2
END FUNCTION C_strcmp
END INTERFACE

PUBLIC :: C_strcmp

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Compare N characters of S1 and S2.
!
!# Introduction
! Compare N characters of S1 and S2.
!
!### CInterface
!
!```c
!extern int strncmp (const char *s1, const char *s2, size_t n)
!```

INTERFACE
PURE FUNCTION C_strncmp(s1, s2, n) result(result) bind(C,name="strncmp")
  IMPORT :: C_int, C_CHAR_PTR, C_SIZE_T
  INTEGER(C_int) :: result
  TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: s1
  TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: s2
  INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: n
END FUNCTION C_strncmp
END INTERFACE

PUBLIC :: C_strncmp

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Return the length of S.
!
!# Introduction
! Return the length of S.
!
!### CInterface
!
!```c
!extern size_t strlen (const char *s)
!```

INTERFACE
PURE FUNCTION C_strlen(s) RESULT(result) BIND(C,name="strlen")
  IMPORT :: C_CHAR_PTR, C_SIZE_T
  INTEGER(C_SIZE_T) :: result
  TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: s  !character(len=*), intent(in)
END FUNCTION C_strlen
END INTERFACE

PUBLIC :: C_strlen

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: CAlloc function
!
!# Introduction
! CAlloc function.
!
!### CInterface
!
!```c
!void *calloc(size_t nmemb, size_t size);
!```

INTERFACE
TYPE(C_void_ptr) FUNCTION C_calloc(nmemb, size) BIND(C,name="calloc")
  IMPORT :: C_void_ptr, C_SIZE_T
  INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: nmemb, size
END FUNCTION C_calloc
END INTERFACE

PUBLIC :: C_calloc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary:
!
!### CInterface
!
!```c
! void *malloc(size_t size);
!```

INTERFACE
TYPE(C_void_ptr) FUNCTION C_malloc(size) BIND(C,name="malloc")
  IMPORT :: C_void_ptr, C_SIZE_T
  INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: size
END FUNCTION C_malloc
END INTERFACE

PUBLIC :: C_malloc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary:
!
!### Usage
!
!```fortran
! void free(void *ptr);
!```

INTERFACE
SUBROUTINE C_free(ptr) BIND(C,name="free")
  IMPORT :: C_void_ptr
  TYPE( C_void_ptr ), VALUE, INTENT( IN ) :: ptr
END SUBROUTINE C_free
END INTERFACE

PUBLIC :: C_free

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary:
!
!
!### CInterface
!
!```c
! void *realloc(void *ptr, size_t size);
!```

INTERFACE
TYPE(C_void_ptr) FUNCTION C_realloc(ptr,size) BIND(C,name="realloc")
  IMPORT :: C_void_ptr, C_SIZE_T
  TYPE(C_void_ptr), VALUE, INTENT(IN) :: ptr
  INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: size
END FUNCTION C_realloc
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE F_string_assign_C_string
END INTERFACE ASSIGNMENT(=)

PUBLIC :: ASSIGNMENT( = )
PUBLIC :: C_ASSOCIATED_PURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE C_F_STRING
  MODULE PROCEDURE F_string_assign_C_string
  MODULE PROCEDURE C_F_STRING_CHARS
END INTERFACE C_F_STRING

PUBLIC :: C_F_STRING

INTERFACE C2Fortran
  MODULE PROCEDURE F_string_assign_C_string, C_F_STRING_CHARS
END INTERFACE C2Fortran

PUBLIC :: C2Fortran

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE F_C_STRING
  MODULE PROCEDURE F_C_STRING_CHARS, F_C_STRING_PTR
END INTERFACE F_C_STRING

PUBLIC F_C_STRING

INTERFACE FortranToC
  MODULE PROCEDURE F_C_STRING_CHARS, F_C_STRING_PTR
END INTERFACE FortranToC

PUBLIC :: FortranToC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: C_STRLEN_SAFE, F_C_STRING_DUP, C_STRING_VALUE, C_STRING_ALLOC
PUBLIC :: C_STRING_FREE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE C_PTR_TO_INT_VEC
  MODULE PROCEDURE C_PTR_TO_Int8_VEC, C_PTR_TO_Int16_VEC, &
    & C_PTR_TO_Int32_VEC, C_PTR_TO_Int64_VEC
END INTERFACE C_PTR_TO_INT_VEC

PUBLIC :: C_PTR_TO_INT_VEC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE C_PTR_TO_Real_VEC
  MODULE PROCEDURE C_PTR_TO_Real32_VEC, C_PTR_TO_Real64_VEC
END INTERFACE C_PTR_TO_Real_VEC

PUBLIC :: C_PTR_TO_Real_VEC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE C2Fortran
  MODULE PROCEDURE C_PTR_TO_Int8_VEC, C_PTR_TO_Int16_VEC, &
    & C_PTR_TO_Int32_VEC, C_PTR_TO_Int64_VEC
  MODULE PROCEDURE C_PTR_TO_Real32_VEC, C_PTR_TO_Real64_VEC
END INTERFACE C2Fortran

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary:

PURE LOGICAL FUNCTION C_ASSOCIATED_PURE(ptr) RESULT(associated)
  TYPE(C_ptr), INTENT(IN) :: ptr
  INTEGER( C_INTPTR_T ) :: iptr
  iptr = TRANSFER(ptr,iptr)
  ASSOCIATED = (iptr /= 0)
END FUNCTION C_ASSOCIATED_PURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Set a fixed-length Fortran string to the value of a C string.
!
!# Introduction
! Copy a C string, passed by pointer, to a Fortran string.
! If the C pointer is NULL, the Fortran string is blanked.
! C_string must be NUL terminated, or at least as long as F_string.
! If C_string is longer, it is truncated. Otherwise, F_string is
! blank-padded at the end.

SUBROUTINE F_string_assign_C_string(F_string, C_string)
  CHARACTER(LEN=*), INTENT(OUT) :: F_string
  TYPE(C_CHAR_PTR), INTENT(IN) :: C_string
  !> internal variables
  CHARACTER( LEN=1, KIND=C_CHAR ), POINTER :: p_chars(:)
  INTEGER( I4B ) :: i
  !> main
  IF( .NOT. C_ASSOCIATED(C_string) ) THEN
    F_string = ''
  ELSE
    CALL C_F_POINTER(C_string,p_chars,[huge(0)])
    i=1
    DO WHILE( p_chars(i) .NE. NUL .AND. I .LE. LEN(F_string) )
      F_string(i:i) = p_chars(i); i=i+1
    END DO
    IF(i .LT. LEN(F_string)) F_string(i:) = ' '
  END IF
END SUBROUTINE F_string_assign_C_string

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Copy a C string, passed as a char-array reference, to a Fortran string.
!
!# Introduction
!
! Copy a C string, passed by pointer, to a Fortran string.
! If the C pointer is NULL, the Fortran string is blanked.
! C_string must be NUL terminated, or at least as long as F_string.
! If C_string is longer, it is truncated. Otherwise, F_string is
! blank-padded at the end.

SUBROUTINE C_F_string_chars(C_string, F_string)
  CHARACTER( LEN=1, KIND=C_CHAR), INTENT(IN) :: C_string(*)
  CHARACTER( LEN=* ), INTENT(OUT) :: F_string
  !> internal variable
  INTEGER( I4B ) :: i
  i=1
  DO WHILE(C_string(i) .NE. NUL .AND. i .LE. LEN(F_string))
    F_string(i:i) = C_string(i)
    i=i+1
  END DO
  IF( i .LT. LEN(F_string) ) F_string(i:) = ' '
END SUBROUTINE C_F_string_chars

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Copy a Fortran string to an allocated C string pointer.
!
!# Introduction
!
! Copy a Fortran string to an allocated C string pointer.
! If the C pointer is NULL, no action is taken. (Maybe auto allocate via libc call?)
! If the length is not passed, the C string must be at least: len(F_string)+1
! If the length is passed and F_string is too long, it is truncated.

SUBROUTINE F_C_STRING_PTR(F_string, C_string, C_string_len)
  CHARACTER(LEN=*), INTENT(IN) :: F_string
  TYPE( C_CHAR_PTR ), INTENT(IN) :: C_string
    !! target = intent(out)
  INTEGER( I4B ), INTENT(IN), OPTIONAL :: C_string_len
    !! Max string length,
    !! INCLUDING THE TERMINAL NUL
  !> internal variables
  CHARACTER( LEN=1, KIND=C_CHAR ), DIMENSION(:), POINTER :: p_chars
  INTEGER( I4B ) :: i, strlen
  !> main
  strlen = LEN(F_string)
  IF( PRESENT( C_string_len ) ) THEN
    IF( C_string_len .LE. 0 ) RETURN
    strlen = MIN(strlen,C_string_len-1)
  END IF
  IF( .NOT. C_ASSOCIATED(C_string)) RETURN
  CALL C_F_POINTER(C_string,p_chars,[strlen+1])
  FORALL (i=1:strlen)
    p_chars(i) = F_string(i:i)
  END FORALL
  p_chars(strlen+1) = NUL
END SUBROUTINE F_C_STRING_PTR

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Copy a Fortran string to a C string passed by char-array reference.
!
!# Introduction
!
! Copy a Fortran string to a C string passed by char-array reference.
! If the length is not passed, the C string must be at least: len(F_string)+1
! If the length is passed and F_string is too long, it is truncated.

SUBROUTINE F_C_STRING_CHARS(F_string, C_string, C_string_len)
  CHARACTER(LEN=*), INTENT(IN) :: F_string
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: C_string
  INTEGER( I4B ), INTENT(IN), OPTIONAL :: C_string_len
    !! Max string length,
    !! INCLUDING THE TERMINAL NUL
  INTEGER( I4B ) :: i, strlen
  strlen = LEN(F_string)
  IF( PRESENT(C_string_len) ) THEN
    IF (C_string_len .LE. 0) RETURN
    strlen = MIN(strlen,C_string_len-1)
  END IF
  FORALL(i=1:strlen)
    C_string(i) = F_string(i:i)
  END FORALL
  C_string(strlen+1) = NUL
END SUBROUTINE F_C_STRING_CHARS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Convert Fortran string to C string

FUNCTION F_C_STRING_DUP(F_string,length) RESULT(C_string)
  CHARACTER( LEN=* ), INTENT( IN ) :: F_string
  INTEGER, INTENT(IN), OPTIONAL :: length
  TYPE( C_PTR ) :: C_string
  !> internal variables
  CHARACTER(LEN=1,KIND=C_CHAR), POINTER :: C_string_ptr(:)
  INTEGER( I4B ) :: i
  INTEGER( C_SIZE_T ) :: strlen
  !> main
  IF( PRESENT(length) ) THEN
    strlen = length
  ELSE
    strlen = LEN(F_string)
  END IF
  IF( strlen .LE. 0 ) THEN
    C_string = C_NULL_PTR
  ELSE
    C_string = C_MALLOC(strlen+1)
    IF( C_ASSOCIATED( C_string ) ) THEN
      CALL C_F_POINTER(C_string,C_string_ptr,[strlen+1])
      FORALL( i=1:strlen )
        C_string_ptr(i) = F_string(i:i)
      END FORALL
      C_string_ptr(strlen+1) = NUL
    END IF
  END IF
END FUNCTION F_C_STRING_DUP

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: This function returns length of string

PURE FUNCTION C_STRLEN_SAFE(s) RESULT(length)
  INTEGER(C_SIZE_T) :: length
  TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: s
  !>
  IF( .NOT. C_ASSOCIATED_PURE( s ) ) THEN
    length = 0
  ELSE
    length = C_STRLEN(s)
  END IF
END FUNCTION C_STRLEN_SAFE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Returns the value of target of C string

FUNCTION C_STRING_VALUE(C_string) result(F_string)
  TYPE(C_CHAR_PTR), INTENT(IN) :: C_string
  CHARACTER(LEN=C_STRLEN_SAFE(C_string)) :: F_string
  !> internal variables
  CHARACTER(LEN=1,kind=C_char), DIMENSION(:), POINTER :: p_chars
  INTEGER( I4B ) :: i, length
  !> main
  length = LEN(F_string)
  IF( length .NE. 0 ) THEN
    CALL C_F_POINTER(C_string,p_chars,[length])
    FORALL (i=1:length)
      F_string(i:i) = p_chars(i)
    END FORALL
  END IF
END FUNCTION C_STRING_VALUE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Allocate memory space and return C string

FUNCTION C_STRING_ALLOC(length) result(C_string)
  INTEGER(C_SIZE_T), INTENT(IN) :: length
  TYPE(C_PTR) :: C_String
  !> internal variables
  CHARACTER(LEN=1,KIND=C_CHAR), POINTER :: C_CHARPTR
  !> main
  C_string = C_MALLOC(length+1)
  IF( C_ASSOCIATED(C_string) ) THEN
    CALL C_F_POINTER(C_string,C_charptr)
    C_CHARPTR = NUL
  END IF
END FUNCTION C_STRING_ALLOC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE C_STRING_FREE(string)
  TYPE(C_PTR), INTENT(INOUT) :: string
  IF( C_ASSOCIATED( string ) ) THEN
    CALL C_FREE(string)
    string = C_NULL_PTR
  END IF
END SUBROUTINE C_STRING_FREE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Converts C pointer to integer vector

SUBROUTINE C_PTR_TO_Int8_VEC(vec, cptr)
  INTEGER( Int8 ), INTENT( OUT ) :: vec( : )
  TYPE( C_PTR ), INTENT( IN ) :: cptr
  !> Internal variables
  INTEGER( I4B ) :: n, ii
  INTEGER( Int8 ), POINTER :: p( : )
  !> main
  n = SIZE( vec ); vec = 0
  IF( C_ASSOCIATED( cptr ) ) THEN
    CALL C_F_POINTER( cptr, p, [n])
    DO ii = 1, n
      vec( ii ) = p(ii)
    END DO
    DEALLOCATE( p )
  END IF
END SUBROUTINE C_PTR_TO_Int8_VEC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Converts C pointer to integer vector

SUBROUTINE C_PTR_TO_Int16_VEC(vec, cptr)
  INTEGER( Int16 ), INTENT( OUT ) :: vec( : )
  TYPE( C_PTR ), INTENT( IN ) :: cptr
  !> Internal variables
  INTEGER( I4B ) :: n, ii
  INTEGER( Int16 ), POINTER :: p( : )
  !> main
  n = SIZE( vec ); vec = 0
  IF( C_ASSOCIATED( cptr ) ) THEN
    CALL C_F_POINTER( cptr, p, [n])
    DO ii = 1, n
      vec( ii ) = p(ii)
    END DO
    DEALLOCATE( p )
  END IF
END SUBROUTINE C_PTR_TO_Int16_VEC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Converts C pointer to integer vector

SUBROUTINE C_PTR_TO_Int32_VEC(vec, cptr)
  INTEGER( Int32 ), INTENT( OUT ) :: vec( : )
  TYPE( C_PTR ), INTENT( IN ) :: cptr
  !> Internal variables
  INTEGER( I4B ) :: n, ii
  INTEGER( Int32 ), POINTER :: p( : )
  !> main
  n = SIZE( vec ); vec = 0
  IF( C_ASSOCIATED( cptr ) ) THEN
    CALL C_F_POINTER( cptr, p, [n])
    DO ii = 1, n
      vec( ii ) = p(ii)
    END DO
    DEALLOCATE( p )
  END IF
END SUBROUTINE C_PTR_TO_Int32_VEC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Converts C pointer to integer vector

SUBROUTINE C_PTR_TO_Int64_VEC(vec, cptr)
  INTEGER( Int64 ), INTENT( OUT ) :: vec( : )
  TYPE( C_PTR ), INTENT( IN ) :: cptr
  !> Internal variables
  INTEGER( I4B ) :: n, ii
  INTEGER( Int64 ), POINTER :: p( : )
  !> main
  n = SIZE( vec ); vec = 0
  IF( C_ASSOCIATED( cptr ) ) THEN
    CALL C_F_POINTER( cptr, p, [n])
    DO ii = 1, n
      vec( ii ) = p(ii)
    END DO
    DEALLOCATE( p )
  END IF
END SUBROUTINE C_PTR_TO_Int64_VEC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Converts C pointer to real vector

SUBROUTINE C_PTR_TO_Real32_VEC(vec, cptr)
  REAL( Real32 ), INTENT( OUT ) :: vec( : )
  TYPE( C_PTR ), INTENT( IN ) :: cptr
  ! Internal variables
  INTEGER :: n, ii
  REAL( Real32 ), POINTER :: p( : )
  !> main
  n = SIZE( vec ); vec = 0
  IF( C_ASSOCIATED( cptr ) ) THEN
    CALL C_F_POINTER( cptr, p, [n])
    DO ii = 1, n
      vec( ii ) = p(ii)
    END DO
    DEALLOCATE( p )
  END IF
END SUBROUTINE C_PTR_TO_Real32_VEC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Converts C pointer to real vector

SUBROUTINE C_PTR_TO_Real64_VEC(vec, cptr)
  REAL( Real64 ), INTENT( OUT ) :: vec( : )
  TYPE( C_PTR ), INTENT( IN ) :: cptr
  ! Internal variables
  INTEGER :: n, ii
  REAL( Real64 ), POINTER :: p( : )
  !> main
  n = SIZE( vec ); vec = 0
  IF( C_ASSOCIATED( cptr ) ) THEN
    CALL C_F_POINTER( cptr, p, [n])
    DO ii = 1, n
      vec( ii ) = p(ii)
    END DO
    DEALLOCATE( p )
  END IF
END SUBROUTINE C_PTR_TO_Real64_VEC

END MODULE CInterface