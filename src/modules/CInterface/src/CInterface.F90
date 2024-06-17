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
USE String_Class, ONLY: String
USE, INTRINSIC :: ISO_C_BINDING, C_PTR => C_PTR, &
  & C_CHAR_PTR => C_PTR, C_CONST_CHAR_PTR => C_PTR, &
  & C_void_ptr => C_PTR, C_CONST_VOID_PTR => C_PTR
IMPLICIT NONE
PRIVATE

PUBLIC :: C_CHAR_PTR, C_VOID_PTR, C_CONST_CHAR_PTR, C_CONST_VOID_PTR
PUBLIC :: CString

INTEGER(I4B), PUBLIC, PARAMETER :: C_ENUM = C_INT
  !! a C enum may not always be a standard C int
CHARACTER(1, KIND=C_CHAR), PUBLIC, PARAMETER :: NUL = C_NULL_CHAR
  !! C string terminator alais using the 3-letter ASCII name.
  !! The C_ prefix is not used because it is just an ASCII character.
  !! In C, "char" is distinct from "signed char", unlike integers.
  !! The plain "char" type is specific for text/string values, whereas
  !! "signed char" should indicate 1-byte integer data.
  !! Most ISO-C systems have wide chars "wchar_t", but Fortran compilers
  !! have limited support for different character kinds. UTF encoding
  !! adds more complexity. This should be updated as Fortran compilers
  !! include support for more character types.
INTEGER(I4B), PUBLIC, PARAMETER :: C_UNSIGNED = C_INT
INTEGER(I4B), PUBLIC, PARAMETER :: C_UNSIGNED_SHORT = C_SHORT
INTEGER(I4B), PUBLIC, PARAMETER :: C_UNSIGNED_LONG = C_LONG
INTEGER(I4B), PUBLIC, PARAMETER :: C_UNSIGNED_LONG_LONG = C_LONG_LONG
INTEGER(I4B), PUBLIC, PARAMETER :: C_UNSIGNED_CHAR = C_SIGNED_CHAR
INTEGER(I4B), PUBLIC, PARAMETER :: C_SSIZE_T = C_SIZE_T
INTEGER(I4B), PUBLIC, PARAMETER :: C_UINT8_T = C_INT8_T
INTEGER(I4B), PUBLIC, PARAMETER :: C_UINT16_T = C_INT16_T
INTEGER(I4B), PUBLIC, PARAMETER :: C_UINT32_T = C_INT32_T
INTEGER(I4B), PUBLIC, PARAMETER :: C_UINT64_T = C_INT64_T
INTEGER(I4B), PUBLIC, PARAMETER :: C_UINT_LEAST8_T = C_INT_LEAST8_T
INTEGER(I4B), PUBLIC, PARAMETER :: C_UINT_LEAST16_T = C_INT_LEAST16_T
INTEGER(I4B), PUBLIC, PARAMETER :: C_UINT_LEAST32_T = C_INT_LEAST32_T
INTEGER(I4B), PUBLIC, PARAMETER :: C_UINT_LEAST64_T = C_INT_LEAST64_T
INTEGER(I4B), PUBLIC, PARAMETER :: C_UINT_FAST8_T = C_INT_FAST8_T
INTEGER(I4B), PUBLIC, PARAMETER :: C_UINT_FAST16_T = C_INT_FAST16_T
INTEGER(I4B), PUBLIC, PARAMETER :: C_UINT_FAST32_T = C_INT_FAST32_T
INTEGER(I4B), PUBLIC, PARAMETER :: C_UINT_FAST64_T = C_INT_FAST64_T
INTEGER(I4B), PUBLIC, PARAMETER :: C_UINTMAX_T = C_INTMAX_T
INTEGER(I4B), PUBLIC, PARAMETER :: C_SHORT_INT = C_SHORT
INTEGER(I4B), PUBLIC, PARAMETER :: C_LONG_INT = C_LONG
INTEGER(I4B), PUBLIC, PARAMETER :: C_LONG_LONG_INT = C_LONG_LONG
INTEGER(I4B), PUBLIC, PARAMETER :: C_UNSIGNED_INT = C_UNSIGNED
INTEGER(I4B), PUBLIC, PARAMETER :: C_UNSIGNED_SHORT_INT = C_SHORT
INTEGER(I4B), PUBLIC, PARAMETER :: C_UNSIGNED_LONG_INT = C_LONG
INTEGER(I4B), PUBLIC, PARAMETER :: C_UNSIGNED_LONG_LONG_INT = C_LONG_LONG

PUBLIC :: C_MEMCPY
PUBLIC :: C_memmove
PUBLIC :: C_memset
PUBLIC :: C_memcmp
PUBLIC :: C_memchr
PUBLIC :: C_strcpy
PUBLIC :: C_strncpy
PUBLIC :: C_strcat
PUBLIC :: C_strncat
PUBLIC :: C_strcmp
PUBLIC :: C_strncmp
PUBLIC :: C_strlen
PUBLIC :: C_calloc
PUBLIC :: C_malloc
PUBLIC :: C_free
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: C_ASSOCIATED_PURE
PUBLIC :: C_F_STRING
PUBLIC :: FString
PUBLIC :: F_C_STRING
PUBLIC :: C_STRLEN_SAFE
PUBLIC :: F_C_STRING_DUP
PUBLIC :: C_STRING_VALUE
PUBLIC :: C_STRING_ALLOC
PUBLIC :: C_STRING_FREE
PUBLIC :: C_PTR_TO_INT_VEC
PUBLIC :: C_PTR_TO_Real_VEC
PUBLIC :: C2Fortran
PUBLIC :: optval_c_int
PUBLIC :: optval_c_size_t
PUBLIC :: optval_c_double
PUBLIC :: optval_c_bool

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE C_F_STRING
  MODULE PROCEDURE F_string_assign_C_string
  MODULE PROCEDURE C_F_STRING_CHARS
END INTERFACE C_F_STRING

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE F_string_assign_C_string
END INTERFACE ASSIGNMENT(=)

INTERFACE FString
  MODULE PROCEDURE Fstring1
END INTERFACE FString

INTERFACE F_C_STRING
  MODULE PROCEDURE F_C_STRING_CHARS, F_C_STRING_PTR
END INTERFACE F_C_STRING

INTERFACE C_PTR_TO_INT_VEC
  MODULE PROCEDURE C_PTR_TO_Int8_VEC, C_PTR_TO_Int16_VEC, &
    & C_PTR_TO_Int32_VEC, C_PTR_TO_Int64_VEC
END INTERFACE C_PTR_TO_INT_VEC

INTERFACE C_PTR_TO_Real_VEC
  MODULE PROCEDURE C_PTR_TO_Real32_VEC, C_PTR_TO_Real64_VEC
END INTERFACE C_PTR_TO_Real_VEC

INTERFACE C2Fortran
  MODULE PROCEDURE C_PTR_TO_Int8_VEC, C_PTR_TO_Int16_VEC, &
    & C_PTR_TO_Int32_VEC, C_PTR_TO_Int64_VEC, C_PTR_TO_Real32_VEC,  &
    & C_PTR_TO_Real64_VEC, F_string_assign_C_string,  &
    & C_F_STRING_CHARS
END INTERFACE C2Fortran

INTERFACE optval_c_int
  MODULE PROCEDURE optval_c_int_1
END INTERFACE optval_c_int

INTERFACE optval_c_size_t
  MODULE PROCEDURE optval_c_size_t_1, optval_c_size_t_2
END INTERFACE optval_c_size_t

INTERFACE optval_c_double
  MODULE PROCEDURE optval_c_double_1, optval_c_double_2
END INTERFACE optval_c_double

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
  FUNCTION C_MEMCPY(dest, src, n) RESULT(RESULT) BIND(C, name="memcpy")
    IMPORT C_void_ptr, C_SIZE_T
    TYPE(C_VOID_PTR) :: RESULT
    TYPE(C_VOID_PTR), VALUE, INTENT(IN) :: dest
    !! target=intent(out)
    TYPE(C_VOID_PTR), VALUE, INTENT(IN) :: src
    !! target=INTENT(IN)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: n
  END FUNCTION C_MEMCPY
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Copy N bytes of SRC to DEST, guaranteeing correct behavior
! for overlapping strings.
!
!# Introduction
! Copy N bytes of SRC to DEST, guaranteeing correct behavior for
! overlapping strings.
!
!### CInterface
!
!```c
! extern void *memmove (void *dest, const void *src, size_t n)
!```

INTERFACE
  FUNCTION C_memmove(dest, src, n) RESULT(RESULT) BIND(C, name="memmove")
    IMPORT C_void_ptr, C_SIZE_T
    TYPE(C_void_ptr) :: RESULT
    TYPE(C_void_ptr), VALUE, INTENT(IN) :: dest ! target=intent(out)
    TYPE(C_void_ptr), VALUE, INTENT(IN) :: src
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: n
  END FUNCTION C_memmove
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
  FUNCTION C_memset(s, c, n) RESULT(RESULT) BIND(C, name="memset")
    IMPORT :: C_void_ptr, C_INT, C_SIZE_T
    TYPE(C_void_ptr) :: RESULT
    TYPE(C_void_ptr), VALUE, INTENT(in) :: s ! target=intent(out)
    INTEGER(C_INT), VALUE, INTENT(in) :: c
    INTEGER(C_SIZE_T), VALUE, INTENT(in) :: n
  END FUNCTION C_memset
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
  PURE FUNCTION C_memcmp(s1, s2, n) RESULT(RESULT) BIND(C, name="memcmp")
    IMPORT :: C_INT, C_void_ptr, C_SIZE_T
    INTEGER(C_INT) :: RESULT
    TYPE(C_void_ptr), VALUE, INTENT(IN) :: s1
    TYPE(C_void_ptr), VALUE, INTENT(IN) :: s2
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: n
  END FUNCTION C_memcmp
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
  PURE FUNCTION C_memchr(s, c, n) RESULT(RESULT) BIND(C, name="memchr")
    IMPORT :: C_void_ptr, C_INT, C_SIZE_T
    TYPE(C_void_ptr) :: RESULT
    TYPE(C_void_ptr), VALUE, INTENT(IN) :: s
    INTEGER(C_INT), VALUE, INTENT(IN) :: c
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: n
  END FUNCTION C_memchr
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
  FUNCTION C_strcpy(dest, src) RESULT(RESULT) BIND(C, name="strcpy")
    IMPORT :: C_CHAR_PTR, C_SIZE_T
    TYPE(C_CHAR_PTR) :: RESULT
    TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: dest ! target=intent(out)
    TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: src
  END FUNCTION C_strcpy
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
  FUNCTION C_strncpy(dest, src, n) RESULT(RESULT) BIND(C, name="strncpy")
    IMPORT C_CHAR_PTR, C_SIZE_T
    TYPE(C_CHAR_PTR) :: RESULT
    TYPE(C_CHAR_PTR), VALUE, INTENT(in) :: dest ! target=intent(out)
    TYPE(C_CHAR_PTR), VALUE, INTENT(in) :: src
    INTEGER(C_SIZE_T), VALUE, INTENT(in) :: n
  END FUNCTION C_strncpy
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
  FUNCTION C_strcat(dest, src) RESULT(RESULT) BIND(C, name="strcat")
    IMPORT :: C_CHAR_PTR, C_SIZE_T
    TYPE(C_CHAR_PTR) :: RESULT
    TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: dest
    !! target=intent(out)
    TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: src
  END FUNCTION C_strcat
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
  FUNCTION C_strncat(dest, src, n) RESULT(RESULT) BIND(C, name="strncat")
    IMPORT :: C_CHAR_PTR, C_SIZE_T
    TYPE(C_CHAR_PTR) :: RESULT
    TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: dest
    !! target=intent(out)
    TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: src
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: n
  END FUNCTION C_strncat
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
  PURE FUNCTION C_strcmp(s1, s2) RESULT(RESULT) BIND(C, name="strcmp")
    IMPORT :: C_INT, C_CHAR_PTR, C_SIZE_T
    INTEGER(C_INT) :: RESULT
    TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: s1
    TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: s2
  END FUNCTION C_strcmp
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
  PURE FUNCTION C_strncmp(s1, s2, n) RESULT(RESULT) BIND(C, name="strncmp")
    IMPORT :: C_INT, C_CHAR_PTR, C_SIZE_T
    INTEGER(C_INT) :: RESULT
    TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: s1
    TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: s2
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: n
  END FUNCTION C_strncmp
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
  PURE FUNCTION C_strlen(s) RESULT(RESULT) BIND(C, name="strlen")
    IMPORT :: C_CHAR_PTR, C_SIZE_T
    INTEGER(C_SIZE_T) :: RESULT
    TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: s !character(len=*), intent(in)
  END FUNCTION C_strlen
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
  TYPE(C_void_ptr) FUNCTION C_calloc(nmemb, size) BIND(C, name="calloc")
    IMPORT :: C_void_ptr, C_SIZE_T
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: nmemb, size
  END FUNCTION C_calloc
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary:
!
!### CInterface
!
!```c
! void *malloc(size_t size);
!```

INTERFACE
  TYPE(C_void_ptr) FUNCTION C_malloc(size) BIND(C, name="malloc")
    IMPORT :: C_void_ptr, C_SIZE_T
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: size
  END FUNCTION C_malloc
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary:
!
!### Usage
!
!```fortran
! void free(void *ptr);
!```

INTERFACE
  SUBROUTINE C_free(ptr) BIND(C, name="free")
    IMPORT :: C_void_ptr
    TYPE(C_void_ptr), VALUE, INTENT(IN) :: ptr
  END SUBROUTINE C_free
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
  TYPE(C_void_ptr) FUNCTION C_realloc(ptr, size) BIND(C, name="realloc")
    IMPORT :: C_void_ptr, C_SIZE_T
    TYPE(C_void_ptr), VALUE, INTENT(IN) :: ptr
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: size
  END FUNCTION C_realloc
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary:

PURE LOGICAL FUNCTION C_ASSOCIATED_PURE(ptr) RESULT(associated)
  TYPE(C_PTR), INTENT(IN) :: ptr
  INTEGER(C_INTPTR_T) :: iptr
  iptr = TRANSFER(ptr, iptr)
  ASSOCIATED = (iptr /= 0)
END FUNCTION C_ASSOCIATED_PURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
  CHARACTER(*), INTENT(OUT) :: F_string
  TYPE(C_CHAR_PTR), INTENT(IN) :: C_string
  !> internal variables
  CHARACTER(1, KIND=C_CHAR), POINTER :: p_chars(:)
  INTEGER(I4B) :: i
  !> main
  IF (.NOT. C_ASSOCIATED(C_string)) THEN
    F_string = ''
  ELSE
    CALL C_F_POINTER(C_string, p_chars, [HUGE(0)])
    i = 1
    DO WHILE (p_chars(i) .NE. NUL .AND. I .LE. LEN(F_string))
      F_string(i:i) = p_chars(i); i = i + 1
    END DO
    IF (i .LT. LEN(F_string)) F_string(i:) = ' '
  END IF
END SUBROUTINE F_string_assign_C_string

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
  CHARACTER(1, KIND=C_CHAR), INTENT(IN) :: C_string(*)
  CHARACTER(*), INTENT(OUT) :: F_string
  !! F_String is fortran string, it should be allocated
  !! before calling the routine
  !
  ! internal variable
  !
  INTEGER(I4B) :: i
  i = 1
  DO WHILE (C_string(i) .NE. NUL .AND. i .LE. LEN(F_string))
    F_string(i:i) = C_string(i)
    i = i + 1
  END DO
  IF (i .LT. LEN(F_string)) F_string(i:) = ' '
END SUBROUTINE C_F_string_chars

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION Fstring1(C_string) RESULT(F_string)
  CHARACTER(1, KIND=C_CHAR), INTENT(IN) :: C_string(:)
  CHARACTER(:), ALLOCATABLE :: F_string
  !!
  INTEGER(I4B) :: i, n, m
  n = SIZE(C_string)
  m = 0
  DO i = 1, n - 1
    IF (C_string(i) .EQ. NUL) THEN
      EXIT
    ELSE
      m = m + 1
    END IF
  END DO
  ALLOCATE (CHARACTER(m) :: F_string)
  DO i = 1, m
    F_string(i:i) = C_string(i)
  END DO
END FUNCTION Fstring1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! FUNCTION Fstring2(C_string) RESULT(F_string)
!   TYPE(C_CHAR_PTR), INTENT(IN) :: C_string
!   !! C pointer
!   CHARACTER(:), ALLOCATABLE :: F_string
!   !! Fortran string
!
!   ! ! internal variables
!   ! CHARACTER(1, KIND=C_CHAR), POINTER :: p_chars(:)
!   ! INTEGER(I4B) :: i, n, m
!   !
!   ! !> main
!   ! IF (.NOT. C_ASSOCIATED(C_string)) THEN
!   !   F_string = ''
!   !   RETURN
!   ! ELSE
!   !   CALL C_F_POINTER(C_string, p_chars, [HUGE(0)])
!   !   i = 1
!   !   DO WHILE (p_chars(i) .NE. NUL .AND. I .LE. LEN(F_string))
!   !     F_string(i:i) = p_chars(i); i = i + 1
!   !   END DO
!   !   IF (i .LT. LEN(F_string)) F_string(i:) = ' '
!   ! END IF
!   !
!   ! n = SIZE(C_string)
!   ! m = 0
!   !
!   ! DO i = 1, n - 1
!   !   IF (C_string(i) .EQ. NUL) THEN
!   !     EXIT
!   !   ELSE
!   !     m = m + 1
!   !   END IF
!   ! END DO
!   !
!   ! ALLOCATE (CHARACTER(m) :: F_string)
!   !
!   ! DO i = 1, m
!   !   F_string(i:i) = C_string(i)
!   ! END DO
! END FUNCTION Fstring2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
  CHARACTER(*), INTENT(IN) :: F_string
  TYPE(C_CHAR_PTR), INTENT(IN) :: C_string
    !! target = intent(out)
  INTEGER(I4B), INTENT(IN), OPTIONAL :: C_string_len
    !! Max string length,
    !! INCLUDING THE TERMINAL NUL
  !> internal variables
  CHARACTER(1, KIND=C_CHAR), DIMENSION(:), POINTER :: p_chars
  INTEGER(I4B) :: i, strlen
  !> main
  strlen = LEN(F_string)
  IF (PRESENT(C_string_len)) THEN
    IF (C_string_len .LE. 0) RETURN
    strlen = MIN(strlen, C_string_len - 1)
  END IF
  IF (.NOT. C_ASSOCIATED(C_string)) RETURN
  CALL C_F_POINTER(C_string, p_chars, [strlen + 1])
  DO CONCURRENT(i=1:strlen)
    p_chars(i) = F_string(i:i)
  END DO
  p_chars(strlen + 1) = NUL
END SUBROUTINE F_C_STRING_PTR

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Copy a Fortran string to a C string passed by char-array reference.
!
!# Introduction
!
! Copy a Fortran string to a C string passed by char-array reference.
! If the length is not passed, the C string must be at least: len(F_string)+1
! If the length is passed and F_string is too long, it is truncated.

SUBROUTINE F_C_STRING_CHARS(F_string, C_string, C_string_len)
  CHARACTER(*), INTENT(IN) :: F_string
  ! fortran string
  CHARACTER(1, KIND=C_CHAR), INTENT(OUT) :: C_string(*)
  ! c string
  INTEGER(I4B), INTENT(IN), OPTIONAL :: C_string_len
  ! max string length, optional
  !
  ! main
  !
  INTEGER(I4B) :: i, strlen
  !
  strlen = LEN(F_string)
  IF (PRESENT(C_string_len)) THEN
    IF (C_string_len .LE. 0) RETURN
    strlen = MIN(strlen, C_string_len - 1)
  END IF
  !
  DO CONCURRENT(i=1:strlen)
    C_string(i) = F_string(i:i)
  END DO
  !
  C_string(strlen + 1) = NUL
  !
END SUBROUTINE F_C_STRING_CHARS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  23-01-21
! summary: Convert a fortran string to cString

FUNCTION CString(o) RESULT(v)
  CHARACTER(*), INTENT(in) :: o
  CHARACTER(:, kind=C_CHAR), ALLOCATABLE :: v
  v = TRIM(o)//C_NULL_CHAR
END FUNCTION CString

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Convert Fortran string to C string

FUNCTION F_C_STRING_DUP(F_string, length) RESULT(C_string)
  CHARACTER(*), INTENT(IN) :: F_string
  INTEGER, INTENT(IN), OPTIONAL :: length
  TYPE(C_PTR) :: C_string
  !> internal variables
  CHARACTER(1, KIND=C_CHAR), POINTER :: C_string_ptr(:)
  INTEGER(I4B) :: i
  INTEGER(C_SIZE_T) :: strlen
  !> main
  IF (PRESENT(length)) THEN
    strlen = length
  ELSE
    strlen = LEN(F_string)
  END IF
  IF (strlen .LE. 0) THEN
    C_string = C_NULL_PTR
  ELSE
    C_string = C_MALLOC(strlen + 1)
    IF (C_ASSOCIATED(C_string)) THEN
      CALL C_F_POINTER(C_string, C_string_ptr, [strlen + 1])
      DO CONCURRENT(i=1:strlen)
        C_string_ptr(i) = F_string(i:i)
      END DO
      C_string_ptr(strlen + 1) = NUL
    END IF
  END IF
END FUNCTION F_C_STRING_DUP

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: This function returns length of string

PURE FUNCTION C_STRLEN_SAFE(s) RESULT(length)
  INTEGER(C_SIZE_T) :: length
  TYPE(C_CHAR_PTR), VALUE, INTENT(IN) :: s
  !>
  IF (.NOT. C_ASSOCIATED_PURE(s)) THEN
    length = 0
  ELSE
    length = C_STRLEN(s)
  END IF
END FUNCTION C_STRLEN_SAFE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Returns the value of target of C string

FUNCTION C_STRING_VALUE(C_string) RESULT(F_string)
  TYPE(C_CHAR_PTR), INTENT(IN) :: C_string
  CHARACTER(LEN=C_STRLEN_SAFE(C_string)) :: F_string
  !> internal variables
  CHARACTER(1, kind=C_CHAR), DIMENSION(:), POINTER :: p_chars
  INTEGER(I4B) :: i, length
  !> main
  length = LEN(F_string)
  IF (length .NE. 0) THEN
    CALL C_F_POINTER(C_string, p_chars, [length])
    DO CONCURRENT(i=1:length)
      F_string(i:i) = p_chars(i)
    END DO
  END IF
END FUNCTION C_STRING_VALUE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Allocate memory space and return C string

FUNCTION C_STRING_ALLOC(length) RESULT(C_string)
  INTEGER(C_SIZE_T), INTENT(IN) :: length
  TYPE(C_PTR) :: C_String
  !> internal variables
  CHARACTER(1, KIND=C_CHAR), POINTER :: C_CHARPTR
  !> main
  C_string = C_MALLOC(length + 1)
  IF (C_ASSOCIATED(C_string)) THEN
    CALL C_F_POINTER(C_string, C_charptr)
    C_CHARPTR = NUL
  END IF
END FUNCTION C_STRING_ALLOC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE C_STRING_FREE(string)
  TYPE(C_PTR), INTENT(INOUT) :: string
  IF (C_ASSOCIATED(string)) THEN
    CALL C_FREE(string)
    string = C_NULL_PTR
  END IF
END SUBROUTINE C_STRING_FREE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Converts C pointer to integer vector

SUBROUTINE C_PTR_TO_Int8_VEC(vec, cptr)
  INTEGER(INT8), INTENT(OUT) :: vec(:)
  TYPE(C_PTR), INTENT(IN) :: cptr
  !> Internal variables
  INTEGER(I4B) :: n, ii
  INTEGER(INT8), POINTER :: p(:)
  !> main
  n = SIZE(vec); vec = 0
  IF (C_ASSOCIATED(cptr)) THEN
    CALL C_F_POINTER(cptr, p, [n])
    DO ii = 1, n
      vec(ii) = p(ii)
    END DO
    DEALLOCATE (p)
  END IF
END SUBROUTINE C_PTR_TO_Int8_VEC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Converts C pointer to integer vector

SUBROUTINE C_PTR_TO_Int16_VEC(vec, cptr)
  INTEGER(INT16), INTENT(OUT) :: vec(:)
  TYPE(C_PTR), INTENT(IN) :: cptr
  !> Internal variables
  INTEGER(I4B) :: n, ii
  INTEGER(INT16), POINTER :: p(:)
  !> main
  n = SIZE(vec); vec = 0
  IF (C_ASSOCIATED(cptr)) THEN
    CALL C_F_POINTER(cptr, p, [n])
    DO ii = 1, n
      vec(ii) = p(ii)
    END DO
    DEALLOCATE (p)
  END IF
END SUBROUTINE C_PTR_TO_Int16_VEC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Converts C pointer to integer vector

SUBROUTINE C_PTR_TO_Int32_VEC(vec, cptr)
  INTEGER(INT32), INTENT(OUT) :: vec(:)
  TYPE(C_PTR), INTENT(IN) :: cptr
  !> Internal variables
  INTEGER(I4B) :: n, ii
  INTEGER(INT32), POINTER :: p(:)
  !> main
  n = SIZE(vec); vec = 0
  IF (C_ASSOCIATED(cptr)) THEN
    CALL C_F_POINTER(cptr, p, [n])
    DO ii = 1, n
      vec(ii) = p(ii)
    END DO
    DEALLOCATE (p)
  END IF
END SUBROUTINE C_PTR_TO_Int32_VEC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Converts C pointer to integer vector

SUBROUTINE C_PTR_TO_Int64_VEC(vec, cptr)
  INTEGER(INT64), INTENT(OUT) :: vec(:)
  TYPE(C_PTR), INTENT(IN) :: cptr
  !> Internal variables
  INTEGER(I4B) :: n, ii
  INTEGER(INT64), POINTER :: p(:)
  !> main
  n = SIZE(vec); vec = 0
  IF (C_ASSOCIATED(cptr)) THEN
    CALL C_F_POINTER(cptr, p, [n])
    DO ii = 1, n
      vec(ii) = p(ii)
    END DO
    DEALLOCATE (p)
  END IF
END SUBROUTINE C_PTR_TO_Int64_VEC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Converts C pointer to real vector

SUBROUTINE C_PTR_TO_Real32_VEC(vec, cptr)
  REAL(REAL32), INTENT(OUT) :: vec(:)
  TYPE(C_PTR), INTENT(IN) :: cptr
  ! Internal variables
  INTEGER :: n, ii
  REAL(REAL32), POINTER :: p(:)
  !> main
  n = SIZE(vec); vec = 0
  IF (C_ASSOCIATED(cptr)) THEN
    CALL C_F_POINTER(cptr, p, [n])
    DO ii = 1, n
      vec(ii) = p(ii)
    END DO
    DEALLOCATE (p)
  END IF
END SUBROUTINE C_PTR_TO_Real32_VEC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Converts C pointer to real vector

SUBROUTINE C_PTR_TO_Real64_VEC(vec, cptr)
  REAL(REAL64), INTENT(OUT) :: vec(:)
  TYPE(C_PTR), INTENT(IN) :: cptr
  ! Internal variables
  INTEGER :: n, ii
  REAL(REAL64), POINTER :: p(:)
  !> main
  n = SIZE(vec); vec = 0
  IF (C_ASSOCIATED(cptr)) THEN
    CALL C_F_POINTER(cptr, p, [n])
    DO ii = 1, n
      vec(ii) = p(ii)
    END DO
    DEALLOCATE (p)
  END IF
END SUBROUTINE C_PTR_TO_Real64_VEC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Nov 2022
! summary: optional value for `C_INT`
!
!# Introduction
!
! Reference: https://gitlab.onelab.info/gmsh/gmsh/-/blob/master/api/gmsh.f90

PURE FUNCTION optval_c_int_1(default, option) RESULT(res)
  INTEGER(C_INT), INTENT(IN) :: default
  INTEGER(C_INT), OPTIONAL, INTENT(IN) :: option
  INTEGER(C_INT) :: res
  !!
  IF (PRESENT(option)) THEN
    res = INT(option, KIND=C_INT)
  ELSE
    res = INT(default, KIND=C_INT)
  END IF
END FUNCTION optval_c_int_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Nov 2022
! summary: Optional value for `C_SIZE_T`
!
!# Introduction
!
! Reference: https://gitlab.onelab.info/gmsh/gmsh/-/blob/master/api/gmsh.f90

PURE FUNCTION optval_c_size_t_1(default, option) RESULT(res)
  INTEGER(I4B), INTENT(IN) :: default
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: option
  INTEGER(C_SIZE_T) :: res
  !!
  IF (PRESENT(option)) THEN
    res = INT(option, KIND=C_SIZE_T)
  ELSE
    res = INT(default, KIND=C_SIZE_T)
  END IF
END FUNCTION optval_c_size_t_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Nov 2022
! summary: Optional value for `C_SIZE_T`
!
!# Introduction
!
! Reference: https://gitlab.onelab.info/gmsh/gmsh/-/blob/master/api/gmsh.f90

PURE FUNCTION optval_c_size_t_2(default, option) RESULT(res)
  INTEGER(C_SIZE_T), INTENT(IN) :: default
  INTEGER(C_SIZE_T), OPTIONAL, INTENT(IN) :: option
  INTEGER(C_SIZE_T) :: res
  !!
  IF (PRESENT(option)) THEN
    res = INT(option, KIND=C_SIZE_T)
  ELSE
    res = INT(default, KIND=C_SIZE_T)
  END IF
END FUNCTION optval_c_size_t_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Nov 2022
! summary: Optional value for `C_DOUBLE`
!
!# Introduction
!
! Reference: https://gitlab.onelab.info/gmsh/gmsh/-/blob/master/api/gmsh.f90

PURE FUNCTION optval_c_double_1(default, option) RESULT(res)
  REAL(C_DOUBLE), INTENT(in) :: default
  REAL(C_DOUBLE), OPTIONAL, INTENT(in) :: option
  REAL(C_DOUBLE) :: res
  !!
  res = REAL(default, kind=C_DOUBLE)
  IF (PRESENT(option)) res = REAL(option, kind=C_DOUBLE)
END FUNCTION optval_c_double_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Nov 2022
! summary: Optional value for `C_DOUBLE`
!
!# Introduction
!
! Reference: https://gitlab.onelab.info/gmsh/gmsh/-/blob/master/api/gmsh.f90

PURE FUNCTION optval_c_double_2(default, option) RESULT(res)
  REAL, INTENT(in) :: default
  REAL(C_DOUBLE), OPTIONAL, INTENT(in) :: option
  REAL(C_DOUBLE) :: res
  !!
  res = REAL(default, kind=C_DOUBLE)
  IF (PRESENT(option)) res = REAL(option, kind=C_DOUBLE)
END FUNCTION optval_c_double_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Nov 2022
! summary: Optional value for boolean
!
!# Introduction
!
! Reference: https://gitlab.onelab.info/gmsh/gmsh/-/blob/master/api/gmsh.f90

PURE FUNCTION optval_c_bool(default, option) RESULT(res)
  LOGICAL, INTENT(in) :: default
  LOGICAL, OPTIONAL, INTENT(in) :: option
  INTEGER(C_INT) :: res
  !!
  res = MERGE(1_C_INT, 0_C_INT, default)
  IF (PRESENT(option)) res = MERGE(1_C_INT, 0_C_INT, option)
END FUNCTION optval_c_bool

END MODULE CInterface
