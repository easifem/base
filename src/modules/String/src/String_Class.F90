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
!> author: Vikas Sharma, Ph. D.
! date: 21 Oct 2021
! summary:  String datatype

MODULE String_Class
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: iostat_eor, stdout => output_unit
USE BeFor64, ONLY: b64_decode, b64_encode
USE FACE, ONLY: colorize
USE PENF, ONLY: I1P, I2P, I4P, I8P, R4P, R8P, R16P, str
IMPLICIT NONE
PRIVATE
!!
INTEGER, PARAMETER, PUBLIC :: CK = SELECTED_CHAR_KIND('DEFAULT')
! internal parameters
CHARACTER(kind=CK, len=26), PARAMETER :: UPPER_ALPHABET =  &
  & 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
CHARACTER(kind=CK, len=26), PARAMETER :: LOWER_ALPHABET =  &
  & 'abcdefghijklmnopqrstuvwxyz'
CHARACTER(kind=CK, len=1), PARAMETER :: SPACE = ' '
CHARACTER(kind=CK, len=1), PARAMETER :: TAB = ACHAR(9)
CHARACTER(kind=CK, len=1), PARAMETER :: UIX_DIR_SEP = CHAR(47)
CHARACTER(kind=CK, len=1), PARAMETER :: BACKSLASH = CHAR(92)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE strjoin
  MODULE PROCEDURE strjoin_strings, strjoin_characters, &
    & strjoin_strings_array, strjoin_characters_array
END INTERFACE strjoin

PUBLIC :: strjoin

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! builtin overloading
INTERFACE adjustl
  !< Builtin adjustl overloading.
  MODULE PROCEDURE sadjustl_character
END INTERFACE adjustl

PUBLIC :: adjustl

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE adjustr
  !< Builtin adjustr overloading.
  MODULE PROCEDURE sadjustr_character
END INTERFACE adjustr

PUBLIC :: adjustr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE count
  !< Builtin count overloading.
  MODULE PROCEDURE count_substring
END INTERFACE

PUBLIC :: count

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE index
  MODULE PROCEDURE sindex_string_string, sindex_string_character, &
    & sindex_character_string
END INTERFACE index

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE len
  MODULE PROCEDURE slen
END INTERFACE len

PUBLIC :: LEN

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE len_trim
  !< Builtin len_trim overloading.
  MODULE PROCEDURE slen_trim
END INTERFACE len_trim

PUBLIC :: len_trim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE repeat
  MODULE PROCEDURE srepeat_string_string
END INTERFACE repeat

PUBLIC :: repeat

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE scan
  MODULE PROCEDURE sscan_string_string, sscan_string_character, &
    & sscan_character_string
END INTERFACE scan

PUBLIC :: scan

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE trim
  MODULE PROCEDURE strim
END INTERFACE trim

PUBLIC :: trim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE verify
  MODULE PROCEDURE sverify_string_string, sverify_string_character, &
    & sverify_character_string
END INTERFACE verify

PUBLIC :: verify

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE string
  MODULE PROCEDURE constructor1
END INTERFACE string

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE display
  MODULE PROCEDURE display_str
END INTERFACE display

PUBLIC :: display

INTERFACE Reallocate
  MODULE PROCEDURE String_Reallocate1
END INTERFACE Reallocate

PUBLIC :: Reallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 Oct 2021
! summary: String data type
!
!# Introduction
! {!pages/String_.md}

TYPE :: String
  !< OOP designed string class.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: raw
    !! Raw data.
CONTAINS
  ! public methods
  ! builtins replacements
  PROCEDURE, PASS(self) :: adjustl => sadjustl
    !! Adjustl replacement.
  PROCEDURE, PASS(self) :: adjustr => sadjustr
    !! Adjustr replacement.
  PROCEDURE, PASS(self) :: count => scount
    !! Count replacement.
  GENERIC :: index => sindex_string_string, &
    sindex_string_character
    !! Index replacement.
  PROCEDURE, PASS(self) :: len => slen
    !! Len replacement.
  PROCEDURE, PASS(self) :: len_trim => slen_trim
    !! Len_trim replacement.
  GENERIC :: repeat => srepeat_string_string, &
    srepeat_character_string
    !! Repeat replacement.
  GENERIC :: scan => sscan_string_string, &
    sscan_string_character
    !! Scan replacement.
  PROCEDURE, PASS(self) :: trim => strim
    !! Trim replacement.
  GENERIC :: verify => sverify_string_string, &
    sverify_string_character
    !! Verify replacement.
  ! auxiliary methods
  PROCEDURE, PASS(self) :: basedir
    !! Return the base directory name of a string containing a file name.
  PROCEDURE, PASS(self) :: basename
    !! Return the base file name of a string containing a file name.
  PROCEDURE, PASS(self) :: camelcase
    !! Return a string with all words capitalized without spaces.
  PROCEDURE, PASS(self) :: capitalize
    !! Return a string with its first character capitalized and the rest
    !! lowercased.
  PROCEDURE, PASS(self) :: chars
    !! Return the raw characters data.
  GENERIC :: colorize => colorize_str
    !! Colorize and stylize strings.
  PROCEDURE, PASS(self) :: decode
    !! Decode string.
  PROCEDURE, PASS(self) :: encode
    !! Encode string.
  PROCEDURE, PASS(self) :: escape
    !! Escape backslashes (or custom escape character).
  PROCEDURE, PASS(self) :: extension
    !! Return the extension of a string containing a file name.
  PROCEDURE, PASS(self) :: fill
    !! Pad string on the left (or right) with zeros (or other char) to fill
    !! width.
  PROCEDURE, PASS(self) :: free
    !! Free dynamic memory.
  GENERIC :: glob => &
    glob_character, &
    glob_string
    !! Glob search, finds all the pathnames matching a given pattern.
  GENERIC :: insert => &
    insert_string, &
    insert_character
    !! Insert substring into string at a specified position.
  GENERIC :: join => &
    join_strings, &
    join_characters
    !! Return a string that is a join of an array of strings or characters.
  GENERIC :: strjoin => &
    strjoin_strings, &
    strjoin_characters, &
    strjoin_strings_array, &
    strjoin_characters_array
    !! Return a string that is a join of an array of strings or characters;
    !! Return join 1D string array of an 2D array of strings or
    !! characters in columns or rows.
  PROCEDURE, PASS(self) :: lower
    !! Return a string with all lowercase characters.
  PROCEDURE, PASS(self) :: partition
    !! Split string at separator and return the 3 parts (before, the
    !! separator and after).
  PROCEDURE, PASS(self) :: read_file
    !! Read a file a single string stream.
  PROCEDURE, PASS(self) :: read_line
    !! Read line (record) from a connected unit.
  PROCEDURE, PASS(self) :: read_lines
    !! Read (all) lines (records) from a connected unit as a single ascii
    !! stream.
  PROCEDURE, PASS(self) :: replace
    !! Return a string with all occurrences of substring old replaced by new.
  PROCEDURE, PASS(self) :: reverse
    !! Return a reversed string.
  PROCEDURE, PASS(self) :: search
    !! Search for *tagged* record into string.
  PROCEDURE, PASS(self) :: slice
    !! Return the raw characters data sliced.
  PROCEDURE, PASS(self) :: snakecase
    !! Return a string with all words lowercase separated by "_".
  PROCEDURE, PASS(self) :: split
    !! Return a list of substring in the string, using sep as the
    !! delimiter string.
  PROCEDURE, PASS(self) :: split_chunked
    !! Return a list of substring in the string, using sep as the
    !! delimiter string.
  PROCEDURE, PASS(self) :: startcase
    !! Return a string with all words capitalized, e.g. title case.
  PROCEDURE, PASS(self) :: strip
    !! Return a string with the leading and trailing characters removed.
  PROCEDURE, PASS(self) :: swapcase
    !! Return a string with uppercase chars converted to lowercase
    !! and vice versa.
  PROCEDURE, PASS(self) :: tempname
    !! Return a safe temporary name suitable for temporary file
    !! or directories.
  GENERIC :: to_number => &
    to_integer_I1P, &
#ifndef _NVF
    to_integer_I2P, &
#endif
    to_integer_I4P, &
    to_integer_I8P, &
#ifdef _R16P
    to_real_R16P, &
#endif
    to_real_R8P, &
    to_real_R4P
      !! Cast string to number.
  PROCEDURE, PASS(self) :: unescape
    !! Unescape double backslashes (or custom escaped character).
  PROCEDURE, PASS(self) :: unique
    !! Reduce to one (unique) multiple occurrences of a substring into
    !! a string.
  PROCEDURE, PASS(self) :: upper
    !! Return a string with all uppercase characters.
  PROCEDURE, PASS(self) :: write_file
    !! Write a single string stream into file.
  PROCEDURE, PASS(self) :: write_line
    !! Write line (record) to a connected unit.
  PROCEDURE, PASS(self) :: write_lines
    !! Write lines (records) to a connected unit.
  ! inquire methods
  PROCEDURE, PASS(self) :: end_with
    !! Return true if a string ends with a specified suffix.
  PROCEDURE, PASS(self) :: is_allocated
    !! Return true if the string is allocated.
  PROCEDURE, PASS(self) :: is_digit
    !! Return true if all characters in the string are digits.
  PROCEDURE, PASS(self) :: is_integer
    !! Return true if the string contains an integer.
  PROCEDURE, PASS(self) :: is_number
    !! Return true if the string contains a number (real or integer).
  PROCEDURE, PASS(self) :: is_real
    !! Return true if the string contains an real.
  PROCEDURE, PASS(self) :: is_logical
    !! Return true if the string contains logical.
  PROCEDURE, PASS(self) :: is_lower
    !! Return true if all characters in the string are lowercase.
  PROCEDURE, PASS(self) :: is_upper
    !! Return true if all characters in the string are uppercase.
  PROCEDURE, PASS(self) :: start_with
    !! Return true if a string starts with a specified prefix.
  ! operators
  GENERIC :: ASSIGNMENT(=) => string_assign_string, &
    string_assign_character, &
    string_assign_integer_I1P, &
    string_assign_integer_I2P, &
    string_assign_integer_I4P, &
    string_assign_integer_I8P, &
#ifdef _R16P
    string_assign_real_R16P, &
#endif
    string_assign_real_R8P, &
    string_assign_real_R4P
      !! Assignment operator overloading.
  GENERIC :: OPERATOR(//) => &
    & string_concat_string, &
    & string_concat_character, &
    & character_concat_string
      !! Concatenation operator overloading.
  GENERIC :: OPERATOR(.cat.) => &
    & string_concat_string_string, &
    & string_concat_character_string, &
    & character_concat_string_string
      !! Concatenation operator (string output) overloading.
  GENERIC :: OPERATOR(==) => string_eq_string, &
    string_eq_character, &
    character_eq_string
      !! Equal operator overloading.
  GENERIC :: OPERATOR(/=) => string_ne_string, &
    string_ne_character, &
    character_ne_string
      !! Not equal operator overloading.
  GENERIC :: OPERATOR(<) => string_lt_string, &
    string_lt_character, &
    character_lt_string
      !! Lower than operator overloading.
  GENERIC :: OPERATOR(<=) => string_le_string, &
    string_le_character, &
    character_le_string
      !! Lower equal than operator overloading.
  GENERIC :: OPERATOR(>=) => string_ge_string, &
    string_ge_character, &
    character_ge_string
      !! Greater equal than operator overloading.
  GENERIC :: OPERATOR(>) => string_gt_string, &
    string_gt_character, &
    character_gt_string
      !! Greater than operator overloading.
  ! IO
  GENERIC :: READ (formatted) => read_formatted
    !! Formatted input.
  GENERIC :: WRITE (formatted) => write_formatted
    !! Formatted output.
  GENERIC :: READ (unformatted) => read_unformatted
    !! Unformatted input.
  GENERIC :: WRITE (unformatted) => write_unformatted
    !! Unformatted output.
  PROCEDURE, PUBLIC, PASS(self) :: Display => display_str
  ! private methods
  ! builtins replacements
  PROCEDURE, PRIVATE, PASS(self) :: sindex_string_string
    !! Index replacement.
  PROCEDURE, PRIVATE, PASS(self) :: sindex_string_character
    !! Index replacement.
  PROCEDURE, PRIVATE, PASS(self) :: srepeat_string_string
    !! Repeat replacement.
  PROCEDURE, PRIVATE, NOPASS :: srepeat_character_string
    !! Repeat replacement.
  PROCEDURE, PRIVATE, PASS(self) :: sscan_string_string
    !! Scan replacement.
  PROCEDURE, PRIVATE, PASS(self) :: sscan_string_character
    !! Scan replacement.
  PROCEDURE, PRIVATE, PASS(self) :: sverify_string_string
    !! Verify replacement.
  PROCEDURE, PRIVATE, PASS(self) :: sverify_string_character
    !! Verify replacement.
  ! auxiliary methods
  PROCEDURE, PRIVATE, PASS(self) :: colorize_str
    !! Colorize and stylize strings.
  PROCEDURE, PRIVATE, PASS(self) :: glob_character
    !! Glob search (character output).
  PROCEDURE, PRIVATE, PASS(self) :: glob_string
    !! Glob search (string output).
  PROCEDURE, PRIVATE, PASS(self) :: insert_string
    !! Insert substring into string at a specified position.
  PROCEDURE, PRIVATE, PASS(self) :: insert_character
    !! Insert substring into string at a specified position.
  PROCEDURE, PRIVATE, PASS(self) :: join_strings
    !! Return join string of an array of strings.
  PROCEDURE, PRIVATE, PASS(self) :: join_characters
    !! Return join string of an array of characters.
  PROCEDURE, PRIVATE, NOPASS :: strjoin_strings
    !! Return join string of an array of strings.
  PROCEDURE, PRIVATE, NOPASS :: strjoin_characters
    !! Return join string of an array of strings.
  PROCEDURE, PRIVATE, NOPASS :: strjoin_strings_array
    !! Return join 1D string array of an 2D array of strings in columns
    !! or rows.
  PROCEDURE, PRIVATE, NOPASS :: strjoin_characters_array
    !! Return join 1D string array of an 2D array of characters in columns
    !! or rows.
  PROCEDURE, PRIVATE, PASS(self) :: to_integer_I1P
    !! Cast string to integer.
#ifndef _NVF
  PROCEDURE, PRIVATE, PASS(self) :: to_integer_I2P
    !! Cast string to integer.
#endif
  PROCEDURE, PRIVATE, PASS(self) :: to_integer_I4P
    !! Cast string to integer.
  PROCEDURE, PRIVATE, PASS(self) :: to_integer_I8P
    !! Cast string to integer.
  PROCEDURE, PRIVATE, PASS(self) :: to_real_R4P
    !! Cast string to real.
  PROCEDURE, PRIVATE, PASS(self) :: to_real_R8P
    !! Cast string to real.
  PROCEDURE, PRIVATE, PASS(self) :: to_real_R16P
    !! Cast string to real.
  PROCEDURE, PUBLIC, PASS(self) :: to_logical
    !! Convert a string to logical
  ! assignments
  PROCEDURE, PRIVATE, PASS(lhs) :: string_assign_string
    !! Assignment operator from string input.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_assign_character
    !! Assignment operator from character input.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_assign_integer_I1P
    !! Assignment operator from integer input.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_assign_integer_I2P
    !! Assignment operator from integer input.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_assign_integer_I4P
    !! Assignment operator from integer input.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_assign_integer_I8P
    !! Assignment operator from integer input.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_assign_real_R4P
    !! Assignment operator from real input.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_assign_real_R8P
    !! Assignment operator from real input.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_assign_real_R16P
    !! Assignment operator from real input.
  ! concatenation operators
  PROCEDURE, PRIVATE, PASS(lhs) :: string_concat_string
    !! Concatenation with string.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_concat_character
    !! Concatenation with character.
  PROCEDURE, PRIVATE, PASS(rhs) :: character_concat_string
    !! Concatenation with character (inverted).
  PROCEDURE, PRIVATE, PASS(lhs) :: string_concat_string_string
    !! Concatenation with string (string output).
  PROCEDURE, PRIVATE, PASS(lhs) :: string_concat_character_string
    !! Concatenation with character (string output).
  PROCEDURE, PRIVATE, PASS(rhs) :: character_concat_string_string
    !! Concatenation with character (inverted, string output).
    !! logical operators
  PROCEDURE, PRIVATE, PASS(lhs) :: string_eq_string
    !! Equal to string logical operator.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_eq_character
    !! Equal to character logical operator.
  PROCEDURE, PRIVATE, PASS(rhs) :: character_eq_string
    !! Equal to character (inverted) logical operator.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_ne_string
    !! Not equal to string logical operator.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_ne_character
    !! Not equal to character logical operator.
  PROCEDURE, PRIVATE, PASS(rhs) :: character_ne_string
    !! Not equal to character (inverted) logical operator.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_lt_string
    !! Lower than to string logical operator.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_lt_character
    !! Lower than to character logical operator.
  PROCEDURE, PRIVATE, PASS(rhs) :: character_lt_string
    !! Lower than to character (inverted) logical operator.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_le_string
    !! Lower equal than to string logical operator.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_le_character
    !! Lower equal than to character logical operator.
  PROCEDURE, PRIVATE, PASS(rhs) :: character_le_string
    !! Lower equal than to character (inverted) logical operator.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_ge_string
    !! Greater equal than to string logical operator.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_ge_character
    !! Greater equal than to character logical operator.
  PROCEDURE, PRIVATE, PASS(rhs) :: character_ge_string
    !! Greater equal than to character (inverted) logical operator.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_gt_string
    !! Greater than to string logical operator.
  PROCEDURE, PRIVATE, PASS(lhs) :: string_gt_character
    !! Greater than to character logical operator.
  PROCEDURE, PRIVATE, PASS(rhs) :: character_gt_string
    !! Greater than to character (inverted) logical operator.
    !! IO
  PROCEDURE, PRIVATE, PASS(dtv) :: read_formatted
    !! Formatted input.
  PROCEDURE, PRIVATE, PASS(dtv) :: read_delimited
    !! Read a delimited input.
  PROCEDURE, PRIVATE, PASS(dtv) :: read_undelimited
    !! Read an undelimited input.
  PROCEDURE, PRIVATE, PASS(dtv) :: read_undelimited_listdirected
    !! Read an undelimited list directed input.
  PROCEDURE, PRIVATE, PASS(dtv) :: write_formatted
    !! Formatted output.
  PROCEDURE, PRIVATE, PASS(dtv) :: read_unformatted
    !! Unformatted input.
  PROCEDURE, PRIVATE, PASS(dtv) :: write_unformatted
    !! Unformatted output.
  PROCEDURE, PRIVATE, PASS(self) :: replace_one_occurrence
    !! Replace the first occurrence of substring old by new.
  PROCEDURE, PRIVATE, PASS(obj) :: nmatchstr_1, nmatchstr_2
  GENERIC, PUBLIC :: nmatchstr => nmatchstr_1, nmatchstr_2
  PROCEDURE, PRIVATE, PASS(obj) :: strfind_1, strfind_2
  GENERIC, PUBLIC :: strfind => strfind_1, strfind_2
END TYPE string

PUBLIC :: String

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE(String), PUBLIC, PARAMETER :: TypeString = String(raw=NULL())

TYPE :: StringPointer_
  CLASS(String), POINTER :: ptr => NULL()
END TYPE StringPointer_
PUBLIC :: StringPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 26 July 2022
! summary: Overloading glob procedure.
!
!
!```fortran
! type(string)                  :: astring
! character(len=:), allocatable :: alist_chr(:)
! type(string),     allocatable :: alist_str(:)
! integer, parameter            :: Nf=5
! character(14)                 :: files(1:Nf)
! integer                       :: file_unit
! integer                       :: f
! integer                       :: ff
! logical                       :: test_passed
! do f=1, Nf
!    files(f) = astring%tempname(prefix='foo-')
!    open(newunit=file_unit, file=files(f))
!    write(file_unit, *)f
!    close(unit=file_unit)
! enddo
! call glob(self=astring, pattern='foo-*', list=alist_chr)
! call glob(self=astring, pattern='foo-*', list=alist_str)
! do f=1, Nf
!    open(newunit=file_unit, file=files(f))
!    close(unit=file_unit, status='delete')
! enddo
! test_passed = .false.
! outer_chr: do f=1, size(alist_chr, dim=1)
!    do ff=1, Nf
!       test_passed = alist_chr(f) == files(ff)
!       if (test_passed) cycle outer_chr
!    enddo
! enddo outer_chr
! if (test_passed) then
!    test_passed = .false.
!    outer_str: do f=1, size(alist_str, dim=1)
!       do ff=1, Nf
!          test_passed = alist_str(f) == files(ff)
!          if (test_passed) cycle outer_str
!       enddo
!    enddo outer_str
! endif
! print '(L1)', test_passed
!```

INTERFACE glob
  MODULE PROCEDURE glob_character, glob_string
END INTERFACE glob

PUBLIC :: glob

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS
! public non TBP

! creator
PURE FUNCTION string_(c)
  !< Return a string given a character input.
  !<
  !<```fortran
  !< print "(L1)", string('Hello World')//''=='Hello World'
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(IN) :: c       !< Character.
  TYPE(string) :: string_ !< String.

  string_%raw = c
END FUNCTION string_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! builtins replacements
PURE FUNCTION sadjustl_character(s) RESULT(adjusted)
  !< Left adjust a string by removing leading spaces (character output).
  !<
  !<```fortran
  !< type(string) :: astring
  !< astring = '   Hello World!'
  !< print "(L1)", adjustl(astring)=='Hello World!   '
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: s        !< String.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: adjusted !< Adjusted string.

  IF (ALLOCATED(s%raw)) adjusted = ADJUSTL(s%raw)
END FUNCTION sadjustl_character

PURE FUNCTION sadjustr_character(s) RESULT(adjusted)
  !< Right adjust a string by removing leading spaces (character output).
  !<
  !<```fortran
  !< type(string) :: astring
  !< astring = 'Hello World!   '
  !< print "(L1)", adjustr(astring)=='   Hello World!'
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: s        !< String.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: adjusted !< Adjusted string.

  IF (ALLOCATED(s%raw)) adjusted = ADJUSTR(s%raw)
END FUNCTION sadjustr_character

ELEMENTAL FUNCTION count_substring(s, substring) RESULT(No)
  !< Count the number of occurences of a substring into a string.
  !<
  !<```fortran
  !< print "(L1)", count('hello', substring='ll')==1
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(IN) :: s         !< String.
  CHARACTER(*), INTENT(IN) :: substring !< Substring.
  INTEGER(I4P) :: No        !< Number of occurrences.
  INTEGER(I4P) :: c1        !< Counters.
  INTEGER(I4P) :: c2        !< Counters.

  No = 0
  IF (LEN(substring) > LEN(s)) RETURN
  c1 = 1
  DO
    c2 = INDEX(string=s(c1:), substring=substring)
    IF (c2 == 0) RETURN
    No = No + 1
    c1 = c1 + c2 + LEN(substring)
  END DO
END FUNCTION count_substring

ELEMENTAL FUNCTION sindex_character_string(s, substring, back) RESULT(i)
  !< Return the position of the start of the first occurrence of string `substring` as a substring in `string`, counting from one.
  !< If `substring` is not present in `string`, zero is returned. If the back argument is present and true, the return value is
  !< the start of the last occurrence rather than the first.
  !<
  !<```fortran
  !< type(string) :: string1
  !< logical      :: test_passed(2)
  !< string1 = 'llo'
  !< test_passed(1) = index(s='Hello World Hello!', substring=string1)==index(string='Hello World Hello!', substring='llo')
  !< test_passed(2) = index(s='Hello World Hello!', substring=string1, back=.true.)==index(string='Hello World Hello!', &
  !<                                                                                       substring='llo', back=.true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CHARACTER(kind=CK, len=*), INTENT(IN) :: s         !< String.
  TYPE(string), INTENT(IN) :: substring !< Searched substring.
  LOGICAL, INTENT(IN), OPTIONAL :: back      !< Start of the last occurrence rather than the first.
  INTEGER :: i         !< Result of the search.

  IF (ALLOCATED(substring%raw)) THEN
    i = INDEX(string=s, substring=substring%raw, back=back)
  ELSE
    i = 0
  END IF
END FUNCTION sindex_character_string

ELEMENTAL FUNCTION sscan_character_string(s, set, back) RESULT(i)
  !< Return the leftmost (if `back` is either absent or equals false, otherwise the rightmost) character of string that is in `set`.
  !<
  !<```fortran
  !< type(string) :: string1
  !< logical      :: test_passed(2)
  !< string1 = 'llo'
  !< test_passed(1) = scan(s='Hello World Hello!', set=string1)==scan(string='Hello World Hello!', set='llo')
  !< test_passed(2) = scan(s='Hello World Hello!', set=string1, back=.true.)==scan(string='Hello World Hello!', &
  !<                                                                               set='llo', back=.true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CHARACTER(kind=CK, len=*), INTENT(IN) :: s    !< String.
  TYPE(string), INTENT(IN) :: set  !< Searched set.
  LOGICAL, INTENT(IN), OPTIONAL :: back !< Start of the last occurrence rather than the first.
  INTEGER :: i    !< Result of the search.

  IF (ALLOCATED(set%raw)) THEN
    i = SCAN(string=s, set=set%raw, back=back)
  ELSE
    i = 0
  END IF
END FUNCTION sscan_character_string

ELEMENTAL FUNCTION sverify_character_string(s, set, back) RESULT(i)
  !< Return the leftmost (if `back` is either absent or equals false, otherwise the rightmost) character of string that is not
  !< in `set`. If all characters of `string` are found in `set`, the result is zero.
  !<
  !<```fortran
  !< type(string) :: string1
  !< logical      :: test_passed(2)
  !< string1 = 'ell'
  !< test_passed(1) = verify(s='Hello World Hello!', set=string1)==verify(string='Hello World Hello!', set='llo')
  !< test_passed(2) = verify(s='Hello World Hello!', set=string1, back=.true.)==verify(string='Hello World Hello!', set='llo', &
  !<                                                                                   back=.true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CHARACTER(kind=CK, len=*), INTENT(IN) :: s    !< String.
  TYPE(string), INTENT(IN) :: set  !< Searched set.
  LOGICAL, INTENT(IN), OPTIONAL :: back !< Start of the last occurrence rather than the first.
  INTEGER :: i    !< Result of the search.

  IF (ALLOCATED(set%raw)) THEN
    i = VERIFY(string=s, set=set%raw, back=back)
  ELSE
    i = 0
  END IF
END FUNCTION sverify_character_string

! public methods

! builtins replacements
ELEMENTAL FUNCTION sadjustl(self) RESULT(adjusted)
  !< Left adjust a string by removing leading spaces.
  !<
  !<```fortran
  !< type(string) :: astring
  !< astring = '   Hello World!'
  !< print "(L1)", astring%adjustl()//''=='Hello World!   '
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self     !< The string.
  TYPE(string) :: adjusted !< Adjusted string.

  adjusted = self
  IF (ALLOCATED(adjusted%raw)) adjusted%raw = ADJUSTL(adjusted%raw)
END FUNCTION sadjustl

ELEMENTAL FUNCTION sadjustr(self) RESULT(adjusted)
  !< Right adjust a string by removing leading spaces.
  !<
  !<```fortran
  !< type(string) :: astring
  !< astring = 'Hello World!   '
  !< print "(L1)", astring%adjustr()//''=='   Hello World!'
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self     !< The string.
  TYPE(string) :: adjusted !< Adjusted string.

  adjusted = self
  IF (ALLOCATED(adjusted%raw)) adjusted%raw = ADJUSTR(adjusted%raw)
END FUNCTION sadjustr

ELEMENTAL FUNCTION scount(self, substring, ignore_isolated) RESULT(No)
  !< Count the number of occurences of a substring into a string.
  !<
  !< @note If `ignore_isolated` is set to true the eventual "isolated" occurences are ignored: an isolated occurrences are those
  !< occurrences happening at the start of string (thus not having a left companion) or at the end of the string (thus not having a
  !< right companion).
  !<
  !<```fortran
  !< type(string) :: astring
  !< logical      :: test_passed(4)
  !< astring = '   Hello World  !    '
  !< test_passed(1) = astring%count(substring=' ')==10
  !< astring = 'Hello World  !    '
  !< test_passed(2) = astring%count(substring=' ', ignore_isolated=.true.)==6
  !< astring = '    Hello World  !'
  !< test_passed(3) = astring%count(substring=' ', ignore_isolated=.true.)==6
  !< astring = '   Hello World  !    '
  !< test_passed(4) = astring%count(substring=' ', ignore_isolated=.true.)==8
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self             !< The string.
  CHARACTER(*), INTENT(IN) :: substring        !< Substring.
  LOGICAL, INTENT(IN), OPTIONAL :: ignore_isolated  !< Ignore "isolated" occurrences.
  INTEGER :: No               !< Number of occurrences.
  LOGICAL :: ignore_isolated_ !< Ignore "isolated" occurrences, local variable.
  INTEGER :: c1               !< Counter.
  INTEGER :: c2               !< Counter.

  No = 0
  IF (ALLOCATED(self%raw)) THEN
    IF (LEN(substring) > LEN(self%raw)) RETURN
    ignore_isolated_ = .FALSE.; IF (PRESENT(ignore_isolated)) ignore_isolated_ = ignore_isolated
    c1 = 1
    DO
      c2 = INDEX(string=self%raw(c1:), substring=substring)
      IF (c2 == 0) RETURN
      IF (.NOT. ignore_isolated_) THEN
        No = No + 1
      ELSE
        IF (.NOT. ((c1 == 1 .AND. c2 == 1) &
          & .OR. (c1 == LEN(self%raw) - LEN(substring) + 1))) THEN
          No = No + 1
        END IF
      END IF
      c1 = c1 + c2 - 1 + LEN(substring)
    END DO
  END IF
END FUNCTION scount

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION sindex_string_string(self, substring, back) RESULT(i)
  !< Return the position of the start of the first occurrence of string `substring` as a substring in `string`, counting from one.
  !< If `substring` is not present in `string`, zero is returned. If the back argument is present and true, the return value is
  !< the start of the last occurrence rather than the first.
  !<
  !<```fortran
  !< type(string) :: string1
  !< type(string) :: string2
  !< logical      :: test_passed(2)
  !< string1 = 'Hello World Hello!'
  !< string2 = 'llo'
  !< test_passed(1) = string1%index(substring=string2)==index(string='Hello World Hello!', substring='llo')
  !< test_passed(2) = string1%index(substring=string2, back=.true.)==index(string='Hello World Hello!', substring='llo', &
  !<                                                                       back=.true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self      !< The string.
  TYPE(string), INTENT(IN) :: substring !< Searched substring.
  LOGICAL, INTENT(IN), OPTIONAL :: back      !< Start of the last occurrence rather than the first.
  INTEGER :: i         !< Result of the search.

  IF (ALLOCATED(self%raw)) THEN
    i = INDEX(string=self%raw, substring=substring%raw, back=back)
  ELSE
    i = 0
  END IF
END FUNCTION sindex_string_string

ELEMENTAL FUNCTION sindex_string_character(self, substring, back) RESULT(i)
  !< Return the position of the start of the first occurrence of string `substring` as a substring in `string`, counting from one.
  !< If `substring` is not present in `string`, zero is returned. If the back argument is present and true, the return value is
  !< the start of the last occurrence rather than the first.
  !<
  !<```fortran
  !< type(string) :: string1
  !< logical      :: test_passed(2)
  !< string1 = 'Hello World Hello!'
  !< test_passed(1) = string1%index(substring='llo')==index(string='Hello World Hello!', substring='llo')
  !< test_passed(2) = string1%index(substring='llo', back=.true.)==index(string='Hello World Hello!', substring='llo', back=.true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self      !< The string.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: substring !< Searched substring.
  LOGICAL, INTENT(IN), OPTIONAL :: back      !< Start of the last occurrence rather than the first.
  INTEGER :: i         !< Result of the search.

  IF (ALLOCATED(self%raw)) THEN
    i = INDEX(string=self%raw, substring=substring, back=back)
  ELSE
    i = 0
  END IF
END FUNCTION sindex_string_character

ELEMENTAL FUNCTION slen(self) RESULT(l)
  !< Return the length of a string.
  !<
  !<```fortran
  !< type(string) :: astring
  !< astring = 'Hello World!   '
  !< print "(L1)", astring%len()==len('Hello World!   ')
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self !< The string.
  INTEGER :: l    !< String length.

  IF (ALLOCATED(self%raw)) THEN
    l = LEN(string=self%raw)
  ELSE
    l = 0
  END IF
END FUNCTION slen

ELEMENTAL FUNCTION slen_trim(self) RESULT(l)
  !< Return the length of a string, ignoring any trailing blanks.
  !<
  !<```fortran
  !< type(string) :: astring
  !< astring = 'Hello World!   '
  !< print "(L1)", astring%len_trim()==len_trim('Hello World!   ')
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self !< The string.
  INTEGER :: l    !< String length.

  IF (ALLOCATED(self%raw)) THEN
    l = len_TRIM(string=self%raw)
  ELSE
    l = 0
  END IF
END FUNCTION slen_trim

ELEMENTAL FUNCTION srepeat_string_string(self, ncopies) RESULT(repeated)
  !< Concatenates several copies of an input string.
  !<
  !<```fortran
  !< type(string) :: astring
  !< astring = 'x'
  !< print "(L1)", astring%repeat(5)//''=='xxxxx'
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self     !< String to be repeated.
  INTEGER, INTENT(IN) :: ncopies  !< Number of string copies.
  TYPE(string) :: repeated !< Repeated string.
#ifdef _NVF
  CHARACTER(9999) :: nvf_bug  !< Work around for NVFortran bug.
#endif

#ifdef _NVF
  nvf_bug = self%raw
  repeated%raw = REPEAT(string=TRIM(nvf_bug), ncopies=ncopies)
#else
  repeated%raw = REPEAT(string=self%raw, ncopies=ncopies)
#endif
END FUNCTION srepeat_string_string

ELEMENTAL FUNCTION srepeat_character_string(rstring, ncopies) RESULT(repeated)
  !< Concatenates several copies of an input string.
  !<
  !<```fortran
  !< type(string) :: astring
  !< astring = 'y'
  !< print "(L1)", astring%repeat('x', 5)//''=='xxxxx'
  !<```
  !=> T <<<
  CHARACTER(kind=CK, len=*), INTENT(IN) :: rstring  !< String to be repeated.
  INTEGER, INTENT(IN) :: ncopies  !< Number of string copies.
  TYPE(string) :: repeated !< Repeated string.

  repeated%raw = REPEAT(string=rstring, ncopies=ncopies)
END FUNCTION srepeat_character_string

ELEMENTAL FUNCTION sscan_string_string(self, set, back) RESULT(i)
  !< Return the leftmost (if `back` is either absent or equals false, otherwise the rightmost) character of string that is in `set`.
  !<
  !<```fortran
  !< type(string) :: string1
  !< type(string) :: string2
  !< logical      :: test_passed(2)
  !< string1 = 'Hello World Hello!'
  !< string2 = 'llo'
  !< test_passed(1) = string1%scan(set=string2)==scan(string='Hello World Hello!', set='llo')
  !< test_passed(2) = string1%scan(set=string2, back=.true.)==scan(string='Hello World Hello!', set='llo', back=.true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self  !< The string.
  TYPE(string), INTENT(IN) :: set   !< Searched set.
  LOGICAL, INTENT(IN), OPTIONAL :: back  !< Start of the last occurrence rather than the first.
  INTEGER :: i     !< Result of the search.

  IF (ALLOCATED(self%raw) .AND. ALLOCATED(set%raw)) THEN
    i = SCAN(string=self%raw, set=set%raw, back=back)
  ELSE
    i = 0
  END IF
END FUNCTION sscan_string_string

ELEMENTAL FUNCTION sscan_string_character(self, set, back) RESULT(i)
  !< Return the leftmost (if `back` is either absent or equals false, otherwise the rightmost) character of string that is in `set`.
  !<
  !<```fortran
  !< type(string) :: string1
  !< logical      :: test_passed(2)
  !< string1 = 'Hello World Hello!'
  !< test_passed(1) = string1%scan(set='llo')==scan(string='Hello World Hello!', set='llo')
  !< test_passed(2) = string1%scan(set='llo', back=.true.)==scan(string='Hello World Hello!', set='llo', back=.true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self  !< The string.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: set   !< Searched set.
  LOGICAL, INTENT(IN), OPTIONAL :: back  !< Start of the last occurrence rather than the first.
  INTEGER :: i     !< Result of the search.

  IF (ALLOCATED(self%raw)) THEN
    i = SCAN(string=self%raw, set=set, back=back)
  ELSE
    i = 0
  END IF
END FUNCTION sscan_string_character

ELEMENTAL FUNCTION strim(self) RESULT(trimmed)
  !< Remove trailing spaces.
  !<
  !<```fortran
  !< type(string) :: astring
  !< astring = 'Hello World!   '
  !< print "(L1)", astring%trim()==trim('Hello World!   ')
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self    !< The string.
  TYPE(string) :: trimmed !< Trimmed string.

  trimmed = self
  IF (ALLOCATED(trimmed%raw)) trimmed%raw = TRIM(trimmed%raw)
END FUNCTION strim

ELEMENTAL FUNCTION sverify_string_string(self, set, back) RESULT(i)
  !< Return the leftmost (if `back` is either absent or equals false, otherwise the rightmost) character of string that is not
  !< in `set`. If all characters of `string` are found in `set`, the result is zero.
  !<
  !<```fortran
  !< type(string) :: string1
  !< type(string) :: string2
  !< logical      :: test_passed(2)
  !< string1 = 'Hello World Hello!'
  !< string2 = 'llo'
  !< test_passed(1) = string1%verify(set=string2)==verify(string='Hello World Hello!', set='llo')
  !< test_passed(2) = string1%verify(set=string2, back=.true.)==verify(string='Hello World Hello!', set='llo', back=.true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self  !< The string.
  TYPE(string), INTENT(IN) :: set   !< Searched set.
  LOGICAL, INTENT(IN), OPTIONAL :: back  !< Start of the last occurrence rather than the first.
  INTEGER :: i     !< Result of the search.

  IF (ALLOCATED(self%raw) .AND. ALLOCATED(set%raw)) THEN
    i = VERIFY(string=self%raw, set=set%raw, back=back)
  ELSE
    i = 0
  END IF
END FUNCTION sverify_string_string

ELEMENTAL FUNCTION sverify_string_character(self, set, back) RESULT(i)
  !< Return the leftmost (if `back` is either absent or equals false, otherwise the rightmost) character of string that is not
  !< in `set`. If all characters of `string` are found in `set`, the result is zero.
  !<
  !<```fortran
  !< type(string) :: string1
  !< logical      :: test_passed(2)
  !< string1 = 'Hello World Hello!'
  !< test_passed(1) = string1%verify(set='llo')==verify(string='Hello World Hello!', set='llo')
  !< test_passed(2) = string1%verify(set='llo', back=.true.)==verify(string='Hello World Hello!', set='llo', back=.true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self  !< The string.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: set   !< Searched set.
  LOGICAL, INTENT(IN), OPTIONAL :: back  !< Start of the last occurrence rather than the first.
  INTEGER :: i     !< Result of the search.

  IF (ALLOCATED(self%raw)) THEN
    i = VERIFY(string=self%raw, set=set, back=back)
  ELSE
    i = 0
  END IF
END FUNCTION sverify_string_character

! auxiliary methods
ELEMENTAL FUNCTION basedir(self, sep)
  !< Return the base directory name of a string containing a file name.
  !<
  !<```fortran
  !< type(string) :: string1
  !< logical      :: test_passed(4)
  !< string1 = '/bar/foo.tar.bz2'
  !< test_passed(1) = string1%basedir()//''=='/bar'
  !< string1 = './bar/foo.tar.bz2'
  !< test_passed(2) = string1%basedir()//''=='./bar'
  !< string1 = 'bar/foo.tar.bz2'
  !< test_passed(3) = string1%basedir()//''=='bar'
  !< string1 = '\bar\foo.tar.bz2'
  !< test_passed(4) = string1%basedir(sep='\')//''=='\bar'
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self    !< The string.
  CHARACTER(kind=CK, len=*), INTENT(IN), OPTIONAL :: sep     !< Directory separator.
  TYPE(string) :: basedir !< Base directory name.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: sep_    !< Separator, default value.
  INTEGER :: pos     !< Character position.

  IF (ALLOCATED(self%raw)) THEN
    sep_ = UIX_DIR_SEP; IF (PRESENT(sep)) sep_ = sep
    basedir = self
    pos = INDEX(self%raw, sep_, back=.TRUE.)
    IF (pos > 0) basedir%raw = self%raw(1:pos - 1)
  END IF
END FUNCTION basedir

ELEMENTAL FUNCTION basename(self, sep, extension, strip_last_extension)
  !< Return the base file name of a string containing a file name.
  !<
  !< Optionally, the extension is also stripped if provided or the last one if required, e.g.
  !<
  !<```fortran
  !< type(string) :: astring
  !< logical      :: test_passed(5)
  !< astring = 'bar/foo.tar.bz2'
  !< test_passed(1) = astring%basename()//''=='foo.tar.bz2'
  !< test_passed(2) = astring%basename(extension='.tar.bz2')//''=='foo'
  !< test_passed(3) = astring%basename(strip_last_extension=.true.)//''=='foo.tar'
  !< astring = '\bar\foo.tar.bz2'
  !< test_passed(4) = astring%basename(sep='\')//''=='foo.tar.bz2'
  !< astring = 'bar'
  !< test_passed(5) = astring%basename(strip_last_extension=.true.)//''=='bar'
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self                 !< The string.
  CHARACTER(kind=CK, len=*), INTENT(IN), OPTIONAL :: sep                  !< Directory separator.
  CHARACTER(kind=CK, len=*), INTENT(IN), OPTIONAL :: extension            !< File extension.
  LOGICAL, INTENT(IN), OPTIONAL :: strip_last_extension !< Flag to enable the stripping of last extension.
  TYPE(string) :: basename             !< Base file name.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: sep_                 !< Separator, default value.
  INTEGER :: pos                  !< Character position.

  IF (ALLOCATED(self%raw)) THEN
    sep_ = UIX_DIR_SEP; IF (PRESENT(sep)) sep_ = sep
    basename = self
    pos = INDEX(basename%raw, sep_, back=.TRUE.)
    IF (pos > 0) basename%raw = self%raw(pos + 1:)
    IF (PRESENT(extension)) THEN
      pos = INDEX(basename%raw, extension, back=.TRUE.)
      IF (pos > 0) basename%raw = basename%raw(1:pos - 1)
    ELSEIF (PRESENT(strip_last_extension)) THEN
      IF (strip_last_extension) THEN
        pos = INDEX(basename%raw, '.', back=.TRUE.)
        IF (pos > 0) basename%raw = basename%raw(1:pos - 1)
      END IF
    END IF
  END IF
END FUNCTION basename

ELEMENTAL FUNCTION camelcase(self, sep)
  !< Return a string with all words capitalized without spaces.
  !<
  !< @note Multiple subsequent separators are collapsed to one occurence.
  !<
  !<```fortran
  !< type(string) :: astring
  !< astring = 'caMeL caSe var'
  !< print '(L1)', astring%camelcase()//''=='CamelCaseVar'
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self      !< The string.
  CHARACTER(kind=CK, len=*), INTENT(IN), OPTIONAL :: sep       !< Separator.
  TYPE(string) :: camelcase !< Camel case string.
  TYPE(string), ALLOCATABLE :: tokens(:) !< String tokens.

  IF (ALLOCATED(self%raw)) THEN
    CALL self%split(tokens=tokens, sep=sep)
    tokens = tokens%capitalize()
    camelcase = camelcase%join(array=tokens)
  END IF
END FUNCTION camelcase

ELEMENTAL FUNCTION capitalize(self) RESULT(capitalized)
  !< Return a string with its first character capitalized and the rest lowercased.
  !<
  !<```fortran
  !< type(string) :: astring
  !< astring = 'say all Hello WorLD!'
  !< print '(L1)', astring%capitalize()//''=='Say all hello world!'
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self        !< The string.
  TYPE(string) :: capitalized !< Upper case string.
  INTEGER :: c           !< Character counter.

  IF (ALLOCATED(self%raw)) THEN
    capitalized = self%lower()
    c = INDEX(LOWER_ALPHABET, capitalized%raw(1:1))
    IF (c > 0) capitalized%raw(1:1) = UPPER_ALPHABET(c:c)
  END IF
END FUNCTION capitalize

PURE FUNCTION chars(self) RESULT(raw)
  !< Return the raw characters data.
  !<
  !<```fortran
  !< type(string) :: astring
  !< astring = 'say all Hello WorLD!'
  !< print '(L1)', astring%chars()=='say all Hello WorLD!'
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self !< The string.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: raw  !< Raw characters data.

  IF (ALLOCATED(self%raw)) THEN
    raw = self%raw
  ELSE
    raw = ''
  END IF
END FUNCTION chars

PURE FUNCTION colorize_str(self, color_fg, color_bg, style) RESULT(colorized)
  !< Colorize and stylize strings, DEFAULT kind.
  !<
  !<```fortran
  !< type(string) :: astring
  !< astring = 'say all Hello WorLD!'
  !< print '(L1)', astring%colorize(color_fg='red')=='[31msay all Hello WorLD![0m'
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self      !< The string.
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: color_fg  !< Foreground color definition.
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: color_bg  !< Background color definition.
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: style     !< Style definition.
  CHARACTER(len=:), ALLOCATABLE :: colorized !< Colorized string.

      colorized = colorize(string=self%chars(), color_fg=color_fg, color_bg=color_bg, style=style)
END FUNCTION colorize_str

ELEMENTAL FUNCTION decode(self, codec) RESULT(decoded)
  !< Return a string decoded accordingly the codec.
  !<
  !< @note Only BASE64 codec is currently available.
  !<
  !<```fortran
  !< type(string) :: astring
  !< astring = 'SG93IGFyZSB5b3U/'
  !< print '(L1)', astring%decode(codec='base64')//''=='How are you?'
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self    !< The string.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: codec   !< Encoding codec.
  TYPE(string) :: decoded !< Decoded string.
  TYPE(string) :: codec_u !< Encoding codec in upper case string.

  IF (ALLOCATED(self%raw)) THEN
    decoded = self
    codec_u = codec
    SELECT CASE (codec_u%upper()//'')
    CASE ('BASE64')
      CALL b64_decode(code=self%raw, s=decoded%raw)
    END SELECT
    decoded = decoded%strip(remove_nulls=.TRUE.)
  END IF
END FUNCTION decode

ELEMENTAL FUNCTION encode(self, codec) RESULT(encoded)
  !< Return a string encoded accordingly the codec.
  !<
  !< @note Only BASE64 codec is currently available.
  !<
  !<```fortran
  !< type(string) :: astring
  !< astring = 'How are you?'
  !< print '(L1)', astring%encode(codec='base64')//''=='SG93IGFyZSB5b3U/'
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self    !< The string.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: codec   !< Encoding codec.
  TYPE(string) :: encoded !< Encoded string.

  IF (ALLOCATED(self%raw)) THEN
    encoded = codec
    SELECT CASE (encoded%upper()//'')
    CASE ('BASE64')
      CALL b64_encode(s=self%raw, code=encoded%raw)
    END SELECT
  END IF
END FUNCTION encode

ELEMENTAL FUNCTION escape(self, to_escape, esc) RESULT(escaped)
  !< Escape backslashes (or custom escape character).
  !<
  !<```fortran
  !< type(string) :: astring
  !< logical      :: test_passed(2)
  !< astring = '^\s \d+\s*'
  !< test_passed(1) = astring%escape(to_escape='\')//''=='^\\s \\d+\\s*'
  !< test_passed(2) = astring%escape(to_escape='\', esc='|')//''=='^|\s |\d+|\s*'
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self      !< The string.
  CHARACTER(kind=CK, len=1), INTENT(IN) :: to_escape !< Character to be escaped.
  CHARACTER(kind=CK, len=*), INTENT(IN), OPTIONAL :: esc       !< Character used to escape.
  TYPE(string) :: escaped   !< Escaped string.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: esc_      !< Character to escape, local variable.
  INTEGER :: c         !< Character counter.

  IF (ALLOCATED(self%raw)) THEN
    esc_ = BACKSLASH; IF (PRESENT(esc)) esc_ = esc
    escaped%raw = ''
    DO c = 1, LEN(self%raw)
      IF (self%raw(c:c) == to_escape) THEN
        escaped%raw = escaped%raw//esc_//to_escape
      ELSE
        escaped%raw = escaped%raw//self%raw(c:c)
      END IF
    END DO
  END IF
END FUNCTION escape

ELEMENTAL FUNCTION extension(self)
  !< Return the extension of a string containing a file name.
  !<
  !<```fortran
  !< type(string) :: astring
  !< astring = '/bar/foo.tar.bz2'
  !< print '(L1)', astring%extension()//''=='.bz2'
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self      !< The string.
  TYPE(string) :: extension !< Extension file name.
  INTEGER :: pos       !< Character position.

  IF (ALLOCATED(self%raw)) THEN
    extension = ''
    pos = INDEX(self%raw, '.', back=.TRUE.)
    IF (pos > 0) extension%raw = self%raw(pos:)
  END IF
END FUNCTION extension

ELEMENTAL FUNCTION fill(self, width, right, filling_char) RESULT(filled)
  !< Pad string on the left (or right) with zeros (or other char) to fill width.
  !<
  !<```fortran
  !< type(string) :: astring
  !< logical      :: test_passed(4)
  !< astring = 'this is string example....wow!!!'
  !< test_passed(1) = astring%fill(width=40)//''=='00000000this is string example....wow!!!'
  !< test_passed(2) = astring%fill(width=50)//''=='000000000000000000this is string example....wow!!!'
  !< test_passed(3) = astring%fill(width=50, right=.true.)//''=='this is string example....wow!!!000000000000000000'
  !< test_passed(4) = astring%fill(width=40, filling_char='*')//''=='********this is string example....wow!!!'
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self          !< The string.
  INTEGER, INTENT(IN) :: width         !< Final width of filled string.
  LOGICAL, INTENT(IN), OPTIONAL :: right         !< Fill on the right instead of left.
  CHARACTER(kind=CK, len=1), INTENT(IN), OPTIONAL :: filling_char  !< Filling character (default "0").
  TYPE(string) :: filled        !< Filled string.
  LOGICAL :: right_        !< Fill on the right instead of left, local variable.
  CHARACTER(kind=CK, len=1) :: filling_char_ !< Filling character (default "0"), local variable.

  IF (ALLOCATED(self%raw)) THEN
    IF (width > LEN(self%raw)) THEN
      right_ = .FALSE.; IF (PRESENT(right)) right_ = right
      filling_char_ = '0'; IF (PRESENT(filling_char)) filling_char_ = filling_char
      IF (.NOT. right_) THEN
        filled%raw = REPEAT(filling_char_, width - LEN(self%raw))//self%raw
      ELSE
        filled%raw = self%raw//REPEAT(filling_char_, width - LEN(self%raw))
      END IF
    END IF
  END IF
END FUNCTION fill

ELEMENTAL SUBROUTINE free(self)
  !< Free dynamic memory.
  !<
  !<```fortran
  !< type(string) :: astring
  !< astring = 'this is string example....wow!!!'
  !< call astring%free
  !< print '(L1)', astring%is_allocated().eqv..false.
  !<```
  !=> T <<<
  CLASS(string), INTENT(inout) :: self !< The string.

  IF (ALLOCATED(self%raw)) DEALLOCATE (self%raw)
END SUBROUTINE free

SUBROUTINE glob_character(self, pattern, list)
  !< Glob search (character output), finds all the pathnames matching a given pattern according to the rules used by the Unix shell.
  !<
  !< @note Method not portable: works only on Unix/GNU Linux OS.
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< character(len=:), allocatable :: alist_chr(:)
  !< integer, parameter            :: Nf=5
  !< character(14)                 :: files(1:Nf)
  !< integer                       :: file_unit
  !< integer                       :: f
  !< integer                       :: ff
  !< logical                       :: test_passed
  !< do f=1, Nf
  !<    files(f) = astring%tempname(prefix='foo-')
  !<    open(newunit=file_unit, file=files(f))
  !<    write(file_unit, *)f
  !<    close(unit=file_unit)
  !< enddo
  !< call astring%glob(pattern='foo-*', list=alist_chr)
  !< do f=1, Nf
  !<    open(newunit=file_unit, file=files(f))
  !<    close(unit=file_unit, status='delete')
  !< enddo
  !< test_passed = .false.
  !< outer_chr: do f=1, size(alist_chr, dim=1)
  !<    do ff=1, Nf
  !<       test_passed = alist_chr(f) == files(ff)
  !<       if (test_passed) cycle outer_chr
  !<    enddo
  !< enddo outer_chr
  !< print '(L1)', test_passed
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self           !< The string.
  CHARACTER(*), INTENT(IN) :: pattern        !< Given pattern.
  CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: list(:)        !< List of matching pathnames.
  TYPE(string), ALLOCATABLE :: list_(:)       !< List of matching pathnames.
  INTEGER(I4P) :: max_len        !< Maximum length.
  INTEGER(I4P) :: matches_number !< Matches number.
  INTEGER(I4P) :: m              !< Counter.

  CALL self%glob(pattern=pattern, list=list_)
  IF (ALLOCATED(list_)) THEN
    matches_number = SIZE(list_, dim=1)
    max_len = 0
    DO m = 1, matches_number
      max_len = MAX(max_len, list_(m)%LEN())
    END DO
    ALLOCATE (CHARACTER(max_len) :: list(1:matches_number))
    DO m = 1, matches_number
      list(m) = list_(m)%chars()
    END DO
  END IF
END SUBROUTINE glob_character

SUBROUTINE glob_string(self, pattern, list)
  !< Glob search (string output), finds all the pathnames matching a given pattern according to the rules used by the Unix shell.
  !<
  !< @note Method not portable: works only on Unix/GNU Linux OS.
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< type(string),     allocatable :: alist_str(:)
  !< integer, parameter            :: Nf=5
  !< character(14)                 :: files(1:Nf)
  !< integer                       :: file_unit
  !< integer                       :: f
  !< integer                       :: ff
  !< logical                       :: test_passed
  !<
  !< do f=1, Nf
  !<    files(f) = astring%tempname(prefix='foo-')
  !<    open(newunit=file_unit, file=files(f))
  !<    write(file_unit, *)f
  !<    close(unit=file_unit)
  !< enddo
  !< call astring%glob(pattern='foo-*', list=alist_str)
  !< do f=1, Nf
  !<    open(newunit=file_unit, file=files(f))
  !<    close(unit=file_unit, status='delete')
  !< enddo
  !< test_passed = .false.
  !< outer_str: do f=1, size(alist_str, dim=1)
  !<    do ff=1, Nf
  !<       test_passed = alist_str(f) == files(ff)
  !<       if (test_passed) cycle outer_str
  !<    enddo
  !< enddo outer_str
  !< print '(L1)', test_passed
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self     !< The string.
  CHARACTER(*), INTENT(IN) :: pattern  !< Given pattern.
  TYPE(string), ALLOCATABLE, INTENT(out) :: list(:)  !< List of matching pathnames.
  TYPE(string) :: tempfile !< Safe temporary file.
  CHARACTER(len=:), ALLOCATABLE :: tempname !< Safe temporary name.
  INTEGER(I4P) :: tempunit !< Unit of temporary file.

  tempname = self%tempname()
  CALL execute_command_LINE('ls -1 '//TRIM(ADJUSTL(pattern))//' > '//tempname)
  CALL tempfile%read_file(file=tempname)
  CALL tempfile%split(sep=new_LINE('a'), tokens=list)
  OPEN (newunit=tempunit, file=tempname)
  CLOSE (unit=tempunit, status='delete')
END SUBROUTINE glob_string

ELEMENTAL FUNCTION insert_character(self, substring, pos) RESULT(inserted)
  !< Insert substring into string at a specified position.
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< character(len=:), allocatable :: acharacter
  !< logical                       :: test_passed(5)
  !< astring = 'this is string example wow!!!'
  !< acharacter = '... '
  !< test_passed(1) = astring%insert(substring=acharacter, pos=1)//''=='... this is string example wow!!!'
  !< test_passed(2) = astring%insert(substring=acharacter, pos=23)//''=='this is string example...  wow!!!'
  !< test_passed(3) = astring%insert(substring=acharacter, pos=29)//''=='this is string example wow!!!... '
  !< test_passed(4) = astring%insert(substring=acharacter, pos=-1)//''=='... this is string example wow!!!'
  !< test_passed(5) = astring%insert(substring=acharacter, pos=100)//''=='this is string example wow!!!... '
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self      !< The string.
  CHARACTER(len=*), INTENT(IN) :: substring !< Substring.
  INTEGER, INTENT(IN) :: pos       !< Position from which insert substring.
  TYPE(string) :: inserted  !< Inserted string.
  INTEGER :: safepos   !< Safe position from which insert substring.

  IF (ALLOCATED(self%raw)) THEN
    inserted = self
    safepos = MIN(MAX(1, pos), LEN(self%raw))
    IF (safepos == 1) THEN
      inserted%raw = substring//self%raw
    ELSEIF (safepos == LEN(self%raw)) THEN
      inserted%raw = self%raw//substring
    ELSE
      inserted%raw = self%raw(1:safepos - 1)//substring//self%raw(safepos:)
    END IF
  ELSE
    inserted%raw = substring
  END IF
END FUNCTION insert_character

ELEMENTAL FUNCTION insert_string(self, substring, pos) RESULT(inserted)
  !< Insert substring into string at a specified position.
  !<
  !<```fortran
  !< type(string) :: astring
  !< type(string) :: anotherstring
  !< logical      :: test_passed(5)
  !< astring = 'this is string example wow!!!'
  !< anotherstring = '... '
  !< test_passed(1) = astring%insert(substring=anotherstring, pos=1)//''=='... this is string example wow!!!'
  !< test_passed(2) = astring%insert(substring=anotherstring, pos=23)//''=='this is string example...  wow!!!'
  !< test_passed(3) = astring%insert(substring=anotherstring, pos=29)//''=='this is string example wow!!!... '
  !< test_passed(4) = astring%insert(substring=anotherstring, pos=-1)//''=='... this is string example wow!!!'
  !< test_passed(5) = astring%insert(substring=anotherstring, pos=100)//''=='this is string example wow!!!... '
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self      !< The string.
  TYPE(string), INTENT(IN) :: substring !< Substring.
  INTEGER, INTENT(IN) :: pos       !< Position from which insert substring.
  TYPE(string) :: inserted  !< Inserted string.
  INTEGER :: safepos   !< Safe position from which insert substring.

  IF (ALLOCATED(self%raw)) THEN
    inserted = self
    IF (ALLOCATED(substring%raw)) THEN
      safepos = MIN(MAX(1, pos), LEN(self%raw))
      IF (safepos == 1) THEN
        inserted%raw = substring%raw//self%raw
      ELSEIF (safepos == LEN(self%raw)) THEN
        inserted%raw = self%raw//substring%raw
      ELSE
     inserted%raw = self%raw(1:safepos - 1)//substring%raw//self%raw(safepos:)
      END IF
    END IF
  ELSE
    IF (ALLOCATED(substring%raw)) inserted%raw = substring%raw
  END IF
END FUNCTION insert_string

PURE FUNCTION join_strings(self, array, sep) RESULT(join)
  !< Return a string that is a join of an array of strings.
  !<
  !< The join-separator is set equals to self if self has a value or it is set to a null string ''. This value can be overridden
  !< passing a custom separator.
  !<
  !<```fortran
  !< type(string) :: astring
  !< type(string) :: strings(3)
  !< logical      :: test_passed(5)
  !< strings(1) = 'one'
  !< strings(2) = 'two'
  !< strings(3) = 'three'
  !< test_passed(1) = (astring%join(array=strings)//''==strings(1)//strings(2)//strings(3))
  !< test_passed(2) = (astring%join(array=strings, sep='-')//''==strings(1)//'-'//strings(2)//'-'//strings(3))
  !< call strings(1)%free
  !< strings(2) = 'two'
  !< strings(3) = 'three'
  !< test_passed(3) = (astring%join(array=strings, sep='-')//''==strings(2)//'-'//strings(3))
  !< strings(1) = 'one'
  !< strings(2) = 'two'
  !< call strings(3)%free
  !< test_passed(4) = (astring%join(array=strings, sep='-')//''==strings(1)//'-'//strings(2))
  !< strings(1) = 'one'
  !< call strings(2)%free
  !< strings(3) = 'three'
  !< test_passed(5) = (astring%join(array=strings, sep='-')//''==strings(1)//'-'//strings(3))
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self      !< The string.
  TYPE(string), INTENT(IN) :: array(1:) !< Array to be joined.
  CHARACTER(kind=CK, len=*), INTENT(IN), OPTIONAL :: sep       !< Separator.
  TYPE(string) :: join      !< The join of array.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: sep_      !< Separator, default value.
  INTEGER :: a         !< Counter.

  IF (ALLOCATED(self%raw)) THEN
    sep_ = self%raw
  ELSE
    sep_ = ''
  END IF
  IF (PRESENT(sep)) sep_ = sep
  join = ''
  DO a = 2, SIZE(array, dim=1)
    IF (ALLOCATED(array(a)%raw)) join%raw = join%raw//sep_//array(a)%raw
  END DO
  IF (ALLOCATED(array(1)%raw)) THEN
    join%raw = array(1)%raw//join%raw
  ELSE
    join%raw = join%raw(LEN(sep_) + 1:LEN(join%raw))
  END IF
END FUNCTION join_strings

PURE FUNCTION join_characters(self, array, sep) RESULT(join)
  !< Return a string that is a join of an array of characters.
  !<
  !< The join-separator is set equals to self if self has a value or it is set to a null string ''. This value can be overridden
  !< passing a custom separator.
  !<
  !<```fortran
  !< type(string) :: astring
  !< character(5) :: characters(3)
  !< logical      :: test_passed(6)
  !< characters(1) = 'one'
  !< characters(2) = 'two'
  !< characters(3) = 'three'
  !< test_passed(1) = (astring%join(array=characters)//''==characters(1)//characters(2)//characters(3))
  !< test_passed(2) = (astring%join(array=characters, sep='-')//''==characters(1)//'-'//characters(2)//'-'//characters(3))
  !< characters(1) = ''
  !< characters(2) = 'two'
  !< characters(3) = 'three'
  !< test_passed(3) = (astring%join(array=characters, sep='-')//''==characters(2)//'-'//characters(3))
  !< characters(1) = 'one'
  !< characters(2) = 'two'
  !< characters(3) = ''
  !< test_passed(4) = (astring%join(array=characters, sep='-')//''==characters(1)//'-'//characters(2))
  !< characters(1) = 'one'
  !< characters(2) = ''
  !< characters(3) = 'three'
  !< test_passed(5) = (astring%join(array=characters, sep='-')//''==characters(1)//'-'//characters(3))
  !< characters(1) = 'one'
  !< characters(2) = 'two'
  !< characters(3) = 'three'
  !< astring = '_'
  !< test_passed(6) = (astring%join(array=characters)//''==characters(1)//'_'//characters(2)//'_'//characters(3))
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self      !< The string.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: array(1:) !< Array to be joined.
  CHARACTER(kind=CK, len=*), INTENT(IN), OPTIONAL :: sep       !< Separator.
  TYPE(string) :: join      !< The join of array.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: sep_      !< Separator, default value.
  INTEGER :: a         !< Counter.

  IF (ALLOCATED(self%raw)) THEN
    sep_ = self%raw
  ELSE
    sep_ = ''
  END IF
  IF (PRESENT(sep)) sep_ = sep
  join = ''
  DO a = 2, SIZE(array, dim=1)
    IF (array(a) /= '') join%raw = join%raw//sep_//array(a)
  END DO
  IF (array(1) /= '') THEN
    join%raw = array(1)//join%raw
  ELSE
    join%raw = join%raw(LEN(sep_) + 1:LEN(join%raw))
  END IF
END FUNCTION join_characters

PURE FUNCTION strjoin_strings(array, sep) RESULT(join)
  !< Return a string that is a join of an array of strings.
  !<
  !< The join-separator is set equals to a null string '' if custom separator isn't specified.
  !<
  !<```fortran
  !< type(string)     :: strings(3)
  !< logical          :: test_passed(5)
  !< strings(1) = 'one'
  !< strings(2) = 'two'
  !< strings(3) = 'three'
  !< test_passed(1) = (strjoin(array=strings)//''==strings(1)//strings(2)//strings(3))
  !< test_passed(2) = (strjoin(array=strings, sep='-')//''==strings(1)//'-'//strings(2)//'-'//strings(3))
  !< call strings(1)%free
  !< strings(2) = 'two'
  !< strings(3) = 'three'
  !< test_passed(3) = (strjoin(array=strings, sep='-')//''==strings(2)//'-'//strings(3))
  !< strings(1) = 'one'
  !< strings(2) = 'two'
  !< call strings(3)%free
  !< test_passed(4) = (strjoin(array=strings, sep='-')//''==strings(1)//'-'//strings(2))
  !< strings(1) = 'one'
  !< call strings(2)%free
  !< strings(3) = 'three'
  !< test_passed(5) = (strjoin(array=strings, sep='-')//''==strings(1)//'-'//strings(3))
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: array(1:) !< Array to be joined.
  CHARACTER(kind=CK, len=*), INTENT(IN), OPTIONAL :: sep       !< Separator.
  TYPE(string) :: join      !< The join of array.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: sep_      !< Separator, default value.
  INTEGER :: a         !< Counter.

  sep_ = ''
  IF (PRESENT(sep)) sep_ = sep
  join = ''
  DO a = 2, SIZE(array, dim=1)
    IF (ALLOCATED(array(a)%raw)) join%raw = join%raw//sep_//array(a)%raw
  END DO
  IF (ALLOCATED(array(1)%raw)) THEN
    join%raw = array(1)%raw//join%raw
  ELSE
    join%raw = join%raw(LEN(sep_) + 1:LEN(join%raw))
  END IF
END FUNCTION strjoin_strings

PURE FUNCTION strjoin_characters(array, sep, is_trim) RESULT(join)
  !< Return a string that is a join of an array of characters.
  !<
  !< The join-separator is set equals to a null string '' if custom separator isn't specified.
  !< The trim function is applied to array items if optional logical is_trim variable isn't set to .false.
  !<
  !<```fortran
  !< character(5) :: characters(3)
  !< logical      :: test_passed(13)
  !< characters(1) = 'one'
  !< characters(2) = 'two'
  !< characters(3) = 'three'
  !< test_passed(1) = (strjoin(array=characters)//''==trim(characters(1))//trim(characters(2))//trim(characters(3)))
  !< test_passed(2) = (strjoin(array=characters, sep='-')//''==trim(characters(1))//'-'//trim(characters(2))//'-'//trim(characters(3)))
  !< test_passed(3) = ( strjoin(array=characters, is_trim=.false.)//''==characters(1)//characters(2)//characters(3))
  !< test_passed(4) = ( strjoin(array=characters, sep='-', is_trim=.false.)//''==characters(1)//'-'//characters(2)//'-'//characters(3))
  !< characters(1) = ''
  !< characters(2) = 'two'
  !< characters(3) = 'three'
  !< test_passed(5) = (strjoin(array=characters)//''==trim(characters(2))//trim(characters(3)))
  !< characters(1) = 'one'
  !< characters(2) = 'two'
  !< characters(3) = ''
  !< test_passed(6) = (strjoin(array=characters)//''==trim(characters(1))//trim(characters(2)))
  !< characters(1) = 'one'
  !< characters(2) = ''
  !< characters(3) = 'three'
  !< test_passed(7) = (strjoin(array=characters)//''==trim(characters(1))//trim(characters(3)))
  !< characters(1) = ''
  !< characters(2) = 'two'
  !< characters(3) = 'three'
  !< test_passed(8) = (strjoin(array=characters, sep='-')//''==trim(characters(2))//'-'//trim(characters(3)))
  !< characters(1) = 'one'
  !< characters(2) = 'two'
  !< characters(3) = ''
  !< test_passed(9) = (strjoin(array=characters, sep='-')//''==trim(characters(1))//'-'//trim(characters(2)))
  !< characters(1) = 'one'
  !< characters(2) = ''
  !< characters(3) = 'three'
  !< test_passed(10) = (strjoin(array=characters, sep='-')//''==trim(characters(1))//'-'//trim(characters(3)))
  !< characters(1) = ''
  !< characters(2) = 'two'
  !< characters(3) = 'three'
  !< test_passed(11) = (strjoin(array=characters, sep='-', is_trim=.false.)//''==characters(2)//'-'//characters(3))
  !< characters(1) = 'one'
  !< characters(2) = 'two'
  !< characters(3) = ''
  !< test_passed(12) = (strjoin(array=characters, sep='-', is_trim=.false.)//''==characters(1)//'-'//characters(2))
  !< characters(1) = 'one'
  !< characters(2) = ''
  !< characters(3) = 'three'
  !< test_passed(13) = (strjoin(array=characters, sep='-', is_trim=.false.)//''==characters(1)//'-'//characters(3))
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CHARACTER(kind=CK, len=*), INTENT(IN) :: array(1:) !< Array to be joined.
  CHARACTER(kind=CK, len=*), INTENT(IN), OPTIONAL :: sep       !< Separator.
  LOGICAL, INTENT(IN), OPTIONAL :: is_trim   !< Flag to setup trim character or not
  TYPE(string) :: join      !< The join of array.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: sep_      !< Separator, default value.
  LOGICAL :: is_trim_  !< Flag to setup trim character or not
  INTEGER :: a         !< Counter.

  sep_ = ''
  IF (PRESENT(sep)) sep_ = sep
  is_trim_ = .TRUE.; IF (PRESENT(is_trim)) is_trim_ = is_trim
  join = ''

  IF (is_trim_) THEN
    DO a = 2, SIZE(array, dim=1)
      IF (TRIM(array(a)) /= '') join%raw = join%raw//sep_//TRIM(array(a))
    END DO
    IF (TRIM(array(1)) /= '') THEN
      join%raw = TRIM(array(1))//join%raw
    ELSE
      join%raw = join%raw(LEN(sep_) + 1:LEN(join%raw))
    END IF
  ELSE
    DO a = 2, SIZE(array, dim=1)
      IF (array(a) /= '') join%raw = join%raw//sep_//array(a)
    END DO
    IF (array(1) /= '') THEN
      join%raw = array(1)//join%raw
    ELSE
      join%raw = join%raw(LEN(sep_) + 1:LEN(join%raw))
    END IF
  END IF
END FUNCTION strjoin_characters

PURE FUNCTION strjoin_strings_array(array, sep, is_col) RESULT(join)
  !< Return a string that is a join of columns or rows of an array of strings.
  !<
  !< The join-separator is set equals to a null string '' if custom separator isn't specified.
  !< The is_col is setup the direction of join: within default columns (.true.) or rows(.false.).
  !<
  !<```fortran
  !< type(string), allocatable :: strings_arr(:, :)
  !< logical                   :: test_passed(5)
  !<
  !< strings_arr = reshape( source = &
  !<                        [string('one'), string('two'), string('three'),  &
  !<                         string('ONE'), string('TWO'), string('THREE')], &
  !<                        shape = [3, 2] )
  !<
  !< test_passed(1) = all( strjoin(array=strings_arr) == &
  !<                       reshape([string('onetwothree'), string('ONETWOTHREE')], &
  !<                       shape = [2]) )
  !<
  !< test_passed(2) = all( strjoin(array=strings_arr, sep='_') == &
  !<                       reshape([string('one_two_three'), string('ONE_TWO_THREE')], &
  !<                       shape = [2]) )
  !<
  !<  test_passed(3) = all( strjoin(array=strings_arr, is_col=.false.) == &
  !<                        reshape([string('oneONE'), string('twoTWO'), string('threeTHREE')], &
  !<                        shape = [3]) )
  !<
  !<  test_passed(4) = all( strjoin(array=strings_arr, sep='_', is_col=.false.) == &
  !<                        reshape([string('one_ONE'), string('two_TWO'), string('three_THREE')], &
  !<                        shape = [3]) )
  !<
  !< call strings_arr(2, 1)%free
  !< test_passed(5) = all( strjoin(array=strings_arr, sep='_', is_col=.false.) == &
  !<                  reshape([string('one_ONE'), string('TWO'), string('three_THREE')], &
  !<                  shape = [3]) )
  !<
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: array(1:, 1:) !< Array to be joined.
  CHARACTER(kind=CK, len=*), INTENT(IN), OPTIONAL :: sep  !< Separator.
  LOGICAL, INTENT(IN), OPTIONAL :: is_col  !< Direction: 'columns' if .true. or 'rows' if .false.
  TYPE(string), ALLOCATABLE :: join(:)       !< The join of array.
  TYPE(string), ALLOCATABLE :: slice(:)      !< The column or row slice of array
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: sep_          !< Separator, default value.
  LOGICAL :: is_col_       !< Direction, default value.
  INTEGER :: a, join_size, slice_size  !< Counter, sizes of join vector and of slice of array

  sep_ = ''; IF (PRESENT(sep)) sep_ = sep
  is_col_ = .TRUE.; IF (PRESENT(is_col)) is_col_ = is_col

  IF (is_col_) THEN
    join_size = SIZE(array, dim=2)
    slice_size = SIZE(array, dim=1)

    IF (.NOT. ALLOCATED(join)) ALLOCATE (join(join_size))
    IF (.NOT. ALLOCATED(slice)) ALLOCATE (slice(slice_size))
    DO a = 1, join_size
      slice(:) = array(:, a)
      join(a) = strjoin_strings(slice, sep_)
    END DO
  ELSE
    join_size = SIZE(array, dim=1)
    slice_size = SIZE(array, dim=2)

    IF (.NOT. ALLOCATED(join)) ALLOCATE (join(join_size))
    IF (.NOT. ALLOCATED(slice)) ALLOCATE (slice(slice_size))
    DO a = 1, join_size
      slice(:) = array(a, :)
      join(a) = strjoin_strings(slice, sep_)
    END DO
  END IF
END FUNCTION strjoin_strings_array

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 21 July 2022
! summary: Return a string that is a join of columns or rows of an array of characters
!
!# Introduction
!
! The join-separator is set equals to a null string '' if custom separator
! isn't specified.
! The trim function is applied to array items if optional logical is_trim
! variable isn't set to .false.
! The is_col is setup the direction of join: within default columns (.true.)
! or rows(.false.).
!
!```fortran
! character(len=10)         :: chars_arr(3, 2)
! logical                   :: test_passed(9)
! chars_arr(:, 1) = ['one       ', 'two       ', 'three     ']
! chars_arr(:, 2) = ['ONE       ', 'TWO       ', 'THREE     ']
!
! test_passed(1) = all( strjoin(array=chars_arr) == &
!                       reshape([string('onetwothree'), string
! ('ONETWOTHREE')], &
!                       shape = [2]) )
!
! test_passed(2) = all( strjoin(array=chars_arr, is_trim=.false.) ==  &
!                       reshape([string('one       two       three     '),  &
!                                string('ONE       TWO       THREE     ')], &
!                       shape = [2]) )
!
! test_passed(3) = all( strjoin(array=chars_arr, sep='_') == &
!                       reshape([string('one_two_three'), string
! ('ONE_TWO_THREE')], &
!                       shape = [2]) )
!
! test_passed(4) = all( strjoin(array=chars_arr, sep='_', is_trim=.false.)
! ==  &
!                       reshape([string('one       _two       _three
! '),  &
!                                string('ONE       _TWO       _THREE
! ')], &
!                       shape = [2]) )
!
! test_passed(5) = all( strjoin(array=chars_arr, is_col=.false.) == &
!                       reshape([string('oneONE'), string('twoTWO'), string
! ('threeTHREE')], &
!                       shape = [3]) )
!
! test_passed(6) = all( strjoin(array=chars_arr, is_trim=.false., is_col=.
! false.) ==  &
!                       reshape([string('one       ONE       '),  &
!                                string('two       TWO       '),  &
!                                string('three     THREE     ')], &
!                       shape = [3]) )
!
! test_passed(7) = all( strjoin(array=chars_arr, sep='_', is_col=.false.) == &
!                       reshape([string('one_ONE'), string('two_TWO'), string
! ('three_THREE')], &
!                       shape = [3]) )
!
! test_passed(8) = all( strjoin(array=chars_arr, sep='_', is_trim=.false.,
! is_col=.false.) ==  &
!                       reshape([string('one       _ONE       '),  &
!                                string('two       _TWO       '),  &
!                                string('three     _THREE     ')], &
!                       shape = [3]) )
!
! chars_arr(2,1) = ''
! test_passed(9) = all( strjoin(array=chars_arr, sep='_', is_col=.false.)
! ==  &
!                       reshape([string('one_ONE'),  &
!                                string('TWO'),  &
!                                string('three_THREE')], &
!                       shape = [3]) )
!
! print '(L1)', all(test_passed)
!```

PURE FUNCTION strjoin_characters_array(array, sep, is_trim, is_col) &
  & RESULT(join)
  !!
  CHARACTER(kind=CK, len=*), INTENT(IN) :: array(1:, 1:)
  !! Array to be joined.
  CHARACTER(kind=CK, len=*), INTENT(IN), OPTIONAL :: sep
  !! Separator.
  LOGICAL, INTENT(IN), OPTIONAL :: is_trim
  !! Flag to setup trim character or not
  LOGICAL, INTENT(IN), OPTIONAL :: is_col
  !! Direction: 'columns' if .true. or 'rows' if .false.
  TYPE(string), ALLOCATABLE :: join(:)
  !! The join of array.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: slice(:)
  !! The column or row slice of array
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: sep_
  !! Separator, default value.
  LOGICAL :: is_trim_
  !! Flag to setup trim character or not
  LOGICAL :: is_col_
  !! Direction, default value.
  INTEGER :: a, join_size, slice_size
  !! Counter, sizes of join vector and of slice of array
  INTEGER :: item_len
  !! Length of array item (all items of character array have equal lengths)
  !!
  !!
  item_len = LEN(array(1, 1))
  !!
  !! all items of character array have equal lengths
  !!
  sep_ = ''; IF (PRESENT(sep)) sep_ = sep
  is_trim_ = .TRUE.; IF (PRESENT(is_trim)) is_trim_ = is_trim
  is_col_ = .TRUE.; IF (PRESENT(is_col)) is_col_ = is_col
  !!
  IF (is_col_) THEN
    join_size = SIZE(array, dim=2)
    slice_size = SIZE(array, dim=1)
    !!
    IF (.NOT. ALLOCATED(join)) ALLOCATE (join(join_size))
    IF (.NOT. ALLOCATED(slice)) &
      & ALLOCATE (CHARACTER(len=item_len) :: slice(slice_size))
    !!
    DO a = 1, join_size
      slice(:) = array(:, a)
      join(a) = strjoin_characters(slice, sep_, is_trim_)
    END DO
    !!
  ELSE
    !!
    join_size = SIZE(array, dim=1)
    slice_size = SIZE(array, dim=2)
    !!
    IF (.NOT. ALLOCATED(join)) ALLOCATE (join(join_size))
    IF (.NOT. ALLOCATED(slice)) &
      & ALLOCATE (CHARACTER(len=item_len) :: slice(slice_size))
    !!
    DO a = 1, join_size
      slice(:) = array(a, :)
      join(a) = strjoin_characters(slice, sep_, is_trim_)
    END DO
  END IF
  !!
END FUNCTION strjoin_characters_array

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION lower(self)
  !< Return a string with all lowercase characters.
  !<
  !<```fortran
  !< type(string) :: astring
  !< logical      :: test_passed(1)
  !< astring = 'Hello WorLD!'
  !< test_passed(1) = astring%lower()//''=='hello world!'
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self  !< The string.
  TYPE(string) :: lower !< Upper case string.
  INTEGER :: n1    !< Characters counter.
  INTEGER :: n2    !< Characters counter.

  IF (ALLOCATED(self%raw)) THEN
    lower = self
    DO n1 = 1, LEN(self%raw)
      n2 = INDEX(UPPER_ALPHABET, self%raw(n1:n1))
      IF (n2 > 0) lower%raw(n1:n1) = LOWER_ALPHABET(n2:n2)
    END DO
  END IF
END FUNCTION lower

PURE FUNCTION partition(self, sep) RESULT(partitions)
  !< Split string at separator and return the 3 parts (before, the separator and after).
  !<
  !<```fortran
  !< type(string) :: astring
  !< type(string) :: strings(3)
  !< logical      :: test_passed(3)
  !< astring = 'Hello WorLD!'
  !< strings = astring%partition(sep='lo Wo')
  !< test_passed(1) = (strings(1)//''=='Hel'.and.strings(2)//''=='lo Wo'.and.strings(3)//''=='rLD!')
  !< strings = astring%partition(sep='Hello')
  !< test_passed(2) = (strings(1)//''==''.and.strings(2)//''=='Hello'.and.strings(3)//''==' WorLD!')
  !< astring = 'Hello WorLD!'
  !< strings = astring%partition()
  !< test_passed(3) = (strings(1)//''=='Hello'.and.strings(2)//''==' '.and.strings(3)//''=='WorLD!')
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self            !< The string.
  CHARACTER(kind=CK, len=*), INTENT(IN), OPTIONAL :: sep             !< Separator.
  TYPE(string) :: partitions(1:3) !< Partions: before the separator, the separator itsels and
  !< after the separator.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: sep_            !< Separator, default value.
  INTEGER :: c               !< Character counter.

  IF (ALLOCATED(self%raw)) THEN
    sep_ = SPACE; IF (PRESENT(sep)) sep_ = sep

    partitions(1) = self
    partitions(2) = sep_
    partitions(3) = ''
    IF (LEN(sep_) >= LEN(self%raw)) RETURN
    c = INDEX(self%raw, sep_)
    IF (c > 0) THEN
      partitions(1)%raw = self%raw(1:c - 1)
      partitions(2)%raw = self%raw(c:c + LEN(sep_) - 1)
      partitions(3)%raw = self%raw(c + LEN(sep_):)
    END IF
  END IF
END FUNCTION partition

SUBROUTINE read_file(self, file, is_fast, form, iostat, iomsg)
  !< Read a file as a single string stream.
  !<
  !< @note All the lines are stored into the string self as a single ascii stream. Each line (record) is separated by a `new_line`
  !< character.
  !<
  !< @note For unformatted read only `access='stream'` is supported with new_line as line terminator.
  !<
  !< @note *Fast* file reading allows a very efficient reading of streamed file, but it dumps file as single streamed string.
  !<
  !<```fortran
  !< type(string)              :: astring
  !< type(string), allocatable :: strings(:)
  !< type(string)              :: line(3)
  !< integer                   :: iostat
  !< character(len=99)         :: iomsg
  !< integer                   :: scratch
  !< integer                   :: l
  !< logical                   :: test_passed(9)
  !< line(1) = ' Hello World!   '
  !< line(2) = 'How are you?  '
  !< line(3) = '   All say: "Fine thanks"'
  !< open(newunit=scratch, file='read_file_test.tmp')
  !< write(scratch, "(A)") line(1)%chars()
  !< write(scratch, "(A)") line(2)%chars()
  !< write(scratch, "(A)") line(3)%chars()
  !< close(scratch)
  !< call astring%read_file(file='read_file_test.tmp', iostat=iostat, iomsg=iomsg)
  !< call astring%split(tokens=strings, sep=new_line('a'))
  !< test_passed(1) = (size(strings, dim=1)==size(line, dim=1))
  !< do l=1, size(strings, dim=1)
  !<   test_passed(l+1) = (strings(l)==line(l))
  !< enddo
  !< open(newunit=scratch, file='read_file_test.tmp', form='UNFORMATTED', access='STREAM')
  !< write(scratch) line(1)%chars()//new_line('a')
  !< write(scratch) line(2)%chars()//new_line('a')
  !< write(scratch) line(3)%chars()//new_line('a')
  !< close(scratch)
  !< call astring%read_file(file='read_file_test.tmp', form='unformatted', iostat=iostat, iomsg=iomsg)
  !< call astring%split(tokens=strings, sep=new_line('a'))
  !< test_passed(5) = (size(strings, dim=1)==size(line, dim=1))
  !< do l=1, size(strings, dim=1)
  !<   test_passed(l+5) = (strings(l)==line(l))
  !< enddo
  !< open(newunit=scratch, file='read_file_test.tmp', form='UNFORMATTED', access='STREAM')
  !< close(scratch, status='DELETE')
  !< call astring%read_file(file='read_file_test.tmp', iostat=iostat)
  !< test_passed(9) = (iostat/=0)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(inout) :: self       !< The string.
  CHARACTER(len=*), INTENT(IN) :: file       !< File name.
  LOGICAL, INTENT(IN), OPTIONAL :: is_fast    !< Flag to enable (super) fast file reading.
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: form       !< Format of unit.
  INTEGER, INTENT(out), OPTIONAL :: iostat     !< IO status code.
  CHARACTER(len=*), INTENT(inout), OPTIONAL :: iomsg      !< IO status message.
  LOGICAL :: is_fast_   !< Flag to enable (super) fast file reading, local variable.
  TYPE(string) :: form_      !< Format of unit, local variable.
  INTEGER :: iostat_    !< IO status code, local variable.
  CHARACTER(len=:), ALLOCATABLE :: iomsg_     !< IO status message, local variable.
  INTEGER :: unit       !< Logical unit.
  LOGICAL :: does_exist !< Check if file exist.
  INTEGER(I4P) :: filesize   !< Size of the file for fast reading.

  iomsg_ = REPEAT(' ', 99); IF (PRESENT(iomsg)) iomsg_ = iomsg
  INQUIRE (file=file, iomsg=iomsg_, iostat=iostat_, exist=does_exist)
  IF (does_exist) THEN
    is_fast_ = .FALSE.; IF (PRESENT(is_fast)) is_fast_ = is_fast
    IF (is_fast_) THEN
            OPEN (newunit=unit, file=file, access='STREAM', form='UNFORMATTED', iomsg=iomsg_, iostat=iostat_)
      INQUIRE (file=file, size=filesize)
      IF (ALLOCATED(self%raw)) DEALLOCATE (self%raw)
      ALLOCATE (CHARACTER(len=filesize) :: self%raw)
      READ (unit=unit, iostat=iostat_, iomsg=iomsg_) self%raw
      CLOSE (unit)
    ELSE
      form_ = 'FORMATTED'; IF (PRESENT(form)) form_ = form; form_ = form_%upper()
      SELECT CASE (form_%chars())
      CASE ('FORMATTED')
               OPEN (newunit=unit, file=file, status='OLD', action='READ', iomsg=iomsg_, iostat=iostat_, err=10)
      CASE ('UNFORMATTED')
               OPEN (newunit=unit, file=file, status='OLD', action='READ', form='UNFORMATTED', access='STREAM', &
              iomsg=iomsg_, iostat=iostat_, err=10)
      END SELECT
      CALL self%read_lines(unit=unit, form=form, iomsg=iomsg_, iostat=iostat_)
10    CLOSE (unit)
    END IF
  ELSE
    iostat_ = 1
    iomsg_ = 'file not found'
  END IF
  IF (PRESENT(iostat)) iostat = iostat_
  IF (PRESENT(iomsg)) iomsg = iomsg_
END SUBROUTINE read_file

!----------------------------------------------------------------------------
!                                                                  readLine
!----------------------------------------------------------------------------

SUBROUTINE read_line(self, unit, form, iostat, iomsg)
  !< Read line (record) from a connected unit.
  !<
  !< The line is read as an ascii stream read until the eor is reached.
  !<
  !< @note For unformatted read only `access='stream'` is supported with new_line as line terminator.
  !<
  !<```fortran
  !< type(string)      :: astring
  !< type(string)      :: line(3)
  !< integer           :: iostat
  !< character(len=99) :: iomsg
  !< integer           :: scratch
  !< integer           :: l
  !< logical           :: test_passed(6)
  !< line(1) = ' Hello World!   '
  !< line(2) = 'How are you?  '
  !< line(3) = '   All say: "Fine thanks"'
  !< open(newunit=scratch, status='SCRATCH')
  !< write(scratch, "(A)") line(1)%chars()
  !< write(scratch, "(A)") line(2)%chars()
  !< write(scratch, "(A)") line(3)%chars()
  !< rewind(scratch)
  !< l = 0
  !< iostat = 0
  !< do
  !<   l = l + 1
  !<   call astring%read_line(unit=scratch, iostat=iostat, iomsg=iomsg)
  !<   if (iostat/=0.and..not.is_iostat_eor(iostat)) then
  !<     exit
  !<   else
  !<     test_passed(l) = (astring==line(l))
  !<   endif
  !< enddo
  !< close(scratch)
  !< open(newunit=scratch, status='SCRATCH', form='UNFORMATTED', access='STREAM')
  !< write(scratch) line(1)%chars()//new_line('a')
  !< write(scratch) line(2)%chars()//new_line('a')
  !< write(scratch) line(3)%chars()//new_line('a')
  !< rewind(scratch)
  !< l = 0
  !< iostat = 0
  !< do
  !<   l = l + 1
  !<   call astring%read_line(unit=scratch, iostat=iostat, iomsg=iomsg, form='UnfORMatteD')
  !<   if (iostat/=0.and..not.is_iostat_eor(iostat)) then
  !<     exit
  !<   else
  !<     test_passed(l+3) = (astring==line(l))
  !<   endif
  !< enddo
  !< close(scratch)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(inout) :: self    !< The string.
  INTEGER, INTENT(IN) :: unit    !< Logical unit.
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: form    !< Format of unit.
  INTEGER, INTENT(out), OPTIONAL :: iostat  !< IO status code.
  CHARACTER(len=*), INTENT(inout), OPTIONAL :: iomsg   !< IO status message.
  TYPE(string) :: form_   !< Format of unit, local variable.
  INTEGER :: iostat_ !< IO status code, local variable.
  CHARACTER(len=:), ALLOCATABLE :: iomsg_  !< IO status message, local variable.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: line    !< Line storage.
  CHARACTER(kind=CK, len=1) :: ch      !< Character storage.

  form_ = 'FORMATTED'; IF (PRESENT(form)) form_ = form; form_ = form_%upper()
  iomsg_ = REPEAT(' ', 99); IF (PRESENT(iomsg)) iomsg_ = iomsg
  line = ''
  SELECT CASE (form_%chars())
  CASE ('FORMATTED')
    DO
            READ (unit, "(A)", advance='no', iostat=iostat_, iomsg=iomsg_, err=10, END=10, eor=10) ch
      line = line//ch
    END DO
  CASE ('UNFORMATTED')
    DO
      READ (unit, iostat=iostat_, iomsg=iomsg_, err=10, END=10) ch
      IF (ch == new_LINE('a')) THEN
        iostat_ = iostat_eor
        EXIT
      END IF
      line = line//ch
    END DO
  END SELECT
10 IF (line /= '') self%raw = line
  IF (PRESENT(iostat)) iostat = iostat_
  IF (PRESENT(iomsg)) iomsg = iomsg_
END SUBROUTINE read_line

SUBROUTINE read_lines(self, unit, form, iostat, iomsg)
  !< Read (all) lines (records) from a connected unit as a single ascii stream.
  !<
  !< @note All the lines are stored into the string self as a single ascii stream. Each line (record) is separated by a `new_line`
  !< character. The line is read as an ascii stream read until the eor is reached.
  !<
  !< @note The connected unit is rewinded. At a successful exit current record is at eof, at the beginning otherwise.
  !<
  !< @note For unformatted read only `access='stream'` is supported with new_line as line terminator.
  !<
  !<```fortran
  !< type(string)              :: astring
  !< type(string), allocatable :: strings(:)
  !< type(string)              :: line(3)
  !< integer                   :: iostat
  !< character(len=99)         :: iomsg
  !< integer                   :: scratch
  !< integer                   :: l
  !< logical                   :: test_passed(8)
  !<
  !< line(1) = ' Hello World!   '
  !< line(2) = 'How are you?  '
  !< line(3) = '   All say: "Fine thanks"'
  !< open(newunit=scratch, status='SCRATCH')
  !< write(scratch, "(A)") line(1)%chars()
  !< write(scratch, "(A)") line(2)%chars()
  !< write(scratch, "(A)") line(3)%chars()
  !< call astring%read_lines(unit=scratch, iostat=iostat, iomsg=iomsg)
  !< call astring%split(tokens=strings, sep=new_line('a'))
  !< test_passed(1) = (size(strings, dim=1)==size(line, dim=1))
  !< do l=1, size(strings, dim=1)
  !<   test_passed(l+1) = (strings(l)==line(l))
  !< enddo
  !< close(scratch)
  !< open(newunit=scratch, status='SCRATCH', form='UNFORMATTED', access='STREAM')
  !< write(scratch) line(1)%chars()//new_line('a')
  !< write(scratch) line(2)%chars()//new_line('a')
  !< write(scratch) line(3)%chars()//new_line('a')
  !< call astring%read_lines(unit=scratch, form='unformatted', iostat=iostat, iomsg=iomsg)
  !< call astring%split(tokens=strings, sep=new_line('a'))
  !< test_passed(5) = (size(strings, dim=1)==size(line, dim=1))
  !< do l=1, size(strings, dim=1)
  !<   test_passed(l+5) = (strings(l)==line(l))
  !< enddo
  !< close(scratch)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(inout) :: self    !< The string.
  INTEGER, INTENT(IN) :: unit    !< Logical unit.
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: form    !< Format of unit.
  INTEGER, INTENT(out), OPTIONAL :: iostat  !< IO status code.
  CHARACTER(len=*), INTENT(inout), OPTIONAL :: iomsg   !< IO status message.
  INTEGER :: iostat_ !< IO status code, local variable.
  CHARACTER(len=:), ALLOCATABLE :: iomsg_  !< IO status message, local variable.
  TYPE(string) :: lines   !< Lines storage.
  TYPE(string) :: line    !< Line storage.

  iomsg_ = REPEAT(' ', 99); IF (PRESENT(iomsg)) iomsg_ = iomsg
  REWIND (unit)
  iostat_ = 0
  lines%raw = ''
  DO
    line%raw = ''
    CALL line%read_line(unit=unit, form=form, iostat=iostat_, iomsg=iomsg_)
    IF (iostat_ /= 0 .AND. .NOT. is_iostat_eor(iostat_)) THEN
      EXIT
    ELSEIF (line /= '') THEN
      lines%raw = lines%raw//line%raw//new_LINE('a')
    END IF
  END DO
  IF (lines%raw /= '') self%raw = lines%raw
  IF (PRESENT(iostat)) iostat = iostat_
  IF (PRESENT(iomsg)) iomsg = iomsg_
END SUBROUTINE read_lines

ELEMENTAL FUNCTION replace(self, old, NEW, count) RESULT(replaced)
  !< Return a string with all occurrences of substring old replaced by new.
  !<
  !<```fortran
  !< type(string) :: astring
  !< logical      :: test_passed(3)
  !< astring = 'When YOU are sad YOU should think to me :-)'
  !< test_passed(1) = (astring%replace(old='YOU', new='THEY')//''=='When THEY are sad THEY should think to me :-)')
  !< test_passed(2) = (astring%replace(old='YOU', new='THEY', count=1)//''=='When THEY are sad YOU should think to me :-)')
  !< astring = repeat(new_line('a')//'abcd', 20)
  !< astring = astring%replace(old=new_line('a'), new='|cr|')
  !< astring = astring%replace(old='|cr|', new=new_line('a')//'    ')
  !< test_passed(3) = (astring//''==repeat(new_line('a')//'    '//'abcd', 20))
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self     !< The string.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: old      !< Old substring.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: NEW      !< New substring.
  INTEGER, INTENT(IN), OPTIONAL :: count    !< Number of old occurences to be replaced.
  TYPE(string) :: replaced !< The string with old replaced by new.
  INTEGER :: r        !< Counter.

  IF (ALLOCATED(self%raw)) THEN
    replaced = self
    r = 0
    DO
      IF (INDEX(replaced%raw, old) > 0) THEN
        replaced = replaced%replace_one_occurrence(old=old, NEW=NEW)
        r = r + 1
        IF (PRESENT(count)) THEN
          IF (r >= count) EXIT
        END IF
      ELSE
        EXIT
      END IF
    END DO
  END IF
END FUNCTION replace

ELEMENTAL FUNCTION reverse(self) RESULT(reversed)
  !< Return a reversed string.
  !<
  !<```fortran
  !< type(string) :: astring
  !< logical      :: test_passed(2)
  !< astring = 'abcdefghilmnopqrstuvz'
  !< test_passed(1) = (astring%reverse()//''=='zvutsrqponmlihgfedcba')
  !< astring = '0123456789'
  !< test_passed(2) = (astring%reverse()//''=='9876543210')
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self     !< The string.
  TYPE(string) :: reversed !< The reversed string.
  INTEGER :: length   !< Length of the string.
  INTEGER :: c        !< Counter.

  IF (ALLOCATED(self%raw)) THEN
    reversed = self
    length = LEN(self%raw)
    DO c = 1, length
      reversed%raw(c:c) = self%raw(length - c + 1:length - c + 1)
    END DO
  END IF
END FUNCTION reverse

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION search(self, tag_start, tag_end, in_string, in_character, &
  & istart, iend) RESULT(tag)
  !< Search for *tagged* record into string, return the first record found (if any) matching the tags.
  !<
  !< Optionally, returns the indexes of tag start/end, thus this is not an `elemental` function.
  !<
  !< @note The tagged record is searched into self if allocated otherwise into `in_string` if passed or, eventually, into
  !< `in_character` is passed. If tag is not found the return string is not allocated and the start/end indexes (if requested) are
  !< zero.
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< type(string)                  :: anotherstring
  !< character(len=:), allocatable :: acharacter
  !< integer                       :: istart
  !< integer                       :: iend
  !< logical                       :: test_passed(5)
  !< astring = '<test> <first> hello </first> <first> not the first </first> </test>'
  !< anotherstring = astring%search(tag_start='<first>', tag_end='</first>')
  !< test_passed(1) = anotherstring//''=='<first> hello </first>'
  !< astring = '<test> <a> <a> <a> the nested a </a> </a> </a> </test>'
  !< anotherstring = astring%search(tag_start='<a>', tag_end='</a>')
  !< test_passed(2) = anotherstring//''=='<a> <a> <a> the nested a </a> </a> </a>'
  !< call astring%free
  !< anotherstring = '<test> <a> <a> <a> the nested a </a> </a> </a> </test>'
  !< astring = astring%search(in_string=anotherstring, tag_start='<a>', tag_end='</a>')
  !< test_passed(3) = astring//''=='<a> <a> <a> the nested a </a> </a> </a>'
  !< call astring%free
  !< acharacter = '<test> <a> <a> <a> the nested a </a> </a> </a> </test>'
  !< astring = astring%search(in_character=acharacter, tag_start='<a>', tag_end='</a>')
  !< test_passed(4) = astring//''=='<a> <a> <a> the nested a </a> </a> </a>'
  !< acharacter = '<test> <first> hello </first> <sec> <sec>not the first</sec> </sec> </test>'
  !< astring = astring%search(in_character=acharacter, tag_start='<sec>', tag_end='</sec>', istart=istart, iend=iend)
  !< test_passed(5) = astring//''==acharacter(31:67)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self         !< The string.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: tag_start    !< Start tag.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: tag_end      !< End tag.
  TYPE(string), INTENT(IN), OPTIONAL :: in_string    !< Search into this string.
  CHARACTER(kind=CK, len=*), INTENT(IN), OPTIONAL :: in_character !< Search into this character string.
  INTEGER, INTENT(out), OPTIONAL :: istart       !< Starting index of tag inside the string.
  INTEGER, INTENT(out), OPTIONAL :: iend         !< Ending index of tag inside the string.
  TYPE(string) :: tag          !< First tag found.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: raw          !< Raw string into which search the tag.
  INTEGER :: istart_      !< Starting index of tag inside the string, local variable.
  INTEGER :: iend_        !< Ending index of tag inside the string, local variable.
  INTEGER :: nested_tags  !< Number of nested tags inside tag.
  INTEGER :: t            !< Counter.

  raw = ''
  IF (PRESENT(in_string)) THEN
    raw = in_string%raw
  ELSEIF (PRESENT(in_character)) THEN
    raw = in_character
  ELSE
    IF (ALLOCATED(self%raw)) raw = self%raw
  END IF
  istart_ = 0
  iend_ = 0
  IF (raw /= '') THEN
    istart_ = INDEX(raw, tag_start)
    iend_ = INDEX(raw, tag_end)
    IF (istart_ > 0 .AND. iend_ > 0) THEN
      iend_ = iend_ + LEN(tag_end) - 1
      tag%raw = raw(istart_:iend_)
      nested_tags = tag%COUNT(tag_start)
      IF (nested_tags > 1) THEN
        DO t = 2, nested_tags
          iend_ = iend_ + LEN(tag_end) - 1 + INDEX(raw(iend_ + 1:), tag_end)
        END DO
        tag%raw = raw(istart_:iend_)
      END IF
    END IF
  END IF
  IF (PRESENT(istart)) istart = istart_
  IF (PRESENT(iend)) iend = iend_
END FUNCTION search

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION slice(self, istart, iend) RESULT(raw)
  !< Return the raw characters data sliced.
  !<
  !<```fortran
  !< type(string) :: astring
  !< astring = 'the Quick Brown fox Jumps over the Lazy Dog.'
  !< print "(A)", astring%slice(11,25)
  !<```
  !=> Brown fox Jumps <<<
  CLASS(string), INTENT(IN) :: self   !< The string.
  INTEGER, INTENT(IN) :: istart !< Slice start index.
  INTEGER, INTENT(IN) :: iend   !< Slice end   index.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: raw    !< Raw characters data.

  IF (ALLOCATED(self%raw)) THEN
    raw = self%raw(istart:iend)
  ELSE
    raw = ''
  END IF
END FUNCTION slice

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION snakecase(self, sep)
  !< Return a string with all words lowercase separated by "_".
  !<
  !< @note Multiple subsequent separators are collapsed to one occurence.
  !<
  !<```fortran
  !< type(string) :: astring
  !< logical      :: test_passed(1)
  !< astring = 'the Quick Brown fox Jumps over the Lazy Dog.'
  !< test_passed(1) = astring%snakecase()//''=='the_quick_brown_fox_jumps_over_the_lazy_dog.'
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self      !< The string.
  CHARACTER(kind=CK, len=*), INTENT(IN), OPTIONAL :: sep       !< Separator.
  TYPE(string) :: snakecase !< Snake case string.
  TYPE(string), ALLOCATABLE :: tokens(:) !< String tokens.

  IF (ALLOCATED(self%raw)) THEN
    CALL self%split(tokens=tokens, sep=sep)
    tokens = tokens%lower()
    snakecase = snakecase%join(array=tokens, sep='_')
  END IF
END FUNCTION snakecase

!----------------------------------------------------------------------------
!                                                                 Split
!----------------------------------------------------------------------------

!> author: Szaghi
! date: 11 May 2022
! summary: Return a list of substring in the string
!
!# Introduction
!
! Return a list of substring in the string, using sep as the delimiter string
!
!@note
! Multiple subsequent separators are collapsed to one occurrence.
!@endnote
!
!@note
! If `max_tokens` is passed the returned number of tokens is either
! `max_tokens` or `max_tokens + 1`.
!@endnote
!
!```fortran
! type(string)              :: astring
! type(string), allocatable :: strings(:)
! logical                   :: test_passed(11)
! astring = '+ab-++cre-++cre-ab+'
! call astring%split(tokens=strings, sep='+')
! test_passed(1) = (strings(1)//''=='ab-'.and.strings(2)//''=='cre-'.and.
! strings(3)//''=='cre-ab')
! astring = 'ab-++cre-++cre-ab+'
! call astring%split(tokens=strings, sep='+')
! test_passed(2) = (strings(1)//''=='ab-'.and.strings(2)//''=='cre-'.and.
! strings(3)//''=='cre-ab')
! astring = 'ab-++cre-++cre-ab'
! call astring%split(tokens=strings, sep='+')
! test_passed(3) = (strings(1)//''=='ab-'.and.strings(2)//''=='cre-'.and.
! strings(3)//''=='cre-ab')
! astring = 'Hello '//new_line('a')//'World!'
! call astring%split(tokens=strings, sep=new_line('a'))
! test_passed(4) = (strings(1)//''=='Hello '.and.strings(2)//''=='World!')
! astring = 'Hello World!'
! call astring%split(tokens=strings)
! test_passed(5) = (strings(1)//''=='Hello'.and.strings(2)//''=='World!')
! astring = '+ab-'
! call astring%split(tokens=strings, sep='+')
! test_passed(6) = (strings(1)//''=='ab-')
! astring = '+ab-'
! call astring%split(tokens=strings, sep='-')
! test_passed(7) = (strings(1)//''=='+ab')
! astring = '+ab-+cd-'
! call astring%split(tokens=strings, sep='+')
! test_passed(8) = (strings(1)//''=='ab-'.and.strings(2)//''=='cd-')
! astring = 'ab-+cd-+'
! call astring%split(tokens=strings, sep='+')
! test_passed(9) = (strings(1)//''=='ab-'.and.strings(2)//''=='cd-')
! astring = '+ab-+cd-+'
! call astring%split(tokens=strings, sep='+')
! test_passed(10) = (strings(1)//''=='ab-'.and.strings(2)//''=='cd-')
! astring = '1-2-3-4-5-6-7-8'
! call astring%split(tokens=strings, sep='-', max_tokens=3)
! test_passed(11) = (strings(1)//''=='1'.and.strings(2)//''=='2'.and.strings
! (3)//''=='3'.and.strings(4)//''=='4-5-6-7-8')
! print '(L1)', all(test_passed)
!```

PURE SUBROUTINE split(self, tokens, sep, max_tokens)
  CLASS(string), INTENT(IN) :: self
  !! The string.
  TYPE(string), ALLOCATABLE, INTENT(out) :: tokens(:)
  !! Tokens substring.
  CHARACTER(kind=CK, len=*), INTENT(IN), OPTIONAL :: sep
  !! Separator.
  INTEGER, INTENT(IN), OPTIONAL :: max_tokens
  !! Fix the maximum number of returned tokens.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: sep_
  !! Separator, default value.
  INTEGER :: No
  !! Number of occurrences of sep.
  INTEGER :: t
  !! Character counter.
  TYPE(string) :: temporary
  !! Temporary storage.
  TYPE(string), ALLOCATABLE :: temp_toks(:, :)
  !! Temporary tokens substring.
  !!
  !!
  !!
  IF (ALLOCATED(self%raw)) THEN
    !!
    sep_ = SPACE; IF (PRESENT(sep)) sep_ = sep
    !!
    temporary = self%unique(sep_)
    No = temporary%COUNT(sep_)
    !!
    IF (No > 0) THEN
      IF (PRESENT(max_tokens)) THEN
        IF (max_tokens < No .AND. max_tokens > 0) No = max_tokens
      END IF
      ALLOCATE (temp_toks(3, No))
      temp_toks(:, 1) = temporary%partition(sep_)
      IF (No > 1) THEN
        DO t = 2, No
          temp_toks(:, t) = temp_toks(3, t - 1)%partition(sep_)
        END DO
      END IF
      !!
      IF (temp_toks(1, 1)%raw /= '' .AND. temp_toks(3, No)%raw /= '') THEN
        ALLOCATE (tokens(No + 1))
        DO t = 1, No
          IF (t == No) THEN
            tokens(t) = temp_toks(1, t)
            tokens(t + 1) = temp_toks(3, t)
          ELSE
            tokens(t) = temp_toks(1, t)
          END IF
        END DO
      ELSEIF (temp_toks(1, 1)%raw /= '') THEN
        ALLOCATE (tokens(No))
        DO t = 1, No
          tokens(t) = temp_toks(1, t)
        END DO
      ELSEIF (temp_toks(3, No)%raw /= '') THEN
        ALLOCATE (tokens(No))
        DO t = 1, No - 1
          tokens(t) = temp_toks(1, t + 1)
        END DO
        tokens(No) = temp_toks(3, No)
      ELSE
        ALLOCATE (tokens(No - 1))
        DO t = 2, No
          tokens(t - 1) = temp_toks(1, t)
        END DO
      END IF
      !!
    ELSE
      ALLOCATE (tokens(1))
      tokens(1) = self
    END IF
  END IF
END SUBROUTINE split

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Szaghi
! date: 21 July 2022
! summary: Return substrings
!
!# Introduction
!
! Return a list of substring in the string, using sep as the delimiter
! string, chunked (memory-efficient) algorithm.
!
!@note
! Multiple subsequent separators are collapsed to one occurrence.
!@endnote
!
!@note
! The split is performed in chunks of `#chunks` to avoid excessive memory
! consumption.
!@endnote
!
!```fortran
! type(string)              :: astring
! type(string), allocatable :: strings(:)
! logical                   :: test_passed(1)
! astring = '-1-2-3-4-5-6-7-8-'
! call astring%split_chunked(tokens=strings, sep='-', chunks=3)
! test_passed(1) = (strings(1)//''=='1'.and.strings(2)//''=='2'.and.strings
! (3)//''=='3'.and.strings(4)//''=='4'.and. &
!                   strings(5)//''=='5'.and.strings(6)//''=='6'.and.strings
! (7)//''=='7'.and.strings(8)//''=='8')
! print '(L1)', all(test_passed)
!```

PURE SUBROUTINE split_chunked(self, tokens, chunks, sep)
  !!
  CLASS(string), INTENT(IN) :: self
  !! The string.
  TYPE(string), ALLOCATABLE, INTENT(out) :: tokens(:)
  !! Tokens substring.
  INTEGER, INTENT(IN) :: chunks
  !! Number of chunks.
  CHARACTER(kind=CK, len=*), INTENT(IN), OPTIONAL :: sep
  !! Separator.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: sep_
  !! Separator, default value.
  INTEGER :: Nt
  !! Number of actual tokens.
  INTEGER :: t
  !! Counter.
  LOGICAL :: isok
  !!
  !!
  !!
  IF (ALLOCATED(self%raw)) THEN
    sep_ = SPACE; IF (PRESENT(sep)) sep_ = sep
    !!
    Nt = self%COUNT(sep_)
    IF (self%start_with(prefix=sep_)) Nt = Nt - 1
    IF (self%end_with(suffix=sep_)) Nt = Nt - 1
    t = 0
    CALL self%split(tokens=tokens, sep=sep_, max_tokens=chunks)
    DO
      t = SIZE(tokens, dim=1)
      IF (t > Nt) EXIT
      CALL split_last_token(tokens=tokens, max_tokens=chunks, isok=isok)
      IF (isok) THEN
      ELSE
        EXIT
      END IF
    END DO
    !!
    t = SIZE(tokens, dim=1)
    IF (tokens(t)%COUNT(sep_) > 0) THEN
      CALL split_last_token(tokens=tokens, isok=isok)
    END IF
  END IF
  !!
CONTAINS
  !!
  PURE SUBROUTINE split_last_token(tokens, max_tokens, isok)
    !! Split last token.
    TYPE(string), ALLOCATABLE, INTENT(inout) :: tokens(:)
    !! Tokens substring.
    INTEGER, INTENT(IN), OPTIONAL :: max_tokens
    !! Max tokens returned.
    TYPE(string), ALLOCATABLE :: tokens_(:)
    !! Temporary tokens.
    TYPE(string), ALLOCATABLE :: tokens_swap(:)
    !! Swap tokens.
    INTEGER :: Nt_
    !! Number of last created tokens.
    LOGICAL, INTENT(out) :: isok
    !!
    isok = .TRUE.
    CALL tokens(t)%split(tokens=tokens_, sep=sep_, max_tokens=max_tokens)
    IF (ALLOCATED(tokens_)) THEN
      Nt_ = SIZE(tokens_, dim=1)
      IF (Nt_ >= 1) THEN
        ALLOCATE (tokens_swap(1:t - 1 + Nt_))
        tokens_swap(1:t - 1) = tokens(1:t - 1)
        tokens_swap(t:) = tokens_(:)
        CALL move_ALLOC(from=tokens_swap, to=tokens)
      END IF
      IF (Nt_ == 1) THEN
        isok = .FALSE.
      END IF
      DEALLOCATE (tokens_)
    END IF
  END SUBROUTINE split_last_token
  !!
END SUBROUTINE split_chunked

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION startcase(self, sep)
  !< Return a string with all words capitalized, e.g. title case.
  !<
  !< @note Multiple subsequent separators are collapsed to one occurence.
  !<
  !<```fortran
  !< type(string) :: astring
  !< logical      :: test_passed(1)
  !< astring = 'the Quick Brown fox Jumps over the Lazy Dog.'
  !< test_passed(1) = astring%startcase()//''=='The Quick Brown Fox Jumps Over The Lazy Dog.'
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self      !< The string.
  CHARACTER(kind=CK, len=*), INTENT(IN), OPTIONAL :: sep       !< Separator.
  TYPE(string) :: startcase !< Start case string.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: sep_      !< Separator, default value.
  TYPE(string), ALLOCATABLE :: tokens(:) !< String tokens.

  IF (ALLOCATED(self%raw)) THEN
    sep_ = SPACE; IF (PRESENT(sep)) sep_ = sep
    CALL self%split(tokens=tokens, sep=sep_)
    tokens = tokens%capitalize()
    startcase = startcase%join(array=tokens, sep=sep_)
  END IF
END FUNCTION startcase

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION strip(self, remove_nulls)
  !< Return a copy of the string with the leading and trailing characters removed.
  !<
  !< @note Multiple subsequent separators are collapsed to one occurence.
  !<
  !<```fortran
  !< type(string) :: astring
  !< logical      :: test_passed(1)
  !< astring = '  Hello World!   '
  !< test_passed(1) = astring%strip()//''=='Hello World!'
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self         !< The string.
  LOGICAL, INTENT(IN), OPTIONAL :: remove_nulls !< Remove null characters at the end.
  TYPE(string) :: strip        !< The stripped string.
  INTEGER :: c            !< Counter.

  IF (ALLOCATED(self%raw)) THEN
    strip = self%ADJUSTL()
    strip = strip%TRIM()
    IF (PRESENT(remove_nulls)) THEN
      IF (remove_nulls) THEN
        c = INDEX(self%raw, CHAR(0))
        IF (c > 0) strip%raw = strip%raw(1:c - 1)
      END IF
    END IF
  END IF
END FUNCTION strip

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION swapcase(self)
  !< Return a copy of the string with uppercase characters converted to lowercase and vice versa.
  !<
  !<```fortran
  !< type(string) :: astring
  !< logical      :: test_passed(1)
  !< astring = '  Hello World!   '
  !< test_passed(1) = astring%swapcase()//''=='  hELLO wORLD!   '
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self     !< The string.
  TYPE(string) :: swapcase !< Upper case string.
  INTEGER :: n1       !< Characters counter.
  INTEGER :: n2       !< Characters counter.

  IF (ALLOCATED(self%raw)) THEN
    swapcase = self
    DO n1 = 1, LEN(self%raw)
      n2 = INDEX(UPPER_ALPHABET, self%raw(n1:n1))
      IF (n2 > 0) THEN
        swapcase%raw(n1:n1) = LOWER_ALPHABET(n2:n2)
      ELSE
        n2 = INDEX(LOWER_ALPHABET, self%raw(n1:n1))
        IF (n2 > 0) swapcase%raw(n1:n1) = UPPER_ALPHABET(n2:n2)
      END IF
    END DO
  END IF
END FUNCTION swapcase

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION tempname(self, is_file, prefix, path)
  !< Return a safe temporary name suitable for temporary file or directories.
  !<
  !<```fortran
  !< type(string) :: astring
  !< character(len=:), allocatable :: tmpname
  !< logical                       :: test_passed(5)
  !< tmpname = astring%tempname()
  !< inquire(file=tmpname, exist=test_passed(1))
  !< test_passed(1) = .not.test_passed(1)
  !< tmpname = astring%tempname(is_file=.false.)
  !< inquire(file=tmpname, exist=test_passed(2))
  !< test_passed(2) = .not.test_passed(2)
  !< tmpname = astring%tempname(path='./')
  !< inquire(file=tmpname, exist=test_passed(3))
  !< test_passed(3) = .not.test_passed(3)
  !< astring = 'me-'
  !< tmpname = astring%tempname()
  !< inquire(file=tmpname, exist=test_passed(4))
  !< test_passed(4) = .not.test_passed(4)
  !< tmpname = astring%tempname(prefix='you-')
  !< inquire(file=tmpname, exist=test_passed(5))
  !< test_passed(5) = .not.test_passed(5)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self                   !< The string.
  LOGICAL, INTENT(IN), OPTIONAL :: is_file                !< True if tempname should be used for file (the default).
  CHARACTER(*), INTENT(IN), OPTIONAL :: prefix                 !< Name prefix, otherwise self is used (if allocated).
  CHARACTER(*), INTENT(IN), OPTIONAL :: path                   !< Path where file/directory should be used, default `./`.
  CHARACTER(len=:), ALLOCATABLE :: tempname               !< Safe (unique) temporary name.
  LOGICAL :: is_file_               !< True if tempname should be used for file (the default).
  CHARACTER(len=:), ALLOCATABLE :: prefix_                !< Name prefix, otherwise self is used (if allocated).
  CHARACTER(len=:), ALLOCATABLE :: path_                  !< Path where file/directory should be used, default `./`.
  LOGICAL, SAVE :: is_initialized = .FALSE. !< Status of random seed initialization.
  REAL(R4P) :: random_real            !< Random number (real).
  INTEGER(I4P) :: random_integer         !< Random number (integer).
  LOGICAL :: is_hold                !< Flag to check if a safe tempname has been found.

  is_file_ = .TRUE.; IF (PRESENT(is_file)) is_file_ = is_file
  path_ = ''; IF (PRESENT(path)) path_ = path
  prefix_ = ''
  IF (PRESENT(prefix)) THEN
    prefix_ = prefix
  ELSEIF (ALLOCATED(self%raw)) THEN
    prefix_ = self%raw
  END IF
  IF (.NOT. is_initialized) THEN
    CALL random_seed
    is_initialized = .TRUE.
  END IF
  tempname = REPEAT(' ', LEN(path_) + LEN(prefix_) + 10) ! [path_] + [prefix_] + 6 random chars + [.tmp]
  DO
    CALL random_NUMBER(random_real)
    random_integer = TRANSFER(random_real, random_integer)
    random_integer = IAND(random_integer, 16777215_I4P)
    IF (is_file_) THEN
      WRITE (tempname, '(A,Z6.6,A)') path_//prefix_, random_integer, '.tmp'
    ELSE
      WRITE (tempname, '(A,Z6.6)') path_//prefix_, random_integer
      tempname = TRIM(tempname)
    END IF
    INQUIRE (file=tempname, exist=is_hold)
    IF (.NOT. is_hold) EXIT
  END DO
END FUNCTION tempname

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2022
! summary: Cast string to integer (I1P).
!
!# Introduction
!
!```fortran
! use penf
! type(string) :: astring
! integer(I1P) :: integer_
! logical      :: test_passed(1)
! astring = '127'
! integer_ = astring%to_number(kind=1_I1P)
! test_passed(1) = integer_==127_I1P
! print '(L1)', all(test_passed)
!```

ELEMENTAL FUNCTION to_integer_I1P(self, kind) RESULT(to_number)
  CLASS(string), INTENT(IN) :: self
  !! The string.
  INTEGER(I1P), INTENT(IN) :: kind
  !! Mold parameter for kind detection.
  INTEGER(I1P) :: to_number
  !! The number into the string.
  IF (ALLOCATED(self%raw)) THEN
    IF (self%is_integer()) READ (self%raw, *) to_number
  END IF
END FUNCTION to_integer_I1P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#ifndef _NVF
!> author: Vikas Sharma, Ph. D.
! date: 22 July 2023
! summary: Cast string to integer (I2P).
!
!# Introduction
!
!```fortran
! use penf
! type(string) :: astring
! integer(I2P) :: integer_
! logical      :: test_passed(1)
! astring = '127'
! integer_ = astring%to_number(kind=1_I2P)
! test_passed(1) = integer_==127_I2P
! print '(L1)', all(test_passed)
!```

ELEMENTAL FUNCTION to_integer_I2P(self, kind) RESULT(to_number)
  CLASS(string), INTENT(IN) :: self
  !! The string.
  INTEGER(I2P), INTENT(IN) :: kind
  !! Mold parameter for kind detection.
  INTEGER(I2P) :: to_number
  !! The number into the string.
  IF (ALLOCATED(self%raw)) THEN
    IF (self%is_integer()) READ (self%raw, *) to_number
  END IF
END FUNCTION to_integer_I2P
#endif

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2022
! summary: Cast string to integer (I4P).
!
!# Introduction
!
!
!```fortran
! use penf
! type(string) :: astring
! integer(I4P) :: integer_
! logical      :: test_passed(1)
! astring = '127'
! integer_ = astring%to_number(kind=1_I4P)
! test_passed(1) = integer_==127_I4P
! print '(L1)', all(test_passed)
!```

ELEMENTAL FUNCTION to_integer_I4P(self, kind) RESULT(to_number)
  CLASS(string), INTENT(IN) :: self
  !! The string.
  INTEGER(I4P), INTENT(IN) :: kind
  !! Mold parameter for kind detection.
  INTEGER(I4P) :: to_number
  !! The number into the string.
  IF (ALLOCATED(self%raw)) THEN
    IF (self%is_integer()) READ (self%raw, *) to_number
  END IF
END FUNCTION to_integer_I4P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 23 July 2023
! summary: Cast string to integer (I8P).
!
!# Introduction
!
!```fortran
! use penf
! type(string) :: astring
! integer(I8P) :: integer_
! logical      :: test_passed(1)
! astring = '127'
! integer_ = astring%to_number(kind=1_I8P)
! test_passed(1) = integer_==127_I8P
! print '(L1)', all(test_passed)
!```

ELEMENTAL FUNCTION to_integer_I8P(self, kind) RESULT(to_number)
  CLASS(string), INTENT(IN) :: self
  !! The string.
  INTEGER(I8P), INTENT(IN) :: kind
  !! Mold parameter for kind detection.
  INTEGER(I8P) :: to_number
  !! The number into the string.
  IF (ALLOCATED(self%raw)) THEN
    IF (self%is_integer()) READ (self%raw, *) to_number
  END IF
END FUNCTION to_integer_I8P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 23 July 2022
! summary: Cast string to real (R4P).
!
!# Introduction
!
!```fortran
! use penf
! type(string) :: astring
! real(R4P)    :: real_
! logical      :: test_passed(1)
! astring = '3.4e9'
! real_ = astring%to_number(kind=1._R4P)
! test_passed(1) = real_==3.4e9_R4P
! print '(L1)', all(test_passed)
!```

ELEMENTAL FUNCTION to_real_R4P(self, kind) RESULT(to_number)
  CLASS(string), INTENT(IN) :: self
  !! The string.
  REAL(R4P), INTENT(IN) :: kind
  !! Mold parameter for kind detection.
  REAL(R4P) :: to_number
  !! The number into the string.
  IF (ALLOCATED(self%raw)) THEN
    IF (self%is_real()) READ (self%raw, *) to_number
  END IF
END FUNCTION to_real_R4P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 23 July 2022
! summary:         Cast string to real (R8P).
!
!# Introduction
!
!```fortran
! use penf
! type(string) :: astring
! real(R8P)    :: real_
! logical      :: test_passed(1)
! astring = '3.4e9'
! real_ = astring%to_number(kind=1._R8P)
! test_passed(1) = real_==3.4e9_R8P
! print '(L1)', all(test_passed)
!```

ELEMENTAL FUNCTION to_real_R8P(self, kind) RESULT(to_number)
  CLASS(string), INTENT(IN) :: self
  !! The string.
  REAL(R8P), INTENT(IN) :: kind
  !! Mold parameter for kind detection.
  REAL(R8P) :: to_number
  !! The number into the string.
  IF (ALLOCATED(self%raw)) THEN
    IF (self%is_real()) READ (self%raw, *) to_number
  END IF
END FUNCTION to_real_R8P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION to_real_R16P(self, kind) RESULT(to_number)
  !< Cast string to real (R16P).
  !<
  !<```fortran
  !< use penf
  !< type(string) :: astring
  !< real(R16P)   :: real_
  !< logical      :: test_passed(1)
  !< astring = '3.4e9'
  !< real_ = astring%to_number(kind=1._R16P)
  !< test_passed(1) = real_==3.4e9_R16P
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self      !< The string.
  REAL(R16P), INTENT(IN) :: kind      !< Mold parameter for kind detection.
  REAL(R16P) :: to_number !< The number into the string.

  IF (ALLOCATED(self%raw)) THEN
    IF (self%is_real()) READ (self%raw, *) to_number
  END IF
END FUNCTION to_real_R16P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2022
! summary: Convert a string to boolean

ELEMENTAL FUNCTION to_logical(self) RESULT(ans)
  CLASS(string), INTENT(IN) :: self
  !! The string.
  LOGICAL :: ans
  !!
  TYPE(String) :: tmp
  ! True and False options (all lowercase):
  CHARACTER(LEN=*), DIMENSION(4), PARAMETER :: true_str = ['1     ', &
                                                           't     ', &
                                                           'true  ', &
                                                           '.true.']
  CHARACTER(LEN=*), DIMENSION(4), PARAMETER :: false_str = ['0      ', &
                                                            'f      ', &
                                                            'false  ', &
                                                            '.false.']
  !!
  IF (ALLOCATED(self%raw)) THEN
    tmp = self%lower()
    IF (ANY(tmp .EQ. true_str)) THEN
      ans = .TRUE.
    ELSEIF (ANY(tmp .EQ. false_str)) THEN
      ans = .FALSE.
    ELSE
      ans = .FALSE.
    END IF
  END IF
  !!
END FUNCTION to_logical

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION unescape(self, to_unescape, unesc) RESULT(unescaped)
  !< Unescape double backslashes (or custom escaped character).
  !<
  !<```fortran
  !< type(string) :: astring
  !< logical      :: test_passed(2)
  !< astring = '^\\s \\d+\\s*'
  !< test_passed(1) = (astring%unescape(to_unescape='\')//''=='^\s \d+\s*')
  !< test_passed(2) = (astring%unescape(to_unescape='s')//''=='^\s \\d+\s*')
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self
  !! The string.
  CHARACTER(kind=CK, len=1), INTENT(IN) :: to_unescape
  !! Character to be unescaped.
  CHARACTER(kind=CK, len=*), INTENT(IN), OPTIONAL :: unesc
  !! Character used to unescape.
  TYPE(string) :: unescaped
  !! Escaped string.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: unesc_
  !! Character to unescape, local variable.
  INTEGER :: c
  !! Character counter.

  IF (ALLOCATED(self%raw)) THEN
    unesc_ = ''; IF (PRESENT(unesc)) unesc_ = unesc
    unescaped%raw = ''
    c = 1
    DO
      IF (c > LEN(self%raw)) EXIT
      IF (c == LEN(self%raw)) THEN
        unescaped%raw = unescaped%raw//self%raw(c:c)
        EXIT
      ELSE
        IF (self%raw(c:c + 1) == BACKSLASH//to_unescape) THEN
          unescaped%raw = unescaped%raw//to_unescape
          c = c + 2
        ELSE
          unescaped%raw = unescaped%raw//self%raw(c:c)
          c = c + 1
        END IF
      END IF
    END DO
  END IF
END FUNCTION unescape

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION unique(self, substring) RESULT(uniq)
  !< Reduce to one (unique) multiple (sequential) occurrences of a substring into a string.
  !<
  !< For example the string ' ab-cre-cre-ab' is reduce to 'ab-cre-ab' if the substring is '-cre'.
  !< @note Eventual multiple trailing white space are not reduced to one occurrence.
  !<
  !<```fortran
  !< type(string) :: astring
  !< logical      :: test_passed(1)
  !< astring = '+++ab-++cre-++cre-ab+++++'
  !< test_passed(1) = astring%unique(substring='+')//''=='+ab-+cre-+cre-ab+'
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self
  !! The string.
  CHARACTER(kind=CK, len=*), INTENT(IN), OPTIONAL :: substring
  !! Substring which multiple occurences must be reduced to one.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: substring_
  !! Substring, default value.
  TYPE(string) :: uniq
  !! String parsed.
#ifdef _NVF
  CHARACTER(9999) :: nvf_bug
  !! Work around for NVFortran bug.
#endif

  IF (ALLOCATED(self%raw)) THEN
    substring_ = SPACE; IF (PRESENT(substring)) substring_ = substring

    uniq = self
    DO
#ifdef _NVF
      nvf_bug = substring_
      IF (.NOT. uniq%INDEX(REPEAT(TRIM(nvf_bug), 2)) > 0) EXIT
      uniq = uniq%replace(old=REPEAT(TRIM(nvf_bug), 2), NEW=substring_)
#else
      IF (.NOT. uniq%INDEX(REPEAT(substring_, 2)) > 0) EXIT
      uniq = uniq%replace(old=REPEAT(substring_, 2), NEW=substring_)
#endif
    END DO
  END IF
END FUNCTION unique

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION upper(self)
  !< Return a string with all uppercase characters.
  !<
  !<```fortran
  !< type(string) :: astring
  !< logical      :: test_passed(1)
  !< astring = 'Hello WorLD!'
  !< test_passed(1) = astring%upper()//''=='HELLO WORLD!'
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self  !< The string.
  TYPE(string) :: upper !< Upper case string.
  INTEGER :: n1    !< Characters counter.
  INTEGER :: n2    !< Characters counter.

  IF (ALLOCATED(self%raw)) THEN
    upper = self
    DO n1 = 1, LEN(self%raw)
      n2 = INDEX(LOWER_ALPHABET, self%raw(n1:n1))
      IF (n2 > 0) upper%raw(n1:n1) = UPPER_ALPHABET(n2:n2)
    END DO
  END IF
END FUNCTION upper

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE write_file(self, file, form, iostat, iomsg)
  !< Write a single string stream into file.
  !<
  !< @note For unformatted read only `access='stream'` is supported with new_line as line terminator.
  !<
  !<```fortran
  !< type(string)              :: astring
  !< type(string)              :: anotherstring
  !< type(string), allocatable :: strings(:)
  !< type(string)              :: line(3)
  !< integer                   :: iostat
  !< character(len=99)         :: iomsg
  !< integer                   :: scratch
  !< integer                   :: l
  !< logical                   :: test_passed(8)
  !< line(1) = ' Hello World!   '
  !< line(2) = 'How are you?  '
  !< line(3) = '   All say: "Fine thanks"'
  !< anotherstring = anotherstring%join(array=line, sep=new_line('a'))
  !< call anotherstring%write_file(file='write_file_test.tmp', iostat=iostat, iomsg=iomsg)
  !< call astring%read_file(file='write_file_test.tmp', iostat=iostat, iomsg=iomsg)
  !< call astring%split(tokens=strings, sep=new_line('a'))
  !< test_passed(1) = (size(strings, dim=1)==size(line, dim=1))
  !< do l=1, size(strings, dim=1)
  !<   test_passed(l+1) = (strings(l)==line(l))
  !< enddo
  !< call anotherstring%write_file(file='write_file_test.tmp', form='unformatted', iostat=iostat, iomsg=iomsg)
  !< call astring%read_file(file='write_file_test.tmp', form='unformatted', iostat=iostat, iomsg=iomsg)
  !< call astring%split(tokens=strings, sep=new_line('a'))
  !< test_passed(5) = (size(strings, dim=1)==size(line, dim=1))
  !< do l=1, size(strings, dim=1)
  !<   test_passed(l+5) = (strings(l)==line(l))
  !< enddo
  !< open(newunit=scratch, file='write_file_test.tmp')
  !< close(unit=scratch, status='delete')
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self    !< The string.
  CHARACTER(len=*), INTENT(IN) :: file    !< File name.
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: form    !< Format of unit.
  INTEGER, INTENT(out), OPTIONAL :: iostat  !< IO status code.
  CHARACTER(len=*), INTENT(inout), OPTIONAL :: iomsg   !< IO status message.
  TYPE(string) :: form_   !< Format of unit, local variable.
  INTEGER :: iostat_ !< IO status code, local variable.
  CHARACTER(len=:), ALLOCATABLE :: iomsg_  !< IO status message, local variable.
  INTEGER :: unit    !< Logical unit.

  iomsg_ = REPEAT(' ', 99); IF (PRESENT(iomsg)) iomsg_ = iomsg
  form_ = 'FORMATTED'; IF (PRESENT(form)) form_ = form; form_ = form_%upper()
  SELECT CASE (form_%chars())
  CASE ('FORMATTED')
         OPEN (newunit=unit, file=file, action='WRITE', iomsg=iomsg_, iostat=iostat_, err=10)
  CASE ('UNFORMATTED')
         OPEN (newunit=unit, file=file, action='WRITE', form='UNFORMATTED', access='STREAM', iomsg=iomsg_, iostat=iostat_, err=10)
  END SELECT
  CALL self%write_lines(unit=unit, form=form, iomsg=iomsg_, iostat=iostat_)
10 CLOSE (unit)
  IF (PRESENT(iostat)) iostat = iostat_
  IF (PRESENT(iomsg)) iomsg = iomsg_
END SUBROUTINE write_file

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE write_line(self, unit, form, iostat, iomsg)
  !< Write line (record) to a connected unit.
  !<
  !< @note If the connected unit is unformatted a `new_line()` character is added at the end (if necessary) to mark the end of line.
  !<
  !< @note There is no doctests, this being tested by means of [[string:write_file]] doctests.
  CLASS(string), INTENT(IN) :: self    !< The string.
  INTEGER, INTENT(IN) :: unit    !< Logical unit.
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: form    !< Format of unit.
  INTEGER, INTENT(out), OPTIONAL :: iostat  !< IO status code.
  CHARACTER(len=*), INTENT(inout), OPTIONAL :: iomsg   !< IO status message.
  TYPE(string) :: form_   !< Format of unit, local variable.
  INTEGER :: iostat_ !< IO status code, local variable.
  CHARACTER(len=:), ALLOCATABLE :: iomsg_  !< IO status message, local variable.

  iostat_ = 0
  iomsg_ = REPEAT(' ', 99); IF (PRESENT(iomsg)) iomsg_ = iomsg
  IF (ALLOCATED(self%raw)) THEN
    form_ = 'FORMATTED'; IF (PRESENT(form)) form_ = form; form_ = form_%upper()
    SELECT CASE (form_%chars())
    CASE ('FORMATTED')
      WRITE (unit, "(A)", iostat=iostat_, iomsg=iomsg_) self%raw
    CASE ('UNFORMATTED')
      IF (self%end_with(new_LINE('a'))) THEN
        WRITE (unit, iostat=iostat_, iomsg=iomsg_) self%raw
      ELSE
        WRITE (unit, iostat=iostat_, iomsg=iomsg_) self%raw//new_LINE('a')
      END IF
    END SELECT
  END IF
  IF (PRESENT(iostat)) iostat = iostat_
  IF (PRESENT(iomsg)) iomsg = iomsg_
END SUBROUTINE write_line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 23 July 2022
! summary: Write lines (records) to a connected unit.
!
!# Introduction
!
!
! This method checks if self contains more than one line (records) and writes
! them as lines (records).
!
! @note If the connected unit is unformatted a `new_line()` character is
! added at the end (if necessary) to mark the end of line.
!
! @note There is no doctests, this being tested by means of
! [[string:write_file]] doctests.

SUBROUTINE write_lines(self, unit, form, iostat, iomsg)
  CLASS(string), INTENT(IN) :: self
  !! The string.
  INTEGER, INTENT(IN) :: unit
  !! Logical unit.
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: form
  !! Format of unit.
  INTEGER, INTENT(out), OPTIONAL :: iostat
  !! IO status code.
  CHARACTER(len=*), INTENT(inout), OPTIONAL :: iomsg
  !! IO status message.
  TYPE(string), ALLOCATABLE :: lines(:)
  !! Lines.
  INTEGER :: l
  !! Counter.
  !!
  IF (ALLOCATED(self%raw)) THEN
    CALL self%split(tokens=lines, sep=new_LINE('a'))
    DO l = 1, SIZE(lines, dim=1)
    CALL lines(l)%write_line(unit=unit, form=form, iostat=iostat, iomsg=iomsg)
    END DO
  END IF
END SUBROUTINE write_lines

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 23 July 2022
! summary: Return true if a string ends with a specified suffix.
!
!# Introduction
!
!```fortran
! type(string) :: astring
! logical      :: test_passed(5)
! astring = 'Hello WorLD!'
! test_passed(1) = astring%end_with(suffix='LD!').eqv..true.
! test_passed(2) = astring%end_with(suffix='lD!').eqv..false.
! test_passed(3) = astring%end_with(suffix='orLD!', start=5).eqv..true.
! test_passed(4) = astring%end_with(suffix='orLD!', start=8, end=12).eqv..
! true.
! test_passed(5) = astring%end_with(suffix='!').eqv..true.
! print '(L1)', all(test_passed)
!```

ELEMENTAL FUNCTION end_with(self, suffix, start, END, ignore_null_eof)
  CLASS(string), INTENT(IN) :: self
  !! The string.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: suffix
  !! Searched suffix.
  INTEGER, INTENT(IN), OPTIONAL :: start
  !! Start position into the string.
  INTEGER, INTENT(IN), OPTIONAL :: END
  !! End position into the string.
  LOGICAL, INTENT(IN), OPTIONAL :: ignore_null_eof
  !! Ignore null character at the end of file.
  LOGICAL :: end_with
  !! Result of the test.
  INTEGER :: start_
  !! Start position into the string, local variable.
  INTEGER :: end_
  !! End position into the string, local variable.
  LOGICAL :: ignore_null_eof_
  !! Ignore null character at the end of file, local variable.
  !!
  end_with = .FALSE.
  IF (ALLOCATED(self%raw)) THEN
    start_ = 1; IF (PRESENT(start)) start_ = start
    end_ = LEN(self%raw); IF (PRESENT(END)) end_ = END
    ignore_null_eof_ = .FALSE.;
    IF (PRESENT(ignore_null_eof)) ignore_null_eof_ = ignore_null_eof
  IF (ignore_null_eof_ .AND. (self%raw(end_:end_) == CHAR(0))) end_ = end_ - 1
    IF (LEN(suffix) <= LEN(self%raw(start_:end_))) THEN
      end_with = self%raw(end_ - LEN(suffix) + 1:end_) == suffix
    END IF
  END IF
END FUNCTION end_with

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 23 July 2022
! summary: Return true if the string is allocated.
!
!# Introduction
!
!```fortran
! type(string) :: astring
! logical      :: test_passed(2)
! test_passed(1) = astring%is_allocated().eqv..false.
! astring = 'hello'
! test_passed(2) = astring%is_allocated().eqv..true.
! print '(L1)', all(test_passed)
!```

ELEMENTAL FUNCTION is_allocated(self)
  CLASS(string), INTENT(IN) :: self
  !! The string.
  LOGICAL :: is_allocated
  !! Result of the test.
  is_allocated = ALLOCATED(self%raw)
END FUNCTION is_allocated

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 23 July 2022
! summary: Return true if all characters in the string are digits.
!
!# Introduction
!
!```fortran
! type(string) :: astring
! logical      :: test_passed(2)
! astring = '   -1212112.3 '
! test_passed(1) = astring%is_digit().eqv..false.
! astring = '12121123'
! test_passed(2) = astring%is_digit().eqv..true.
! print '(L1)', all(test_passed)
!```

ELEMENTAL FUNCTION is_digit(self)
  CLASS(string), INTENT(IN) :: self
  !! The string.
  LOGICAL :: is_digit
  !! Result of the test.
  INTEGER :: c
  !! Character counter.
  is_digit = .FALSE.
  IF (ALLOCATED(self%raw)) THEN
    DO c = 1, LEN(self%raw)
      SELECT CASE (self%raw(c:c))
      CASE ('0':'9')
        is_digit = .TRUE.
      CASE default
        is_digit = .FALSE.
        EXIT
      END SELECT
    END DO
  END IF
END FUNCTION is_digit

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 23 July 2022
! summary: Return true if the string contains an integer.
!
!# Introduction
!
!
! The regular expression is `\s*[\+\-]?\d+([eE]\+?\d+)?\s*`. The parse
! algorithm is done in stages:
!
! | S0  | S1      | S2  | S3   | S4  | S5  | S6  |
! |-----|---------|-----|------|-----|-----|-----|
! |`\s*`|`[\+\-]?`|`\d+`|`[eE]`|`\+?`|`\d+`|`\s*`|
!
! Exit on stages-parsing results in:
!
! | S0 | S1 | S2 | S3 | S4 | S5 | S6 |
! |----|----|----|----|----|----|----|
! |  F |  F |  T |  F |  F |  T |  T |
!
! @note This implementation is courtesy of
! [tomedunn](https://github.com/tomedunn/fortran-string-utility-module/blob/
! master/src/string_utility_module.f90#L294)
!
!```fortran
! type(string) :: astring
! logical      :: test_passed(6)
! astring = '   -1212112 '
! test_passed(1) = astring%is_integer().eqv..true.
! astring = '   -1212112'
! test_passed(2) = astring%is_integer(allow_spaces=.false.).eqv..false.
! astring = '-1212112   '
! test_passed(3) = astring%is_integer(allow_spaces=.false.).eqv..false.
! astring = '+2e20'
! test_passed(4) = astring%is_integer().eqv..true.
! astring = ' -2E13 '
! test_passed(5) = astring%is_integer().eqv..true.
! astring = ' -2 E13 '
! test_passed(6) = astring%is_integer().eqv..false.
! print '(L1)', all(test_passed)
!```

ELEMENTAL FUNCTION is_integer(self, allow_spaces)
  CLASS(string), INTENT(IN) :: self
  !! The string.
  LOGICAL, INTENT(IN), OPTIONAL :: allow_spaces
  !! Allow leading-trailing spaces.
  LOGICAL :: is_integer
  !! Result of the test.
  LOGICAL :: allow_spaces_
  !! Allow leading-trailing spaces, local variable.
  INTEGER :: stage
  !! Stages counter.
  INTEGER :: c
  !! Character counter.
  !!
  IF (ALLOCATED(self%raw)) THEN
    allow_spaces_ = .TRUE.
    IF (PRESENT(allow_spaces)) allow_spaces_ = allow_spaces
    stage = 0
    is_integer = .TRUE.
    DO c = 1, LEN(self%raw)
      SELECT CASE (self%raw(c:c))
      CASE (SPACE, TAB)
        SELECT CASE (stage)
        CASE (0, 6)
          is_integer = allow_spaces_
        CASE (2, 5)
          is_integer = allow_spaces_
          stage = 6
        CASE default
          is_integer = .FALSE.
        END SELECT
      CASE ('-')
        SELECT CASE (stage)
        CASE (0)
          stage = 1
        CASE default
          is_integer = .FALSE.
        END SELECT
      CASE ('+')
        SELECT CASE (stage)
        CASE (0)
          stage = 1
        CASE (3)
          stage = 4
        CASE default
          is_integer = .FALSE.
        END SELECT
      CASE ('0':'9')
        SELECT CASE (stage)
        CASE (0:1)
          stage = 2
        CASE (3:4)
          stage = 5
        CASE default
          CONTINUE
        END SELECT
      CASE ('e', 'E')
        SELECT CASE (stage)
        CASE (2)
          stage = 3
        CASE default
          is_integer = .FALSE.
        END SELECT
      CASE default
        is_integer = .FALSE.
      END SELECT
      IF (.NOT. is_integer) EXIT
    END DO
  END IF
  IF (is_integer) THEN
    SELECT CASE (stage)
    CASE (2, 5, 6)
      is_integer = .TRUE.
    CASE default
      is_integer = .FALSE.
    END SELECT
  END IF
END FUNCTION is_integer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 23 July 2022
! summary: Return true if the string contains a number (real or integer).
!
!# Introduction
!
!```fortran
! type(string) :: astring
! logical      :: test_passed(7)
! astring = '   -1212112 '
! test_passed(1) = astring%is_number().eqv..true.
! astring = '   -121.2112 '
! test_passed(2) = astring%is_number().eqv..true.
! astring = '   -1212112'
! test_passed(3) = astring%is_number(allow_spaces=.false.).eqv..false.
! astring = '-12121.12   '
! test_passed(4) = astring%is_number(allow_spaces=.false.).eqv..false.
! astring = '+2e20'
! test_passed(5) = astring%is_number().eqv..true.
! astring = ' -2.4E13 '
! test_passed(6) = astring%is_number().eqv..true.
! astring = ' -2 E13 '
! test_passed(7) = astring%is_number().eqv..false.
! print '(L1)', all(test_passed)
!```

ELEMENTAL FUNCTION is_number(self, allow_spaces)
  CLASS(string), INTENT(IN) :: self
  !! The string.
  LOGICAL, INTENT(IN), OPTIONAL :: allow_spaces
  !! Allow leading-trailing spaces.
  LOGICAL :: is_number    !< Result of the test.
  !!
  is_number = (self%is_integer(allow_spaces=allow_spaces) &
    & .OR. self%is_real(allow_spaces=allow_spaces))
  !!
END FUNCTION is_number

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 23 July 2022
! summary: Return true if the string contains a real.
!
!# Introduction
!
! The regular expression is `\s*[\+\-]?\d*(|\.?\d*([deDE][\+\-]?\d+)?)\s*`. The parse algorithm is done in stages:
!
! | S0  | S1      | S2  | S3  | S4  | S5     | S6      | S7  | S8  |
! |-----|---------|-----|-----|-----|--------|---------|-----|-----|
! |`\s*`|`[\+\-]?`|`\d*`|`\.?`|`\d*`|`[deDE]`|`[\+\-]?`|`\d*`|`\s*`|
!
! Exit on stages-parsing results in:
!
! | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 |
! |----|----|----|----|----|----|----|----|----|
!  |  F |  F |  T |  T |  T |  F |  F |  T |  T |
!
! @note This implementation is courtesy of
! [tomedunn](https://github.com/tomedunn/fortran-string-utility-module/blob/
! master/src/string_utility_module.f90#L614)
!
!```fortran
! type(string) :: astring
! logical      :: test_passed(6)
! astring = '   -1212112.d0 '
! test_passed(1) = astring%is_real().eqv..true.
! astring = '   -1212112.d0'
! test_passed(2) = astring%is_real(allow_spaces=.false.).eqv..false.
! astring = '-1212112.d0   '
! test_passed(3) = astring%is_real(allow_spaces=.false.).eqv..false.
! astring = '+2.e20'
! test_passed(4) = astring%is_real().eqv..true.
! astring = ' -2.01E13 '
! test_passed(5) = astring%is_real().eqv..true.
! astring = ' -2.01 E13 '
! test_passed(6) = astring%is_real().eqv..false.
! print '(L1)', all(test_passed)
!```

ELEMENTAL FUNCTION is_real(self, allow_spaces)
  CLASS(string), INTENT(IN) :: self
  !! The string.
  LOGICAL, INTENT(IN), OPTIONAL :: allow_spaces
  !! Allow leading-trailing spaces.
  LOGICAL :: is_real
  !! Result of the test.
  LOGICAL :: allow_spaces_
  !! Allow leading-trailing spaces, local variable.
  LOGICAL :: has_leading_digit
  !! Check the presence of leading digits.
  INTEGER :: stage
  !! Stages counter.
  INTEGER :: c
  !! Character counter.
  IF (ALLOCATED(self%raw)) THEN
    allow_spaces_ = .TRUE.
    IF (PRESENT(allow_spaces)) allow_spaces_ = allow_spaces
    stage = 0
    is_real = .TRUE.
    has_leading_digit = .FALSE.
    DO c = 1, LEN(self%raw)
      SELECT CASE (self%raw(c:c))
      CASE (SPACE, TAB)
        SELECT CASE (stage)
        CASE (0, 8)
          is_real = allow_spaces_
          CONTINUE
        CASE (2:4, 7)
          is_real = allow_spaces_
          stage = 8
        CASE default
          is_real = .FALSE.
        END SELECT
      CASE ('+', '-')
        SELECT CASE (stage)
        CASE (0)
          stage = 1
        CASE (5)
          stage = 6
        CASE default
          is_real = .FALSE.
        END SELECT
      CASE ('0':'9')
        SELECT CASE (stage)
        CASE (0:1)
          stage = 2
          has_leading_digit = .TRUE.
        CASE (3)
          stage = 4
        CASE (5:6)
          stage = 7
        CASE default
          CONTINUE
        END SELECT
      CASE ('.')
        SELECT CASE (stage)
        CASE (0:2)
          stage = 3
        CASE default
          is_real = .FALSE.
        END SELECT
      CASE ('e', 'E', 'd', 'D')
        SELECT CASE (stage)
        CASE (2:4)
          stage = 5
        CASE default
          is_real = .FALSE.
        END SELECT
      CASE default
        is_real = .FALSE.
      END SELECT
      IF (.NOT. is_real) EXIT
    END DO
  END IF
  IF (is_real) THEN
    SELECT CASE (stage)
    CASE (2, 4, 7, 8)
      is_real = .TRUE.
    CASE (3)
      is_real = has_leading_digit
    CASE default
      is_real = .FALSE.
    END SELECT
  END IF
END FUNCTION is_real

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2022
! summary: Returns true if string contains boolean character
!
!# Introduction
!

ELEMENTAL FUNCTION is_logical(self)
  CLASS(string), INTENT(IN) :: self
  !! The string.
  LOGICAL :: is_logical
  !!
  TYPE(String) :: tmp
  ! True and False options (all lowercase):
  CHARACTER(LEN=*), DIMENSION(4), PARAMETER :: true_str = ['1     ', &
                                                           't     ', &
                                                           'true  ', &
                                                           '.true.']
  CHARACTER(LEN=*), DIMENSION(4), PARAMETER :: false_str = ['0      ', &
                                                            'f      ', &
                                                            'false  ', &
                                                            '.false.']
  IF (ALLOCATED(self%raw)) THEN
    tmp = self%lower()
    IF (ANY(tmp .EQ. true_str)) THEN
      is_logical = .TRUE.
    ELSEIF (ANY(tmp .EQ. false_str)) THEN
      is_logical = .FALSE.
    ELSE
      is_logical = .FALSE.
    END IF
  END IF
  !!
END FUNCTION is_logical

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 23 July 2022
! summary: Return true if all characters in the string are lowercase.
!
!# Introduction
!
!
!```fortran
! type(string) :: astring
! logical      :: test_passed(3)
! astring = ' Hello World'
! test_passed(1) = astring%is_lower().eqv..false.
! astring = ' HELLO WORLD'
! test_passed(2) = astring%is_lower().eqv..false.
! astring = ' hello world'
! test_passed(3) = astring%is_lower().eqv..true.
! print '(L1)', all(test_passed)
!```

ELEMENTAL FUNCTION is_lower(self)
  CLASS(string), INTENT(IN) :: self     !< The string.
  LOGICAL :: is_lower !< Result of the test.
  INTEGER :: c        !< Character counter.

  is_lower = .FALSE.
  IF (ALLOCATED(self%raw)) THEN
    is_lower = .TRUE.
    DO c = 1, LEN(self%raw)
      IF (INDEX(UPPER_ALPHABET, self%raw(c:c)) > 0) THEN
        is_lower = .FALSE.
        EXIT
      END IF
    END DO
  END IF
END FUNCTION is_lower

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL FUNCTION is_upper(self)
  !< Return true if all characters in the string are uppercase.
  !<
  !<```fortran
  !< type(string) :: astring
  !< logical      :: test_passed(3)
  !< astring = ' Hello World'
  !< test_passed(1) = astring%is_upper().eqv..false.
  !< astring = ' HELLO WORLD'
  !< test_passed(2) = astring%is_upper().eqv..true.
  !< astring = ' hello world'
  !< test_passed(3) = astring%is_upper().eqv..false.
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self     !< The string.
  LOGICAL :: is_upper !< Result of the test.
  INTEGER :: c        !< Character counter.

  is_upper = .FALSE.
  IF (ALLOCATED(self%raw)) THEN
    is_upper = .TRUE.
    DO c = 1, LEN(self%raw)
      IF (INDEX(LOWER_ALPHABET, self%raw(c:c)) > 0) THEN
        is_upper = .FALSE.
        EXIT
      END IF
    END DO
  END IF
END FUNCTION is_upper

ELEMENTAL FUNCTION start_with(self, prefix, start, END)
  !< Return true if a string starts with a specified prefix.
  !<
  !<```fortran
  !< type(string) :: astring
  !< logical      :: test_passed(4)
  !< astring = 'Hello WorLD!'
  !< test_passed(1) = astring%start_with(prefix='Hello').eqv..true.
  !< test_passed(2) = astring%start_with(prefix='hell').eqv..false.
  !< test_passed(3) = astring%start_with(prefix='llo Wor', start=3).eqv..true.
  !< test_passed(4) = astring%start_with(prefix='lo W', start=4, end=7).eqv..true.
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: self       !< The string.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: prefix     !< Searched prefix.
  INTEGER, INTENT(IN), OPTIONAL :: start      !< Start position into the string.
  INTEGER, INTENT(IN), OPTIONAL :: END        !< End position into the string.
  LOGICAL :: start_with !< Result of the test.
  INTEGER :: start_     !< Start position into the string, local variable.
  INTEGER :: end_       !< End position into the string, local variable.

  start_with = .FALSE.
  IF (ALLOCATED(self%raw)) THEN
    start_ = 1; IF (PRESENT(start)) start_ = start
    end_ = LEN(self%raw); IF (PRESENT(END)) end_ = END
    IF (LEN(prefix) <= LEN(self%raw(start_:end_))) THEN
      start_with = INDEX(self%raw(start_:end_), prefix) == 1
    END IF
  END IF
END FUNCTION start_with

! private methods

! assignments
PURE SUBROUTINE string_assign_string(lhs, rhs)
  !< Assignment operator from string input.
  !<
  !<```fortran
  !< type(string) :: astring
  !< type(string) :: anotherstring
  !< logical      :: test_passed(1)
  !< astring = 'hello'
  !< anotherstring = astring
  !< test_passed(1) = astring%chars()==anotherstring%chars()
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(inout) :: lhs !< Left hand side.
  TYPE(string), INTENT(IN) :: rhs !< Right hand side.

  IF (ALLOCATED(rhs%raw)) lhs%raw = rhs%raw
END SUBROUTINE string_assign_string

PURE SUBROUTINE string_assign_character(lhs, rhs)
  !< Assignment operator from character input.
  !<
  !<```fortran
  !< type(string) :: astring
  !< logical      :: test_passed(1)
  !< astring = 'hello'
  !< test_passed(1) = astring%chars()=='hello'
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(inout) :: lhs !< Left hand side.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: rhs !< Right hand side.

  lhs%raw = rhs
END SUBROUTINE string_assign_character

PURE SUBROUTINE string_assign_integer_I1P(lhs, rhs)
  !< Assignment operator from integer input.
  !<
  !<```fortran
  !< use penf
  !< type(string) :: astring
  !< logical      :: test_passed(1)
  !< astring = 127_I1P
  !< test_passed(1) = astring%to_number(kind=1_I1P)==127_I1P
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(inout) :: lhs !< Left hand side.
  INTEGER(I1P), INTENT(IN) :: rhs !< Right hand side.

  lhs%raw = TRIM(str(rhs))
END SUBROUTINE string_assign_integer_I1P

PURE SUBROUTINE string_assign_integer_I2P(lhs, rhs)
  !< Assignment operator from integer input.
  !<
  !<```fortran
  !< use penf
  !< type(string) :: astring
  !< logical      :: test_passed(1)
  !< astring = 127_I2P
  !< test_passed(1) = astring%to_number(kind=1_I2P)==127_I2P
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(inout) :: lhs !< Left hand side.
  INTEGER(I2P), INTENT(IN) :: rhs !< Right hand side.

  lhs%raw = TRIM(str(rhs))
END SUBROUTINE string_assign_integer_I2P

PURE SUBROUTINE string_assign_integer_I4P(lhs, rhs)
  !< Assignment operator from integer input.
  !<
  !<```fortran
  !< use penf
  !< type(string) :: astring
  !< logical      :: test_passed(1)
  !< astring = 127_I4P
  !< test_passed(1) = astring%to_number(kind=1_I4P)==127_I4P
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(inout) :: lhs !< Left hand side.
  INTEGER(I4P), INTENT(IN) :: rhs !< Right hand side.

  lhs%raw = TRIM(str(rhs))
END SUBROUTINE string_assign_integer_I4P

PURE SUBROUTINE string_assign_integer_I8P(lhs, rhs)
  !< Assignment operator from integer input.
  !<
  !<```fortran
  !< use penf
  !< type(string) :: astring
  !< logical      :: test_passed(1)
  !< astring = 127_I8P
  !< test_passed(1) = astring%to_number(kind=1_I8P)==127_I8P
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(inout) :: lhs !< Left hand side.
  INTEGER(I8P), INTENT(IN) :: rhs !< Right hand side.

  lhs%raw = TRIM(str(rhs))
END SUBROUTINE string_assign_integer_I8P

PURE SUBROUTINE string_assign_real_R4P(lhs, rhs)
  !< Assignment operator from real input.
  !<
  !<```fortran
  !< use penf
  !< type(string) :: astring
  !< logical      :: test_passed(1)
  !< astring = 3.021e6_R4P
  !< test_passed(1) = astring%to_number(kind=1._R4P)==3.021e6_R4P
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(inout) :: lhs !< Left hand side.
  REAL(R4P), INTENT(IN) :: rhs !< Right hand side.

  lhs%raw = TRIM(str(rhs))
END SUBROUTINE string_assign_real_R4P

PURE SUBROUTINE string_assign_real_R8P(lhs, rhs)
  !< Assignment operator from real input.
  !<
  !<```fortran
  !< use penf
  !< type(string) :: astring
  !< logical      :: test_passed(1)
  !< astring = 3.021e6_R8P
  !< test_passed(1) = astring%to_number(kind=1._R8P)==3.021e6_R8P
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(inout) :: lhs !< Left hand side.
  REAL(R8P), INTENT(IN) :: rhs !< Right hand side.

  lhs%raw = TRIM(str(rhs))
END SUBROUTINE string_assign_real_R8P

PURE SUBROUTINE string_assign_real_R16P(lhs, rhs)
  !< Assignment operator from real input.
  !<
  !<```fortran
  !< use penf
  !< type(string) :: astring
  !< logical      :: test_passed(1)
  !< astring = 3.021e6_R8P
  !< test_passed(1) = astring%to_number(kind=1._R8P)==3.021e6_R8P
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(inout) :: lhs !< Left hand side.
  REAL(R16P), INTENT(IN) :: rhs !< Right hand side.

  lhs%raw = TRIM(str(rhs))
END SUBROUTINE string_assign_real_R16P

! contatenation operators
PURE FUNCTION string_concat_string(lhs, rhs) RESULT(concat)
  !< Concatenation with string.
  !<
  !<```fortran
  !< type(string) :: astring
  !< type(string) :: anotherstring
  !< logical      :: test_passed(1)
  !< astring = 'Hello '
  !< anotherstring = 'Bye bye'
  !< test_passed(1) = astring//anotherstring=='Hello Bye bye'
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: lhs    !< Left hand side.
  TYPE(string), INTENT(IN) :: rhs    !< Right hand side.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: concat !< Concatenated string.

  concat = ''
  IF (ALLOCATED(lhs%raw)) concat = lhs%raw
  IF (ALLOCATED(rhs%raw)) concat = concat//rhs%raw
END FUNCTION string_concat_string

PURE FUNCTION string_concat_character(lhs, rhs) RESULT(concat)
  !< Concatenation with character.
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< character(len=:), allocatable :: acharacter
  !< logical                       :: test_passed(1)
  !< astring = 'Hello '
  !< acharacter = 'World!'
  !< test_passed(1) = astring//acharacter=='Hello World!'
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: lhs    !< Left hand side.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: rhs    !< Right hand side.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: concat !< Concatenated string.

  IF (ALLOCATED(lhs%raw)) THEN
    concat = lhs%raw//rhs
  ELSE
    concat = rhs
  END IF
END FUNCTION string_concat_character

PURE FUNCTION character_concat_string(lhs, rhs) RESULT(concat)
  !< Concatenation with character (inverted).
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< character(len=:), allocatable :: acharacter
  !< logical                       :: test_passed(1)
  !< astring = 'Hello '
  !< acharacter = 'World!'
  !< test_passed(1) = acharacter//astring=='World!Hello '
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CHARACTER(kind=CK, len=*), INTENT(IN) :: lhs    !< Left hand side.
  CLASS(string), INTENT(IN) :: rhs    !< Right hand side.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: concat !< Concatenated string.

  IF (ALLOCATED(rhs%raw)) THEN
    concat = lhs//rhs%raw
  ELSE
    concat = lhs
  END IF
END FUNCTION character_concat_string

ELEMENTAL FUNCTION string_concat_string_string(lhs, rhs) RESULT(concat)
  !< Concatenation with string.
  !<
  !<```fortran
  !< type(string) :: astring
  !< type(string) :: anotherstring
  !< type(string) :: yetanotherstring
  !< logical      :: test_passed(1)
  !< astring = 'Hello '
  !< anotherstring = 'Bye bye'
  !< yetanotherstring = astring.cat.anotherstring
  !< test_passed(1) = yetanotherstring%chars()=='Hello Bye bye'
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: lhs       !< Left hand side.
  TYPE(string), INTENT(IN) :: rhs       !< Right hand side.
  TYPE(string) :: concat    !< Concatenated string.
  CHARACTER(kind=CK, len=:), ALLOCATABLE :: temporary !< Temporary concatenated string.

  temporary = ''
  IF (ALLOCATED(lhs%raw)) temporary = lhs%raw
  IF (ALLOCATED(rhs%raw)) temporary = temporary//rhs%raw
  IF (temporary /= '') concat%raw = temporary
END FUNCTION string_concat_string_string

ELEMENTAL FUNCTION string_concat_character_string(lhs, rhs) RESULT(concat)
  !< Concatenation with character.
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< type(string)                  :: yetanotherstring
  !< character(len=:), allocatable :: acharacter
  !< logical                       :: test_passed(1)
  !< astring = 'Hello '
  !< acharacter = 'World!'
  !< yetanotherstring = astring.cat.acharacter
  !< test_passed(1) = yetanotherstring%chars()=='Hello World!'
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: lhs    !< Left hand side.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: rhs    !< Right hand side.
  TYPE(string) :: concat !< Concatenated string.

  IF (ALLOCATED(lhs%raw)) THEN
    concat%raw = lhs%raw//rhs
  ELSE
    concat%raw = rhs
  END IF
END FUNCTION string_concat_character_string

ELEMENTAL FUNCTION character_concat_string_string(lhs, rhs) RESULT(concat)
  !< Concatenation with character (inverted).
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< type(string)                  :: yetanotherstring
  !< character(len=:), allocatable :: acharacter
  !< logical                       :: test_passed(1)
  !< astring = 'Hello '
  !< acharacter = 'World!'
  !< yetanotherstring = acharacter.cat.astring
  !< test_passed(1) = yetanotherstring%chars()=='World!Hello '
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CHARACTER(kind=CK, len=*), INTENT(IN) :: lhs    !< Left hand side.
  CLASS(string), INTENT(IN) :: rhs    !< Right hand side.
  TYPE(string) :: concat !< Concatenated string.

  IF (ALLOCATED(rhs%raw)) THEN
    concat%raw = lhs//rhs%raw
  ELSE
    concat%raw = lhs
  END IF
END FUNCTION character_concat_string_string

! logical operators
ELEMENTAL FUNCTION string_eq_string(lhs, rhs) RESULT(is_it)
  !< Equal to string logical operator.
  !<
  !<```fortran
  !< type(string) :: astring
  !< type(string) :: anotherstring
  !< logical      :: test_passed(2)
  !< astring = '  one '
  !< anotherstring = 'two'
  !< test_passed(1) = ((astring==anotherstring).eqv..false.)
  !< astring = 'the same '
  !< anotherstring = 'the same '
  !< test_passed(2) = ((astring==anotherstring).eqv..true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: lhs   !< Left hand side.
  TYPE(string), INTENT(IN) :: rhs   !< Right hand side.
  LOGICAL :: is_it !< Opreator test result.

  is_it = lhs%raw == rhs%raw
END FUNCTION string_eq_string

ELEMENTAL FUNCTION string_eq_character(lhs, rhs) RESULT(is_it)
  !< Equal to character logical operator.
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< character(len=:), allocatable :: acharacter
  !< logical                       :: test_passed(2)
  !< astring = '  one '
  !< acharacter = 'three'
  !< test_passed(1) = ((astring==acharacter).eqv..false.)
  !< astring = 'the same '
  !< acharacter = 'the same '
  !< test_passed(2) = ((astring==acharacter).eqv..true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: lhs   !< Left hand side.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: rhs   !< Right hand side.
  LOGICAL :: is_it !< Opreator test result.

  is_it = lhs%raw == rhs
END FUNCTION string_eq_character

ELEMENTAL FUNCTION character_eq_string(lhs, rhs) RESULT(is_it)
  !< Equal to character (inverted) logical operator.
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< character(len=:), allocatable :: acharacter
  !< logical                       :: test_passed(2)
  !< astring = '  one '
  !< acharacter = 'three'
  !< test_passed(1) = ((acharacter==astring).eqv..false.)
  !< astring = 'the same '
  !< acharacter = 'the same '
  !< test_passed(2) = ((acharacter==astring).eqv..true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CHARACTER(kind=CK, len=*), INTENT(IN) :: lhs   !< Left hand side.
  CLASS(string), INTENT(IN) :: rhs   !< Right hand side.
  LOGICAL :: is_it !< Opreator test result.

  is_it = rhs%raw == lhs
END FUNCTION character_eq_string

ELEMENTAL FUNCTION string_ne_string(lhs, rhs) RESULT(is_it)
  !< Not equal to string logical operator.
  !<
  !<```fortran
  !< type(string) :: astring
  !< type(string) :: anotherstring
  !< logical      :: test_passed(2)
  !< astring = '  one '
  !< anotherstring = 'two'
  !< test_passed(1) = ((astring/=anotherstring).eqv..true.)
  !< astring = 'the same '
  !< anotherstring = 'the same '
  !< test_passed(2) = ((astring/=anotherstring).eqv..false.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: lhs   !< Left hand side.
  TYPE(string), INTENT(IN) :: rhs   !< Right hand side.
  LOGICAL :: is_it !< Opreator test result.

  is_it = lhs%raw /= rhs%raw
END FUNCTION string_ne_string

ELEMENTAL FUNCTION string_ne_character(lhs, rhs) RESULT(is_it)
  !< Not equal to character logical operator.
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< character(len=:), allocatable :: acharacter
  !< logical                       :: test_passed(2)
  !< astring = '  one '
  !< acharacter = 'three'
  !< test_passed(1) = ((astring/=acharacter).eqv..true.)
  !< astring = 'the same '
  !< acharacter = 'the same '
  !< test_passed(2) = ((astring/=acharacter).eqv..false.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: lhs   !< Left hand side.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: rhs   !< Right hand side.
  LOGICAL :: is_it !< Opreator test result.

  is_it = lhs%raw /= rhs
END FUNCTION string_ne_character

ELEMENTAL FUNCTION character_ne_string(lhs, rhs) RESULT(is_it)
  !< Not equal to character (inverted) logical operator.
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< character(len=:), allocatable :: acharacter
  !< logical                       :: test_passed(2)
  !< astring = '  one '
  !< acharacter = 'three'
  !< test_passed(1) = ((acharacter/=astring).eqv..true.)
  !< astring = 'the same '
  !< acharacter = 'the same '
  !< test_passed(2) = ((acharacter/=astring).eqv..false.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CHARACTER(kind=CK, len=*), INTENT(IN) :: lhs   !< Left hand side.
  CLASS(string), INTENT(IN) :: rhs   !< Right hand side.
  LOGICAL :: is_it !< Opreator test result.

  is_it = rhs%raw /= lhs
END FUNCTION character_ne_string

ELEMENTAL FUNCTION string_lt_string(lhs, rhs) RESULT(is_it)
  !< Lower than to string logical operator.
  !<
  !<```fortran
  !< type(string) :: astring
  !< type(string) :: anotherstring
  !< logical      :: test_passed(2)
  !< astring = 'one'
  !< anotherstring = 'ONE'
  !< test_passed(1) = ((astring<anotherstring).eqv..false.)
  !< astring = 'ONE'
  !< anotherstring = 'one'
  !< test_passed(2) = ((astring<anotherstring).eqv..true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: lhs   !< Left hand side.
  TYPE(string), INTENT(IN) :: rhs   !< Right hand side.
  LOGICAL :: is_it !< Opreator test result.

  is_it = lhs%raw < rhs%raw
END FUNCTION string_lt_string

ELEMENTAL FUNCTION string_lt_character(lhs, rhs) RESULT(is_it)
  !< Lower than to character logical operator.
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< character(len=:), allocatable :: acharacter
  !< logical                       :: test_passed(2)
  !< astring = 'one'
  !< acharacter = 'ONE'
  !< test_passed(1) = ((astring<acharacter).eqv..false.)
  !< astring = 'ONE'
  !< acharacter = 'one'
  !< test_passed(2) = ((astring<acharacter).eqv..true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: lhs   !< Left hand side.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: rhs   !< Right hand side.
  LOGICAL :: is_it !< Opreator test result.

  is_it = lhs%raw < rhs
END FUNCTION string_lt_character

ELEMENTAL FUNCTION character_lt_string(lhs, rhs) RESULT(is_it)
  !< Lower than to character (inverted) logical operator.
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< character(len=:), allocatable :: acharacter
  !< logical                       :: test_passed(2)
  !< astring = 'one'
  !< acharacter = 'ONE'
  !< test_passed(1) = ((acharacter<astring).eqv..true.)
  !< astring = 'ONE'
  !< acharacter = 'one'
  !< test_passed(2) = ((acharacter<astring).eqv..false.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CHARACTER(kind=CK, len=*), INTENT(IN) :: lhs   !< Left hand side.
  CLASS(string), INTENT(IN) :: rhs   !< Right hand side.
  LOGICAL :: is_it !< Opreator test result.

  is_it = lhs < rhs%raw
END FUNCTION character_lt_string

ELEMENTAL FUNCTION string_le_string(lhs, rhs) RESULT(is_it)
  !< Lower equal than to string logical operator.
  !<
  !<```fortran
  !< type(string) :: astring
  !< type(string) :: anotherstring
  !< logical      :: test_passed(3)
  !< astring = 'one'
  !< anotherstring = 'ONE'
  !< test_passed(1) = ((astring<=anotherstring).eqv..false.)
  !< astring = 'ONE'
  !< anotherstring = 'one'
  !< test_passed(2) = ((astring<=anotherstring).eqv..true.)
  !< astring = 'ONE'
  !< anotherstring = 'ONE'
  !< test_passed(3) = ((astring<=anotherstring).eqv..true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: lhs   !< Left hand side.
  TYPE(string), INTENT(IN) :: rhs   !< Right hand side.
  LOGICAL :: is_it !< Opreator test result.

  is_it = lhs%raw <= rhs%raw
END FUNCTION string_le_string

ELEMENTAL FUNCTION string_le_character(lhs, rhs) RESULT(is_it)
  !< Lower equal than to character logical operator.
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< character(len=:), allocatable :: acharacter
  !< logical                       :: test_passed(3)
  !< astring = 'one'
  !< acharacter = 'ONE'
  !< test_passed(1) = ((astring<=acharacter).eqv..false.)
  !< astring = 'ONE'
  !< acharacter = 'one'
  !< test_passed(2) = ((astring<=acharacter).eqv..true.)
  !< astring = 'ONE'
  !< acharacter = 'ONE'
  !< test_passed(3) = ((astring<=acharacter).eqv..true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: lhs   !< Left hand side.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: rhs   !< Right hand side.
  LOGICAL :: is_it !< Opreator test result.

  is_it = lhs%raw <= rhs
END FUNCTION string_le_character

ELEMENTAL FUNCTION character_le_string(lhs, rhs) RESULT(is_it)
  !< Lower equal than to character (inverted) logical operator.
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< character(len=:), allocatable :: acharacter
  !< logical                       :: test_passed(3)
  !< astring = 'one'
  !< acharacter = 'ONE'
  !< test_passed(1) = ((acharacter<=astring).eqv..true.)
  !< astring = 'ONE'
  !< acharacter = 'one'
  !< test_passed(2) = ((acharacter<=astring).eqv..false.)
  !< astring = 'ONE'
  !< acharacter = 'ONE'
  !< test_passed(3) = ((acharacter<=astring).eqv..true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CHARACTER(kind=CK, len=*), INTENT(IN) :: lhs   !< Left hand side.
  CLASS(string), INTENT(IN) :: rhs   !< Right hand side.
  LOGICAL :: is_it !< Opreator test result.

  is_it = lhs <= rhs%raw
END FUNCTION character_le_string

ELEMENTAL FUNCTION string_ge_string(lhs, rhs) RESULT(is_it)
  !< Greater equal than to string logical operator.
  !<
  !<```fortran
  !< type(string) :: astring
  !< type(string) :: anotherstring
  !< logical      :: test_passed(3)
  !< astring = 'one'
  !< anotherstring = 'ONE'
  !< test_passed(1) = ((astring>=anotherstring).eqv..true.)
  !< astring = 'ONE'
  !< anotherstring = 'one'
  !< test_passed(2) = ((astring>=anotherstring).eqv..false.)
  !< astring = 'ONE'
  !< anotherstring = 'ONE'
  !< test_passed(3) = ((astring>=anotherstring).eqv..true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: lhs   !< Left hand side.
  TYPE(string), INTENT(IN) :: rhs   !< Right hand side.
  LOGICAL :: is_it !< Opreator test result.

  is_it = lhs%raw >= rhs%raw
END FUNCTION string_ge_string

ELEMENTAL FUNCTION string_ge_character(lhs, rhs) RESULT(is_it)
  !< Greater equal than to character logical operator.
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< character(len=:), allocatable :: acharacter
  !< logical                       :: test_passed(3)
  !< astring = 'one'
  !< acharacter = 'ONE'
  !< test_passed(1) = ((astring>=acharacter).eqv..true.)
  !< astring = 'ONE'
  !< acharacter = 'one'
  !< test_passed(2) = ((astring>=acharacter).eqv..false.)
  !< astring = 'ONE'
  !< acharacter = 'ONE'
  !< test_passed(3) = ((astring>=acharacter).eqv..true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: lhs   !< Left hand side.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: rhs   !< Right hand side.
  LOGICAL :: is_it !< Opreator test result.

  is_it = lhs%raw >= rhs
END FUNCTION string_ge_character

ELEMENTAL FUNCTION character_ge_string(lhs, rhs) RESULT(is_it)
  !< Greater equal than to character (inverted) logical operator.
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< character(len=:), allocatable :: acharacter
  !< logical                       :: test_passed(3)
  !< astring = 'one'
  !< acharacter = 'ONE'
  !< test_passed(1) = ((acharacter>=astring).eqv..false.)
  !< astring = 'ONE'
  !< acharacter = 'one'
  !< test_passed(2) = ((acharacter>=astring).eqv..true.)
  !< astring = 'ONE'
  !< acharacter = 'ONE'
  !< test_passed(3) = ((acharacter>=astring).eqv..true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CHARACTER(kind=CK, len=*), INTENT(IN) :: lhs   !< Left hand side.
  CLASS(string), INTENT(IN) :: rhs   !< Right hand side.
  LOGICAL :: is_it !< Opreator test result.

  is_it = lhs >= rhs%raw
END FUNCTION character_ge_string

ELEMENTAL FUNCTION string_gt_string(lhs, rhs) RESULT(is_it)
  !< Greater than to string logical operator.
  !<
  !<```fortran
  !< type(string) :: astring
  !< type(string) :: anotherstring
  !< logical      :: test_passed(2)
  !< astring = 'one'
  !< anotherstring = 'ONE'
  !< test_passed(1) = ((astring>anotherstring).eqv..true.)
  !< astring = 'ONE'
  !< anotherstring = 'one'
  !< test_passed(2) = ((astring>anotherstring).eqv..false.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: lhs   !< Left hand side.
  TYPE(string), INTENT(IN) :: rhs   !< Right hand side.
  LOGICAL :: is_it !< Opreator test result.

  is_it = lhs%raw > rhs%raw
END FUNCTION string_gt_string

ELEMENTAL FUNCTION string_gt_character(lhs, rhs) RESULT(is_it)
  !< Greater than to character logical operator.
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< character(len=:), allocatable :: acharacter
  !< logical                       :: test_passed(2)
  !< astring = 'one'
  !< acharacter = 'ONE'
  !< test_passed(1) = ((astring>acharacter).eqv..true.)
  !< astring = 'ONE'
  !< acharacter = 'one'
  !< test_passed(2) = ((astring>acharacter).eqv..false.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CLASS(string), INTENT(IN) :: lhs   !< Left hand side.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: rhs   !< Right hand side.
  LOGICAL :: is_it !< Opreator test result.

  is_it = lhs%raw > rhs
END FUNCTION string_gt_character

ELEMENTAL FUNCTION character_gt_string(lhs, rhs) RESULT(is_it)
  !< Greater than to character (inverted) logical operator.
  !<
  !<```fortran
  !< type(string)                  :: astring
  !< character(len=:), allocatable :: acharacter
  !< logical                       :: test_passed(2)
  !< astring = 'one'
  !< acharacter = 'ONE'
  !< test_passed(1) = ((acharacter>astring).eqv..false.)
  !< astring = 'ONE'
  !< acharacter = 'one'
  !< test_passed(2) = ((acharacter>astring).eqv..true.)
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CHARACTER(kind=CK, len=*), INTENT(IN) :: lhs   !< Left hand side.
  CLASS(string), INTENT(IN) :: rhs   !< Right hand side.
  LOGICAL :: is_it !< Opreator test result.

  is_it = lhs > rhs%raw
END FUNCTION character_gt_string

! IO
SUBROUTINE read_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
  !< Formatted input.
  !<
  !< @bug Change temporary acks: find a more precise length of the input string and avoid the trimming!
  !<
  !< @bug Read listdirected with and without delimiters does not work.
  CLASS(string), INTENT(inout) :: dtv         !< The string.
  INTEGER, INTENT(IN) :: unit        !< Logical unit.
  CHARACTER(len=*), INTENT(IN) :: iotype      !< Edit descriptor.
  INTEGER, INTENT(IN) :: v_list(:)   !< Edit descriptor list.
  INTEGER, INTENT(out) :: iostat      !< IO status code.
  CHARACTER(len=*), INTENT(inout) :: iomsg       !< IO status message.
  CHARACTER(len=LEN(iomsg)) :: local_iomsg !< Local variant of iomsg, so it doesn't get inappropriately redefined.
  CHARACTER(kind=CK, len=1) :: delim       !< String delimiter, if any.
  CHARACTER(kind=CK, len=100) :: temporary   !< Temporary storage string.

  IF (iotype == 'LISTDIRECTED') THEN
         CALL get_next_non_blank_character_any_record(unit=unit, ch=delim, iostat=iostat, iomsg=iomsg)
    IF (iostat /= 0) RETURN
    IF (delim == '"' .OR. delim == "'") THEN
            CALL dtv%read_delimited(unit=unit, delim=delim, iostat=iostat, iomsg=local_iomsg)
    ELSE
      ! step back before the non-blank
      READ (unit, "(TL1)", iostat=iostat, iomsg=iomsg)
      IF (iostat /= 0) RETURN
            CALL dtv%read_undelimited_listdirected(unit=unit, iostat=iostat, iomsg=local_iomsg)
    END IF
    IF (is_iostat_eor(iostat)) THEN
      ! suppress IOSTAT_EOR
      iostat = 0
    ELSEIF (iostat /= 0) THEN
      iomsg = local_iomsg
    END IF
    RETURN
  ELSE
    READ (unit, "(A)", iostat=iostat, iomsg=iomsg) temporary
    dtv%raw = TRIM(temporary)
  END IF
END SUBROUTINE read_formatted

SUBROUTINE read_delimited(dtv, unit, delim, iostat, iomsg)
  !< Read a delimited string from a unit connected for formatted input.
  !<
  !< If the closing delimiter is followed by end of record, then we return end of record.
  !<
  !< @note This does not need a doctest, it being tested by [[string::read_formatted]].
  CLASS(string), INTENT(out) :: dtv       !< The string.
  INTEGER, INTENT(IN) :: unit      !< Logical unit.
  CHARACTER(kind=CK, len=1), INTENT(IN) :: delim     !< String delimiter.
  INTEGER, INTENT(out) :: iostat    !< IO status code.
  CHARACTER(kind=CK, len=*), INTENT(inout) :: iomsg     !< IO status message.
  CHARACTER(kind=CK, len=1) :: ch        !< A character read.
  LOGICAL :: was_delim !< Indicates that the last character read was a delimiter.

  was_delim = .FALSE.
  dtv%raw = ''
  DO
    READ (unit, "(A)", iostat=iostat, iomsg=iomsg) ch
    IF (is_iostat_eor(iostat)) THEN
      IF (was_delim) THEN
        ! end of delimited string followed by end of record is end of the string. Pass back the
        ! end of record condition to the caller
        RETURN
      ELSE
        ! end of record without terminating delimiter - move along
        CYCLE
      END IF
    ELSEIF (iostat /= 0) THEN
      RETURN
    END IF
    IF (ch == delim) THEN
      IF (was_delim) THEN
        ! doubled delimiter is one delimiter in the value
        dtv%raw = dtv%raw//ch
        was_delim = .FALSE.
      ELSE
        ! need to test next character to see what is happening
        was_delim = .TRUE.
      END IF
    ELSEIF (was_delim) THEN
      ! the previous character was actually the delimiter for the end of the string. Put back this character
      READ (unit, "(TL1)", iostat=iostat, iomsg=iomsg)
      RETURN
    ELSE
      dtv%raw = dtv%raw//ch
    END IF
  END DO
END SUBROUTINE read_delimited

SUBROUTINE read_undelimited_listdirected(dtv, unit, iostat, iomsg)
  !< Read an undelimited (no leading apostrophe or double quote) character value according to the rules for list directed input.
  !<
  !< A blank, comma/semicolon (depending on the decimal mode), slash or end of record terminates the string.
  !<
  !< If input is terminated by end of record, then this procedure returns an end-of-record condition.
  CLASS(string), INTENT(inout) :: dtv           !< The string.
  INTEGER, INTENT(IN) :: unit          !< Logical unit.
  INTEGER, INTENT(out) :: iostat        !< IO status code.
  CHARACTER(len=*), INTENT(inout) :: iomsg         !< IO status message.
  LOGICAL :: decimal_point !<True if DECIMAL=POINT in effect.

      CALL get_decimal_mode(unit=unit, decimal_point=decimal_point, iostat=iostat, iomsg=iomsg)
  IF (iostat /= 0) RETURN
      CALL dtv%read_undelimited(unit=unit, terminators=' '//'/'//MERGE(CK_',', CK_';', decimal_point), iostat=iostat, iomsg=iomsg)
END SUBROUTINE read_undelimited_listdirected

SUBROUTINE read_undelimited(dtv, unit, terminators, iostat, iomsg)
  !< Read an undelimited string up until end of record or a character from a set of terminators is encountered.
  !<
  !< If a terminator is encountered, the file position will be at that terminating character. If end of record is encountered, the
  !< file remains at end of record.
  CLASS(string), INTENT(inout) :: dtv         !< The string.
  INTEGER, INTENT(IN) :: unit        !< Logical unit.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: terminators !< Characters that are considered to terminate the string.
  !< Blanks in this string are meaningful.
  INTEGER, INTENT(out) :: iostat      !< IO status code.
  CHARACTER(len=*), INTENT(inout) :: iomsg       !< IO status message.
  CHARACTER(kind=CK, len=1) :: ch          !< A character read.

  dtv%raw = ''
  DO
    READ (unit, "(A)", iostat=iostat, iomsg=iomsg) ch
    IF (is_iostat_eor(iostat)) THEN
      ! end of record just means end of string. We pass on the condition
      RETURN
    ELSEIF (iostat /= 0) THEN
      ! something odd happened
      RETURN
    END IF
    IF (SCAN(ch, terminators) /= 0) THEN
      ! change the file position so that the next read sees the terminator
      READ (unit, "(TL1)", iostat=iostat, iomsg=iomsg)
      IF (iostat /= 0) RETURN
      iostat = 0
      RETURN
    END IF
    ! we got a character - append it
    dtv%raw = dtv%raw//ch
  END DO
END SUBROUTINE read_undelimited

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

SUBROUTINE write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
  !< Formatted output.
  CLASS(string), INTENT(IN) :: dtv       !< The string.
  INTEGER, INTENT(IN) :: unit      !< Logical unit.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: iotype    !< Edit descriptor.
  INTEGER, INTENT(IN) :: v_list(:) !< Edit descriptor list.
  INTEGER, INTENT(out) :: iostat    !< IO status code.
  CHARACTER(kind=CK, len=*), INTENT(inout) :: iomsg     !< IO status message.

  IF (ALLOCATED(dtv%raw)) THEN
    WRITE (unit, "(A)", iostat=iostat, iomsg=iomsg) dtv%raw
  ELSE
    WRITE (unit, "(A)", iostat=iostat, iomsg=iomsg) ''
  END IF
END SUBROUTINE write_formatted

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE read_unformatted(dtv, unit, iostat, iomsg)
  !< Unformatted input.
  !<
  !< @bug Change temporary acks: find a more precise length of the input string and avoid the trimming!
  CLASS(string), INTENT(inout) :: dtv       !< The string.
  INTEGER, INTENT(IN) :: unit      !< Logical unit.
  INTEGER, INTENT(out) :: iostat    !< IO status code.
  CHARACTER(kind=CK, len=*), INTENT(inout) :: iomsg     !< IO status message.
  CHARACTER(kind=CK, len=100) :: temporary !< Temporary storage string.

  READ (unit, iostat=iostat, iomsg=iomsg) temporary
  dtv%raw = TRIM(temporary)
END SUBROUTINE read_unformatted

SUBROUTINE write_unformatted(dtv, unit, iostat, iomsg)
  !< Unformatted output.
  CLASS(string), INTENT(IN) :: dtv    !< The string.
  INTEGER, INTENT(IN) :: unit   !< Logical unit.
  INTEGER, INTENT(out) :: iostat !< IO status code.
  CHARACTER(kind=CK, len=*), INTENT(inout) :: iomsg  !< IO status message.

  IF (ALLOCATED(dtv%raw)) THEN
    WRITE (unit, iostat=iostat, iomsg=iomsg) dtv%raw
  ELSE
    WRITE (unit, iostat=iostat, iomsg=iomsg) ''
  END IF
END SUBROUTINE write_unformatted

! miscellanea
ELEMENTAL FUNCTION replace_one_occurrence(self, old, NEW) RESULT(replaced)
  !< Return a string with the first occurrence of substring old replaced by new.
  !<
  !< @note The doctest is not necessary, this being tested by [[string:replace]].
  CLASS(string), INTENT(IN) :: self      !< The string.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: old       !< Old substring.
  CHARACTER(kind=CK, len=*), INTENT(IN) :: NEW       !< New substring.
  TYPE(string) :: replaced  !< The string with old replaced by new.
  INTEGER :: pos       !< Position from which replace old.

  IF (ALLOCATED(self%raw)) THEN
    replaced = self
    pos = INDEX(string=self%raw, substring=old)
    IF (pos > 0) THEN
      IF (pos == 1) THEN
        replaced%raw = NEW//self%raw(LEN(old) + 1:)
      ELSE
        replaced%raw = self%raw(1:pos - 1)//NEW//self%raw(pos + LEN(old):)
      END IF
    END IF
  END IF
END FUNCTION replace_one_occurrence

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 21 July 2021
! summary: Get the DELIM changeable connection mode for the given unit.
!
!# Introduction
!
! If the unit is connected to an internal file, then the default value of
! NONE is always returned.

! non type-bound-procedures
SUBROUTINE get_delimiter_mode(unit, delim, iostat, iomsg)
  USE, INTRINSIC :: iso_fortran_env, ONLY: iostat_inquire_internal_unit
  INTEGER, INTENT(IN) :: unit
  !! The unit for the connection.
  CHARACTER(len=1, kind=CK), INTENT(out) :: delim
  !! Represents the value of the DELIM mode.
  INTEGER, INTENT(out) :: iostat
  !! IOSTAT error code, non-zero on error.
  CHARACTER(*), INTENT(inout) :: iomsg
  !! IOMSG explanatory message - only defined if iostat is non-zero.
  CHARACTER(10) :: delim_buffer
  !! Buffer for INQUIRE about DELIM, sized for APOSTROHPE.
  CHARACTER(LEN(iomsg)) :: local_iomsg
  !! Local variant of iomsg, so it doesn't get inappropriately redefined.
  !!
  !! get the string representation of the changeable mode
  !!
  INQUIRE (unit, delim=delim_buffer, iostat=iostat, iomsg=local_iomsg)
  !!
  IF (iostat == iostat_inquire_internal_unit) THEN
    ! no way of determining the DELIM mode for an internal file
    iostat = 0
    delim = ''
    RETURN
  ELSEIF (iostat /= 0) THEN
    iomsg = local_iomsg
    RETURN
  END IF
  ! interpret the DELIM string
  IF (delim_buffer == 'QUOTE') THEN
    delim = '"'
  ELSEIF (delim_buffer == 'APOSTROPHE') THEN
    delim = ''''
  ELSE
    delim = '"'
  END IF
END SUBROUTINE get_delimiter_mode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 21 July 2022
! summary: Get the next non-blank character in the current record.

SUBROUTINE get_next_non_blank_character_this_record(unit, ch, iostat, iomsg)
  INTEGER, INTENT(IN) :: unit
  !! Logical unit.
  CHARACTER(kind=CK, len=1), INTENT(out) :: ch
  !! The non-blank character read. Not valid if IOSTAT is non-zero.
  INTEGER, INTENT(out) :: iostat
  !! IO status code.
  CHARACTER(kind=CK, len=*), INTENT(inout) :: iomsg
  !! IO status message.
  !!
  DO
    ! we spcify non-advancing, just in case we want this callable outside the
    ! context of a child input statement
    ! the PAD specifier simply saves the need for the READ statement to
    ! define ch if EOR is hit
    ! read(unit, "(A)", iostat=iostat, iomsg=iomsg, advance='NO') ch
    ! ...but that causes ifort to blow up at runtime
    READ (unit, "(A)", iostat=iostat, iomsg=iomsg, pad='NO') ch
    IF (iostat .NE. 0) RETURN
    IF (ch .NE. '') EXIT
  END DO
END SUBROUTINE get_next_non_blank_character_this_record

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 21 July 2022
! summary: Get the next non-blank character, advancing records if necessary.

SUBROUTINE get_next_non_blank_character_any_record(unit, ch, iostat, iomsg)
  INTEGER, INTENT(IN) :: unit
  !! Logical unit.
  CHARACTER(kind=CK, len=1), INTENT(out) :: ch
  !! The non-blank character read. Not valid if IOSTAT is non-zero.
  INTEGER, INTENT(out) :: iostat
  !! IO status code.
  CHARACTER(kind=CK, len=*), INTENT(inout) :: iomsg
  !! IO status message.
  CHARACTER(LEN(iomsg)) :: local_iomsg
  !! Local variant of iomsg, so it doesn't get inappropriately redefined.
  !!
  DO
    CALL get_next_non_blank_character_this_record(unit=unit, ch=ch, &
      & iostat=iostat, iomsg=local_iomsg)
    IF (IS_IOSTAT_EOR(iostat)) THEN
      ! try again on the next record
      READ (unit, "(/)", iostat=iostat, iomsg=iomsg)
      IF (iostat .NE. 0) RETURN
    ELSEIF (iostat .NE. 0) THEN
      ! some sort of problem
      iomsg = local_iomsg
      RETURN
    ELSE
      ! got it
      EXIT
    END IF
  END DO
END SUBROUTINE get_next_non_blank_character_any_record

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date: 21 July 2022
! summary: Get the DECIMAL changeable connection mode for the given unit.
!
!# Introduction
!
! If the unit is connected to an internal file,
! then the default value of DECIMAL is always returned.
! This may not be the actual value in force at the time of the call
! to this procedure.

SUBROUTINE get_decimal_mode(unit, decimal_point, iostat, iomsg)
  USE, INTRINSIC :: iso_fortran_env, ONLY: iostat_inquire_internal_unit
  INTEGER, INTENT(IN) :: unit
  !! Logical unit.
  LOGICAL, INTENT(out) :: decimal_point
  !! True if the decimal mode is POINT, false otherwise.
  INTEGER, INTENT(out) :: iostat
  !! IO status code.
  CHARACTER(kind=CK, len=*), INTENT(inout) :: iomsg
  !! IO status message.
  CHARACTER(5) :: decimal_buffer
  !! Buffer for INQUIRE about DECIMAL, sized for POINT or COMMA.
  CHARACTER(LEN(iomsg)) :: local_iomsg
  !! Local iomsg, so it doesn't get inappropriately redefined.
  !!
  !!
  INQUIRE (unit, decimal=decimal_buffer, iostat=iostat, iomsg=local_iomsg)
  !!
  IF (iostat .EQ. iostat_inquire_internal_unit) THEN
    ! no way of determining the decimal mode for an internal file
    iostat = 0
    decimal_point = .TRUE.
    RETURN
  ELSE IF (iostat .NE. 0) THEN
    iomsg = local_iomsg
    RETURN
  END IF
  decimal_point = decimal_buffer == 'POINT'
END SUBROUTINE get_decimal_mode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 July 2022
! summary:         Display the contents of a given string
!
!# Introduction
!
!```fortran
! type(string) :: astring
! astring = '   Hello World!'
! call display( astring, "hello-world" )
!```

SUBROUTINE display_str(self, msg, unitno, advance)
  CLASS(String), INTENT(IN) :: self
  CHARACTER(LEN=*), INTENT(IN) :: msg
  INTEGER(I4P), OPTIONAL, INTENT(IN) :: unitno
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
  !!
  TYPE(String) :: adv0
  INTEGER(i4p) :: i
  !!
  IF (PRESENT(advance)) THEN
    adv0 = TRIM(advance)
  ELSE
    adv0 = "YES"
  END IF
  !!
  IF (PRESENT(unitno)) THEN
    i = unitno
  ELSE
    i = stdout
  END IF
  !!
  WRITE (i, "(A)", ADVANCE=adv0%chars()) TRIM(msg)//self%chars()
END SUBROUTINE display_str

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION constructor1(c) RESULT(self)
  !< Constructor of string from intrinsic fortran data type
  !<
  !<```fortran
  !< type(string) :: astring
  !< astring = String('hello')
  !< astring = String( 1 )
  !< astring = String( 1.0 )
  !<```
  !=> T <<<
  TYPE(string) :: self
  CLASS(*), INTENT(IN) :: c
  SELECT TYPE (c)
  TYPE is (CHARACTER(*))
    self = c
  TYPE is (REAL(r4p))
    self = c
  TYPE is (REAL(r8p))
    self = c
#if defined _R16P
  TYPE is (REAL(r16p))
    self = c
#endif
  TYPE is (INTEGER(i1p))
    self = str(c, .TRUE.)
  TYPE is (INTEGER(i2p))
    self = str(c, .TRUE.)
  TYPE is (INTEGER(i4p))
    self = str(c, .TRUE.)
  TYPE is (INTEGER(i8p))
    self = str(c, .TRUE.)
  TYPE is (string)
    self = c
  END SELECT
END FUNCTION

!----------------------------------------------------------------------------
!                                                                 NmatchStr
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 May 2021
! summary: Returns the total number of times the substring pattern is found
!
!### Introduction
! Returns the total number of times the substring @c pattern is found in
! string.
!
!@note
! Does not handle trailing spaces that can be eliminated by TRIM() so
! strings should be trimmed when passing into function.
!@endnote

PURE FUNCTION nmatchstr_1(obj, pattern) RESULT(ans)
  CLASS(String), INTENT(IN) :: obj
  !! the string to search
  CHARACTER(LEN=*), INTENT(IN) :: pattern
  !! the pattern to be searched
  INTEGER(I4P) :: ans
  !! number of mathces
  INTEGER(I4P) :: ii, n

  ans = 0; n = obj%LEN()
  DO ii = 1, n
    IF ((ii + LEN(pattern) - 1) .GT. n) EXIT
    IF (obj%raw(ii:ii + LEN(pattern) - 1) .EQ. pattern) ans = ans + 1
  END DO
END FUNCTION nmatchstr_1

!----------------------------------------------------------------------------
!                                                                 NmatchStr
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 May 2021
! summary: Returns the total number of times the substring pattern is found
!
!### Introduction
! Returns the total number of times the substring @c pattern is found in
! string.
!
!@note
! Does not handle trailing spaces that can be eliminated by TRIM() so
! strings should be trimmed when passing into function.
!@endnote

PURE FUNCTION nmatchstr_2(obj, pattern) RESULT(ans)
  CLASS(String), INTENT(IN) :: obj
  !! the string to search
  TYPE(String), INTENT(IN) :: pattern
  !! the pattern to be searched
  INTEGER(I4P) :: ans
  !! number of mathces
  INTEGER(I4P) :: ii, n, m

  ans = 0; n = obj%LEN(); m = pattern%LEN()
  DO ii = 1, n
    IF ((ii + m - 1) .GT. n) EXIT
    IF (obj%raw(ii:ii + m - 1) .EQ. pattern%raw(1:m)) ans = ans + 1
  END DO
END FUNCTION nmatchstr_2

!----------------------------------------------------------------------------
!                                                                 findStr
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 May 2021
! summary: Returns the indices in a string where substring pattern are found
!
!### Introduction
! Function returns the indices in a string where substring pattern is found.

PURE SUBROUTINE strfind_1(obj, pattern, indices)
  CLASS(String), INTENT(IN) :: obj
  CHARACTER(LEN=*), INTENT(IN) :: pattern
  INTEGER(I4P), ALLOCATABLE, INTENT(OUT) :: indices(:)
  ! Internal variables
  INTEGER(I4P) :: i, n, m, count

  n = obj%LEN(); m = LEN(pattern); count = 0
  IF (ALLOCATED(indices)) DEALLOCATE (indices)
  ALLOCATE (indices(obj%nmatchstr(pattern)))
  DO i = 1, n
    IF ((i + m - 1) .GT. n) EXIT
    IF (obj%raw(i:i + m - 1) .EQ. pattern(1:m)) THEN
      count = count + 1
      indices(count) = i
    END IF
  END DO
END SUBROUTINE strfind_1

!----------------------------------------------------------------------------
!                                                                    strfind
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 May 2021
! summary: Returns the indices in a string where substring pattern are found
!
!### Introduction
! Function returns the indices in a string where substring pattern is found.

PURE SUBROUTINE strfind_2(obj, pattern, indices)
  CLASS(String), INTENT(IN) :: obj
  CLASS(String), INTENT(IN) :: pattern
  INTEGER(I4P), ALLOCATABLE, INTENT(OUT) :: indices(:)
  ! Internal variables
  CALL strfind_1(obj, TRIM(pattern%chars()), indices)
END SUBROUTINE strfind_2

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2022
! summary: Reallocate string

PURE SUBROUTINE String_Reallocate1(obj, row)
  TYPE(String), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  INTEGER(I4P), INTENT(IN) :: row
  !!
  IF (ALLOCATED(obj)) THEN
    IF (SIZE(obj) .NE. row) THEN
      DEALLOCATE (obj)
      ALLOCATE (obj(row))
    END IF
  ELSE
    ALLOCATE (obj(row))
  END IF
  !!
END SUBROUTINE String_Reallocate1

END MODULE String_Class

!! Changed stringifor_string_t to StringiFor_Class
