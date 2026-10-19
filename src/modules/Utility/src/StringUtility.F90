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
! date: 2021-09-05
! summary: Routines for character strings and file paths.
!
! Description:
!   This module contains routines that work on `CHARACTER(*)` strings.
!
!   - Case conversion: `UpperCase`, `ToUpperCase`, `LowerCase`,
!     `ToLowerCase`. Only the ASCII letters `a-z` and `A-Z` change.
!   - Tests: `IsWhiteChar`, `IsBlank`, `IsPresent`.
!   - Search and replace: `NumMatchStr`, `StrFind`, `FindReplace`.
!   - Fields: `NumStrings`, `GetField` (blank separated words).
!   - File names: `GetPath`, `GetFileName`, `GetFileNameExt`,
!     `GetFileParts`, `GetExtension`, `SlashRep`.
!   - Paths: `PathJoin`, `PathBase`, `PathDir`.
!   - `DEALLOCATE` for an allocatable vector of `String`.
!
!   The file separator is `CHAR_SLASH` from `GlobalData`. It is `/`
!   on macOS and Linux, and a backslash when `WIN32` is defined.
!
!   Unless stated otherwise, the routines work on the full length of
!   the actual argument, including its trailing blanks. Pass
!   `TRIM(chars)` when the trailing blanks should be ignored.
!
!   Several routines are adopted from the `IO_Strings` module of
!   Futility (https://github.com/CASL/Futility).

MODULE StringUtility
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE String_Class, ONLY: String

IMPLICIT NONE

PRIVATE

PUBLIC :: FindReplace
PUBLIC :: GetFileParts
PUBLIC :: GetPath
PUBLIC :: GetFileName
PUBLIC :: GetFileNameExt
PUBLIC :: GetExtension
PUBLIC :: GetField
PUBLIC :: LowerCase
PUBLIC :: ToLowerCase
PUBLIC :: IsWhiteChar
PUBLIC :: IsBlank
PUBLIC :: NumStrings
PUBLIC :: NumMatchStr
PUBLIC :: IsPresent
PUBLIC :: StrFind
PUBLIC :: SlashRep
PUBLIC :: ToUpperCase
PUBLIC :: UpperCase

PUBLIC :: PathJoin
PUBLIC :: PathBase
PUBLIC :: PathDir
PUBLIC :: DEALLOCATE

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-24
! summary: Deallocate a vector of String.
!
! Description:
!   Sets every entry of `obj` to an empty string and then deallocates
!   `obj`. If `obj` is not allocated, nothing is done, so it is safe to
!   call this routine more than once.
!
! Arguments:
!   obj : TYPE(String), ALLOCATABLE, rank(1) - vector to deallocate
!
! Returns:
!   obj is not allocated on return.
!
! Formula:
!   $$\mathrm{ALLOCATED}(\mathrm{obj}) = \mathrm{.FALSE.}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE String_Class, ONLY: String
! USE StringUtility, ONLY: DEALLOCATE
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(String), ALLOCATABLE :: obj(:)
!
! ALLOCATE (obj(3))
! obj(1) = "a"
! obj(2) = "bb"
! obj(3) = "ccc"
!
! CALL DEALLOCATE(obj)
! CALL OK(.NOT. ALLOCATED(obj), "obj is deallocated")
!
! ! calling it again on an unallocated vector is safe
! CALL DEALLOCATE(obj)
! CALL OK(.NOT. ALLOCATED(obj), "unallocated obj is left alone")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE StringVectorDeallocate(obj)
    TYPE(String), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE StringVectorDeallocate
END INTERFACE

INTERFACE DEALLOCATE
  MODULE PROCEDURE StringVectorDeallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                                 PathBase
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-17
! summary: Returns the last element of a path.
!
! Description:
!   Returns the last element of `path`, like `path.Base` in Go.
!
!   - Trailing blanks of `path` are ignored.
!   - Trailing slashes are removed before the last element is found.
!   - An empty (blank) path gives `"."`.
!   - A path made only of slashes gives `"/"`.
!   - A path without a slash is returned unchanged.
!
!   The separator is `/`. On Windows `CHAR_SLASH` (`\`) is a
!   separator as well.
!
! Arguments:
!   path : CHARACTER(*) - the path
!
! Returns:
!   ans : CHARACTER(:), ALLOCATABLE - the last element of `path`
!
! Formula:
!   For `path = "d1/d2/name"` (and `"d1/d2/name/"`):
!   $$\mathrm{ans} = \mathrm{name}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE StringUtility, ONLY: PathBase
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=100) :: astr
!
! astr = " "
! CALL OK(PathBase(astr) .EQ. ".", "empty path gives .")
!
! CALL OK(PathBase("/") .EQ. "/", "root")
! CALL OK(PathBase("////") .EQ. "/", "only slashes")
! CALL OK(PathBase("/a/b/c") .EQ. "c", "/a/b/c")
! CALL OK(PathBase("/a/b/c/") .EQ. "c", "trailing slash is removed")
! CALL OK(PathBase("a/b") .EQ. "b", "relative path")
! CALL OK(PathBase("abc") .EQ. "abc", "no slash")
! CALL OK(PathBase("x") .EQ. "x", "single character")
!
! ! trailing blanks of the argument are ignored
! astr = "/home/user/file.txt"
! CALL OK(PathBase(astr) .EQ. "file.txt", "padded argument")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION PathBase(path) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: path
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION PathBase
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    PathJoin
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-17
! summary: Join two paths with the file separator.
!
! Description:
!   Returns `TRIM(path1)` and `TRIM(path2)` joined by exactly one
!   separator `CHAR_SLASH`.
!
!   - Trailing blanks of both arguments are removed.
!   - Separators at the end of `path1` and at the start of `path2` are
!     merged into one, so `PathJoin("a/", "/b")` gives `"a/b"`. A root
!     `"/"` is kept, so `PathJoin("/", "b")` gives `"/b"`.
!   - An empty argument is skipped, so `PathJoin("", "b")` gives `"b"`.
!
! Arguments:
!   path1 : CHARACTER(*) - the first path
!   path2 : CHARACTER(*) - the second path
!
! Returns:
!   ans : CHARACTER(:), ALLOCATABLE - the joined path
!
! Formula:
!   For `path1 = "d1"` and `path2 = "d2"`:
!   $$\mathrm{ans} = \mathrm{d1/d2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE StringUtility, ONLY: PathJoin
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(:), ALLOCATABLE :: ans
!
! ans = PathJoin("a", "b")
! CALL OK(ans .EQ. "a/b", "a + b")
! CALL OK(LEN(ans) .EQ. 3_I4B, "length of a/b")
!
! CALL OK(PathJoin("/usr/local", "bin") .EQ. "/usr/local/bin", &
!         "absolute path")
!
! ! trailing blanks of both arguments are removed
! CALL OK(PathJoin("a  ", "b  ") .EQ. "a/b", "trailing blanks")
!
! ! exactly one separator is put between the paths
! CALL OK(PathJoin("a/", "b") .EQ. "a/b", "a/ + b")
! CALL OK(PathJoin("a//", "/b") .EQ. "a/b", "a// + /b")
! CALL OK(PathJoin("/", "b") .EQ. "/b", "root + b")
!
! ! an empty path is skipped
! CALL OK(PathJoin("", "b") .EQ. "b", "empty first path")
! CALL OK(PathJoin("a", "") .EQ. "a", "empty second path")
!
! END PROGRAM main
!```

INTERFACE PathJoin
  MODULE PURE FUNCTION PathJoin1(path1, path2) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: path1
    CHARACTER(*), INTENT(IN) :: path2
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION PathJoin1
END INTERFACE PathJoin

!----------------------------------------------------------------------------
!                                                                    PathJoin
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-17
! summary: Join a vector of String with the file separator.
!
! Description:
!   Joins the parts in `paths` from left to right, in the same way as
!   `PathJoin(path1, path2)`.
!
!   - `["a", "b"]` gives `"a/b"`. For an absolute path, start the first
!     part with the separator, for example `["/a", "b"]`.
!   - Separators between two parts are merged into one, and empty
!     parts are skipped. The parts are not trimmed.
!   - A vector of size 0 gives an empty string.
!
! Arguments:
!   paths : TYPE(String), rank(1) - the parts of the path
!
! Returns:
!   ans : CHARACTER(:), ALLOCATABLE - the joined path
!
! Formula:
!   $$\mathrm{ans} = p_1\,\mathrm{/}\,p_2 \cdots
!     \mathrm{/}\,p_n, \quad n = \mathrm{SIZE}(\mathrm{paths})$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE String_Class, ONLY: String
! USE StringUtility, ONLY: PathJoin
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(String) :: paths(3), one(1), empty(0)
! CHARACTER(:), ALLOCATABLE :: ans
!
! paths(1) = "usr"
! paths(2) = "local"
! paths(3) = "bin"
!
! ! the parts are joined like PathJoin(path1, path2)
! ans = PathJoin(paths)
! CALL OK(ans .EQ. "usr/local/bin", "three parts")
!
! ! an absolute path starts with a separator in the first part
! paths(1) = "/usr/"
! CALL OK(PathJoin(paths) .EQ. "/usr/local/bin", "absolute path")
!
! one(1) = "a"
! CALL OK(PathJoin(one) .EQ. "a", "one part")
!
! ans = PathJoin(empty)
! CALL OK(LEN(ans) .EQ. 0_I4B, "no parts gives an empty string")
!
! END PROGRAM main
!```

INTERFACE PathJoin
  MODULE PURE FUNCTION PathJoin2(paths) RESULT(ans)
    TYPE(String), INTENT(IN) :: paths(:)
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION PathJoin2
END INTERFACE PathJoin

!----------------------------------------------------------------------------
!                                                                   PathDir
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-17
! summary: Returns all but the last element of a path.
!
! Description:
!   Returns the directory part of `path`, like `path.Dir` in Go.
!
!   - Trailing blanks of `path` are ignored.
!   - Trailing slashes are removed first, then the last element is
!     dropped, then the trailing slashes of the rest are removed. This
!     is the same as `dirname` in POSIX, so `"a/"` gives `"."`.
!   - An empty (blank) path, or a path without a slash, gives `"."`.
!   - A path made only of slashes, or of the form `"/name"`, gives
!     `"/"`.
!   - In any other case, the result does not end with a slash.
!
!   The separator is `/`. On Windows `CHAR_SLASH` (`\`) is a
!   separator as well.
!
! Arguments:
!   path : CHARACTER(*) - the path
!
! Returns:
!   ans : CHARACTER(:), ALLOCATABLE - the directory part of `path`
!
! Formula:
!   For `path = "d1/d2/name"` (and `"d1/d2/name/"`):
!   $$\mathrm{ans} = \mathrm{d1/d2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE StringUtility, ONLY: PathDir
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=100) :: astr
!
! astr = " "
! CALL OK(PathDir(astr) .EQ. ".", "empty path gives .")
!
! CALL OK(PathDir("/") .EQ. "/", "root")
! CALL OK(PathDir("////") .EQ. "/", "only slashes")
! CALL OK(PathDir("/a/b/c") .EQ. "/a/b", "/a/b/c")
! CALL OK(PathDir("/a/b/c/") .EQ. "/a/b", "trailing slash")
! CALL OK(PathDir("/abc") .EQ. "/", "file in root")
! CALL OK(PathDir("a/b") .EQ. "a", "relative path")
! CALL OK(PathDir("a//b") .EQ. "a", "repeated slashes")
! CALL OK(PathDir("abc") .EQ. ".", "no slash gives .")
! CALL OK(PathDir("x") .EQ. ".", "single character gives .")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION PathDir(path) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: path
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION PathDir
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   GetPath
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! summary: Returns the directory part of a file name.
!
! Description:
!   Returns the part of `chars` up to and including the last
!   separator. Backslashes are first replaced by `CHAR_SLASH` (see
!   `SlashRep`).
!
!   - If `chars` has no separator, `path` is blank.
!   - Unlike `PathDir`, the result keeps the trailing slash.
!   - `path` is blank padded. If it is shorter than the result, the
!     result is truncated.
!
! Arguments:
!   chars : CHARACTER(*) - the file name
!   path : CHARACTER(*), INTENT(OUT) - the directory part
!
! Returns:
!   path : directory part of `chars`, ending with the separator
!
! Formula:
!   $$\mathrm{path} = \mathrm{chars}(1:k), \quad
!     k = \text{position of the last separator}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE StringUtility, ONLY: GetPath
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=100) :: path
!
! CALL GetPath("/dir1/dir2/HelloWorld.txt", path)
! CALL OK(TRIM(path) .EQ. "/dir1/dir2/", "absolute file name")
!
! CALL GetPath("./HelloWorld.txt", path)
! CALL OK(TRIM(path) .EQ. "./", "relative file name")
!
! CALL GetPath("dir/", path)
! CALL OK(TRIM(path) .EQ. "dir/", "path ending with slash")
!
! CALL GetPath("HelloWorld.txt", path)
! CALL OK(LEN_TRIM(path) .EQ. 0_I4B, "no directory gives blank")
!
! END PROGRAM main
!```

INTERFACE GetPath
  MODULE PURE SUBROUTINE GetPath_chars(chars, path)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(*), INTENT(OUT) :: path
  END SUBROUTINE GetPath_chars
END INTERFACE GetPath

!----------------------------------------------------------------------------
!                                                                 UpperCase
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-05
! summary: Returns an upper case copy of a string.
!
! Description:
!   Returns a copy of `chars` in which the letters `a-z` are replaced
!   by `A-Z`. All other characters are unchanged, and the result has
!   the same length as `chars`. See `ToUpperCase` for the in-place
!   version.
!
! Arguments:
!   chars : CHARACTER(*) - input string
!
! Returns:
!   ans : CHARACTER(:), ALLOCATABLE - upper case copy,
!     `LEN(ans) = LEN(chars)`
!
! Formula:
!   $$\mathrm{ans}_i = \begin{cases}
!     \mathrm{chars}_i - 32, & \mathrm{chars}_i \in [a, z] \\
!     \mathrm{chars}_i, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE StringUtility, ONLY: UpperCase
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=*), PARAMETER :: astr = "Hello World 123!"
! CHARACTER(:), ALLOCATABLE :: ans
!
! ans = UpperCase(astr)
! CALL OK(ans .EQ. "HELLO WORLD 123!", "mixed case")
! CALL OK(LEN(ans) .EQ. LEN(astr), "length is kept")
!
! CALL OK(UpperCase("abc_xyz") .EQ. "ABC_XYZ", "only a-z change")
! CALL OK(UpperCase("ABC") .EQ. "ABC", "already upper case")
!
! ans = UpperCase("")
! CALL OK(LEN(ans) .EQ. 0_I4B, "empty string")
!
! END PROGRAM main
!```

INTERFACE UpperCase
  MODULE PURE FUNCTION UpperCase_char(chars) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(len=:), ALLOCATABLE :: ans
  END FUNCTION UpperCase_char
END INTERFACE UpperCase

!----------------------------------------------------------------------------
!                                                               ToUpperCase
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-05
! summary: Converts a string to upper case in place.
!
! Description:
!   Replaces the letters `a-z` in `chars` by `A-Z`. All other
!   characters are unchanged. See `UpperCase` for a version that
!   returns a copy.
!
! Arguments:
!   chars : CHARACTER(*), INTENT(INOUT) - string to convert
!
! Returns:
!   chars : the upper case string
!
! Formula:
!   $$\mathrm{chars}_i \leftarrow \mathrm{chars}_i - 32
!     \quad \text{if } \mathrm{chars}_i \in [a, z]$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE StringUtility, ONLY: ToUpperCase
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=20) :: astr
!
! astr = "Hello, World"
! CALL ToUpperCase(astr)
! CALL OK(astr .EQ. "HELLO, WORLD", "mixed case")
!
! astr = "a1_b2"
! CALL ToUpperCase(astr)
! CALL OK(astr .EQ. "A1_B2", "digits and _ are unchanged")
!
! astr = "ABC"
! CALL ToUpperCase(astr)
! CALL OK(astr .EQ. "ABC", "already upper case")
!
! END PROGRAM main
!```

INTERFACE ToUpperCase
  MODULE PURE SUBROUTINE ToUpperCase_Char(chars)
    CHARACTER(*), INTENT(INOUT) :: chars
  END SUBROUTINE ToUpperCase_Char
END INTERFACE ToUpperCase

!----------------------------------------------------------------------------
!                                                                 LowerCase
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-05
! summary: Returns a lower case copy of a string.
!
! Description:
!   Returns a copy of `chars` in which the letters `A-Z` are replaced
!   by `a-z`. All other characters are unchanged, and the result has
!   the same length as `chars`. See `ToLowerCase` for the in-place
!   version.
!
! Arguments:
!   chars : CHARACTER(*) - input string
!
! Returns:
!   ans : CHARACTER(:), ALLOCATABLE - lower case copy,
!     `LEN(ans) = LEN(chars)`
!
! Formula:
!   $$\mathrm{ans}_i = \begin{cases}
!     \mathrm{chars}_i + 32, & \mathrm{chars}_i \in [A, Z] \\
!     \mathrm{chars}_i, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE StringUtility, ONLY: LowerCase
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=*), PARAMETER :: astr = "Hello World 123!"
! CHARACTER(:), ALLOCATABLE :: ans
!
! ans = LowerCase(astr)
! CALL OK(ans .EQ. "hello world 123!", "mixed case")
! CALL OK(LEN(ans) .EQ. LEN(astr), "length is kept")
!
! CALL OK(LowerCase("ABC_XYZ") .EQ. "abc_xyz", "only A-Z change")
! CALL OK(LowerCase("abc") .EQ. "abc", "already lower case")
!
! ans = LowerCase("")
! CALL OK(LEN(ans) .EQ. 0_I4B, "empty string")
!
! END PROGRAM main
!```

INTERFACE LowerCase
  MODULE PURE FUNCTION LowerCase_char(chars) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION LowerCase_char
END INTERFACE LowerCase

!----------------------------------------------------------------------------
!                                                               ToLowerCase
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-05
! summary: Converts a string to lower case in place.
!
! Description:
!   Replaces the letters `A-Z` in `chars` by `a-z`. All other
!   characters are unchanged. See `LowerCase` for a version that
!   returns a copy.
!
! Arguments:
!   chars : CHARACTER(*), INTENT(INOUT) - string to convert
!
! Returns:
!   chars : the lower case string
!
! Formula:
!   $$\mathrm{chars}_i \leftarrow \mathrm{chars}_i + 32
!     \quad \text{if } \mathrm{chars}_i \in [A, Z]$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE StringUtility, ONLY: ToLowerCase
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=20) :: astr
!
! astr = "Hello, World"
! CALL ToLowerCase(astr)
! CALL OK(astr .EQ. "hello, world", "mixed case")
!
! astr = "A1_B2"
! CALL ToLowerCase(astr)
! CALL OK(astr .EQ. "a1_b2", "digits and _ are unchanged")
!
! astr = "abc"
! CALL ToLowerCase(astr)
! CALL OK(astr .EQ. "abc", "already lower case")
!
! END PROGRAM main
!```

INTERFACE ToLowerCase
  MODULE PURE SUBROUTINE ToLowerCase_Char(chars)
    CHARACTER(*), INTENT(INOUT) :: chars
  END SUBROUTINE ToLowerCase_Char
END INTERFACE ToLowerCase

!----------------------------------------------------------------------------
!                                                               IsWhiteChar
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-05
! summary: Returns true if a character is a blank or a tab.
!
! Description:
!   Returns `.TRUE.` if `char` is a blank (ASCII 32) or a horizontal tab
!   (ASCII 9). Other control characters, such as a new line, are not
!   white characters.
!
! Arguments:
!   char : CHARACTER(1) - the character
!
! Returns:
!   ans : LOGICAL(LGT) - `.TRUE.` for a blank or a tab
!
! Formula:
!   $$\mathrm{ans} = (\mathrm{IACHAR}(c) = 32) \lor
!     (\mathrm{IACHAR}(c) = 9)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE StringUtility, ONLY: IsWhiteChar
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CALL OK(IsWhiteChar(" "), "blank")
! CALL OK(IsWhiteChar(ACHAR(9)), "tab")
! CALL OK(.NOT. IsWhiteChar("a"), "letter")
! CALL OK(.NOT. IsWhiteChar("_"), "underscore")
! CALL OK(.NOT. IsWhiteChar(ACHAR(10)), "new line is not white")
! CALL OK(.NOT. IsWhiteChar(ACHAR(13)), "carriage return is not white")
!
! END PROGRAM main
!```

INTERFACE IsWhiteChar
  MODULE PURE FUNCTION IsWhiteChar_char(char) RESULT(Ans)
    CHARACTER(1), INTENT(IN) :: char
    LOGICAL(LGT) :: ans
  END FUNCTION IsWhiteChar_char
END INTERFACE IsWhiteChar

!----------------------------------------------------------------------------
!                                                                   IsBlank
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-05
! summary: Returns true if a string has only blanks and tabs.
!
! Description:
!   Returns `.TRUE.` if every character of `chars` is a white character
!   (see `IsWhiteChar`). An empty string is blank.
!
! Arguments:
!   chars : CHARACTER(*) - the string
!
! Returns:
!   ans : LOGICAL(LGT) - `.TRUE.` if `chars` is blank
!
! Formula:
!   $$\mathrm{ans} = \bigwedge_{i=1}^{\mathrm{LEN}(\mathrm{chars})}
!     \mathrm{IsWhiteChar}(\mathrm{chars}_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE StringUtility, ONLY: IsBlank
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=10) :: astr
!
! CALL OK(IsBlank("   "), "blanks")
! CALL OK(IsBlank(""), "empty string")
! CALL OK(IsBlank(" "//ACHAR(9)//" "), "blanks and a tab")
! CALL OK(.NOT. IsBlank(" a "), "a letter")
! CALL OK(.NOT. IsBlank(ACHAR(10)), "new line")
!
! astr = " "
! CALL OK(IsBlank(astr), "blank variable")
!
! astr = "x"
! CALL OK(.NOT. IsBlank(astr), "padded variable with a letter")
!
! END PROGRAM main
!```

INTERFACE IsBlank
  MODULE PURE FUNCTION IsBlank_chars(chars) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: chars
    LOGICAL(LGT) :: ans
  END FUNCTION IsBlank_chars
END INTERFACE IsBlank

!----------------------------------------------------------------------------
!                                                                NumStrings
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-05
! summary: Returns the number of blank separated words in a string.
!
! Description:
!   Returns the number of substrings of `chars` that are separated by
!   blanks or tabs. Trailing blanks are ignored. This routine has been
!   adopted from `IO_Strings` of Futility
!   (https://github.com/CASL/Futility/blob/master/src/IO_Strings.F90).
!
!   - A comma is not a separator, so `"a, b"` has two words.
!   - A quoted substring (`'...'` or `"..."`) counts as one word,
!     even if it has blanks.
!   - A list-directed repeat count `r*value` counts as `r` words, for
!     example `"a 2*b c"` has 4 words, and so has `"3*a b"`.
!   - If the number of `'` quotes is odd, the result is -1. If the
!     number of `"` quotes is odd, the result is -2.
!
! Arguments:
!   chars : CHARACTER(*) - the string
!
! Returns:
!   ans : INTEGER(I4B) - number of words, or -1 / -2 for an
!     unmatched quote
!
! Formula:
!   For `chars = "w1 w2 ... wn"`: $$\mathrm{ans} = n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE StringUtility, ONLY: NumStrings
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=40) :: astr
!
! astr = "Hello world how are you!"
! CALL OK(NumStrings(astr) .EQ. 5_I4B, "five words")
!
! astr = "Hello world, how are you"
! CALL OK(NumStrings(astr) .EQ. 5_I4B, "a comma is part of a word")
!
! astr = "  a  b   c  "
! CALL OK(NumStrings(astr) .EQ. 3_I4B, "many blanks")
!
! astr = "a"//ACHAR(9)//"b"
! CALL OK(NumStrings(astr) .EQ. 2_I4B, "tab is a separator")
!
! astr = "'a b' c"
! CALL OK(NumStrings(astr) .EQ. 2_I4B, "quoted words count once")
!
! astr = "a 2*b c"
! CALL OK(NumStrings(astr) .EQ. 4_I4B, "repeat count 2*b")
!
! astr = " "
! CALL OK(NumStrings(astr) .EQ. 0_I4B, "blank string")
!
! ! an unmatched quote is an error
! astr = "a 'b"
! CALL OK(NumStrings(astr) .EQ. -1_I4B, "odd number of ' quotes")
!
! astr = 'a "b'
! CALL OK(NumStrings(astr) .EQ. -2_I4B, 'odd number of " quotes')
!
! END PROGRAM main
!```

INTERFACE NumStrings
  MODULE PURE FUNCTION NumStrings_chars(chars) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: chars
    INTEGER(I4B) :: ans
  END FUNCTION NumStrings_chars
END INTERFACE NumStrings

!----------------------------------------------------------------------------
!                                                               NumMatchStr
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-05
! summary: Returns the number of times a pattern occurs in a string.
!
! Description:
!   Counts every position `i` where `chars(i:i+LEN(pattern)-1)` equals
!   `pattern`.
!
!   - Overlapping matches are counted, so `"aaaa"` has 3 matches of
!     `"aa"`.
!   - The trailing blanks of `chars` and `pattern` are part of the
!     search. Use `TRIM` to ignore them.
!   - The search is case sensitive.
!   - An empty `pattern` has no match, so the result is 0.
!
! Arguments:
!   chars : CHARACTER(*) - the string to search
!   pattern : CHARACTER(*) - the pattern to find
!
! Returns:
!   ans : INTEGER(I4B) - number of matches
!
! Formula:
!   $$\mathrm{ans} = \#\{\, i : \mathrm{chars}(i:i+m-1) =
!     \mathrm{pattern} \,\}, \quad m = \mathrm{LEN}(\mathrm{pattern})$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE StringUtility, ONLY: NumMatchStr
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=10) :: astr
!
! CALL OK(NumMatchStr("hello world", "o") .EQ. 2_I4B, "two matches")
! CALL OK(NumMatchStr("hello world", "xyz") .EQ. 0_I4B, "no match")
! CALL OK(NumMatchStr("ab", "abc") .EQ. 0_I4B, "pattern too long")
!
! ! overlapping matches are counted
! CALL OK(NumMatchStr("aaaa", "aa") .EQ. 3_I4B, "overlapping matches")
!
! ! trailing blanks of the string are searched too
! astr = "a b"
! CALL OK(NumMatchStr(astr, " ") .EQ. 8_I4B, "padded string")
! CALL OK(NumMatchStr(TRIM(astr), " ") .EQ. 1_I4B, "trimmed string")
!
! END PROGRAM main
!```

INTERFACE numMatchStr
  MODULE PURE FUNCTION numMatchStr_chars(chars, pattern) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(*), INTENT(IN) :: pattern
    INTEGER(I4B) :: ans
  END FUNCTION numMatchStr_chars
END INTERFACE numMatchStr

!----------------------------------------------------------------------------
!                                                                 IsPresent
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-05
! summary: Returns true if a pattern is found in a string.
!
! Description:
!   Returns `.TRUE.` if `pattern` is a substring of `chars`. The search
!   is case sensitive. If `chars` or `pattern` is empty, the result is
!   `.FALSE.`.
!
!@note
! Trailing blanks are part of both strings. Pass trimmed strings if
! trailing blanks should be ignored.
!@endnote
!
! Arguments:
!   chars : CHARACTER(*) - the string to search
!   pattern : CHARACTER(*) - the pattern to find
!
! Returns:
!   ans : LOGICAL(LGT) - `.TRUE.` if `pattern` is found
!
! Formula:
!   $$\mathrm{ans} = \mathrm{INDEX}(\mathrm{chars}, \mathrm{pattern})
!     > 0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE StringUtility, ONLY: IsPresent
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=20) :: astr
!
! CALL OK(IsPresent("Hello world", "world"), "pattern found")
! CALL OK(.NOT. IsPresent("Hello world", "World"), "case matters")
! CALL OK(.NOT. IsPresent("abc", "abcd"), "pattern too long")
! CALL OK(.NOT. IsPresent("abc", ""), "empty pattern")
! CALL OK(.NOT. IsPresent("", "a"), "empty string")
!
! ! trailing blanks are part of the string
! astr = "abc"
! CALL OK(IsPresent(astr, "c "), "blank after c in padded string")
! CALL OK(.NOT. IsPresent(TRIM(astr), "c "), "trimmed string")
!
! END PROGRAM main
!```

INTERFACE isPresent
  MODULE PURE FUNCTION isPresent_chars(chars, pattern) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(*), INTENT(IN) :: pattern
    LOGICAL(LGT) :: ans
  END FUNCTION isPresent_chars
END INTERFACE isPresent

!----------------------------------------------------------------------------
!                                                                   StrFind
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-05
! summary: Returns the indices where a pattern starts in a string.
!
! Description:
!   Returns, in increasing order, every index `i` where `pattern`
!   starts in `chars`. The matches are the same as in `NumMatchStr`,
!   so overlapping matches are included and trailing blanks are part
!   of the search.
!
!   `indices` is always allocated on return. If there is no match, or
!   `pattern` is empty, it has size 0.
!
! Arguments:
!   chars : CHARACTER(*) - the string to search
!   pattern : CHARACTER(*) - the pattern to find
!   indices : INTEGER(I4B), ALLOCATABLE, rank(1), INTENT(OUT) - start
!     indices, `SIZE(indices) = NumMatchStr(chars, pattern)`
!
! Returns:
!   indices : start index of each match
!
! Formula:
!   $$\mathrm{indices} = \{\, i : \mathrm{chars}(i:i+m-1) =
!     \mathrm{pattern} \,\}, \quad m = \mathrm{LEN}(\mathrm{pattern})$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE StringUtility, ONLY: StrFind
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=100) :: astr
! INTEGER(I4B), ALLOCATABLE :: indices(:)
!
! astr = "/a/b/c"
! CALL StrFind(astr, "/", indices)
! CALL OK(ALL(indices .EQ. [1_I4B, 3_I4B, 5_I4B]), "/a/b/c")
!
! astr = "/a/b/c/"
! CALL StrFind(astr, "/", indices)
! CALL OK(SIZE(indices) .EQ. 4_I4B, "/a/b/c/ has four slashes")
! CALL OK(indices(4) .EQ. 7_I4B, "last slash at 7")
!
! ! overlapping matches
! CALL StrFind("aaaa", "aa", indices)
! CALL OK(ALL(indices .EQ. [1_I4B, 2_I4B, 3_I4B]), "overlapping")
!
! ! no match gives an allocated vector of size 0
! astr = "abc"
! CALL StrFind(astr, "/", indices)
! CALL OK(ALLOCATED(indices), "allocated when there is no match")
! CALL OK(SIZE(indices) .EQ. 0_I4B, "size 0 when there is no match")
!
! END PROGRAM main
!```

INTERFACE StrFind
  MODULE PURE SUBROUTINE strFind_chars(chars, pattern, indices)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(*), INTENT(IN) :: pattern
    INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: indices(:)
  END SUBROUTINE strFind_chars
END INTERFACE strFind

!----------------------------------------------------------------------------
!                                                               FindReplace
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-05
! summary: Replaces every match of a pattern in a string.
!
! Description:
!   Replaces every occurrence of `findp` in `chars` by `repp`, in
!   place.
!
!   - `repp` may be longer or shorter than `findp`, or empty.
!   - Trailing and leading blanks of `findp` and `repp` are part of
!     the strings.
!   - If the new string does not fit in `LEN(chars)`, that is if
!     `LEN_TRIM(chars) + n*(LEN(repp) - LEN(findp)) > LEN(chars)` for
!     `n` replacements, `chars` is left unchanged and `isok` is
!     `.FALSE.`. In every other case `isok` is `.TRUE.`.
!   - Matches are found from left to right, and the search goes on
!     after the end of each match, so `"aa"` in `"aaa"` is replaced
!     once. This differs from `NumMatchStr` and `StrFind`, which
!     also count overlapping matches.
!   - An empty `findp` changes nothing.
!
!@note
! `repp` can be longer than `findp` as long as `chars` can hold the
! longer string after all replacements.
!@endnote
!
! Arguments:
!   chars : CHARACTER(*), INTENT(INOUT) - string to change
!   findp : CHARACTER(*) - the pattern to find
!   repp : CHARACTER(*) - the new substring
!   isok : LOGICAL(LGT), OPTIONAL, INTENT(OUT) - `.FALSE.` if the new
!     string does not fit in `chars`
!
! Returns:
!   chars : the string with every `findp` replaced by `repp`
!   isok : `.TRUE.` unless the new string does not fit
!
! Formula:
!   $$\mathrm{LEN\_TRIM}(\mathrm{chars}_{new}) =
!     \mathrm{LEN\_TRIM}(\mathrm{chars}) +
!     n\,(\mathrm{LEN}(\mathrm{repp}) - \mathrm{LEN}(\mathrm{findp}))$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE StringUtility, ONLY: FindReplace
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=40) :: astr
! CHARACTER(LEN=10) :: bstr
! CHARACTER(LEN=5) :: cstr
!
! astr = "Hello world, how are you"
! CALL FindReplace(astr, "you", "you!")
! CALL OK(astr .EQ. "Hello world, how are you!", "longer replacement")
!
! bstr = "cat cat"
! CALL FindReplace(bstr, "cat", "dog")
! CALL OK(bstr .EQ. "dog dog", "same length, two matches")
!
! bstr = "a-b-c"
! CALL FindReplace(bstr, "-", "--")
! CALL OK(bstr .EQ. "a--b--c", "string grows")
!
! bstr = "a--b--c"
! CALL FindReplace(bstr, "--", "-")
! CALL OK(bstr .EQ. "a-b-c", "string shrinks")
!
! bstr = "a-b-c"
! CALL FindReplace(bstr, "-", "")
! CALL OK(bstr .EQ. "abc", "empty replacement")
!
! bstr = "abc"
! CALL FindReplace(bstr, "xyz", "123")
! CALL OK(bstr .EQ. "abc", "no match")
!
! ! the result does not fit in LEN(cstr), so nothing is changed
! cstr = "abcde"
! CALL FindReplace(cstr, "a", "aa")
! CALL OK(cstr .EQ. "abcde", "no room: string is unchanged")
!
! ! overlapping matches: scan from the left, skip each match
! bstr = "aaa"
! CALL FindReplace(bstr, "aa", "b")
! CALL OK(bstr .EQ. "ba", "aa -> b in aaa gives ba")
!
! bstr = "aaaa"
! CALL FindReplace(bstr, "aa", "b")
! CALL OK(bstr .EQ. "bb", "aa -> b in aaaa gives bb")
!
! bstr = "abababa"
! CALL FindReplace(bstr, "aba", "x")
! CALL OK(bstr .EQ. "xbx", "aba -> x in abababa gives xbx")
!
! ! an empty pattern changes nothing
! bstr = "aaa"
! CALL FindReplace(bstr, "", "b")
! CALL OK(bstr .EQ. "aaa", "empty pattern")
!
! END PROGRAM main
!```

INTERFACE FindReplace
  MODULE PURE SUBROUTINE FindReplace_chars(chars, findp, repp, isok)
    CHARACTER(*), INTENT(INOUT) :: chars
    CHARACTER(*), INTENT(IN) :: findp
    CHARACTER(*), INTENT(IN) :: repp
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isok
  END SUBROUTINE FindReplace_chars
END INTERFACE FindReplace

!----------------------------------------------------------------------------
!                                                                  GetField
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-08
! summary: Returns the i-th field of a string.
!
! Description:
!   Returns the `i`-th field of `chars`. Fields are read with a
!   list-directed `READ`, so they are separated by blanks or commas,
!   and quotes around a quoted field are removed. The fields are
!   counted with `NumStrings`.
!
!   - A slash `/` does not end a field (a list-directed `READ` would
!     normally stop at it).
!   - If `1 <= i <= NumStrings(chars)`, `field` is allocated and
!     `ierr` is the `IOSTAT` of the `READ` (0 on success).
!   - Otherwise `field` is an empty string and `ierr` is
!     `IOSTAT_END`.
!
! Arguments:
!   i : INTEGER(I4B) - field number, starting at 1
!   chars : CHARACTER(*) - the string
!   field : CHARACTER(:), ALLOCATABLE, INTENT(OUT) - the field
!   ierr : INTEGER(I4B), OPTIONAL, INTENT(OUT) - error code
!
! Returns:
!   field : the `i`-th field without trailing blanks
!   ierr : 0, the `IOSTAT` of the `READ`, or `IOSTAT_END` if `i` is
!     out of range
!
! Formula:
!   For `chars = "w1 w2 ... wn"`: $$\mathrm{field} = w_i$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE ISO_FORTRAN_ENV, ONLY: IOSTAT_END
! USE GlobalData, ONLY: I4B
! USE StringUtility, ONLY: GetField
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=100) :: astr
! CHARACTER(:), ALLOCATABLE :: field
! CHARACTER(LEN=5) :: want(5)
! INTEGER(I4B) :: ierr, ii
!
! astr = "Hello world, how are you"
! want = [CHARACTER(LEN=5) :: "Hello", "world", "how", "are", "you"]
!
! DO ii = 1, 5
!   CALL GetField(i=ii, chars=astr, field=field, ierr=ierr)
!   CALL OK(field .EQ. TRIM(want(ii)), "field "//want(ii))
!   CALL OK(ierr .EQ. 0_I4B, "ierr is 0 for field "//want(ii))
! END DO
!
! ! i out of range: ierr is IOSTAT_END and field is empty
! CALL GetField(i=6, chars=astr, field=field, ierr=ierr)
! CALL OK(ierr .EQ. IOSTAT_END, "i > number of fields")
! CALL OK(LEN(field) .EQ. 0_I4B, "field is empty")
!
! CALL GetField(i=0, chars=astr, field=field, ierr=ierr)
! CALL OK(ierr .EQ. IOSTAT_END, "i = 0")
!
! END PROGRAM main
!```

INTERFACE GetField
  MODULE PURE SUBROUTINE GetField_chars(i, chars, field, ierr)
    INTEGER(I4B), INTENT(IN) :: i
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: field
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: ierr
  END SUBROUTINE GetField_chars
END INTERFACE GetField

!----------------------------------------------------------------------------
!                                                                  SlashRep
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-08
! summary: Replaces slashes by the file separator of the system.
!
! Description:
!   Replaces, in place, the slashes in a file path by `CHAR_SLASH`, the
!   file separator of the system. Only the first `LEN_TRIM(chars)`
!   characters are changed.
!
!   - On macOS and Linux every backslash is replaced by `/`.
!   - When `WIN32` is defined, every `/` is replaced by a backslash.
!
! Arguments:
!   chars : CHARACTER(*), INTENT(INOUT) - the file path
!
! Returns:
!   chars : the path with the separator of the system
!
! Formula:
!   $$\mathrm{chars}_i \leftarrow \mathrm{CHAR\_SLASH}
!     \quad \text{if } \mathrm{chars}_i \text{ is the other slash}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE StringUtility, ONLY: SlashRep
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=20) :: astr
! CHARACTER(LEN=1), PARAMETER :: bslash = ACHAR(92)
!
! ! on macOS and Linux a backslash is replaced by /
! astr = "a"//bslash//"b"//bslash//"c.txt"
! CALL SlashRep(astr)
! CALL OK(astr .EQ. "a/b/c.txt", "backslashes are replaced")
!
! astr = "a/b/c.txt"
! CALL SlashRep(astr)
! CALL OK(astr .EQ. "a/b/c.txt", "forward slashes are kept")
!
! astr = "file.txt"
! CALL SlashRep(astr)
! CALL OK(astr .EQ. "file.txt", "no slash")
!
! END PROGRAM main
!```

INTERFACE SlashRep
  MODULE PURE SUBROUTINE SlashRep_chars(chars)
    CHARACTER(*), INTENT(INOUT) :: chars
  END SUBROUTINE SlashRep_chars
END INTERFACE SlashRep

!----------------------------------------------------------------------------
!                                                              GetFileParts
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-08
! summary: Returns the path, name and extension of a file name.
!
! Description:
!   Splits the file name `chars` into three parts.
!
!   - `path` is the result of `GetPath`: everything up to and
!     including the last separator, or blank.
!   - `fname` is the result of `GetFileName` without the extension:
!     everything after the last separator, up to the last dot.
!   - `ext` is the result of `GetFileNameExt`: the extension with its
!     dot, or blank.
!
!   For `"archive.tar.gz"` the parts are `""`, `"archive.tar"` and
!   `".gz"`. The output arguments are blank padded.
!
! Arguments:
!   chars : CHARACTER(*) - the file name
!   path : CHARACTER(*), INTENT(OUT) - directory part
!   fname : CHARACTER(*), INTENT(OUT) - name without extension
!   ext : CHARACTER(*), INTENT(OUT) - extension with the dot
!
! Returns:
!   path, fname, ext : the three parts of `chars`
!
! Formula:
!   $$\mathrm{chars} = \mathrm{path} \,//\, \mathrm{fname} \,//\,
!     \mathrm{ext}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE StringUtility, ONLY: GetFileParts
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=100) :: path, fname, ext
!
! CALL GetFileParts("./HelloWorld.txt", path, fname, ext)
! CALL OK(TRIM(path) .EQ. "./", "path of ./HelloWorld.txt")
! CALL OK(TRIM(fname) .EQ. "HelloWorld", "name of ./HelloWorld.txt")
! CALL OK(TRIM(ext) .EQ. ".txt", "extension of ./HelloWorld.txt")
!
! CALL GetFileParts("/dir1/dir2/HelloWorld.txt", path, fname, ext)
! CALL OK(TRIM(path) .EQ. "/dir1/dir2/", "absolute path")
! CALL OK(TRIM(fname) .EQ. "HelloWorld", "absolute name")
! CALL OK(TRIM(ext) .EQ. ".txt", "absolute extension")
!
! ! only the last extension is split off
! CALL GetFileParts("archive.tar.gz", path, fname, ext)
! CALL OK(LEN_TRIM(path) .EQ. 0_I4B, "no path")
! CALL OK(TRIM(fname) .EQ. "archive.tar", "name keeps .tar")
! CALL OK(TRIM(ext) .EQ. ".gz", "extension .gz")
!
! CALL GetFileParts("README", path, fname, ext)
! CALL OK(TRIM(fname) .EQ. "README", "name without extension")
! CALL OK(LEN_TRIM(ext) .EQ. 0_I4B, "no extension")
!
! END PROGRAM main
!```

INTERFACE GetFileParts
  MODULE PURE SUBROUTINE GetFileParts_chars(chars, path, fname, ext)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(*), INTENT(OUT) :: path
    CHARACTER(*), INTENT(OUT) :: fname
    CHARACTER(*), INTENT(OUT) :: ext
  END SUBROUTINE GetFileParts_chars
END INTERFACE GetFileParts

!----------------------------------------------------------------------------
!                                                               GetFileName
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! summary: Returns the file name with its extension.
!
! Description:
!   Returns the part of `chars` after the last separator, extension
!   included. Backslashes are first replaced by `CHAR_SLASH` (see
!   `SlashRep`).
!
!   - If `chars` has no separator, `fname` is `chars`.
!   - If `chars` ends with a separator, `fname` is blank.
!   - `fname` is blank padded.
!
! Arguments:
!   chars : CHARACTER(*) - the file name
!   fname : CHARACTER(*), INTENT(OUT) - name with extension
!
! Returns:
!   fname : the part of `chars` after the last separator
!
! Formula:
!   $$\mathrm{fname} = \mathrm{chars}(k+1:\mathrm{LEN\_TRIM}), \quad
!     k = \text{position of the last separator}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE StringUtility, ONLY: GetFileName
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=100) :: fname
!
! CALL GetFileName("/dir1/dir2/HelloWorld.txt", fname)
! CALL OK(TRIM(fname) .EQ. "HelloWorld.txt", "absolute file name")
!
! CALL GetFileName("./a.txt", fname)
! CALL OK(TRIM(fname) .EQ. "a.txt", "relative file name")
!
! CALL GetFileName("HelloWorld.txt", fname)
! CALL OK(TRIM(fname) .EQ. "HelloWorld.txt", "no directory")
!
! CALL GetFileName("dir/", fname)
! CALL OK(LEN_TRIM(fname) .EQ. 0_I4B, "path ending with slash")
!
! END PROGRAM main
!```

INTERFACE GetFileName
  MODULE PURE SUBROUTINE GetFileName_chars(chars, fname)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(*), INTENT(OUT) :: fname
  END SUBROUTINE GetFileName_chars
END INTERFACE GetFileName

!----------------------------------------------------------------------------
!                                                            GetFileNameExt
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! summary: Returns the extension of a file name, with the dot.
!
! Description:
!   Returns the part of the file name from the last dot to the end,
!   dot included, for example `".txt"`. Backslashes are first replaced
!   by `CHAR_SLASH`.
!
!   - Only the last extension is returned: `"a.tar.gz"` gives `".gz"`.
!   - A dot before the last separator is not an extension, so
!     `"dir.d/file"` gives a blank `ext`.
!   - If there is no extension, `ext` is blank.
!   - `ext` is blank padded. See `GetExtension` for a function that
!     returns the extension without the dot.
!
! Arguments:
!   chars : CHARACTER(*) - the file name
!   ext : CHARACTER(*), INTENT(OUT) - the extension with the dot
!
! Returns:
!   ext : the extension of `chars`
!
! Formula:
!   $$\mathrm{ext} = \mathrm{chars}(d:\mathrm{LEN\_TRIM}), \quad
!     d = \text{position of the last dot after the last separator}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE StringUtility, ONLY: GetFileNameExt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=100) :: ext
!
! CALL GetFileNameExt("./HelloWorld.txt", ext)
! CALL OK(TRIM(ext) .EQ. ".txt", "relative file name")
!
! CALL GetFileNameExt("/dir1/dir2/HelloWorld.txt", ext)
! CALL OK(TRIM(ext) .EQ. ".txt", "absolute file name")
!
! CALL GetFileNameExt("a.tar.gz", ext)
! CALL OK(TRIM(ext) .EQ. ".gz", "only the last extension")
!
! ! a dot in a directory name is not an extension
! CALL GetFileNameExt("dir.d/file", ext)
! CALL OK(LEN_TRIM(ext) .EQ. 0_I4B, "dot in directory")
!
! CALL GetFileNameExt("README", ext)
! CALL OK(LEN_TRIM(ext) .EQ. 0_I4B, "no extension")
!
! END PROGRAM main
!```

INTERFACE GetFileNameExt
  MODULE PURE SUBROUTINE GetFileNameExt_chars(chars, ext)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(*), INTENT(OUT) :: ext
  END SUBROUTINE GetFileNameExt_chars
END INTERFACE GetFileNameExt

!----------------------------------------------------------------------------
!                                                              GetExtension
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-24
! summary: Returns the text after the last dot of a file name.
!
! Description:
!   Returns everything after the last dot in the last element of the
!   path `char`, without the dot. For example
!   `GetExtension("helloworld.F90")` gives `"F90"`.
!
!   - The case is kept; the result is not converted to lower case.
!   - If the last element has no dot, or ends with a dot, the result
!     is empty. A dot in a directory name is not used, so
!     `"dir.d/file"` gives `""`.
!   - Trailing blanks of `char` are ignored.
!
! Arguments:
!   char : CHARACTER(*) - the file name
!
! Returns:
!   ext : CHARACTER(:), ALLOCATABLE - text after the last dot
!
! Formula:
!   For `char = "d1/name.ext"`: $$\mathrm{ext} = \mathrm{ext}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE StringUtility, ONLY: GetExtension
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(:), ALLOCATABLE :: ext
!
! ! the dot is not included and the case is kept
! CALL OK(GetExtension("helloworld.F90") .EQ. "F90", "F90 is kept")
! CALL OK(GetExtension("mesh.h5") .EQ. "h5", "h5")
! CALL OK(GetExtension("a.tar.gz") .EQ. "gz", "only the last extension")
!
! ext = GetExtension("README")
! CALL OK(LEN(ext) .EQ. 0_I4B, "no dot gives an empty string")
!
! ext = GetExtension("file.")
! CALL OK(LEN(ext) .EQ. 0_I4B, "dot at the end")
!
! END PROGRAM main
!```

INTERFACE GetExtension
  MODULE PURE FUNCTION GetExtension_chars(char) RESULT(ext)
    CHARACTER(*), INTENT(IN) :: char
    CHARACTER(:), ALLOCATABLE :: ext
  END FUNCTION GetExtension_chars
END INTERFACE GetExtension

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE StringUtility
