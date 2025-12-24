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

MODULE StringUtility
USE GlobalData, ONLY: I4B, LGT
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

!----------------------------------------------------------------------------
!                                                                 PathBase
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-17
! summary: Returns the base of the path
!
!# Introduction
!
! Base returns the last element of path.
! Trailing slashes are removed before extracting the
! last element.
! If the path is empty, Base returns ".".
! If the path consists entirely of slashes, Base returns "/".
!
! func main() {
!         fmt.Println(path.Base("/a/b"))
!         fmt.Println(path.Base("/"))
!         fmt.Println(path.Base(""))
! }
! b
! /
! .

INTERFACE
  MODULE PURE FUNCTION PathBase(path) RESULT(ans)
    CHARACTER(*), INTENT(in) :: path
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION PathBase
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    PathJoin
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-17
! summary: Join two paths

INTERFACE PathJoin
  MODULE PURE FUNCTION PathJoin1(path1, path2) RESULT(ans)
    CHARACTER(*), INTENT(in) :: path1
    CHARACTER(*), INTENT(in) :: path2
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION PathJoin1
END INTERFACE PathJoin

!----------------------------------------------------------------------------
!                                                                    PathJoin
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-17
! summary: Join two paths

INTERFACE PathJoin
  MODULE PURE FUNCTION PathJoin2(paths) RESULT(ans)
    TYPE(String), INTENT(IN) :: paths(:)
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION PathJoin2
END INTERFACE PathJoin

!----------------------------------------------------------------------------
!                                                GetPath@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-17
! summary: Returns the parent directory
!
!# Introduction
!
! Dir returns all but the last element of path,
! typically the path's directory.
! After dropping the final element using Split,
! the path is Cleaned and trailing slashes are removed.
! If the path is empty, Dir returns ".".
! If the path consists entirely of slashes followed by non-slash bytes,
! Dir returns a single slash.
! In any other case, the returned path does not end in a slash.

INTERFACE
  MODULE PURE FUNCTION PathDir(path) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: path
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION PathDir
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetPath@StringMethods
!----------------------------------------------------------------------------

INTERFACE GetPath
  MODULE PURE SUBROUTINE GetPath_chars(chars, path)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(*), INTENT(OUT) :: path
  END SUBROUTINE GetPath_chars
END INTERFACE GetPath

!----------------------------------------------------------------------------
!                                                    UpperCase@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary:         Returns the upperCase version of chars

INTERFACE UpperCase
  MODULE PURE FUNCTION UpperCase_char(chars) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(len=:), ALLOCATABLE :: ans
  END FUNCTION UpperCase_char
END INTERFACE UpperCase

!----------------------------------------------------------------------------
!                                                  ToUpperCase@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary:         Returns the upperCase version of chars

INTERFACE ToUpperCase
  MODULE PURE SUBROUTINE ToUpperCase_Char(chars)
    CHARACTER(*), INTENT(INOUT) :: chars
  END SUBROUTINE ToUpperCase_Char
END INTERFACE ToUpperCase

!----------------------------------------------------------------------------
!                                                    LowerCase@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary:         Returns the LowerCase version of chars

INTERFACE LowerCase
  MODULE PURE FUNCTION LowerCase_char(chars) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION LowerCase_char
END INTERFACE LowerCase

!----------------------------------------------------------------------------
!                                                  ToLowerCase@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary:         Returns the LowerCase version of chars

INTERFACE ToLowerCase
  MODULE PURE SUBROUTINE ToLowerCase_Char(chars)
    CHARACTER(*), INTENT(INOUT) :: chars
  END SUBROUTINE ToLowerCase_Char
END INTERFACE ToLowerCase

!----------------------------------------------------------------------------
!                                                  IsWhiteChar@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary:         Returns true if the char is a space(32) or a tab(9).

INTERFACE IsWhiteChar
  MODULE PURE FUNCTION IsWhiteChar_char(char) RESULT(Ans)
    CHARACTER(1), INTENT(IN) :: char
    LOGICAL(LGT) :: ans
  END FUNCTION IsWhiteChar_char
END INTERFACE IsWhiteChar

!----------------------------------------------------------------------------
!                                                  IsBlank@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary:         Returns true of the entire string is blank

INTERFACE IsBlank
  MODULE PURE FUNCTION IsBlank_chars(chars) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: chars
    LOGICAL(LGT) :: ans
  END FUNCTION IsBlank_chars
END INTERFACE IsBlank

!----------------------------------------------------------------------------
!                                                    numString@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary: Returns number of substrings contained in input string 'chars'
! delimited by white space.
!
!# Introduction
! Returns number of substrings contained in input string 'chars' delimited by
! white space.
! This routien has been adopted from
! [https://github.com/CASL/Futility/blob/master/src/IO_Strings.F90]
! (https://github.com/CASL/Futility/blob/master/src/IO_Strings.F90)
!

INTERFACE NumStrings
  MODULE PURE FUNCTION NumStrings_chars(chars) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: chars
    INTEGER(I4B) :: ans
  END FUNCTION NumStrings_chars
END INTERFACE NumStrings

!----------------------------------------------------------------------------
!                                                   nmatchstr@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 sept 2021
! summary: Returns the total number of times the substring pattern is
! found in the main string

INTERFACE numMatchStr
  MODULE PURE FUNCTION numMatchStr_chars(chars, pattern) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(*), INTENT(IN) :: pattern
    INTEGER(I4B) :: ans
  END FUNCTION numMatchStr_chars
END INTERFACE numMatchStr

!----------------------------------------------------------------------------
!                                                  isPresent@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 sept 2021
! summary: Returns whether or not a substring pattern is found within string
!
!# Introduction
! Returns whether or not a substring pattern is found within string
!
!@note
! Does not handle trailing spaces that can be eliminated by TRIM() so
! strings should be trimmed when passing into function.
!@endnote

INTERFACE isPresent
  MODULE PURE FUNCTION isPresent_chars(chars, pattern) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(*), INTENT(IN) :: pattern
    LOGICAL(LGT) :: ans
  END FUNCTION isPresent_chars
END INTERFACE isPresent

!----------------------------------------------------------------------------
!                                                       StrFind@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 sept 2021
! summary: Function returns the indices in a string where substring pattern

INTERFACE StrFind
  MODULE PURE SUBROUTINE strFind_chars(chars, pattern, indices)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(*), INTENT(IN) :: pattern
    INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: indices(:)
  END SUBROUTINE strFind_chars
END INTERFACE strFind

!----------------------------------------------------------------------------
!                                                 FindReplace@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 sept 2021
! summary: Replaces a substring pattern with a different substring in a string
!
!# Introduction
! Replaces a substring pattern with a different substring in a string.
! - chars the string which will have substrings replaced.
! - findp the substring pattern to find and replace
! - repp the new substring that will be replace parts of string
!
!@note
! repp can be larger than findp and as long as the size of string can
! accomodate the increased length of all replacements. Trailing and preceding
! spaces are counted in all strings.
!@endnote

INTERFACE FindReplace
  MODULE PURE SUBROUTINE FindReplace_chars(chars, findp, repp)
    CHARACTER(*), INTENT(INOUT) :: chars
    CHARACTER(*), INTENT(IN) :: findp
    CHARACTER(*), INTENT(IN) :: repp
  END SUBROUTINE FindReplace_chars
END INTERFACE FindReplace

!----------------------------------------------------------------------------
!                                                    GetField@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 sept 2021
! summary: Replaces a substring pattern with a different substring in a string

INTERFACE GetField
  MODULE PURE SUBROUTINE GetField_chars(i, chars, field, ierr)
    INTEGER(I4B), INTENT(IN) :: i
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: field
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: ierr
  END SUBROUTINE GetField_chars
END INTERFACE GetField

!----------------------------------------------------------------------------
!                                                    SlashRep@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 sept 2021
! summary: routine replaces slash character in file path names with
! the system appropriate file separator slash.
!
!# Introduction
! This routine returns the path, filename, and extension.

INTERFACE SlashRep
  MODULE PURE SUBROUTINE SlashRep_chars(chars)
    CHARACTER(*), INTENT(INOUT) :: chars
  END SUBROUTINE SlashRep_chars
END INTERFACE SlashRep

!----------------------------------------------------------------------------
!                                                GetFileParts@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 sept 2021
! summary: Returns the path,filename, and extension
!
!# Introduction
! This routine returns the path, filename, and extension.

INTERFACE GetFileParts
  MODULE PURE SUBROUTINE GetFileParts_chars(chars, path, fname, ext)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(*), INTENT(OUT) :: path
    CHARACTER(*), INTENT(OUT) :: fname
    CHARACTER(*), INTENT(OUT) :: ext
  END SUBROUTINE GetFileParts_chars
END INTERFACE GetFileParts

!----------------------------------------------------------------------------
!                                                GetFileName@StringMethods
!----------------------------------------------------------------------------

INTERFACE GetFileName
  MODULE PURE SUBROUTINE GetFileName_chars(chars, fname)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(*), INTENT(OUT) :: fname
  END SUBROUTINE GetFileName_chars
END INTERFACE GetFileName

!----------------------------------------------------------------------------
!                                           GetFileNameExt@StringMethods
!----------------------------------------------------------------------------

INTERFACE GetFileNameExt
  MODULE PURE SUBROUTINE GetFileNameExt_chars(chars, ext)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(*), INTENT(OUT) :: ext
  END SUBROUTINE GetFileNameExt_chars
END INTERFACE GetFileNameExt

!----------------------------------------------------------------------------
!                                                GetExtension@StringMethods
!----------------------------------------------------------------------------

!> author: Dr. Vikas Sharma
!
! This function Get the extension from a file
!
! ## Usage
! ```fortran
! call display( GetExtension("helloworld.F90") .EQ. "f90", &
! & msg="test1:: ")
! ```

INTERFACE GetExtension
  MODULE FUNCTION GetExtension_chars(char) RESULT(ext)
    CHARACTER(*), INTENT(IN) :: char
    CHARACTER(:), ALLOCATABLE :: ext
  END FUNCTION
END INTERFACE GetExtension

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE StringUtility
