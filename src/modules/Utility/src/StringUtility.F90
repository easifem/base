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
USE GlobalData
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

!----------------------------------------------------------------------------
!                                                    UpperCase@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary:         Returns the upperCase version of chars

INTERFACE UpperCase
  MODULE PURE FUNCTION UpperCase_char(chars) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(LEN(chars)) :: ans
  END FUNCTION UpperCase_char
END INTERFACE UpperCase

!----------------------------------------------------------------------------
!                                                  toUpperCase@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary:         Returns the upperCase version of chars

INTERFACE toUpperCase
  MODULE PURE SUBROUTINE ToUpperCase_Char(chars)
    CHARACTER(*), INTENT(INOUT) :: chars
  END SUBROUTINE ToUpperCase_Char
END INTERFACE toUpperCase

!----------------------------------------------------------------------------
!                                                    LowerCase@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary:         Returns the LowerCase version of chars

INTERFACE LowerCase
  MODULE PURE FUNCTION LowerCase_char(chars) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(LEN(chars)) :: ans
  END FUNCTION LowerCase_char
END INTERFACE LowerCase

!----------------------------------------------------------------------------
!                                                  toLowerCase@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary:         Returns the LowerCase version of chars

INTERFACE toLowerCase
  MODULE PURE SUBROUTINE ToLowerCase_Char(chars)
    CHARACTER(*), INTENT(INOUT) :: chars
  END SUBROUTINE ToLowerCase_Char
END INTERFACE toLowerCase

!----------------------------------------------------------------------------
!                                                  isWhiteChar@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary:         Returns true if the char is a space(32) or a tab(9).

INTERFACE isWhiteChar
  MODULE PURE FUNCTION isWhiteChar_char(char) RESULT(Ans)
    CHARACTER(1), INTENT(IN) :: char
    LOGICAL(LGT) :: ans
  END FUNCTION isWhiteChar_char
END INTERFACE isWhiteChar

!----------------------------------------------------------------------------
!                                                  isBlank@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary:         Returns true of the entire string is blank

INTERFACE isBlank
  MODULE PURE FUNCTION isBlank_chars(chars) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: chars
    LOGICAL(LGT) :: ans
  END FUNCTION isBlank_chars
END INTERFACE isBlank

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

INTERFACE numStrings
  MODULE PURE FUNCTION numStrings_chars(chars) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: chars
    INTEGER(I4B) :: ans
  END FUNCTION numStrings_chars
END INTERFACE numStrings

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
!                                                       strFind@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 sept 2021
! summary: Function returns the indices in a string where substring pattern

INTERFACE strFind
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
!                                                GetPath@StringMethods
!----------------------------------------------------------------------------

INTERFACE GetPath
  MODULE PURE SUBROUTINE GetPath_chars(chars, path)
    CHARACTER(*), INTENT(IN) :: chars
    CHARACTER(*), INTENT(OUT) :: path
  END SUBROUTINE GetPath_chars
END INTERFACE GetPath

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
