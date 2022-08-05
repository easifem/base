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

!----------------------------------------------------------------------------
!                                                    UpperCase@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary:         Returns the upperCase version of chars

INTERFACE
  MODULE PURE FUNCTION UpperCase_char(chars) RESULT(Ans)
    CHARACTER(LEN=*), INTENT(IN) :: chars
    CHARACTER(LEN=LEN(chars)) :: ans
  END FUNCTION UpperCase_char
END INTERFACE

INTERFACE UpperCase
  MODULE PROCEDURE UpperCase_char
END INTERFACE UpperCase

PUBLIC :: UpperCase

!----------------------------------------------------------------------------
!                                                  toUpperCase@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary:         Returns the upperCase version of chars

INTERFACE
  MODULE PURE SUBROUTINE ToUpperCase_Char(chars)
    CHARACTER(LEN=*), INTENT(INOUT) :: chars
  END SUBROUTINE ToUpperCase_Char
END INTERFACE

INTERFACE toUpperCase
  MODULE PROCEDURE ToUpperCase_Char
END INTERFACE toUpperCase

PUBLIC :: toUpperCase

!----------------------------------------------------------------------------
!                                                    LowerCase@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary:         Returns the LowerCase version of chars

INTERFACE
  MODULE PURE FUNCTION LowerCase_char(chars) RESULT(Ans)
    CHARACTER(LEN=*), INTENT(IN) :: chars
    CHARACTER(LEN=LEN(chars)) :: ans
  END FUNCTION LowerCase_char
END INTERFACE

INTERFACE LowerCase
  MODULE PROCEDURE LowerCase_char
END INTERFACE LowerCase

PUBLIC :: LowerCase

!----------------------------------------------------------------------------
!                                                  toLowerCase@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary:         Returns the LowerCase version of chars

INTERFACE
  MODULE PURE SUBROUTINE ToLowerCase_Char(chars)
    CHARACTER(LEN=*), INTENT(INOUT) :: chars
  END SUBROUTINE ToLowerCase_Char
END INTERFACE

INTERFACE toLowerCase
  MODULE PROCEDURE ToLowerCase_Char
END INTERFACE toLowerCase

PUBLIC :: toLowerCase

!----------------------------------------------------------------------------
!                                                  isWhiteChar@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary:         Returns true if the char is a space(32) or a tab(9).

INTERFACE
  MODULE PURE FUNCTION isWhiteChar_char(char) RESULT(Ans)
    CHARACTER(LEN=1), INTENT(IN) :: char
    LOGICAL(LGT) :: ans
  END FUNCTION isWhiteChar_char
END INTERFACE

INTERFACE isWhiteChar
  MODULE PROCEDURE isWhiteChar_char
END INTERFACE isWhiteChar

PUBLIC :: isWhiteChar

!----------------------------------------------------------------------------
!                                                  isBlank@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2021
! summary:         Returns true of the entire string is blank

INTERFACE
  MODULE PURE FUNCTION isBlank_chars(chars) RESULT(Ans)
    CHARACTER(LEN=*), INTENT(IN) :: chars
    LOGICAL(LGT) :: ans
  END FUNCTION isBlank_chars
END INTERFACE

INTERFACE isBlank
  MODULE PROCEDURE isBlank_chars
END INTERFACE isBlank

PUBLIC :: isBlank

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

INTERFACE
  MODULE PURE FUNCTION numStrings_chars(chars) RESULT(Ans)
    CHARACTER(LEN=*), INTENT(IN) :: chars
    INTEGER(I4B) :: ans
  END FUNCTION numStrings_chars
END INTERFACE

INTERFACE numStrings
  MODULE PROCEDURE numStrings_chars
END INTERFACE numStrings

PUBLIC :: numStrings

!----------------------------------------------------------------------------
!                                                   nmatchstr@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 sept 2021
! summary: Returns the total number of times the substring pattern is
! found in the main string

INTERFACE
  MODULE PURE FUNCTION numMatchStr_chars(chars, pattern) RESULT(Ans)
    CHARACTER(LEN=*), INTENT(IN) :: chars
    CHARACTER(LEN=*), INTENT(IN) :: pattern
    INTEGER(I4B) :: ans
  END FUNCTION numMatchStr_chars
END INTERFACE

INTERFACE numMatchStr
  MODULE PROCEDURE numMatchStr_chars
END INTERFACE numMatchStr

PUBLIC :: numMatchStr

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

INTERFACE
  MODULE PURE FUNCTION isPresent_chars(chars, pattern) RESULT(Ans)
    CHARACTER(LEN=*), INTENT(IN) :: chars
    CHARACTER(LEN=*), INTENT(IN) :: pattern
    LOGICAL(LGT) :: ans
  END FUNCTION isPresent_chars
END INTERFACE

INTERFACE isPresent
  MODULE PROCEDURE isPresent_chars
END INTERFACE isPresent

PUBLIC :: isPresent

!----------------------------------------------------------------------------
!                                                       strFind@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 sept 2021
! summary: Function returns the indices in a string where substring pattern

INTERFACE
  MODULE PURE SUBROUTINE strFind_chars(chars, pattern, indices)
    CHARACTER(LEN=*), INTENT(IN) :: chars
    CHARACTER(LEN=*), INTENT(IN) :: pattern
    INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: indices(:)
  END SUBROUTINE strFind_chars
END INTERFACE

INTERFACE strFind
  MODULE PROCEDURE strFind_chars
END INTERFACE strFind

PUBLIC :: strFind

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
! repp can be larger than @c findp and as long as the size of string can
! accomodate the increased length of all replacements. Trailing and preceding
! spaces are counted in all strings.
!@endnote

INTERFACE
  MODULE PURE SUBROUTINE FindReplace_chars(chars, findp, repp)
    CHARACTER(LEN=*), INTENT(INOUT) :: chars
    CHARACTER(LEN=*), INTENT(IN) :: findp
    CHARACTER(LEN=*), INTENT(IN) :: repp
  END SUBROUTINE FindReplace_chars
END INTERFACE

INTERFACE FindReplace
  MODULE PROCEDURE FindReplace_chars
END INTERFACE FindReplace

PUBLIC :: FindReplace

!----------------------------------------------------------------------------
!                                                    getField@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 sept 2021
! summary: Replaces a substring pattern with a different substring in a string
!
!# Introduction
! Replaces a substring pattern with a different substring in a string.
! - chars the string which will have substrings replaced.
! - findp the substring pattern to find and replace
! - repp the new substring that will be replace parts of string
!
!@note
! repp can be larger than @c findp and as long as the size of string can
! accomodate the increased length of all replacements. Trailing and preceding
! spaces are counted in all strings.
!@endnote

INTERFACE
  MODULE PURE SUBROUTINE getField_chars(i, chars, field, ierr)
    INTEGER(I4B), INTENT(IN) :: i
    CHARACTER(LEN=*), INTENT(IN) :: chars
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: field
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: ierr
  END SUBROUTINE getField_chars
END INTERFACE

INTERFACE getField
  MODULE PROCEDURE getField_chars
END INTERFACE getField

PUBLIC :: getField

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

INTERFACE
  MODULE PURE SUBROUTINE SlashRep_chars(chars)
    CHARACTER(LEN=*), INTENT(INOUT) :: chars
  END SUBROUTINE SlashRep_chars
END INTERFACE

INTERFACE SlashRep
  MODULE PROCEDURE SlashRep_chars
END INTERFACE SlashRep

PUBLIC :: SlashRep

!----------------------------------------------------------------------------
!                                                getFileParts@StringMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 sept 2021
! summary: Returns the path,filename, and extension
!
!# Introduction
! This routine returns the path, filename, and extension.

INTERFACE
  MODULE PURE SUBROUTINE getFileParts_chars(chars, path, fname, ext)
    CHARACTER(LEN=*), INTENT(IN) :: chars
    CHARACTER(LEN=*), INTENT(OUT) :: path
    CHARACTER(LEN=*), INTENT(OUT) :: fname
    CHARACTER(LEN=*), INTENT(OUT) :: ext
  END SUBROUTINE getFileParts_chars
END INTERFACE

INTERFACE getFileParts
  MODULE PROCEDURE getFileParts_chars
END INTERFACE getFileParts

PUBLIC :: getFileParts

!----------------------------------------------------------------------------
!                                                getPath@StringMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE getPath_chars(chars, path)
    CHARACTER(LEN=*), INTENT(IN) :: chars
    CHARACTER(LEN=*), INTENT(OUT) :: path
  END SUBROUTINE getPath_chars
END INTERFACE

INTERFACE getPath
  MODULE PROCEDURE getPath_chars
END INTERFACE getPath

PUBLIC :: getPath

!----------------------------------------------------------------------------
!                                                getFileName@StringMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE getFileName_chars(chars, fname)
    CHARACTER(LEN=*), INTENT(IN) :: chars
    CHARACTER(LEN=*), INTENT(OUT) :: fname
  END SUBROUTINE getFileName_chars
END INTERFACE

INTERFACE getFileName
  MODULE PROCEDURE getFileName_chars
END INTERFACE getFileName

PUBLIC :: getFileName

!----------------------------------------------------------------------------
!                                           getFileNameExt@StringMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE getFileNameExt_chars(chars, ext)
    CHARACTER(LEN=*), INTENT(IN) :: chars
    CHARACTER(LEN=*), INTENT(OUT) :: ext
  END SUBROUTINE getFileNameExt_chars
END INTERFACE

INTERFACE getFileNameExt
  MODULE PROCEDURE getFileNameExt_chars
END INTERFACE getFileNameExt

PUBLIC :: getFileNameExt

!----------------------------------------------------------------------------
!                                                getExtension@StringMethods
!----------------------------------------------------------------------------

!> author: Dr. Vikas Sharma
!
! This function get the extension from a file
!
! ## Usage
! ```fortran
! call display( getExtension("helloworld.F90") .EQ. "f90", &
! & msg="test1:: ")
! ```

INTERFACE
  MODULE FUNCTION getExtension_chars(char) RESULT(ext)
    CHARACTER(LEN=*), INTENT(IN) :: char
    CHARACTER(7) :: ext
  END FUNCTION
END INTERFACE

INTERFACE getExtension
  MODULE PROCEDURE getExtension_chars
END INTERFACE getExtension

PUBLIC :: getExtension

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE StringUtility