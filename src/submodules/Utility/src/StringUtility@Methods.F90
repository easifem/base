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

SUBMODULE(StringUtility) Methods
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: IOSTAT_END
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 UpperCase
!----------------------------------------------------------------------------

MODULE PROCEDURE UpperCase_Char
ans = chars
CALL ToUpperCase_Char(ans)
END PROCEDURE UpperCase_Char

!----------------------------------------------------------------------------
!                                                                ToUpperCase
!----------------------------------------------------------------------------

MODULE PROCEDURE ToUpperCase_Char
INTEGER(I4B) :: i, diff
CHARACTER(1) :: c

diff = ICHAR('A') - ICHAR('a')
DO i = 1, LEN(chars)
  c = chars(i:i)
  IF (ICHAR(c) .GE. ICHAR('a') .AND. ICHAR(c) <= ICHAR('z')) THEN
    chars(i:i) = CHAR(ICHAR(c) + diff)
  END IF
END DO
END PROCEDURE ToUpperCase_Char

!----------------------------------------------------------------------------
!                                                                 LowerCase
!----------------------------------------------------------------------------

MODULE PROCEDURE LowerCase_Char
ans = chars
CALL ToLowerCase_Char(ans)
END PROCEDURE LowerCase_Char

!----------------------------------------------------------------------------
!                                                                ToLowerCase
!----------------------------------------------------------------------------

MODULE PROCEDURE ToLowerCase_Char
INTEGER(I4B) :: i, diff
CHARACTER(1) :: c
!>
diff = ICHAR('A') - ICHAR('a')
DO i = 1, LEN(chars)
  c = chars(i:i)
  IF (ICHAR(c) .GE. ICHAR('A') .AND. ICHAR(c) .LE. ICHAR('Z')) THEN
    chars(i:i) = CHAR(ICHAR(c) - diff)
  END IF
END DO
END PROCEDURE ToLowerCase_Char

!----------------------------------------------------------------------------
!                                                                isWhiteChar
!----------------------------------------------------------------------------

MODULE PROCEDURE isWhiteChar_char
INTEGER(I4B) :: ia
ia = IACHAR(char)
IF (ia .EQ. 32 .OR. ia .EQ. 9) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE isWhiteChar_char

!----------------------------------------------------------------------------
!                                                                  isBlank
!----------------------------------------------------------------------------

MODULE PROCEDURE isBlank_chars
INTEGER(I4B) :: i, j
j = 0
ans = .TRUE.
DO i = 1, LEN(chars)
  IF (.NOT. isWhiteChar(chars(i:i))) THEN
    ans = .FALSE.
    EXIT
  END IF
END DO
END PROCEDURE isBlank_chars

!----------------------------------------------------------------------------
!                                                               numMatchStr
!----------------------------------------------------------------------------

MODULE PROCEDURE numMatchStr_chars
INTEGER(I4B) :: i
ans = 0
DO i = 1, LEN(chars)
  IF (i + LEN(pattern) - 1 > LEN(chars)) EXIT
  IF (chars(i:i + LEN(pattern) - 1) == pattern) ans = ans + 1
END DO
END PROCEDURE numMatchStr_chars

!----------------------------------------------------------------------------
!                                                                numStrings
!----------------------------------------------------------------------------

MODULE PROCEDURE numStrings_chars
INTEGER(I4B) :: i, multidcol, n, ncol, nmult, ioerr
LOGICAL(LGT) :: nonblankd, nonblank, multidata, inQuotes

!Check for single-quoted strings, if the number of single quotes is odd
!then return with a value of -1 to signal an error
IF (MOD(numMatchStr(TRIM(chars), "'"), 2) /= 0) THEN
  ans = -1
  RETURN
END IF

!Check for double-quoted strings, if the number of double quotes is odd
!then return with a value of -2 to signal an error
IF (MOD(numMatchStr(TRIM(chars), '"'), 2) /= 0) THEN
  ans = -2
  RETURN
END IF

nonblankd = .FALSE.
multidata = .FALSE.
inQuotes = .FALSE.
ncol = LEN_TRIM(chars)
IF (ncol > 2) THEN
  ans = 1
ELSE
  ans = 0
END IF

n = 0
DO i = ncol, 1, -1
  IF (chars(i:i) == "'" .OR. chars(i:i) == '"') THEN
    IF (inQuotes) THEN
      inQuotes = .FALSE.
      n = n + 2
      CYCLE
    ELSE
      inQuotes = .TRUE.
    END IF
  END IF
  !Process the spaces and multiplier characters if not in a quoted string
  IF (.NOT. inQuotes) THEN
    IF (chars(i:i) == ' ' .OR. ICHAR(chars(i:i)) == 9) THEN !ichar(tab)=9
      nonblank = .FALSE.
    ELSE
      IF (chars(i:i) == '*') THEN
        multidata = .TRUE.
        multidcol = i
      END IF
      nonblank = .TRUE.
    END IF
    IF ((.NOT. nonblankd .AND. nonblank) .OR. &
        (nonblankd .AND. .NOT. nonblank)) THEN
      n = n + 1
    END IF
    IF (multidata .AND. (nonblankd .AND. .NOT. nonblank)) THEN
      !ioerr will be non-zero if the sub-string is not an integer
      READ (chars(i + 1:multidcol - 1), *, IOSTAT=ioerr) nmult
      IF (ioerr /= 0) nmult = 1
      n = n + (nmult - 1) * 2

      !If we are multiplying a quoted string need to subtract 1.
      IF (multidcol < ncol) THEN
        IF (chars(multidcol + 1:multidcol + 1) == '"' .OR. &
            chars(multidcol + 1:multidcol + 1) == "'") &
          n = n - 1
      END IF

      multidata = .FALSE.
    END IF
    nonblankd = nonblank
  END IF
END DO
IF (MOD(n, 2) /= 0) THEN
  ans = n / 2 + 1
ELSE
  ans = n / 2
END IF
END PROCEDURE numStrings_chars

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE isPresent_chars
ans = MERGE(INDEX(chars, pattern) > 0, .FALSE., &
  & (LEN(pattern) > 0 .AND. LEN(chars) > 0))
END PROCEDURE isPresent_chars

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE strFind_chars
INTEGER(I4B) :: i, n
n = 0
ALLOCATE (indices(numMatchStr(chars, pattern)))
DO i = 1, LEN(chars)
  IF (i + LEN(pattern) - 1 > LEN(chars)) EXIT
  IF (chars(i:i + LEN(pattern) - 1) == pattern) THEN
    n = n + 1
    indices(n) = i
  END IF
END DO
END PROCEDURE strFind_chars

!----------------------------------------------------------------------------
!                                                        FindReplace
!----------------------------------------------------------------------------

MODULE PROCEDURE FindReplace_chars
CHARACTER(LEN(chars)) :: string2
INTEGER(I4B), ALLOCATABLE :: indices(:)
INTEGER(I4B) :: i, n, stt, stp, dlen, slen, rlen, flen, tlen
!>
slen = LEN(chars)
tlen = LEN_TRIM(chars)
rlen = LEN(repp)
flen = LEN(findp)
dlen = rlen - flen
string2 = chars
n = numMatchStr(chars, findp)
CALL strfind(chars, findp, indices)
IF (slen >= tlen + n * dlen) THEN
  DO i = 1, n
    stt = indices(i)
    stp = stt + rlen - 1
    chars(stt:stp) = repp
    chars(stp + 1:slen) = string2(stt + flen - (i - 1) * dlen:slen)
    IF (i < n) indices(i + 1) = indices(i + 1) + dlen * i
  END DO
END IF
DEALLOCATE (indices)
END PROCEDURE FindReplace_chars

!----------------------------------------------------------------------------
!                                                                  getField
!----------------------------------------------------------------------------

MODULE PROCEDURE getField_chars
INTEGER(I4B) :: j, ioerr, nf
CHARACTER(LEN(chars)) :: temp, temp2

temp = chars
temp2 = ''
nf = numStrings(temp)
IF (0 < i .AND. i <= nf) THEN
  !The fortran READ(*,*) parses at the '/' character
  !we don't want this to occur. We only want it to parse for '*'
  !and ' ' characters. So if slashes are present we treat things
  !differently.
  IF (isPresent(temp, CHAR_FSLASH)) THEN
    !Temporarily change the CHAR_FSLASH character to a BSLASH character
    !to get correct parsing behavior
    CALL FindReplace(temp, CHAR_FSLASH, CHAR_BSLASH)
    READ (temp, *, IOSTAT=ioerr) (temp2, j=1, i)
    CALL FindReplace(temp, CHAR_BSLASH, CHAR_FSLASH)
    CALL FindReplace(temp2, CHAR_BSLASH, CHAR_FSLASH)
  ELSE
    READ (temp, *, IOSTAT=ioerr) (temp2, j=1, i)
  END IF
  field = TRIM(temp2)
  IF (PRESENT(ierr)) ierr = ioerr
ELSE
  IF (PRESENT(ierr)) ierr = IOSTAT_END
END IF
END PROCEDURE getField_chars

!----------------------------------------------------------------------------
!                                                                  SlashRep
!----------------------------------------------------------------------------

MODULE PROCEDURE SlashRep_chars
INTEGER(I4B) :: i
DO i = 1, LEN_TRIM(chars)
#ifdef WIN32
  IF (chars(i:i) == CHAR_FSLASH) chars(i:i) = CHAR_SLASH
#else
  IF (chars(i:i) == CHAR_BSLASH) chars(i:i) = CHAR_SLASH
#endif
END DO
END PROCEDURE SlashRep_chars

!----------------------------------------------------------------------------
!                                                              getFileParts
!----------------------------------------------------------------------------

MODULE PROCEDURE getFileParts_chars
INTEGER(I4B) :: i
CALL getPath_chars(chars, path)
CALL getFileName_chars(chars, fname)
DO i = LEN_TRIM(fname), 1, -1
  IF (fname(i:i) .EQ. CHAR_DOT) THEN
    fname = fname(1:i - 1)
    EXIT
  END IF
END DO
CALL getFileNameExt_chars(chars, ext)
END PROCEDURE getFileParts_chars

!----------------------------------------------------------------------------
!                                                                   getPath
!----------------------------------------------------------------------------

MODULE PROCEDURE getPath_chars
CHARACTER(LEN(chars)) :: chars2
INTEGER(I4B) :: i
!>
chars2 = chars
CALL SlashRep(chars2)
path = ''
DO i = LEN_TRIM(chars2), 1, -1
  IF (chars2(i:i) .EQ. CHAR_SLASH) THEN
    path = chars2(1:i)
    EXIT
  END IF
END DO
END PROCEDURE getPath_chars

!----------------------------------------------------------------------------
!                                                               getExtension
!----------------------------------------------------------------------------

MODULE PROCEDURE getExtension_chars
! Define internal variables
INTEGER(I4B) :: n, m
ext = "       "
n = 0
n = INDEX(char, ".", back=.TRUE.)
m = LEN(char)
ext(1:m - n + 1) = CHAR(n + 1:m)
END PROCEDURE getExtension_chars

!----------------------------------------------------------------------------
!                                                              getFileName
!----------------------------------------------------------------------------

MODULE PROCEDURE getFileName_chars
CHARACTER(LEN(chars)) :: chars2
INTEGER(I4B) :: i
chars2 = chars
CALL SlashRep(chars2)
fname = chars
DO i = LEN_TRIM(chars2), 1, -1
  IF (chars2(i:i) .EQ. CHAR_SLASH) THEN
    fname = chars2(i + 1:LEN_TRIM(chars2))
    EXIT
  END IF
END DO
END PROCEDURE getFileName_chars

!----------------------------------------------------------------------------
!                                                            getFileNameExt
!----------------------------------------------------------------------------

MODULE PROCEDURE getFileNameExt_chars
CHARACTER(LEN(chars)) :: chars2
INTEGER(I4B) :: i, SLASHloc

chars2 = chars
CALL SlashRep(chars2)
ext = ''
SLASHloc = 1
DO i = LEN_TRIM(chars2), 1, -1
  IF (chars2(i:i) == CHAR_SLASH) THEN
    SLASHloc = i
    EXIT
  END IF
END DO
DO i = LEN_TRIM(chars2), SLASHloc, -1
  IF (chars2(i:i) == CHAR_DOT) THEN
    ext = chars2(i:LEN_TRIM(chars2))
    EXIT
  END IF
END DO
END PROCEDURE getFileNameExt_chars

END SUBMODULE Methods
