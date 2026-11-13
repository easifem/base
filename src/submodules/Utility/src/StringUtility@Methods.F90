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
USE GlobalData, ONLY: CHAR_BSLASH, CHAR_DOT, CHAR_FSLASH, CHAR_SLASH
USE ISO_FORTRAN_ENV, ONLY: IOSTAT_END

IMPLICIT NONE

! the file separators; on macOS and Linux both are /
CHARACTER(*), PARAMETER :: SEPS = CHAR_SLASH//CHAR_FSLASH

CONTAINS

!----------------------------------------------------------------------------
!                                                                  Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE StringVectorDeallocate
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

isok = ALLOCATED(obj)

IF (.NOT. isok) RETURN

tsize = SIZE(obj)
DO ii = 1, tsize
  obj(ii) = ""
END DO
DEALLOCATE (obj)
END PROCEDURE StringVectorDeallocate

!----------------------------------------------------------------------------
!                                                                 PathDir
!----------------------------------------------------------------------------

MODULE PROCEDURE PathDir
INTEGER(I4B) :: n, k

n = LEN_TRIM(path)
IF (n .EQ. 0) THEN
  ans = "."
  RETURN
END IF

! remove trailing separators, but keep a root separator
DO WHILE (n .GT. 1)
  IF (.NOT. IsSep(path(n:n))) EXIT
  n = n - 1
END DO

IF (n .EQ. 1 .AND. IsSep(path(1:1))) THEN
  ans = path(1:1)
  RETURN
END IF

k = SCAN(path(1:n), SEPS, back=.TRUE.)

! no separator in the path
IF (k .EQ. 0) THEN
  ans = "."
  RETURN
END IF

! remove the separators before the last element
DO WHILE (k .GT. 1)
  IF (.NOT. IsSep(path(k - 1:k - 1))) EXIT
  k = k - 1
END DO

! /abc type pattern
IF (k .EQ. 1) THEN
  ans = path(1:1)
  RETURN
END IF

ans = path(1:k - 1)
END PROCEDURE PathDir

!----------------------------------------------------------------------------
!                                                                 PathBase
!----------------------------------------------------------------------------

MODULE PROCEDURE PathBase
INTEGER(I4B) :: n, k

n = LEN_TRIM(path)
IF (n .EQ. 0) THEN
  ans = "."
  RETURN
END IF

! remove trailing separators, but keep a root separator
DO WHILE (n .GT. 1)
  IF (.NOT. IsSep(path(n:n))) EXIT
  n = n - 1
END DO

k = 0
IF (n .GT. 1) k = SCAN(path(1:n), SEPS, back=.TRUE.)
ans = path(k + 1:n)
END PROCEDURE PathBase

!----------------------------------------------------------------------------
!                                                                 PathJoin
!----------------------------------------------------------------------------

MODULE PROCEDURE PathJoin1
ans = JoinTwo(TRIM(path1), TRIM(path2))
END PROCEDURE PathJoin1

!----------------------------------------------------------------------------
!                                                                 PathJoin
!----------------------------------------------------------------------------

MODULE PROCEDURE PathJoin2
INTEGER(I4B) :: ii

ans = ""
DO ii = 1, SIZE(paths)
  ans = JoinTwo(ans, paths(ii)%chars())
END DO
END PROCEDURE PathJoin2

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
IF (LEN(pattern) .EQ. 0) RETURN
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
    END IF

    inQuotes = .TRUE.
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
            chars(multidcol + 1:multidcol + 1) == "'") THEN
          n = n - 1
        END IF
      END IF

      multidata = .FALSE.
    END IF
    nonblankd = nonblank
  END IF
END DO

! the string starts with a word: treat position 0 as a blank, so that a
! repeat count at the start is expanded as well
IF (nonblankd .AND. .NOT. inQuotes) THEN
  n = n + 1
  IF (multidata) THEN
    READ (chars(1:multidcol - 1), *, IOSTAT=ioerr) nmult
    IF (ioerr /= 0) nmult = 1
    n = n + (nmult - 1) * 2
    IF (multidcol < ncol) THEN
      IF (chars(multidcol + 1:multidcol + 1) == '"' .OR. &
          chars(multidcol + 1:multidcol + 1) == "'") THEN
        n = n - 1
      END IF
    END IF
  END IF
END IF

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
IF (LEN(pattern) .EQ. 0) RETURN
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
CHARACTER(:), ALLOCATABLE :: res
INTEGER(I4B) :: ii, slen, flen

slen = LEN(chars)
flen = LEN(findp)
IF (PRESENT(isok)) isok = .TRUE.
IF (flen .EQ. 0) RETURN

! scan from left to right; after a match continue after it, so
! overlapping matches such as "aa" in "aaa" are replaced once
res = ''
ii = 1
DO WHILE (ii .LE. slen)
  IF (ii + flen - 1 .LE. slen) THEN
    IF (chars(ii:ii + flen - 1) .EQ. findp) THEN
      res = res//repp
      ii = ii + flen
      CYCLE
    END IF
  END IF
  res = res//chars(ii:ii)
  ii = ii + 1
END DO

! if the result does not fit, chars is not changed
IF (LEN_TRIM(res) .LE. slen) THEN
  chars = res
ELSE
  IF (PRESENT(isok)) isok = .FALSE.
END IF
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
  field = ""
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
INTEGER(I4B) :: n, k, d

! only the last element of the path is searched for a dot
n = LEN_TRIM(char)
k = SCAN(char(1:n), SEPS, back=.TRUE.)
d = INDEX(char(k + 1:n), CHAR_DOT, back=.TRUE.)
IF (d .EQ. 0) THEN
  ext = ""
ELSE
  ext = char(k + d + 1:n)
END IF
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
CHARACTER(:), ALLOCATABLE :: chars2
INTEGER(I4B) :: i, SLASHloc

chars2 = TRIM(chars)
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

!----------------------------------------------------------------------------
!                                                                     IsSep
!----------------------------------------------------------------------------

! true if c is a file separator; on Windows both / and \ are separators
PURE FUNCTION IsSep(c) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: c
  LOGICAL(LGT) :: ans
  ans = (c .EQ. CHAR_SLASH) .OR. (c .EQ. CHAR_FSLASH)
END FUNCTION IsSep

!----------------------------------------------------------------------------
!                                                                   JoinTwo
!----------------------------------------------------------------------------

! join a and b with exactly one separator; an empty part is skipped
PURE FUNCTION JoinTwo(a, b) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: a
  CHARACTER(*), INTENT(IN) :: b
  CHARACTER(:), ALLOCATABLE :: ans
  INTEGER(I4B) :: na, ib

  IF (LEN(a) .EQ. 0) THEN
    ans = b
    RETURN
  END IF

  IF (LEN(b) .EQ. 0) THEN
    ans = a
    RETURN
  END IF

  ! separators at the end of a, but keep a root separator
  na = LEN(a)
  DO WHILE (na .GT. 1)
    IF (.NOT. IsSep(a(na:na))) EXIT
    na = na - 1
  END DO

  ! separators at the start of b
  ib = VERIFY(b, SEPS)
  IF (ib .EQ. 0) THEN
    ans = a(1:na)
    RETURN
  END IF

  IF (IsSep(a(na:na))) THEN
    ans = a(1:na)//b(ib:)
  ELSE
    ans = a(1:na)//CHAR_SLASH//b(ib:)
  END IF
END FUNCTION JoinTwo

END SUBMODULE Methods
