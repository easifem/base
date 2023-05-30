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

SUBMODULE(CSRMatrix_MatrixMarketIO) Methods
USE String_Class, ONLY: String
USE BaseMethod, ONLY: Tostring, ErrorMsg, ToUpperCase, Display, Reallocate
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE parse_first_dataline(aline, intvar, ierr, errmsg)
  CHARACTER(*), INTENT(IN) :: aline
  INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: intvar(:)
  INTEGER(I4B), INTENT(OUT) :: ierr
  CHARACTER(*), INTENT(OUT) :: errmsg
  !
  TYPE(string) :: astr
  TYPE(string), ALLOCATABLE :: tokens(:)
  INTEGER(I4B) :: n, ii
  !
  astr = TRIM(aline)
  CALL astr%split(tokens=tokens)
  IF (.NOT. ALLOCATED(tokens)) THEN
    ierr = -10
    errmsg = "Tokenization failed while parsing first data line"
    RETURN
  END IF
  !
  n = SIZE(tokens)
  CALL reallocate(intvar, n)
  !
  DO ii = 1, n
    intvar(ii) = tokens(ii)%to_number(1_I4B)
  END DO
  !
  ierr = 0
  errmsg = ""
  !
  DEALLOCATE (tokens)
  astr = ""
END SUBROUTINE parse_first_dataline

!----------------------------------------------------------------------------
!                                                               ParseHeader
!----------------------------------------------------------------------------

MODULE PROCEDURE ParseHeader
TYPE(String) :: astr
TYPE(String), ALLOCATABLE :: tokens(:)
INTEGER(I4B) :: n, ii

astr = aline
CALL astr%split(tokens=tokens)

IF (ALLOCATED(tokens)) THEN
  n = SIZE(tokens)
ELSE
  ierr = -10
  errmsg = "Cannot create tokens from header"
  RETURN
END IF

IF (n .EQ. 5) THEN
  h1 = tokens(1)%chars()
  h2 = tokens(2)%chars()
  h3 = tokens(3)%chars()
  h4 = tokens(4)%chars()
  h5 = tokens(5)%chars()
  ierr = 0
  errmsg = ""
  DEALLOCATE (tokens)
  RETURN
ELSE IF (n .GT. 5) THEN
  ierr = n
  errmsg = "Number of tokens are greater than 5"
  DEALLOCATE (tokens)
  RETURN
ELSE IF (n .LT. 5) THEN
  ierr = n
  errmsg = "Number of tokens are "//Tostring(n)//" , which is less than 5; "
  DEALLOCATE (tokens)
  RETURN
END IF

END PROCEDURE ParseHeader

!----------------------------------------------------------------------------
!                                                                 MMRead
!----------------------------------------------------------------------------

MODULE PROCEDURE MMRead
CHARACTER(1024) :: aline, errmsg, iomsg
INTEGER(I4B) :: ierr, iostat, ii
INTEGER(I4B), ALLOCATABLE :: aint_vec(:)
CHARACTER(15) :: h1
CHARACTER(6) :: h2
CHARACTER(10) :: h3
CHARACTER(7) :: h4
CHARACTER(19) :: h5

READ (unitno, "(A)", iostat=iostat, iomsg=iomsg) aline

IF (iostat .NE. 0) THEN
  CALL ErrorMsg( &
    & msg="Error while reading the header of given file", &
    & file=__FILE__, line=__LINE__, &
    & routine="MMRead()", &
    & unitno=stdout)
  RETURN
END IF

CALL ParseHeader(aline, h1, h2, h3, h4, h5, ierr, errmsg)

IF (ierr .NE. 0) THEN
  CALL ErrorMsg( &
    & msg="Error while parsing the header: msg = "//TRIM(errmsg), &
    & file=__FILE__, line=__LINE__, &
    & routine="MMRead()", &
    & unitno=stdout)
  RETURN
END IF

CALL ToUpperCase(h2)

IF (h2 .NE. "MATRIX") THEN
  CALL ErrorMsg( &
  & msg="The second arg of header is "//TRIM(h2)//" it should be matrix.", &
    & file=__FILE__, line=__LINE__, &
    & routine="MMRead()", &
    & unitno=stdout)
  RETURN
END IF

field = TRIM(h4)
CALL ToUpperCase(h4)
IF (h4 .NE. "REAL" .AND. h4 .NE. "INTEGER"  &
  & .AND. h4 .NE. "COMPLEX" .AND. h4 .NE. "PATTERN") THEN
  CALL ErrorMsg( &
    & msg="The fourth arg of header is "//TRIM(h4)//", it should real or &
    & INTEGER.", &
    & file=__FILE__, line=__LINE__, &
    & routine="MMRead() ", &
    & unitno=stdout)
  RETURN
END IF

rep = TRIM(h3)
CALL ToUpperCase(h3)

IF (h3 .NE. "COORDINATE" .AND. h3 .NE. "ARRAY") THEN
  CALL ErrorMsg( &
    & msg="The third arg of header is "//TRIM(h3)// &
    & ", it should coordinate or array.", &
    & file=__FILE__, line=__LINE__, &
    & routine="MMRead()", &
    & unitno=stdout)
  RETURN
END IF

symm = TRIM(h5)
CALL ToUpperCase(h5)

IF (h5 .NE. 'GENERAL' .AND. h5 .NE. 'SYMMETRIC' &
  & .AND. h5 .NE. 'HERMITIAN' &
  & .AND. h5 .NE. 'SKEW-SYMMETRIC') THEN
  CALL ErrorMsg( &
    & msg="The 5th arg of header is "//TRIM(h5)//", &
      & it should be ['geneal', 'symmetric', &
      & 'skew-symmetric'].", &
      & file=__FILE__, line=__LINE__, &
      & routine="MMRead()", &
      & unitno=stdout)
  RETURN
END IF
!
! Read comments
!
DO
  READ (unitno, "(A)", iostat=iostat, iomsg=iomsg) aline

  IF (aline(1:1) .NE. "%") THEN
    EXIT
  END IF

  IF (IS_IOSTAT_END(iostat)) THEN
    EXIT
  ELSE IF (iostat .NE. 0) THEN
    CALL ErrorMsg(&
      & msg="Error while reading comments in file; msg = "//TRIM(iomsg), &
      & file=__FILE__, routine="MMRead()", &
      & line=__LINE__, unitno=stdout)
    RETURN
  END IF

END DO
!
! Read the main data
!
BACKSPACE (unitno)
READ (unitno, "(A)", iostat=iostat, iomsg=iomsg) aline

IF (iostat .NE. 0) THEN
  CALL ErrorMsg(&
    & msg="Error while reading first line of data (after comments); msg = "&
    & //TRIM(iomsg), &
    & file=__FILE__, routine="MMRead()", &
    & line=__LINE__, unitno=stdout)
  RETURN
END IF

CALL parse_first_dataline(aline, aint_vec, ierr, errmsg)

IF (ierr .NE. 0) THEN
  CALL ErrorMsg(&
    & msg="Error while reading first line of data (after comments); msg = " &
    & //TRIM(errmsg), &
    & file=__FILE__, routine="MMRead()", &
    & line=__LINE__, unitno=stdout)
  RETURN
END IF

IF (h3 .EQ. "COORDINATE") THEN
  IF (SIZE(aint_vec) .NE. 3) THEN
    CALL ErrorMsg(&
      & msg="For sparse matrix three args should be defined &
      & in first row of data (below comments)", &
      & file=__FILE__, routine="MMRead()", &
      & line=__LINE__, unitno=stdout)
    RETURN
  END IF
  rows = aint_vec(1)
  cols = aint_vec(2)
  nnz = aint_vec(3)
ELSEIF (h3 .EQ. "ARRAY") THEN
  IF (SIZE(aint_vec) .NE. 2) THEN
    CALL ErrorMsg(&
      & msg="For dense matrix two args should be defined &
      & in first row of data (below comments)", &
      & file=__FILE__, routine="MMRead()", &
      & line=__LINE__, unitno=stdout)
    RETURN
  END IF
  rows = aint_vec(1)
  cols = aint_vec(2)
  nnz = rows * cols
END IF

CALL Reallocate(indx, nnz, jndx, nnz)

IF (h4 .EQ. "REAL") THEN
  IF (.NOT. PRESENT(rval)) THEN
    CALL ErrorMsg(&
          & msg="rval should be present for real field", &
          & file=__FILE__, routine="MMRead()", &
          & line=__LINE__, unitno=stdout)
    RETURN
  END IF
  CALL Reallocate(rval, nnz)
  !
  DO ii = 1, nnz
    READ (unitno, *, iostat=iostat, iomsg=iomsg) indx(ii), jndx(ii), rval(ii)
    IF (IS_IOSTAT_END(iostat)) THEN
      EXIT
    ELSE IF (iostat .NE. 0) THEN
      CALL ErrorMsg(&
        & msg="Error while reading "//Tostring(ii)// &
        & "th nonzero entry from the file; msg = "//TRIM(iomsg), &
        & file=__FILE__, routine="MMRead()", &
        & line=__LINE__, unitno=stdout)
      RETURN
    END IF
  END DO
ELSEIF (h4 .EQ. "INTEGER") THEN
  IF (.NOT. PRESENT(ival)) THEN
    CALL ErrorMsg(&
          & msg="ival should be present for integer field", &
          & file=__FILE__, routine="MMRead()", &
          & line=__LINE__, unitno=stdout)
    RETURN
  END IF
  CALL Reallocate(ival, nnz)
  !
  DO ii = 1, nnz
    READ (unitno, *, iostat=iostat, iomsg=iomsg) indx(ii), jndx(ii), ival(ii)
    IF (IS_IOSTAT_END(iostat)) THEN
      EXIT
    ELSE IF (iostat .NE. 0) THEN
      CALL ErrorMsg(&
        & msg="Error while reading "//Tostring(ii)// &
        & "th nonzero entry from the file; msg = "//TRIM(iomsg), &
        & file=__FILE__, routine="MMRead()", &
        & line=__LINE__, unitno=stdout)
      RETURN
    END IF
  END DO
ELSEIF (h4 .EQ. "COMPLEX") THEN
  CALL ErrorMsg(&
    & msg="Currently complex field is not supported", &
    & file=__FILE__, routine="MMRead()", &
    & line=__LINE__, unitno=stdout)
  RETURN
  !
ELSEIF (h4 .EQ. "pattern") THEN
  DO ii = 1, nnz
    READ (unitno, *, iostat=iostat, iomsg=iomsg) indx(ii), jndx(ii)
    IF (IS_IOSTAT_END(iostat)) THEN
      EXIT
    ELSE IF (iostat .NE. 0) THEN
      CALL ErrorMsg(&
        & msg="Error while reading "//Tostring(ii)// &
        & "th nonzero entry from the file; msg = "//TRIM(iomsg), &
        & file=__FILE__, routine="MMRead()", &
        & line=__LINE__, unitno=stdout)
      RETURN
    END IF
  END DO
END IF

IF (ALLOCATED(aint_vec)) DEALLOCATE (aint_vec)

END PROCEDURE MMRead

END SUBMODULE Methods
