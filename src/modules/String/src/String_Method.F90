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
! summary: Additional String Methods

MODULE String_Method
USE String_Class, ONLY: repeat, string
IMPLICIT NONE
PRIVATE
! expose StingiFor new procedures
PUBLIC :: read_file, read_lines, write_file, write_lines

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE read_file(file, lines, form, iostat, iomsg)
  !< Read a file as a single string stream.
  !<
  !< The lines are returned as an array of strings that are read until the eof is reached.
  !< The line is read as an ascii stream read until the eor is reached.
  !<
  !< @note For unformatted read only `access='stream'` is supported with new_line as line terminator.
  !<
  !<```fortran
  !< type(string)              :: astring
  !< type(string), allocatable :: strings(:)
  !< type(string)              :: line(3)
  !< integer                   :: iostat
  !< character(99)         :: iomsg
  !< integer                   :: scratch
  !< integer                   :: l
  !< logical                   :: test_passed(8)
  !< line(1) = ' Hello World!   '
  !< line(2) = 'How are you?  '
  !< line(3) = '   All say: "Fine thanks"'
  !< open(newunit=scratch, file='read_file_test.tmp')
  !< write(scratch, "(A)") line(1)%chars()
  !< write(scratch, "(A)") line(2)%chars()
  !< write(scratch, "(A)") line(3)%chars()
  !< close(scratch)
  !< call read_file(file='read_file_test.tmp', lines=strings, iostat=iostat, iomsg=iomsg)
  !< test_passed(1) = (size(strings, dim=1)==size(line, dim=1))
  !< do l=1, size(strings, dim=1)
  !<   test_passed(l+1) = (strings(l)==line(l))
  !< enddo
  !< open(newunit=scratch, file='read_file_test.tmp', form='UNFORMATTED', access='STREAM')
  !< write(scratch) line(1)%chars()//new_line('a')
  !< write(scratch) line(2)%chars()//new_line('a')
  !< write(scratch) line(3)%chars()//new_line('a')
  !< close(scratch)
  !< call read_file(file='read_file_test.tmp', lines=strings, form='unformatted', iostat=iostat, iomsg=iomsg)
  !< test_passed(5) = (size(strings, dim=1)==size(line, dim=1))
  !< do l=1, size(strings, dim=1)
  !<   test_passed(l+5) = (strings(l)==line(l))
  !< enddo
  !< open(newunit=scratch, file='read_file_test.tmp', form='UNFORMATTED', access='STREAM')
  !< close(scratch, status='DELETE')
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: file !< File name.
  TYPE(string), INTENT(out), ALLOCATABLE :: lines(:) !< The lines.
  CHARACTER(*), INTENT(in), OPTIONAL :: form !< Format of unit.
  INTEGER, INTENT(out), OPTIONAL :: iostat !< IO status code.
  CHARACTER(*), INTENT(inout), OPTIONAL :: iomsg !< IO status message.
  TYPE(string) :: form_ !< Format of unit, local variable.
  INTEGER :: iostat_ !< IO status code, local variable.
  CHARACTER(:), ALLOCATABLE :: iomsg_ !< IO status message, local variable.
  INTEGER :: unit !< Logical unit.
  LOGICAL :: does_exist !< Check if file exist.

  iomsg_ = REPEAT(' ', 99); IF (PRESENT(iomsg)) iomsg_ = iomsg
  INQUIRE (file=file, iomsg=iomsg_, iostat=iostat_, exist=does_exist)
  IF (does_exist) THEN
    form_ = 'FORMATTED'; IF (PRESENT(form)) form_ = form; form_ = form_%upper()
    SELECT CASE (form_%chars())
    CASE ('FORMATTED')
            open (newunit=unit, file=file, status='OLD', action='READ', iomsg=iomsg_, iostat=iostat_, err=10)
    CASE ('UNFORMATTED')
            open (newunit=unit, file=file, status='OLD', action='READ', form='UNFORMATTED', access='STREAM', &
            iomsg=iomsg_, iostat=iostat_, err=10)
    END SELECT
    CALL read_lines(unit=unit, lines=lines, form=form, iomsg=iomsg_, &
      & iostat=iostat_)
10  CLOSE (unit)
  END IF
  IF (PRESENT(iostat)) iostat = iostat_
  IF (PRESENT(iomsg)) iomsg = iomsg_
END SUBROUTINE read_file

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE read_lines(unit, lines, form, iostat, iomsg)
  !< Read lines (records) from a connected-formatted unit.
  !<
  !< @note The connected unit is rewinded. At a successful exit current record is at eof, at the beginning otherwise.
  !<
  !< The lines are returned as an array of strings that are read until the eof is reached.
  !< The line is read as an ascii stream read until the eor is reached.
  !<
  !< @note For unformatted read only `access='stream'` is supported with new_line as line terminator.
  !<
  !< @note There is no doctests, this being tested by means of [[read_file]] doctests.
  INTEGER, INTENT(in) :: unit !< Logical unit.
  TYPE(string), INTENT(out), ALLOCATABLE :: lines(:) !< The lines.
  CHARACTER(*), INTENT(in), OPTIONAL :: form !< Format of unit.
  INTEGER, INTENT(out), OPTIONAL :: iostat !< IO status code.
  CHARACTER(*), INTENT(inout), OPTIONAL :: iomsg !< IO status message.
  TYPE(string) :: form_ !< Format of unit, local variable.
  INTEGER :: iostat_ !< IO status code, local variable.
  CHARACTER(:), ALLOCATABLE :: iomsg_ !< IO status message, local variable.
  CHARACTER(1) :: ch !< Character storage.
  INTEGER :: l !< Counter.

  form_ = 'FORMATTED'; IF (PRESENT(form)) form_ = form; form_ = form_%upper()
  iomsg_ = REPEAT(' ', 99); IF (PRESENT(iomsg)) iomsg_ = iomsg
  REWIND (unit)
  SELECT CASE (form_%chars())
  CASE ('FORMATTED')
    l = 0
    DO
      READ (unit, *, err=10, END=10)
      l = l + 1
    END DO
  CASE ('UNFORMATTED')
    l = 0
    DO
      READ (unit, err=10, END=10) ch
      IF (ch == NEW_LINE('a')) l = l + 1
    END DO
  END SELECT
10 REWIND (unit)
  IF (l > 0) THEN
    ALLOCATE (lines(1:l))
    l = 1
    iostat_ = 0
    DO
   CALL lines(l)%read_line(unit=unit, form=form, iostat=iostat_, iomsg=iomsg_)
            if ((iostat_ /= 0 .and. .not. is_iostat_eor(iostat_)) .or. (l >= size(lines, dim=1))) then
        EXIT
      END IF
      l = l + 1
    END DO
  END IF
  IF (PRESENT(iostat)) iostat = iostat_
  IF (PRESENT(iomsg)) iomsg = iomsg_
END SUBROUTINE read_lines

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE write_file(file, lines, form, iostat, iomsg)
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
  !< character(99)         :: iomsg
  !< integer                   :: scratch
  !< integer                   :: l
  !< logical                   :: test_passed(8)
  !< line(1) = ' Hello World!   '
  !< line(2) = 'How are you?  '
  !< line(3) = '   All say: "Fine thanks"'
  !< anotherstring = anotherstring%join(array=line, sep=new_line('a'))
  !< call write_file(file='write_file_test.tmp', lines=line, iostat=iostat, iomsg=iomsg)
  !< call astring%read_file(file='write_file_test.tmp', iostat=iostat, iomsg=iomsg)
  !< call astring%split(tokens=strings, sep=new_line('a'))
  !< test_passed(1) = (size(strings, dim=1)==size(line, dim=1))
  !< do l=1, size(strings, dim=1)
  !<   test_passed(l+1) = (strings(l)==line(l))
  !< enddo
  !< call write_file(file='write_file_test.tmp', lines=line, form='unformatted', iostat=iostat, iomsg=iomsg)
  !< call astring%read_file(file='write_file_test.tmp', form='unformatted', iostat=iostat, iomsg=iomsg)
  !< call astring%split(tokens=strings, sep=new_line('a'))
  !< test_passed(5) = (size(strings, dim=1)==size(line, dim=1))
  !< do l=1, size(strings, dim=1)
  !<   test_passed(l+5) = (strings(l)==line(l))
  !< enddo
  !< open(newunit=scratch, file='write_file_test.tmp')
  !< close(scratch, status='DELETE')
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  CHARACTER(*), INTENT(in) :: file !< File name.
  TYPE(string), INTENT(in) :: lines(1:) !< The lines.
  CHARACTER(*), INTENT(in), OPTIONAL :: form !< Format of unit.
  INTEGER, INTENT(out), OPTIONAL :: iostat !< IO status code.
  CHARACTER(*), INTENT(inout), OPTIONAL :: iomsg !< IO status message.
  TYPE(string) :: form_ !< Format of unit, local variable.
  INTEGER :: iostat_ !< IO status code, local variable.
  CHARACTER(:), ALLOCATABLE :: iomsg_ !< IO status message, local variable.
  INTEGER :: unit !< Logical unit.

  iomsg_ = REPEAT(' ', 99); IF (PRESENT(iomsg)) iomsg_ = iomsg
  form_ = 'FORMATTED'; IF (PRESENT(form)) form_ = form; form_ = form_%upper()
  SELECT CASE (form_%chars())
  CASE ('FORMATTED')
         open (newunit=unit, file=file, action='WRITE', iomsg=iomsg_, iostat=iostat_, err=10)
  CASE ('UNFORMATTED')
         open (newunit=unit, file=file, action='WRITE', form='UNFORMATTED', access='STREAM', iomsg=iomsg_, iostat=iostat_, err=10)
  END SELECT
      call write_lines(unit=unit, lines=lines, form=form, iomsg=iomsg_, iostat=iostat_)
10 CLOSE (unit)
  IF (PRESENT(iostat)) iostat = iostat_
  IF (PRESENT(iomsg)) iomsg = iomsg_
END SUBROUTINE write_file

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE write_lines(unit, lines, form, iostat, iomsg)
  !< Write lines (records) to a connected-formatted unit.
  !<
  !< @note There is no doctests, this being tested by means of [[write_file]] doctests.
  INTEGER, INTENT(in) :: unit !< Logical unit.
  TYPE(string), INTENT(in) :: lines(1:) !< The lines.
  CHARACTER(*), INTENT(in), OPTIONAL :: form !< Format of unit.
  INTEGER, INTENT(out), OPTIONAL :: iostat !< IO status code.
  CHARACTER(*), INTENT(inout), OPTIONAL :: iomsg !< IO status message.
  INTEGER :: l !< Counter.

  DO l = 1, SIZE(lines, dim=1)
    CALL lines(l)%write_line(unit=unit, form=form, iostat=iostat, iomsg=iomsg)
  END DO
END SUBROUTINE write_lines
END MODULE String_Method
