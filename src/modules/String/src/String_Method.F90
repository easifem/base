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

module String_Method
use String_Class, only: repeat, CK, string
implicit none
private
! expose StingiFor new procedures
public :: read_file, read_lines, write_file, write_lines

contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine read_file(file, lines, form, iostat, iomsg)
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
  !< character(len=99)         :: iomsg
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
  character(len=*), intent(in) :: file       !< File name.
  type(string), intent(out), allocatable :: lines(:)   !< The lines.
  character(len=*), intent(in), optional :: form       !< Format of unit.
  integer, intent(out), optional :: iostat     !< IO status code.
  character(len=*), intent(inout), optional :: iomsg      !< IO status message.
  type(string) :: form_      !< Format of unit, local variable.
  integer :: iostat_    !< IO status code, local variable.
  character(len=:), allocatable :: iomsg_     !< IO status message, local variable.
  integer :: unit       !< Logical unit.
  logical :: does_exist !< Check if file exist.

  iomsg_ = repeat(' ', 99); if (present(iomsg)) iomsg_ = iomsg
  inquire (file=file, iomsg=iomsg_, iostat=iostat_, exist=does_exist)
  if (does_exist) then
    form_ = 'FORMATTED'; if (present(form)) form_ = form; form_ = form_ % upper()
    select case (form_ % chars())
    case ('FORMATTED')
            open (newunit=unit, file=file, status='OLD', action='READ', iomsg=iomsg_, iostat=iostat_, err=10)
    case ('UNFORMATTED')
            open (newunit=unit, file=file, status='OLD', action='READ', form='UNFORMATTED', access='STREAM', &
            iomsg=iomsg_, iostat=iostat_, err=10)
    end select
      call read_lines(unit=unit, lines=lines, form=form, iomsg=iomsg_, &
        & iostat=iostat_)
10  close (unit)
  end if
  if (present(iostat)) iostat = iostat_
  if (present(iomsg)) iomsg = iomsg_
end subroutine read_file

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine read_lines(unit, lines, form, iostat, iomsg)
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
  integer, intent(in) :: unit     !< Logical unit.
  type(string), intent(out), allocatable :: lines(:) !< The lines.
  character(len=*), intent(in), optional :: form     !< Format of unit.
  integer, intent(out), optional :: iostat   !< IO status code.
  character(len=*), intent(inout), optional :: iomsg    !< IO status message.
  type(string) :: form_    !< Format of unit, local variable.
  integer :: iostat_  !< IO status code, local variable.
  character(len=:), allocatable :: iomsg_   !< IO status message, local variable.
  character(kind=CK, len=1) :: ch       !< Character storage.
  integer :: l        !< Counter.

  form_ = 'FORMATTED'; if (present(form)) form_ = form; form_ = form_ % upper()
  iomsg_ = repeat(' ', 99); if (present(iomsg)) iomsg_ = iomsg
  rewind (unit)
  select case (form_ % chars())
  case ('FORMATTED')
    l = 0
    do
      read (unit, *, err=10, end=10)
      l = l + 1
    end do
  case ('UNFORMATTED')
    l = 0
    do
      read (unit, err=10, end=10) ch
      if (ch == new_line('a')) l = l + 1
    end do
  end select
10 rewind (unit)
  if (l > 0) then
    allocate (lines(1:l))
    l = 1
    iostat_ = 0
    do
 call lines(l) % read_line(unit=unit, form=form, iostat=iostat_, iomsg=iomsg_)
            if ((iostat_ /= 0 .and. .not. is_iostat_eor(iostat_)) .or. (l >= size(lines, dim=1))) then
        exit
      end if
      l = l + 1
    end do
  end if
  if (present(iostat)) iostat = iostat_
  if (present(iomsg)) iomsg = iomsg_
end subroutine read_lines

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine write_file(file, lines, form, iostat, iomsg)
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
  character(len=*), intent(in) :: file      !< File name.
  type(string), intent(in) :: lines(1:) !< The lines.
  character(len=*), intent(in), optional :: form      !< Format of unit.
  integer, intent(out), optional :: iostat    !< IO status code.
  character(len=*), intent(inout), optional :: iomsg     !< IO status message.
  type(string) :: form_     !< Format of unit, local variable.
  integer :: iostat_   !< IO status code, local variable.
  character(len=:), allocatable :: iomsg_    !< IO status message, local variable.
  integer :: unit      !< Logical unit.

  iomsg_ = repeat(' ', 99); if (present(iomsg)) iomsg_ = iomsg
  form_ = 'FORMATTED'; if (present(form)) form_ = form; form_ = form_ % upper()
  select case (form_ % chars())
  case ('FORMATTED')
         open (newunit=unit, file=file, action='WRITE', iomsg=iomsg_, iostat=iostat_, err=10)
  case ('UNFORMATTED')
         open (newunit=unit, file=file, action='WRITE', form='UNFORMATTED', access='STREAM', iomsg=iomsg_, iostat=iostat_, err=10)
  end select
      call write_lines(unit=unit, lines=lines, form=form, iomsg=iomsg_, iostat=iostat_)
10 close (unit)
  if (present(iostat)) iostat = iostat_
  if (present(iomsg)) iomsg = iomsg_
end subroutine write_file

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine write_lines(unit, lines, form, iostat, iomsg)
  !< Write lines (records) to a connected-formatted unit.
  !<
  !< @note There is no doctests, this being tested by means of [[write_file]] doctests.
  integer, intent(in) :: unit      !< Logical unit.
  type(string), intent(in) :: lines(1:) !< The lines.
  character(len=*), intent(in), optional :: form      !< Format of unit.
  integer, intent(out), optional :: iostat    !< IO status code.
  character(len=*), intent(inout), optional :: iomsg     !< IO status message.
  integer :: l         !< Counter.

  do l = 1, size(lines, dim=1)
  call lines(l) % write_line(unit=unit, form=form, iostat=iostat, iomsg=iomsg)
  end do
end subroutine write_lines
end module String_Method
