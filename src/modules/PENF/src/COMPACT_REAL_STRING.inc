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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure subroutine compact_real_string(string)
  !< author: Izaak Beekman
  !< date: 02/24/2015
  !<
  !< Compact a string representing a real number, so that the same value is displayed with fewer characters.
  !<
  !< @note No need to add doctest: this is tested by a lot of doctests of other TBPs.
  character(len=*), intent(inout) :: string      !< string representation of a real number.
  character(len=len(string)) :: significand !< Significand characters.
  character(len=len(string)) :: expnt       !< Exponent characters.
  character(len=2) :: separator   !< Separator characters.
  integer(I4P) :: exp_start   !< Start position of exponent.
  integer(I4P) :: decimal_pos !< Decimal positions.
  integer(I4P) :: sig_trim    !< Signature trim.
  integer(I4P) :: exp_trim    !< Exponent trim.
  integer(I4P) :: i           !< counter

  string = adjustl(string)
  exp_start = scan(string, 'eEdD')
  if (exp_start == 0) exp_start = scan(string, '-+', back=.true.)
  decimal_pos = scan(string, '.')
  if (exp_start /= 0) separator = string(exp_start:exp_start)
  if (exp_start < decimal_pos) then ! possibly signed, exponent-less float
    significand = string
    sig_trim = len(trim(significand))
    do i = len(trim(significand)), decimal_pos + 2, -1 ! look from right to left at 0s, but save one after the decimal place
      if (significand(i:i) == '0') then
        sig_trim = i - 1
      else
        exit
      end if
    end do
    string = trim(significand(1:sig_trim))
  elseif (exp_start > decimal_pos) then ! float has exponent
    significand = string(1:exp_start - 1)
    sig_trim = len(trim(significand))
    do i = len(trim(significand)), decimal_pos + 2, -1 ! look from right to left at 0s
      if (significand(i:i) == '0') then
        sig_trim = i - 1
      else
        exit
      end if
    end do
    expnt = adjustl(string(exp_start + 1:))
    if (expnt(1:1) == '+' .or. expnt(1:1) == '-') then
      separator = trim(adjustl(separator))//expnt(1:1)
      exp_start = exp_start + 1
      expnt = adjustl(string(exp_start + 1:))
    end if
    exp_trim = 1
    do i = 1, (len(trim(expnt)) - 1) ! look at exponent leading zeros saving last
      if (expnt(i:i) == '0') then
        exp_trim = i + 1
      else
        exit
      end if
    end do
    string = trim(adjustl(significand(1:sig_trim)))// &
            trim(adjustl(separator))// &
            trim(adjustl(expnt(exp_trim:)))
    !else ! mal-formed real, BUT this code should be unreachable
  end if
end subroutine compact_real_string
