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

pure function str_ascii_default(input) result(output)
  !< Convert string of default kind to ASCII string.
  !<
  !<```fortran
  !< use penf
  !< character(len=:, kind=ASCII), allocatable :: string
  !< string = str_ascii('I was DEFAULT kind, but now I am ASCII')
  !< print "(A)", string
  !<```
  !=> I was DEFAULT kind, but now I am ASCII <<<
  character(len=*), intent(in) :: input  !< Input string of default kind.
  character(len=:, kind=ASCII), allocatable :: output !< Output string of ASCII kind.

  output = input
end function str_ascii_default

pure function str_ascii_ascii(input) result(output)
  !< Convert string of ASCII kind to ASCII string, just for convenience in sanitize strings.
  !<
  !<```fortran
  !< use penf
  !< character(len=:, kind=ASCII), allocatable :: string
  !< string = str_ascii('I was ASCII kind and I am still ASCII')
  !< print "(A)", string
  !<```
  !=> I was ASCII kind and I am still ASCII <<<
  character(len=*, kind=ASCII), intent(in) :: input  !< Input string of ASCII kind.
  character(len=:, kind=ASCII), allocatable :: output !< Output string of ASCII kind.

  output = input
end function str_ascii_ascii

pure function str_ascii_ucs4(input) result(output)
  !< Convert string of UCS4 kind to ASCII string.
  !<
  !<```fortran
  !< use penf
  !< character(len=:, kind=ASCII), allocatable :: string
  !< string = str_ascii(UCS4_'I was UCS4 kind, but now I am ASCII')
  !< print "(A)", string
  !<```
  !=> I was UCS4 kind, but now I am ASCII <<<
  character(len=*, kind=UCS4), intent(in) :: input  !< Input string of UCS4 kind.
  character(len=:, kind=ASCII), allocatable :: output !< Output string of ASCII kind.

  output = input
end function str_ascii_ucs4
