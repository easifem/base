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

pure function str_ucs4_default(input) result(output)
  !< Convert string of default kind to UCS4 string.
  !<
  !<```fortran
  !< use penf
  !< character(len=:, kind=UCS4), allocatable :: string
  !< string = str_ascii('I was DEFAULT kind, but now I am UCS4')
  !< print "(A)", string
  !<```
  !=> I was DEFAULT kind, but now I am UCS4 <<<
  character(len=*), intent(in) :: input  !< Input string of default kind.
  character(len=:, kind=UCS4), allocatable :: output !< Output string of UCS4 kind.

  output = input
end function str_ucs4_default

pure function str_ucs4_ascii(input) result(output)
  !< Convert string of ASCII kind to UCS4 string.
  !<
  !<```fortran
  !< use penf
  !< character(len=:, kind=UCS4), allocatable :: string
  !< string = str_ascii(ASCII_'I was ASCII kind, but now I am UCS4')
  !< print "(A)", string
  !<```
  !=> I was ASCII kind, but now I am UCS4 <<<
  character(len=*, kind=ASCII), intent(in) :: input  !< Input string of ASCII kind.
  character(len=:, kind=UCS4), allocatable :: output !< Output string of UCS4 kind.

  output = input
end function str_ucs4_ascii

pure function str_ucs4_ucs4(input) result(output)
  !< Convert string of UCS4 kind to UCS4 string, just for convenience in sanitize strings.
  !<
  !<```fortran
  !< use penf
  !< character(len=:, kind=UCS4), allocatable :: string
  !< string = str_ascii(UCS4_'I was UCS4 kind and I am still UCS4')
  !< print "(A)", string
  !<```
  !=> I was UCS4 kind and I am still UCS4 <<<
  character(len=*, kind=UCS4), intent(in) :: input  !< Input string of UCS4 kind.
  character(len=:, kind=UCS4), allocatable :: output !< Output string of UCS4 kind.

  output = input
end function str_ucs4_ucs4
