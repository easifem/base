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

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary:  Convert real to string.
!
!# Introduction
!
!
!```fortran
! use penf
! print "(A)", str(fm=FR16P, n=1._R16P)
!```

elemental function strf_R16P(fm, n) result(str)
  character(*), intent(in) :: fm
  !! Format different from the standard for the kind.
  real(R16P), intent(in) :: n
  !! Real to be converted.
  character(DR16P) :: str
  !! Returned string containing input number.

  write (str, trim(fm)) n
end function strf_R16P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Convert real to string.
!
!# Introduction
!
!
!```fortran
! use penf
! print "(A)", str(fm=FR8P, n=1._R8P)
!```

elemental function strf_R8P(fm, n) result(str)
  character(*), intent(in) :: fm
  !! Format different from the standard for the kind.
  real(R8P), intent(in) :: n
  !! Real to be converted.
  character(DR8P) :: str
  !! Returned string containing input number.
  write (str, trim(fm)) n
end function strf_R8P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Convert real to string.
!
!# Introduction
!
!```fortran
! use penf
! print "(A)", str(fm=FR4P, n=1._R4P)
!```

elemental function strf_R4P(fm, n) result(str)
  character(*), intent(in) :: fm
  !! Format different from the standard for the kind.
  real(R4P), intent(in) :: n
  !! Real to be converted.
  character(DR4P) :: str
  !! Returned string containing input number.
  write (str, trim(fm)) n
end function strf_R4P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Convert integer to string.
!
!# Introduction
!
!```fortran
! use penf
! print "(A)", str(fm=FI8P, n=1_I8P)
!```

elemental function strf_I8P(fm, n) result(str)
  character(*), intent(in) :: fm
  !! Format different from the standard for the kind.
  integer(I8P), intent(in) :: n
  !! Integer to be converted.
  character(DI8P) :: str
  !! Returned string containing input number.
  write (str, trim(fm)) n
end function strf_I8P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Convert integer to string.
!
!# Introduction
!
!```fortran
! use penf
! print "(A)", str(fm=FI4P, n=1_I4P)
!```

elemental function strf_I4P(fm, n) result(str)
  character(*), intent(in) :: fm
  !! Format different from the standard for the kind.
  integer(I4P), intent(in) :: n
  !! Integer to be converted.
  character(DI4P) :: str
  !! Returned string containing input number.
  write (str, trim(fm)) n
end function strf_I4P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Convert integer to string.
!
!# Introduction
!
!```fortran
! use penf
! print "(A)", str(fm=FI2P, n=1_I2P)
!```

elemental function strf_I2P(fm, n) result(str)
  character(*), intent(in) :: fm
  !! Format different from the standard for the kind.
  integer(I2P), intent(in) :: n
  !! Integer to be converted.
  character(DI2P) :: str
  !! Returned string containing input number.
  write (str, trim(fm)) n
end function strf_I2P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Convert integer to string.
!
!# Introduction
!
!```fortran
! use penf
! print "(A)", str(fm=FI1P, n=1_I1P)
!```

elemental function strf_I1P(fm, n) result(str)
  character(*), intent(in) :: fm
  !! Format different from the standard for the kind.
  integer(I1P), intent(in) :: n
  !! Integer to be converted.
  character(DI1P) :: str
  !! Returned string containing input number.

  write (str, trim(fm)) n
end function strf_I1P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Convert real to string.
!
!# Introduction
!
!
!```fortran
! use penf
! print "(A)", str(n=-1._R16P)
!```
!=> -0.100000000000000000000000000000000E+0001 <<<
!
!```fortran
! use penf
! print "(A)", str(n=-1._R16P, no_sign=.true.)
!```
!=> 0.100000000000000000000000000000000E+0001 <<<
!
!```fortran
! use penf
! print "(A)", str(n=-1._R16P, compact=.true.)
!```

elemental function str_R16P(n, no_sign, compact) result(str)
  real(R16P), intent(in) :: n
  !! Real to be converted.
  logical, intent(in), optional :: no_sign
  !! Flag for leaving out the sign.
  logical, intent(in), optional :: compact
  !! Flag for *compacting* string encoding.
  character(DR16P) :: str
  !! Returned string containing input number.
  !!
  write (str, FR16P) n               ! Casting of n to string.
  !!
  if (present(no_sign)) then
    if( no_sign ) then
      str = str(2:) ! Leaving out the sign.
    else
      if (n > 0._R16P) str(1:1) = '+'       ! Prefixing plus if n>0.
    end if
  end if
  !!
  if (present(compact)) then
    if (compact) call compact_real_string(string=str)
  end if
  !!
end function str_R16P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Convert real to string.
!
!# Introduction
!
!```fortran
! use penf
! print "(A)", str(n=-1._R8P)
!```
!=> -0.100000000000000E+001 <<<
!
!```fortran
! use penf
! print "(A)", str(n=-1._R8P, no_sign=.true.)
!```
!=> 0.100000000000000E+001 <<<
!
!```fortran
! use penf
! print "(A)", str(n=-1._R8P, compact=.true.)
!```

elemental function str_R8P(n, no_sign, compact) result(str)
  real(R8P), intent(in) :: n
  !! Real to be converted.
  logical, intent(in), optional :: no_sign
  !! Flag for leaving out the sign.
  logical, intent(in), optional :: compact
  !! Flag for *compacting* string encoding.
  character(DR8P) :: str
  !! Returned string containing input number.
  !!
  write (str, FR8P) n                ! Casting of n to string.
  !!
  if (present(no_sign)) then
    if( no_sign ) then
      str = str(2:) ! Leaving out the sign.
    else
      if (n > 0._R8P) str(1:1) = '+'        ! Prefixing plus if n>0.
    end if
  end if
  !!
  if (present(compact)) then
    if (compact) call compact_real_string(string=str)
  end if
end function str_R8P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Convert real to string.
!
!# Introduction
!
!```fortran
! use penf
! print "(A)", str(n=-1._R4P)
!```
!=> -0.100000E+01 <<<
!
!```fortran
! use penf
! print "(A)", str(n=-1._R4P, no_sign=.true.)
!```
!=> 0.100000E+01 <<<
!
!```fortran
! use penf
! print "(A)", str(n=-1._R4P, compact=.true.)
!```

elemental function str_R4P(n, no_sign, compact) result(str)
  real(R4P), intent(in) :: n
  !! Real to be converted.
  logical, intent(in), optional :: no_sign
  !! Flag for leaving out the sign.
  logical, intent(in), optional :: compact
  !! Flag for *compacting* string encoding.
  character(DR4P) :: str
  !! Returned string containing input number.
  !!
  write (str, FR4P) n                ! Casting of n to string.
  !!
  if (present(no_sign)) then
    if( no_sign ) then
      str = str(2:) ! Leaving out the sign.
    else
      if (n > 0._R4P) str(1:1) = '+'        ! Prefixing plus if n>0.
    end if
  end if
  !!
  if (present(compact)) then
    if (compact) call compact_real_string(string=str)
  end if
end function str_R4P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Convert integer to string.
!
!# Introduction
!
!```fortran
! use penf
! print "(A)", str(n=-1_I8P)
!```
!=> -1 <<<
!
!```fortran
! use penf
! print "(A)", str(n=-1_I8P, no_sign=.true.)
!```

elemental function str_I8P(n, no_sign) result(str)
  integer(I8P), intent(in) :: n
  !! Integer to be converted.
  logical, intent(in), optional :: no_sign
  !! Flag for leaving out the sign.
  character(DI8P) :: str
  !! Returned string containing input number plus padding zeros.

  write (str, FI8P) n                ! Casting of n to string.
  str = adjustl(trim(str))          ! Removing white spaces.
  !!
  if (present(no_sign)) then
    if( no_sign ) then
      str = str(2:) ! Leaving out the sign.
    else
      if (n >= 0_I8P) str = '+'//trim(str)  ! Prefixing plus if n>0.
    end if
  end if
  !!
end function str_I8P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Converting integer to string.
!
!# Introduction
!
!
!```fortran
! use penf
! print "(A)", str(n=-1_I4P)
!```
!=> -1 <<<
!
!```fortran
! use penf
! print "(A)", str(n=-1_I4P, no_sign=.true.)
!```
!=> 1 <<<

elemental function str_I4P(n, no_sign) result(str)
  integer(I4P), intent(in) :: n
  !! Integer to be converted.
  logical, intent(in), optional :: no_sign
  !! Flag for leaving out the sign.
  character(DI4P) :: str
  !! Returned string containing input number plus padding zeros.
  !!
  write (str, FI4P) n                ! Casting of n to string.
  str = adjustl(trim(str))          ! Removing white spaces.
  !!
  if (present(no_sign)) then
    if( no_sign ) then
      str = str(2:) ! Leaving out the sign.
    else
      if (n >= 0_I4P) str = '+'//trim(str)  ! Prefixing plus if n>0.
    end if
  end if
  !!
end function str_I4P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Convert integer to string.
!
!# Introduction
!
!```fortran
! use penf
! print "(A)", str(n=-1_I2P)
!```
!=> -1 <<<
!
!```fortran
! use penf
! print "(A)", str(n=-1_I2P, no_sign=.true.)
!```
!=> 1 <<<

elemental function str_I2P(n, no_sign) result(str)
  integer(I2P), intent(in) :: n
  !! Integer to be converted.
  logical, intent(in), optional :: no_sign
  !! Flag for leaving out the sign.
  character(DI2P) :: str
  !! Returned string containing input number plus padding zeros.

  write (str, FI2P) n                ! Casting of n to string.
  str = adjustl(trim(str))          ! Removing white spaces.
  !!
  if (present(no_sign)) then
    if( no_sign ) then
      str = str(2:) ! Leaving out the sign.
    else
      if (n >= 0_I2P) str = '+'//trim(str)  ! Prefixing plus if n>0.
    end if
  end if
  !!
end function str_I2P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Convert integer to string.
!
!# Introduction
!
!
!```fortran
! use penf
! print "(A)", str(n=-1_I1P)
!```
!=> -1 <<<
!
!```fortran
! use penf
! print "(A)", str(n=-1_I1P, no_sign=.true.)
!```
!=> 1 <<<

elemental function str_I1P(n, no_sign) result(str)
  integer(I1P), intent(in) :: n
  !! Integer to be converted.
  logical, intent(in), optional :: no_sign
  !! Flag for leaving out the sign.
  character(DI1P) :: str
  !! Returned string containing input number plus padding zeros.

  write (str, FI1P) n                ! Casting of n to string.
  str = adjustl(trim(str))          ! Removing white spaces.
  !!
  if (present(no_sign)) then
    if( no_sign ) then
      str = str(2:) ! Leaving out the sign.
    else
      if (n >= 0_I1P) str = '+'//trim(str)  ! Prefixing plus if n>0.
    end if
  end if
  !!
end function str_I1P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Convert logical to string.
!
!# Introduction
!
!
!```fortran
! use penf
! print "(A)", str(n=.true.)
!```
!=> T <<<

elemental function str_bol(n) result(str)
  logical, intent(in) :: n
  !! Logical to be converted.
  character(1) :: str
  !! Returned string containing input number plus padding zeros.

  write (str, '(L1)') n
end function str_bol

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Converting real array to string.
!
!# Introduction
!
!```fortran
! use penf
! print "(A)", str(n=[1._R16P, -2._R16P])
!```
!=> +0.100000000000000000000000000000000E+0001,
! -0.200000000000000000000000000000000E+0001 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1._R16P, 2._R16P], no_sign=.true.)
!```
!=> 0.100000000000000000000000000000000E+0001,
! 0.200000000000000000000000000000000E+0001 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1._R16P, -2._R16P], separator='|')
!```
!=> +0.100000000000000000000000000000000E+0001|
! -0.200000000000000000000000000000000E+0001 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1._R16P, -2._R16P], delimiters=['(', ')'])
!```
!=> (+0.100000000000000000000000000000000E+0001,
! -0.200000000000000000000000000000000E+0001) <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1._R16P, -2._R16P], compact=.true.)
!```
!=> +0.1E+1,-0.2E+1 <<<

pure function str_a_R16P(n, no_sign, separator, delimiters, compact) &
  & result(str)
  real(R16P), intent(in) :: n(:)
  !! Real array to be converted.
  logical, intent(in), optional :: no_sign
  !! Flag for leaving out the sign.
  character(1), intent(in), optional :: separator
  !! Eventual separator of array values.
  character(*), intent(in), optional :: delimiters(1:2)
  !! Eventual delimiters of array values.
  logical, intent(in), optional :: compact
  !! Flag for *compacting* string encoding.
  character(len=:), allocatable :: str
  !! Returned string containing input number.
  character(DR16P) :: strn
  !! String containing of element of input array number.
  character(len=1) :: sep
  !! Array values separator
  integer :: i
  !! Counter.
  !!
  str = ''
  sep = ','
  if (present(separator)) sep = separator
  do i = 1, size(n)
    strn = str_R16P(no_sign=no_sign, compact=compact, n=n(i))
    str = str//sep//trim(strn)
  end do
  str = trim(str(2:))
  if (present(delimiters)) str = delimiters(1)//str//delimiters(2)
end function str_a_R16P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Convert real array to string.
!
!# Introduction
!
!
!```fortran
! use penf
! print "(A)", str(n=[1._R8P, -2._R8P])
!```
!=> +0.100000000000000E+001,-0.200000000000000E+001 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1._R8P, 2._R8P], no_sign=.true.)
!```
!=> 0.100000000000000E+001,0.200000000000000E+001 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1._R8P, -2._R8P], separator='|')
!```
!=> +0.100000000000000E+001|-0.200000000000000E+001 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1._R8P, -2._R8P], delimiters=['(', ')'])
!```
!=> (+0.100000000000000E+001,-0.200000000000000E+001) <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1._R8P, -2._R8P], compact=.true.)
!```
!=> +0.1E+1,-0.2E+1 <<<


pure function str_a_R8P(n, no_sign, separator, delimiters, compact) &
  & result(str)
  real(R8P), intent(in) :: n(:)
  !! Real array to be converted.
  logical, intent(in), optional :: no_sign
  !! Flag for leaving out the sign.
  character(1), intent(in), optional :: separator
  !! Eventual separator of array values.
  character(*), intent(in), optional :: delimiters(1:2)
  !! Eventual delimiters of array values.
  logical, intent(in), optional :: compact
  !! Flag for *compacting* string encoding.
  character(len=:), allocatable :: str
  !! Returned string containing input number.
  character(DR8P) :: strn
  !! String containing of element of input array number.
  character(len=1) :: sep
  !! Array values separator
  integer :: i
  !! Counter.
  str = ''
  sep = ','
  if (present(separator)) sep = separator
  do i = 1, size(n)
    strn = str_R8P(no_sign=no_sign, compact=compact, n=n(i))
    str = str//sep//trim(strn)
  end do
  str = trim(str(2:))
  if (present(delimiters)) str = delimiters(1)//str//delimiters(2)
end function str_a_R8P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Convert real array to string.
!
!# Introduction
!
!
!```fortran
! use penf
! print "(A)", str(n=[1._R4P, -2._R4P])
!```
!=> +0.100000E+01,-0.200000E+01 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1._R4P, 2._R4P], no_sign=.true.)
!```
!=> 0.100000E+01,0.200000E+01 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1._R4P, -2._R4P], separator='|')
!```
!=> +0.100000E+01|-0.200000E+01 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1._R4P, -2._R4P], delimiters=['(', ')'])
!```
!=> (+0.100000E+01,-0.200000E+01) <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1._R4P, -2._R4P], compact=.true.)
!```
!=> +0.1E+1,-0.2E+1 <<<

pure function str_a_R4P(n, no_sign, separator, delimiters, compact) &
  & result(str)
  real(R4P), intent(in) :: n(:)
  !! Real array to be converted.
  logical, intent(in), optional :: no_sign
  !! Flag for leaving out the sign.
  character(1), intent(in), optional :: separator
  !! Eventual separator of array values.
  character(*), intent(in), optional :: delimiters(1:2)
  !! Eventual delimiters of array values.
  logical, intent(in), optional :: compact
  !! Flag for *compacting* string encoding.
  character(len=:), allocatable :: str
  !! Returned string containing input number.
  character(DR4P) :: strn
  !! String containing of element of input array number.
  character(len=1) :: sep
  !! Array values separator
  integer :: i
  !! Counter.

  str = ''
  sep = ','
  if (present(separator)) sep = separator
  do i = 1, size(n)
    strn = str_R4P(no_sign=no_sign, compact=compact, n=n(i))
    str = str//sep//trim(strn)
  end do
  str = trim(str(2:))
  if (present(delimiters)) str = delimiters(1)//str//delimiters(2)
end function str_a_R4P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Convert integer array to string.
!
!# Introduction
!
!
!```fortran
! use penf
! print "(A)", str(n=[1_I8P, -2_I8P])
!```
!=> +1,-2 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1_I8P, 2_I8P], no_sign=.true.)
!```
!=> 1,2 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1_I8P, -2_I8P], separator='|')
!```
!=> +1|-2 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1_I8P, -2_I8P], delimiters=['(', ')'])
!```
!=> (+1,-2) <<<

pure function str_a_I8P(n, no_sign, separator, delimiters) result(str)
  integer(I8P), intent(in) :: n(:)
  !! Integer array to be converted.
  logical, intent(in), optional :: no_sign
  !! Flag for leaving out the sign.
  character(1), intent(in), optional :: separator
  !! Eventual separator of array values.
  character(*), intent(in), optional :: delimiters(1:2)
  !! Eventual delimiters of array values.
  character(len=:), allocatable :: str
  !! Returned string containing input number.
  character(DI8P) :: strn
  !! String containing of element of input array number.
  character(len=1) :: sep
  !! Array values separator
  integer :: i
  !! Counter.
  str = ''
  sep = ','
  if (present(separator)) sep = separator
  if (present(no_sign)) then
    do i = 1, size(n)
      strn = str_I8P(no_sign=no_sign, n=n(i))
      str = str//sep//trim(strn)
    end do
  else
    do i = 1, size(n)
      strn = str_I8P(n=n(i))
      str = str//sep//trim(strn)
    end do
  end if
  str = trim(str(2:))
  if (present(delimiters)) str = delimiters(1)//str//delimiters(2)
end function str_a_I8P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Convert integer array to string.
!
!# Introduction
!
!```fortran
! use penf
! print "(A)", str(n=[1_I4P, -2_I4P])
!```
!=> +1,-2 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1_I4P, 2_I4P], no_sign=.true.)
!```
!=> 1,2 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1_I4P, -2_I4P], separator='|')
!```
!=> +1|-2 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1_I4P, -2_I4P], delimiters=['(', ')'])
!```
!=> (+1,-2) <<<

pure function str_a_I4P(n, no_sign, separator, delimiters) result(str)
  integer(I4P), intent(in) :: n(:)
  !! Integer array to be converted.
  logical, intent(in), optional :: no_sign
  !! Flag for leaving out the sign.
  character(1), intent(in), optional :: separator
  !! Eventual separator of array values.
  character(*), intent(in), optional :: delimiters(1:2)
  !! Eventual delimiters of array values.
  character(len=:), allocatable :: str
  !! Returned string containing input number.
  character(DI4P) :: strn
  !! String containing of element of input array number.
  character(len=1) :: sep
  !! Array values separator
  integer :: i
  !! Counter.
  !!
  str = ''
  sep = ','
  if (present(separator)) sep = separator
  if (present(no_sign)) then
    do i = 1, size(n)
      strn = str_I4P(no_sign=no_sign, n=n(i))
      str = str//sep//trim(strn)
    end do
  else
    do i = 1, size(n)
      strn = str_I4P(n=n(i))
      str = str//sep//trim(strn)
    end do
  end if
  str = trim(str(2:))
  if (present(delimiters)) str = delimiters(1)//str//delimiters(2)
end function str_a_I4P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Convert integer array to string.
!
!# Introduction
!
!```fortran
! use penf
! print "(A)", str(n=[1_I2P, -2_I2P])
!```
!=> +1,-2 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1_I2P, 2_I2P], no_sign=.true.)
!```
!=> 1,2 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1_I2P, -2_I2P], separator='|')
!```
!=> +1|-2 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1_I2P, -2_I2P], delimiters=['(', ')'])
!```
!=> (+1,-2) <<<

pure function str_a_I2P(n, no_sign, separator, delimiters) result(str)
  integer(I2P), intent(in) :: n(:)
  !! Integer array to be converted.
  logical, intent(in), optional :: no_sign
  !! Flag for leaving out the sign.
  character(1), intent(in), optional :: separator
  !! Eventual separator of array values.
  character(*), intent(in), optional :: delimiters(1:2)
  !! Eventual delimiters of array values.
  character(len=:), allocatable :: str
  !! Returned string containing input number.
  character(DI2P) :: strn
  !! String containing of element of input array number.
  character(len=1) :: sep
  !! Array values separator
  integer :: i
  !! Counter.
  !!
  str = ''
  sep = ','
  if (present(separator)) sep = separator
  if (present(no_sign)) then
    do i = 1, size(n)
      strn = str_I2P(no_sign=no_sign, n=n(i))
      str = str//sep//trim(strn)
    end do
  else
    do i = 1, size(n)
      strn = str_I2P(n=n(i))
      str = str//sep//trim(strn)
    end do
  end if
  str = trim(str(2:))
  if (present(delimiters)) str = delimiters(1)//str//delimiters(2)
end function str_a_I2P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Stefano Zaghi, https://github.com/szaghi
! date:
! summary: Convert integer array to string.
!
!# Introduction
!
!```fortran
! use penf
! print "(A)", str(n=[1_I1P, -2_I1P])
!```
!=> +1,-2 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1_I1P, 2_I1P], no_sign=.true.)
!```
!=> 1,2 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1_I1P, -2_I1P], separator='|')
!```
!=> +1|-2 <<<
!
!```fortran
! use penf
! print "(A)", str(n=[1_I1P, -2_I1P], delimiters=['(', ')'])
!```
!=> (+1,-2) <<<

pure function str_a_I1P(n, no_sign, separator, delimiters) result(str)
  integer(I1P), intent(in) :: n(:)
  !! Integer array to be converted.
  logical, intent(in), optional :: no_sign
  !! Flag for leaving out the sign.
  character(1), intent(in), optional :: separator
  !! Eventual separator of array values.
  character(*), intent(in), optional :: delimiters(1:2)
  !! Eventual delimiters of array values.
  character(len=:), allocatable :: str
  !! Returned string containing input number.
  character(DI1P) :: strn
  !! String containing of element of input array number.
  character(len=1) :: sep
  !! Array values separator
  integer :: i
  !! Counter.

  str = ''
  sep = ','
  if (present(separator)) sep = separator
  if (present(no_sign)) then
    do i = 1, size(n)
      strn = str_I1P(no_sign=no_sign, n=n(i))
      str = str//sep//trim(strn)
    end do
  else
    do i = 1, size(n)
      strn = str_I1P(n=n(i))
      str = str//sep//trim(strn)
    end do
  end if
  str = trim(str(2:))
  if (present(delimiters)) str = delimiters(1)//str//delimiters(2)
end function str_a_I1P
