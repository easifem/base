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

function ctor_R16P(str, knd, pref, error) result(n)
  !< Convert string to real.
  !<
  !<```fortran
  !< use penf
  !< print FR16P, cton(str='-1.0', knd=1._R16P)
  !<```
  !=> -0.100000000000000000000000000000000E+0001 <<<
  character(*), intent(in) :: str   !< String containing input number.
  real(R16P), intent(in) :: knd   !< Number kind.
  character(*), optional, intent(in) :: pref  !< Prefixing string.
  integer(I4P), optional, intent(out) :: error !< Error trapping flag: 0 no errors, >0 error occurs.
  real(R16P) :: n     !< Number returned.
  integer(I4P) :: err   !< Error trapping flag: 0 no errors, >0 error occurs.
  character(len=:), allocatable :: prefd !< Prefixing string.

  read (str, *, iostat=err) n ! Casting of str to n.
  if (err /= 0) then
    prefd = ''; if (present(pref)) prefd = pref
  write(stderr, '(A,I1,A)') prefd//' Error: conversion of string "'//str//'" to real failed! real(', kind(knd), ')'
  end if
  if (present(error)) error = err
end function ctor_R16P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

function ctor_R8P(str, knd, pref, error) result(n)
  !< Convert string to real.
  !<
  !<```fortran
  !< use penf
  !< print FR8P, cton(str='-1.0', knd=1._R8P)
  !<```
  !=> -0.100000000000000E+001 <<<
  character(*), intent(in) :: str   !< String containing input number.
  real(R8P), intent(in) :: knd   !< Number kind.
  character(*), optional, intent(in) :: pref  !< Prefixing string.
  integer(I4P), optional, intent(out) :: error !< Error trapping flag: 0 no errors, >0 error occurs.
  real(R8P) :: n     !< Number returned.
  integer(I4P) :: err   !< Error trapping flag: 0 no errors, >0 error occurs.
  character(len=:), allocatable :: prefd !< Prefixing string.

  read (str, *, iostat=err) n ! Casting of str to n.
  if (err /= 0) then
    prefd = ''; if (present(pref)) prefd = pref
  write(stderr, '(A,I1,A)') prefd//' Error: conversion of string "'//str//'" to real failed! real(', kind(knd), ')'
  end if
  if (present(error)) error = err
end function ctor_R8P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

function ctor_R4P(str, knd, pref, error) result(n)
  !< Convert string to real.
  !<
  !<```fortran
  !< use penf
  !< print FR4P, cton(str='-1.0', knd=1._R4P)
  !<```
  !=> -0.100000E+01 <<<
  character(*), intent(in) :: str   !< String containing input number.
  real(R4P), intent(in) :: knd   !< Number kind.
  character(*), optional, intent(in) :: pref  !< Prefixing string.
  integer(I4P), optional, intent(out) :: error !< Error trapping flag: 0 no errors, >0 error occurs.
  real(R4P) :: n     !< Number returned.
  integer(I4P) :: err   !< Error trapping flag: 0 no errors, >0 error occurs.
  character(len=:), allocatable :: prefd !< Prefixing string.

  read (str, *, iostat=err) n ! Casting of str to n.
  if (err /= 0) then
    prefd = ''; if (present(pref)) prefd = pref
  write(stderr, '(A,I1,A)') prefd//' Error: conversion of string "'//str//'" to real failed! real(', kind(knd), ')'
  end if
  if (present(error)) error = err
end function ctor_R4P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

function ctoi_I8P(str, knd, pref, error) result(n)
  !< Convert string to integer.
  !<
  !<```fortran
  !< use penf
  !< print FI8P, cton(str='-1', knd=1_I8P)
  !<```
  !=> -1 <<<
  character(*), intent(in) :: str   !< String containing input number.
  integer(I8P), intent(in) :: knd   !< Number kind.
  character(*), optional, intent(in) :: pref  !< Prefixing string.
  integer(I4P), optional, intent(out) :: error !< Error trapping flag: 0 no errors, >0 error occurs.
  integer(I8P) :: n     !< Number returned.
  integer(I4P) :: err   !< Error trapping flag: 0 no errors, >0 error occurs.
  character(len=:), allocatable :: prefd !< Prefixing string.

  read (str, *, iostat=err) n ! Casting of str to n.
  if (err /= 0) then
    prefd = ''; if (present(pref)) prefd = pref
  write(stderr, '(A,I1,A)') prefd//' Error: conversion of string "'//str//'" to integer failed! integer(', kind(knd), ')'
  end if
  if (present(error)) error = err
end function ctoi_I8P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

function ctoi_I4P(str, knd, pref, error) result(n)
  !< Convert string to integer.
  !<
  !<```fortran
  !< use penf
  !< print FI4P, cton(str='-1', knd=1_I4P)
  !<```
  !=> -1 <<<
  character(*), intent(in) :: str   !< String containing input number.
  integer(I4P), intent(in) :: knd   !< Number kind.
  character(*), optional, intent(in) :: pref  !< Prefixing string.
  integer(I4P), optional, intent(out) :: error !< Error trapping flag: 0 no errors, >0 error occurs.
  integer(I4P) :: n     !< Number returned.
  integer(I4P) :: err   !< Error trapping flag: 0 no errors, >0 error occurs.
  character(len=:), allocatable :: prefd !< Prefixing string.

  read (str, *, iostat=err) n ! Casting of str to n.
  if (err /= 0) then
    prefd = ''; if (present(pref)) prefd = pref
  write(stderr, '(A,I1,A)') prefd//' Error: conversion of string "'//str//'" to integer failed! integer(', kind(knd), ')'
  end if
  if (present(error)) error = err
end function ctoi_I4P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

function ctoi_I2P(str, knd, pref, error) result(n)
  !< Convert string to integer.
  !<
  !<```fortran
  !< use penf
  !< print FI2P, cton(str='-1', knd=1_I2P)
  !<```
  !=> -1 <<<
  character(*), intent(in) :: str   !< String containing input number.
  integer(I2P), intent(in) :: knd   !< Number kind.
  character(*), optional, intent(in) :: pref  !< Prefixing string.
  integer(I4P), optional, intent(out) :: error !< Error trapping flag: 0 no errors, >0 error occurs.
  integer(I2P) :: n     !< Number returned.
  integer(I4P) :: err   !< Error trapping flag: 0 no errors, >0 error occurs.
  character(len=:), allocatable :: prefd !< Prefixing string.

  read (str, *, iostat=err) n ! Casting of str to n.
  if (err /= 0) then
    prefd = ''; if (present(pref)) prefd = pref
  write(stderr, '(A,I1,A)') prefd//' Error: conversion of string "'//str//'" to integer failed! integer(', kind(knd), ')'
  end if
  if (present(error)) error = err
end function ctoi_I2P

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

function ctoi_I1P(str, knd, pref, error) result(n)
  !< Convert string to integer.
  !<
  !<```fortran
  !< use penf
  !< print FI1P, cton(str='-1', knd=1_I1P)
  !<```
  !=> -1 <<<
  character(*), intent(in) :: str   !< String containing input number.
  integer(I1P), intent(in) :: knd   !< Number kind.
  character(*), optional, intent(in) :: pref  !< Prefixing string.
  integer(I4P), optional, intent(out) :: error !< Error trapping flag: 0 no errors, >0 error occurs.
  integer(I1P) :: n     !< Number returned.
  integer(I4P) :: err   !< Error trapping flag: 0 no errors, >0 error occurs.
  character(len=:), allocatable :: prefd !< Prefixing string.

  read (str, *, iostat=err) n ! Casting of str to n.
  if (err /= 0) then
    prefd = ''; if (present(pref)) prefd = pref
  write(stderr, '(A,I1,A)') prefd//' Error: conversion of string "'//str//'" to integer failed! integer(', kind(knd), ')'
  end if
  if (present(error)) error = err
end function ctoi_I1P
