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

module test_Display
use Display_Method
use GlobalData
implicit none

contains
!----------------------------------------------------------------------------
!                                                                 test1
!----------------------------------------------------------------------------
subroutine test1
  call display(msg="msg = ", unitno=stdout)
end subroutine test1

!----------------------------------------------------------------------------
!                                                                 test2
!----------------------------------------------------------------------------

subroutine test2
  call display( msg="hello", val=" world!", unitno=stdout)
end subroutine test2

!----------------------------------------------------------------------------
!                                                                 test3
!----------------------------------------------------------------------------

subroutine test3
  call display( val=1.0_DFP, msg="var=", unitno=stdout)
end subroutine test3

!----------------------------------------------------------------------------
!                                                                 test4
!----------------------------------------------------------------------------

subroutine test4
  call display( val=1_I4B, msg="var=", unitno=stdout)
end subroutine test4

!----------------------------------------------------------------------------
!                                                                 test5
!----------------------------------------------------------------------------

subroutine test5
  call display( val=.TRUE., msg="var=", unitno=stdout)
end subroutine test5

!----------------------------------------------------------------------------
!                                                                 test6
!----------------------------------------------------------------------------

subroutine test6
  real( dfp ) :: vec(10)
  call RANDOM_NUMBER(vec)
  call display( val=vec, msg="var=", unitno=stdout)
  call display( val=vec, msg="var=", unitno=stdout, orient="col")
end subroutine test6

!----------------------------------------------------------------------------
!                                                                 test7
!----------------------------------------------------------------------------

subroutine test7
  integer( i4b ) :: vec(10)
  vec(1:5) = 1; vec(6:)=2
  call display( val=vec, msg="var=", unitno=stdout)
  call display( val=vec, msg="var=", unitno=stdout, orient="col")
end subroutine test7

!----------------------------------------------------------------------------
!                                                                 test8
!----------------------------------------------------------------------------

subroutine test8
  real( dfp ) :: mat(10, 10)
  call RANDOM_NUMBER(mat)
  call display( val=mat, msg="var=", unitno=stdout)
end subroutine test8

!----------------------------------------------------------------------------
!                                                                 test9
!----------------------------------------------------------------------------

subroutine test9
  real( dfp ) :: mat(5, 5, 2)
  call RANDOM_NUMBER(mat)
  call display( val=mat, msg="var=", unitno=stdout)
end subroutine test9

!----------------------------------------------------------------------------
!                                                                 test10
!----------------------------------------------------------------------------

subroutine test10
  real( dfp ) :: mat(3, 3, 2, 2)
  call RANDOM_NUMBER(mat)
  call display( val=mat, msg="var=", unitno=stdout)
end subroutine test10

end module test_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
  use test_Display
  implicit none

  call test1
  call test2
  call test3
  call test4
  call test5
  call test6
  call test7
  call test8
  call test9
  call test10
  call TIMESTAMP()
end program main