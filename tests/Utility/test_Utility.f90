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

module test_Utility
  use GlobalData
  use Utility
  use Display_Method
  implicit none

  contains

!----------------------------------------------------------------------------
!                                                                 test1
!----------------------------------------------------------------------------

subroutine test1
  call display( getExtension("helloworld.f90") .EQ. "f90", &
    & msg="test1:: ")
end subroutine test1

!----------------------------------------------------------------------------
!                                                                 test2
!----------------------------------------------------------------------------
subroutine test2
  call display( ABS(radian(180.0_DFP) - Pi) .LE. 0.001, &
    & msg="test2:: "  )
end subroutine

!----------------------------------------------------------------------------
!                                                                 test3
!----------------------------------------------------------------------------
subroutine test3
  call display( ABS(radian(180) - Pi) .LE. 0.001, &
    & msg="test3:: "  )
end subroutine

!----------------------------------------------------------------------------
!                                                                 test4
!----------------------------------------------------------------------------

subroutine test4
  real( dfp ) :: xij( 2, 20 ), x( 2 )
  integer( i4b ) :: id

  call random_number( xij )
  x = [11.0, 100.0]
  xij( 1:2, 15 ) = x
  id = searchNearestCoord(Array=xij, x=x)
  call display( id==15, "test4:: " )
end subroutine


end module test_Utility

program main
  use test_Utility
  implicit none
  call test1
  call test2
  call test3
  call test4
end program main