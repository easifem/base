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

module test_BoundingBox
use easifemBase
implicit none
contains

!----------------------------------------------------------------------------
!                                                                 test1
!----------------------------------------------------------------------------

subroutine test1
  type(BoundingBox_) :: obj
  call initiate( obj, nsd = 2, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP] )
  call display( obj, msg="test1" )
end subroutine test1

!----------------------------------------------------------------------------
!                                                                 test2
!----------------------------------------------------------------------------

subroutine test2
  type(BoundingBox_) :: obj, obj2
  call initiate( obj, 2, [0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP] )
  call initiate(obj2, obj)
  call display( obj2, msg="test2")
end subroutine test2

!----------------------------------------------------------------------------
!                                                                 test3
!----------------------------------------------------------------------------

subroutine test3
  type(BoundingBox_) :: obj
  obj = BoundingBox( nsd = 2, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP] )
  call display( obj, msg="test3" )
end subroutine test3

!----------------------------------------------------------------------------
!                                                                 test4
!----------------------------------------------------------------------------

subroutine test4
  type(BoundingBox_) :: obj, obj2
  call initiate( obj, 2, [0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP] )
  obj2 = BoundingBox(obj)
  call display( obj2, msg="test4")
end subroutine test4

!----------------------------------------------------------------------------
!                                                                 test5
!----------------------------------------------------------------------------

subroutine test5
  type(BoundingBox_) :: obj
  obj = boundingBox(RESHAPE([0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP], [2,3]))
  call display(obj, "test5")
end subroutine test5

!----------------------------------------------------------------------------
!                                                                 test6
!----------------------------------------------------------------------------

subroutine test6
  type(BoundingBox_) :: obj
  type(BoundingBox_), pointer :: obj2

  call initiate( obj, 2, [0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP] )
  obj2 => BoundingBox_Pointer(obj)
  call display( obj2, msg="test6")
end subroutine test6

!----------------------------------------------------------------------------
!                                                                 test7
!----------------------------------------------------------------------------

subroutine test7
  type(BoundingBox_), pointer :: obj
  obj => BoundingBox_Pointer(nsd=3, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP])
  call display(obj, "test7")
end subroutine test7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

end module test_BoundingBox

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_BoundingBox
implicit none

call test1
call test2
call test3
call test4
call test5
call test6
call test7

end program main
