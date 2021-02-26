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

module test_IntVector
use easifemBase
implicit none

contains

!----------------------------------------------------------------------------
!                                                                 test1
!----------------------------------------------------------------------------

subroutine test1
type(IntVector_) :: obj
call display( "test1" )
call EqualLine()
call initiate(obj=obj, tSize=10)
call display( obj, msg = "test1")
call DashLine()
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
type(IntVector_), allocatable :: obj( : )
integer( I4B ) :: tsize( 4 )
tsize = [5,5,10,10]
call display( "test2" )
call EqualLine()
call initiate(obj=obj, tSize=tsize)
call display( obj, msg = "test2")
call DashLine()
end
end module test_IntVector

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_IntVector
implicit none
call test1
call test2
end program main