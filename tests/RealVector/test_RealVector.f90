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

#define _obj_ RealVector_

module test_RealVector
use easifemBase
implicit none
contains
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type( _obj_ ) :: obj
  call display("test1")
  call equalline()
  call allocateData(obj, 10)
  call display( obj, "test1=")
  call dashline()
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
type(_obj_), allocatable :: obj( : )
call display("test2")
call equalline()
call initiate(obj, [10,10,5,5])
call display( obj, "initiate Obj(:)=")
call dashline()
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
type(_obj_) :: obj
call display("test3:: initiate_obj_ab")
call equalline()
call initiate(obj, 2, 10)
obj%val(2) = 1.0_DFP
call display( obj, "initiate Obj(a:b)=")
call dashline()
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test4
type(_obj_) :: obj
call display("test4:: initiate_obj_ab")
call equalline()
call initiate(obj, 2, 10)
obj%val(2) = 1.0_DFP
call display( obj, "initiate Obj(a:b)=")
call dashline()
end

end module test_RealVector

!----------------------------------------------------------------------------
!                                                                 main
!----------------------------------------------------------------------------

program main
use test_RealVector
implicit none
call test1
call test2
call test3
end program main