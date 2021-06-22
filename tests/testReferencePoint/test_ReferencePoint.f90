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

module test_m
use easifemBase
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test0
  type( ReferencePoint_ ) :: obj1, obj2, obj3
  class( ReferenceElement_ ), pointer :: obj_ptr1 => NULL()
  real( dfp ) :: xij( 3, 1 )
  call random_number( xij )
  call initiate( Obj=obj1, NSD=3 )
  call display( obj1, "obj1 : " )
  call display( MeasureSimplex(obj1, obj1%xij), "measure = ")
  obj2 = ReferencePoint(nsd=3)
  call display( obj2, 'obj2 : ' )
  call obj1%LagrangeElement( Order=2, HighOrderObj=obj3 )
  call display( obj3, "Second Order Lagrange Element : ")
  obj_ptr1 => ReferencePoint_Pointer( nsd = 3, xij = xij )
  call display( obj_ptr1, "obj_ptr1 : ")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

end module

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_m
implicit none
call test0
end program main