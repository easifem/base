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

module test_ReferenceTriangle
use easifemBase
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type( ReferenceTriangle_ ) :: obj
  real( dfp ) :: xij( 3, 3 )
  xij( 1, 1:3 ) = [1.0, 2.0, 1.0]
  xij( 2, 1:3 ) = [0.0, 0.0, 1.0]
  xij( 3, : ) = 0.0
  call initiate( obj, nsd = 2, xij = xij )
  call display( obj, "obj : " )
end

! subroutine test2
!   type( ReferenceTriangle_ ) :: obj
!   obj = referenceTriangle( nsd = 2 )
!   call display( obj, "obj : " )
! end

! subroutine test3
!   class( ReferenceElement_ ), pointer :: obj => null()
!   obj => referenceTriangle_pointer( nsd = 2 )
!   call display( obj, "obj : " )
! end

! subroutine test4
!   class( ReferenceElement_ ), pointer :: obj_ptr => null()
!   type( ReferenceTriangle_ ) :: obj
!   obj_ptr => referenceTriangle_pointer( nsd = 2 )
!   call obj_ptr%LagrangeElement( order = 2, HighOrderobj = obj )
!   call display( obj, "higher order obj : ")
!   call obj_ptr%LagrangeElement( order = 3, HighOrderobj = obj )
!   call display( obj, "3rd order obj : ")
! end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

end module

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_ReferenceTriangle
implicit none
call test1
! call BlankLines(nol=3)
! call test2
! call BlankLines(nol=3)
! call test3
! call BlankLines(nol=3)
! call test4
end program main
