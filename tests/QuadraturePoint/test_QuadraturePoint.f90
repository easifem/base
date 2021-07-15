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

module test_ReferenceQuadraturePoint
use easifemBase
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type( ReferenceTriangle_ ) :: refelem
  type( QuadraturePoint_ ) :: obj
  call initiate( refelem, nsd = 2 )
  obj = GaussLegendreQuadrature(refelem, order=1)
  call display( obj, "1st order triangle")
  obj = GaussLegendreQuadrature(refelem, order=2)
  call display( obj, "2st order triangle")
  obj = GaussLegendreQuadrature(refelem, order=3)
  call display( obj, "3st order triangle")
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
use test_ReferenceQuadraturePoint
implicit none
call test1
! call BlankLines(nol=3)
! call test2
! call BlankLines(nol=3)
! call test3
! call BlankLines(nol=3)
! call test4
end program main
