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

module test_ReferenceElement
use easifemBase
implicit none
contains

subroutine test0
  type( ReferenceTriangle_ ) :: obj
  type( ReferenceElement_ ), ALLOCATABLE :: facetElems( : )
  INTEGER( I4B ) :: ii

  CALL initiate( obj, nsd = 2 )
  facetElems = FacetElements( obj )

  DO ii = 1, size( facetElems )
    CALL display( facetElems( ii ), "facetElements( " // str( ii, .true.) // " ) = " )
    CALL Blanklines( NOL = 4 )
  END DO
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
type( ReferenceTopology_ ) :: obj
obj = ReferenceTopology( Nptrs = [1,2,3], Name=Triangle3 )
call display( obj, "test-1 obj : ")
call display( .NNE. obj, "nne : ")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

end module

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_ReferenceElement
implicit none
call test0
end program main