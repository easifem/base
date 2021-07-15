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

MODULE test_m
USE BaseMethod
IMPLICIT NONE
CONTAINS


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test0
  type( csrSparsity_) :: csr
  integer( i4b ) :: i
  type( IntVector_ ) :: IA, JA
  type( RealVector_ ) :: A
  CALL display( 'testing csr matrix constructor using IA, JA, A')
  IA = [1,3,6,9,10,13]
  JA = [1,4,1,2,4,2,3,5,4,5,3,2]
  A = 1.0_DFP*[10.0,-1.0,-2.0,11.0,-3.0,-4.0,12.0,-5.0,13.0,14.0,-9.0,-8.0]
  call initiate( obj=csr, IA=IA%val, JA=JA%val )
  call setSparsity(csr)
  call display( csr, "csr" )
  call deallocateData( csr )
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type( CSRSparsity_ ) :: obj, obj2
  type( dof_ ) :: dofobj
  integer( i4b ) :: i
  call initiate( obj=dofobj, tNodes=[12], names=['K'], &
    & spaceCompo=[1], timeCompo=[1], storageFMT=NODES_FMT )
  call initiate( obj, ncol=12, nrow=12, dof=dofobj )
  call setsparsity( obj, 1, [1,2,6,5] )
  call setsparsity( obj, 2, [2,1,3,5,6,7] )
  call setsparsity( obj, 3, [3,2,4,6,7,8] )
  call setsparsity( obj, 4, [4,3,7,8] )
  call setsparsity( obj, 5, [5,1,9,2,6,10] )
  call setsparsity( obj, 6, [6,2,10,1,5,9,3,7,11] )
  call setsparsity( obj, 7, [7,6,8,2,3,4,10,11,12] )
  call setsparsity( obj, 8, [8,4,12,3,7,11] )
  call setsparsity( obj, 9, [9,5,6,10] )
  call setsparsity( obj, 10, [10,9,11,5,6,7] )
  call setsparsity( obj, 11, [11,10,12,6,7,8] )
  call setsparsity( obj, 12, [12,7,8,11] )
  call setSparsity(obj)
  call display( obj, "obj=" )
  obj2 = obj
  call display( "========================" )
  call display( obj2, "obj2")

  call display( shape(obj), "shape(obj) = " )
  call display( size(obj), "size(obj) = " )
  call display( getNNZ(obj), "getNNZ(obj) = " )
  call DeallocateData( obj )
  call DeallocateData( obj2 )
end subroutine

END MODULE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_m
implicit none
call test0
end program main