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

MODULE test_SparseMatrix
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type( sparsematrix_ ) :: obj
  integer( i4b ) :: i, tdof, tnodes
  tdof = 1; tnodes = 4
  call initiate( obj, tdof, [tnodes], 'UNSYM', FMT_NODES)
  do i = 1, tnodes
    call setsparsity( obj, i, [1,2,3,4] )
  end do
  call setSparsity(Obj)
  call display( obj, "obj" )
end subroutine


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
  type( sparsematrix_ ) :: obj
  type( intvector_ ), allocatable :: col( : )
  integer( i4b ), allocatable :: row( : )
  integer( i4b ) :: i, tdof, tnodes
  tdof = 1; tnodes = 3
  call initiate( obj, tdof, [tnodes] )
  allocate( col( tnodes ), row( tnodes ) )
  row = [1,2,3]
  do i = 1, tnodes
    col( i ) % val = [1,2,3,4]
  end do
  call setsparsity( obj, Row, Col )
  call setsparsity( obj )
  call display( obj, "obj" )
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
  type( sparsematrix_ ) :: tanmat
  real( dfp ), allocatable :: mat( :, : )

  call initiate( obj = tanmat, tdof = 1, tnodes = [10] )
  call setsparsity( obj = tanmat, row = 1, col = [1,2] )
  call setsparsity( obj = tanmat, row = 2, col = [1,2,3] )
  call setsparsity( obj = tanmat, row = 3, col = [2,3,4] )
  call setsparsity( obj = tanmat, row = 4, col = [3,4,5] )
  call setsparsity( obj = tanmat, row = 5, col = [4,5,6] )
  call setsparsity( obj = tanmat, row = 6, col = [5,6,7] )
  call setsparsity( obj = tanmat, row = 7, col = [6,7,8] )
  call setsparsity( obj = tanmat, row = 8, col = [7,8,9] )
  call setsparsity( obj = tanmat, row = 9, col = [8,9,10] )
  call setsparsity( obj = tanmat, row = 10, col = [9,10] )
  call setsparsity( tanmat )

  allocate( mat( 2, 2 ) )
  mat = reshape( [1,-1,-1,1], [2,2])
  call addcontribution( tanmat, [1,2], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [2,3], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [3,4], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [4,5], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [5,6], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [6,7], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [7,8], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [8,9], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [9,10], mat, 1.0_dfp, NONE )
  call display( tanmat, 'tanamt = ' )
  call convert( from = tanmat, to = mat )
  call display( mat, 'dense mat = ' )
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test4
  type( sparsematrix_ ) :: tanmat
  integer( i4b ), parameter :: tdof = 2, nns = 2
  real( dfp ) :: mat( nns*tdof, nns*tdof )
  real( dfp ), allocatable :: mat0( :, : )

  call initiate( obj = tanmat, tdof = 2, tnodes = [10, 10], storageFMT= FMT_DOF )
  call setsparsity( obj = tanmat, row = 1, col = [1,2] )
  call setsparsity( obj = tanmat, row = 2, col = [1,2,3] )
  call setsparsity( obj = tanmat, row = 3, col = [2,3,4] )
  call setsparsity( obj = tanmat, row = 4, col = [3,4,5] )
  call setsparsity( obj = tanmat, row = 5, col = [4,5,6] )
  call setsparsity( obj = tanmat, row = 6, col = [5,6,7] )
  call setsparsity( obj = tanmat, row = 7, col = [6,7,8] )
  call setsparsity( obj = tanmat, row = 8, col = [7,8,9] )
  call setsparsity( obj = tanmat, row = 9, col = [8,9,10] )
  call setsparsity( obj = tanmat, row = 10, col = [9,10] )
  call setsparsity( tanmat )

  call random_number( mat )
  call addcontribution( tanmat, [1,2], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [2,3], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [3,4], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [4,5], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [5,6], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [6,7], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [7,8], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [8,9], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [9,10], mat, 1.0_dfp, NONE )
  call display( tanmat, 'tanamt = ' )
  call convert( from = tanmat, to = mat0 )
  call display( mat0, 'dense mat = ' )
end subroutine

END MODULE test_SparseMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_SparseMatrix
implicit none
! call test1
! call test2
! call test3
call test4
end program main