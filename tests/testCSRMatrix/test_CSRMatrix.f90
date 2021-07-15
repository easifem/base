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
  type( csrmatrix_ ) :: obj, obj2
  integer( i4b ) :: i
  type( IntVector_ ) :: IA, JA
  type( RealVector_ ) :: A
  CALL display( 'testing getLowerTriangle')
  IA = [1,3,6,9,10,13]
  JA = [1,4,1,2,4,2,3,5,4,5,3,2]
  A = 1.0_DFP*[10.0,-1.0,-2.0,11.0,-3.0,-4.0,12.0,-5.0,13.0,14.0,-9.0,-8.0]
  call initiate( obj=obj, A=A%val, IA=IA%val, JA=JA%val )
  call getLowerTriangle( obj, obj2 )
  call display( obj2, "Lower triangle part = " )
  call getUpperTriangle( obj, obj2 )
  call display( obj2, "Upper triangle part = " )
  call deallocateData( obj )
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test9
  type( csrmatrix_ ) :: obj
  integer( i4b ) :: i
  type( IntVector_ ) :: IA, JA
  type( RealVector_ ) :: A
  CALL display( 'testing getDiagonal')
  IA = [1,3,6,9,10,13]
  JA = [1,4,1,2,4,2,3,5,4,5,3,2]
  A = 1.0_DFP*[10.0,-1.0,-2.0,11.0,-3.0,-4.0,12.0,-5.0,13.0,14.0,-9.0,-8.0]
  call initiate( obj=obj, A=A%val, IA=IA%val, JA=JA%val )
  call getDiagonal( obj=obj, diag=A%val, idiag=IA%val, offset=0 )
  call display( A, "diag=")
  call display( IA, "idiag=")
  call getDiagonal( obj=obj, diag=A%val, idiag=IA%val, offset=-1 )
  call display( A, "diag=")
  call display( IA, "idiag=")
  call getDiagonal( obj=obj, diag=A%val, idiag=IA%val, offset=2 )
  call display( A, "diag=")
  call display( IA, "idiag=")
  call deallocateData( obj )
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test8
  type( csrmatrix_ ) :: obj
  integer( i4b ) :: i
  type( IntVector_ ) :: IA, JA
  type( RealVector_ ) :: A
  CALL display( 'testing TRANSPOSE')
  IA = [1,3,6,9]
  JA = [1,4,1,2,4,2,3,5]
  A = 1.0_DFP*[10.0,-1.0,-2.0,11.0,-3.0,-4.0,12.0,-5.0]
  call initiate( obj=obj, A=A%val, IA=IA%val, JA=JA%val )
  call getTranspose(obj)
  call display( obj, "obj=" )
  call deallocateData( obj )
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test7
  type( csrmatrix_ ) :: obj
  integer( i4b ) :: i
  type( IntVector_ ) :: IA, JA
  type( RealVector_ ) :: A
  CALL display( 'testing TRANSPOSE')
  IA = [1,3,6,9,10,13]
  JA = [1,4,1,2,4,2,3,5,4,5,3,2]
  A = 1.0_DFP*[10.0,-1.0,-2.0,11.0,-3.0,-4.0,12.0,-5.0,13.0,14.0,-9.0,-8.0]
  call initiate( obj=obj, A=A%val, IA=IA%val, JA=JA%val )
  call getTranspose(obj)
  call display( obj, "obj=" )
  call deallocateData( obj )
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test6
  type( csrmatrix_ ) :: obj, obj2
  integer( i4b ) :: i
  type( IntVector_ ) :: IA, JA
  type( RealVector_ ) :: A
  CALL display( 'testing DropEntry IA, JA, A')
  IA = [1,3,6,9,10,13]
  JA = [1,4,1,2,4,2,3,5,4,5,3,2]
  A = 1.0_DFP*[10.0,-1.0,-2.0,11.0,-3.0,-4.0,12.0,-5.0,13.0,14.0,-9.0,-8.0]
  call initiate( obj=obj, A=A%val, IA=IA%val, JA=JA%val )
  ! call setSparsity(obj) !! Not required
  call DropEntry(objIn=obj, objOut=obj2, dropTol=4.0_DFP)
  call display( obj2, "obj2=" )
  call DropEntry(objIn=obj, objOut=obj, dropTol=4.0_DFP)
  call display( obj, "obj=" )
  call deallocateData( obj )
  call deallocateData( obj2 )
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test5
  type( csrmatrix_ ) :: obj, obj2
  integer( i4b ) :: i
  type( IntVector_ ) :: IA, JA
  type( RealVector_ ) :: A
  CALL display( 'testing unary using IA, JA, A')
  IA = [1,3,6,9,10,13]
  JA = [1,4,1,2,4,2,3,5,4,5,3,2]
  A = 1.0_DFP*[10.0,-1.0,-2.0,11.0,-3.0,-4.0,12.0,-5.0,13.0,14.0,-9.0,-8.0]
  call initiate( obj=obj, A=A%val, IA=IA%val, JA=JA%val )
  ! call setSparsity(obj) !! Not required
  call initiate(obj=obj2, obj2=obj, i1=1, i2=2, j1=1, j2=5)
  call setSparsity( obj2 )
  call display( obj2, "obj2=" )
  call deallocateData( obj )
  call deallocateData( obj2 )
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test4
  type( csrmatrix_ ) :: obj, obj2
  integer( i4b ) :: i
  type( IntVector_ ) :: IA, JA
  type( RealVector_ ) :: A
  CALL display( 'testing unary using IA, JA, A')
  IA = [1,3,6,9,10,13]
  JA = [1,4,1,2,4,2,3,5,4,5,3,2]
  A = 1.0_DFP*[10.0,-1.0,-2.0,11.0,-3.0,-4.0,12.0,-5.0,13.0,14.0,-9.0,-8.0]
  call initiate( obj=obj, A=A%val, IA=IA%val, JA=JA%val )
  !call setSparsity(obj) !! Not required
  call display( get( obj, 1,1) .APPROXEQ. 10.0_DFP, "test 1 : ")
  call display( get( obj, 5,3) .APPROXEQ. -9.0_DFP, "test 2 : ")

  call initiate(obj=obj2, obj2=obj, i1=1, i2=5, j1=1, j2=5)
  call deallocateData( obj )
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
  type( csrmatrix_ ) :: obj
  integer( i4b ) :: i
  type( IntVector_ ) :: IA, JA
  type( RealVector_ ) :: A
  CALL display( 'testing csr matrix constructor using IA, JA, A')
  IA = [1,3,6,9,10,13]
  JA = [1,4,1,2,4,2,3,5,4,5,3,2]
  A = 1.0_DFP*[10.0,-1.0,-2.0,11.0,-3.0,-4.0,12.0,-5.0,13.0,14.0,-9.0,-8.0]
  call initiate( obj=obj, A=A%val, IA=IA%val, JA=JA%val )
  call setSparsity(obj)
  call display( obj, "obj" )
  call deallocateData( obj )
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
  type( csrmatrix_ ) :: obj
  type( csrSparsity_ ) :: csr
  type( dof_ ) :: dofobj
  integer( i4b ) :: i, unitNo
  !
  call initiate( obj=dofobj, tNodes=[12], names=['K'], &
    & spaceCompo=[1], timeCompo=[1], storageFMT=NODES_FMT )
  call initiate( csr, ncol=12, nrow=12, dof=dofobj )
  call setsparsity( csr, 1, [1,2,6,5] )
  call setsparsity( csr, 2, [2,1,3,5,6,7] )
  call setsparsity( csr, 3, [3,2,4,6,7,8] )
  call setsparsity( csr, 4, [4,3,7,8] )
  call setsparsity( csr, 5, [5,1,9,2,6,10] )
  call setsparsity( csr, 6, [6,2,10,1,5,9,3,7,11] )
  call setsparsity( csr, 7, [7,6,8,2,3,4,10,11,12] )
  call setsparsity( csr, 8, [8,4,12,3,7,11] )
  call setsparsity( csr, 9, [9,5,6,10] )
  call setsparsity( csr, 10, [10,9,11,5,6,7] )
  call setsparsity( csr, 11, [11,10,12,6,7,8] )
  call setsparsity( csr, 12, [12,7,8,11] )
  call setSparsity( csr )
  call initiate( obj=obj, csr=csr )
  call setSparsity(obj)
  obj = 2.0_DFP
  call display( obj, "obj" )
  call deallocateData( obj )
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type( csrmatrix_ ) :: obj
  type( dof_ ) :: dofobj
  integer( i4b ) :: i, unitNo

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
  obj = 2.0_DFP
  OPEN( NEWUNIT=unitNo, FILE="example.ps", &
    & STATUS="REPLACE", ACTION="WRITE" )
  call display( obj, "obj" )
  call deallocateData( obj )
end subroutine

END MODULE test_m

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_m
implicit none
call test0
! call test2
! call test3
! call test4
end program main