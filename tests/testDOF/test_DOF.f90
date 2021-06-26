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
  type( dof_ ) :: dofObj

  call display( "Testing getIndex on FMT_DOF" )
  call initiate( dofObj, tNodes=[10, 10], &
    & names=["P", "V"], spaceCompo=[1, 3], &
    & timeCompo=[1, 1], storageFMT = FMT_DOF )
  call display( dofObj, "Degree of Freedom : " )

  call display( &
    & getIndex( dofObj, nodeNum=5 ), "getIndex1 : nodeNum=5 : " )

  call display( &
    & getIndex( dofObj, nodeNum=5, iVar=1 ), "getIndex2 : nodeNum=5 : " )

  call display( &
    & getIndex( dofObj, nodeNum=5, varName="P" ), "getIndex3 : nodeNum=5 : " )

  call display( &
    & getIndex( dofObj, nodeNum=5, iVar=2 ), "getIndex4 : nodeNum=5 : " )

  call display( &
    & getIndex( dofObj, nodeNum=5, varName="V" ), &
    & "getIndex5 : nodeNum=5 : " )

  call DeallocateData( dofObj )
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type( dof_ ) :: dofObj
  call initiate( dofObj, tNodes=[10], names=["U"], spaceCompo=[3], timeCompo=[1], storageFMT = FMT_DOF )
  call display( .tDOF. dofObj, "tDOF : " )
  call display( dofObj .tDOF. "U", "tDOF of U : " )
  call DeallocateData( dofObj )
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

end module test_m


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_m
implicit none
call test0
end program main
