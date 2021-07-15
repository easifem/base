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

!> authors: Vikas Sharma, Ph. D.
! date: 13 July 2021
! summary: 	Methods related to CSR sparsity

SUBMODULE( CSRSparsity_Method ) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_initiate1
  IF( obj%isInitiated ) THEN
    CALL ErrorMSG( &
      & "Instance of CSRSparsity is already initiated!", &
      & "CSRSparsity_Method@Constructor.f90", &
      & "csr_initiate1()", &
      & 32, stderr )
    STOP
  END IF
  obj%nnz = 0
  obj%ncol = ncol
  obj%nrow = nrow
  IF( PRESENT( dof ) ) THEN
    obj%dof = dof
  ELSE
    CALL initiate( obj=obj%dof, tNodes=[nrow], names=['K'], &
      & spaceCompo=[1], timeCompo=[1], storageFMT=NODES_FMT )
  END IF
  CALL Reallocate( obj%IA, nrow + 1, obj%RowSize, nrow, &
    & obj%ColSize, ncol, obj%DiagIndx, nrow )
  IF( ALLOCATED( obj%row ) ) DEALLOCATE( obj%row )
  IF( ALLOCATED( obj%JA ) ) DEALLOCATE( obj%JA )
  obj%isInitiated = .TRUE.
  obj%isSparsityLock = .FALSE.
END PROCEDURE csr_initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_initiate2
  INTEGER( I4B ) :: ii, n
  obj%nnz = obj2%nnz
  obj%ncol = obj2%ncol
  obj%nrow = obj2%nrow
  obj%isSorted = obj2%isSorted
  obj%dof = obj2%dof
  obj%isInitiated = obj2%isInitiated
  obj%isSparsityLock = obj2%isSparsityLock
  IF( ALLOCATED( obj2%IA ) ) obj%IA = obj2%IA
  IF( ALLOCATED( obj2%JA ) ) obj%JA = obj2%JA
  IF( ALLOCATED( obj2%ColSize ) ) obj%ColSize = obj2%ColSize
  IF( ALLOCATED( obj2%RowSize ) ) obj%RowSize = obj2%RowSize
  IF( ALLOCATED( obj2%DiagIndx ) ) obj%DiagIndx = obj2%DiagIndx
  IF( ALLOCATED( obj%row ) ) THEN
    n = SIZE( obj%row )
    DO ii = 1, n
      obj%row( ii ) = obj2%row( ii )
    END DO
  END IF
END PROCEDURE csr_initiate2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_Initiate3
  INTEGER( I4B ) :: nrow, ncol
  nrow = SIZE( IA ) - 1; ncol = MAXVAL( JA )
  CALL initiate( obj=obj, nrow=nrow, ncol=ncol )
  obj%nnz = SIZE( JA )
  obj%IA = IA
  obj%JA = JA
END PROCEDURE csr_Initiate3

!----------------------------------------------------------------------------
!                                                                     Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_shape
  Ans = [ obj%nrow,  obj%ncol ]
END PROCEDURE csr_shape

!----------------------------------------------------------------------------
!                                                                      Size
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_size
  IF( PRESENT( Dims ) ) THEN
    IF( Dims .EQ. 1 ) THEN
      Ans = obj%nrow
    ELSE
      Ans = obj%ncol
    END IF
  ELSE
    Ans = obj%nrow * obj%ncol
  END IF
END PROCEDURE csr_size

!----------------------------------------------------------------------------
!                                                                      getNNZ
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_getNNZ
  INTEGER( I4B ) :: irow
  Ans = obj%nnz
END PROCEDURE csr_getNNZ

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_DeallocateData
  IF( ALLOCATED( obj%IA ) ) DEALLOCATE( obj%IA )
  IF( ALLOCATED( obj%JA ) ) DEALLOCATE( obj%JA )
  IF( ALLOCATED( obj%ColSize ) ) DEALLOCATE( obj%ColSize )
  IF( ALLOCATED( obj%RowSize ) ) DEALLOCATE( obj%RowSize )
  IF( ALLOCATED( obj%DiagIndx ) ) DEALLOCATE( obj%DiagIndx )
  IF( ALLOCATED( obj%Row ) ) DEALLOCATE( obj%Row )
  CALL DeallocateData( obj%dof )
  obj%nnz =  0
  obj%nrow = 0
  obj%ncol = 0
  obj%isSorted = .FALSE.
  obj%isSparsityLock = .FALSE.
  obj%isInitiated = .FALSE.
END PROCEDURE csr_DeallocateData

END SUBMODULE Constructor