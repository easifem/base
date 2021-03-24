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
! date: 	22 March 2021
! summary: 	This submodule contains method for constructing [[SparseMatrix_]]

SUBMODULE( SparseMatrix_Method ) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj1

  IF( SIZE( tNodes ) .NE. tdof ) THEN
    CALL ErrorMSG( MSG = "SIZE(tNodes) should be equal to tdof", &
      & File = "SparseMatrix_Method@Constructor.f90", Line = __LINE__, Routine = "Initiate_Obj1()", &
      & UnitNo = stderr )
    STOP
  END IF
  Obj%tDOF = tdof
  Obj%tNodes = tNodes
  Obj%nrow = SUM( tNodes )
  Obj%ncol = Obj%nrow
  Obj%nnz = 0
  IF( ALLOCATED( Obj%Row ) ) DEALLOCATE( Obj%Row )
  ALLOCATE( Obj%Row( Obj%nrow ))
  IF( ALLOCATED( Obj%A ) ) DEALLOCATE( Obj%A )
  IF( ALLOCATED( Obj%JA ) ) DEALLOCATE( Obj%JA )
  CALL Reallocate( Obj%IA, Obj%nrow + 1, Obj%RowSize, Obj%nrow, &
    & Obj%ColSize, Obj%nrow, Obj%DiagIndx, Obj%nrow, Obj%IndexUT, Obj%nrow )
  CALL Reallocate( Obj%Diag, Obj%nrow )
  CALL setTotalDimension( Obj, 2_I4B )
  Obj%MatrixProp = INPUT( Option=MatrixProp, Default='UNSYM' )
  Obj%StorageFMT = INPUT( Option=StorageFMT, Default=Nodes_FMT )
END PROCEDURE initiate_obj1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj2
  Obj%nnz = SIZE( JA )
  CALL initiate_obj1( Obj = Obj, tdof = 1, tNodes = [ SIZE( Obj%IA ) - 1 ], &
    & MatrixProp = MatrixProp )
  Obj%IA = IA
  Obj%JA = JA
  CALL COPY( Y=Obj%A, X=A )
  CALL SetSparsity( Obj )
END PROCEDURE initiate_obj2

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj3
  Obj%nnz = SIZE( JA )
  CALL initiate_obj1( Obj = Obj, tdof = 1, tNodes = [ SIZE( Obj%IA ) - 1 ], &
    & MatrixProp = MatrixProp )
  Obj%IA = IA
  Obj%JA = JA
  CALL Reallocate( Obj%A, Obj%nnz )
  CALL SetSparsity( Obj )
END PROCEDURE initiate_obj3

!----------------------------------------------------------------------------
!                                                                     Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE get_shape
  Ans = [ Obj%nrow,  Obj%ncol ]
END PROCEDURE get_shape

!----------------------------------------------------------------------------
!                                                                       Size
!----------------------------------------------------------------------------

MODULE PROCEDURE get_size
  IF( PRESENT( Dims ) ) THEN
    IF( Dims .EQ. 1 ) THEN
      Ans = Obj%nrow
    ELSE
      Ans = Obj%ncol
    END IF
  ELSE
    Ans = Obj%nrow * Obj%ncol
  END IF
END PROCEDURE get_size

!----------------------------------------------------------------------------
!                                                                      getNNZ
!----------------------------------------------------------------------------

MODULE PROCEDURE get_nnz
  INTEGER( I4B ) :: irow
  Ans = Obj%nnz
END PROCEDURE get_nnz

!----------------------------------------------------------------------------
!                                                               AllocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE Allocate_data
  CALL Initiate( Obj=Obj, tDOF=1, tNodes=[Dims( 2 )], &
    & MatrixProp = MatrixProp )
END PROCEDURE Allocate_data

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_data
  IF( ALLOCATED( Obj%Row ) ) DEALLOCATE( Obj%Row )
  IF( ALLOCATED( Obj%A ) ) DEALLOCATE( Obj%A )
  IF( ALLOCATED( Obj%IA ) ) DEALLOCATE( Obj%IA )
  IF( ALLOCATED( Obj%JA ) ) DEALLOCATE( Obj%JA )
  IF( ALLOCATED( Obj%RowSize ) ) DEALLOCATE( Obj%RowSize )
  IF( ALLOCATED( Obj%ColSize ) ) DEALLOCATE( Obj%ColSize )
  IF( ALLOCATED( Obj%DiagIndx ) ) DEALLOCATE( Obj%DiagIndx )
  IF( ALLOCATED( Obj%Diag ) ) DEALLOCATE( Obj%Diag )
  IF( ALLOCATED( Obj%IndexUT ) ) DEALLOCATE( Obj%IndexUT )
  IF( ALLOCATED( Obj%tNodes ) ) DEALLOCATE( Obj%tNodes )
  Obj%tDOF = 0
  Obj%nnz = 0
  Obj%nrow = 0
  Obj%ncol = 0
END PROCEDURE Deallocate_data

END SUBMODULE Constructor
