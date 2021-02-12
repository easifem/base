SUBMODULE( SparseMatrix_Method ) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj1
  Obj % tDOF = tdof
  Obj % tNodes = tNodes( 1 )
  Obj % nrow = tdof * tNodes( 1 )
  Obj % ncol = Obj % nrow
  Obj % nnz = 0

  IF( ALLOCATED( Obj % Row ) ) DEALLOCATE( Obj % Row )
  IF( ALLOCATED( Obj % A ) ) DEALLOCATE( Obj % A )
  IF( ALLOCATED( Obj % Diag ) ) DEALLOCATE( Obj % Diag )
  IF( ALLOCATED( Obj % JA ) ) DEALLOCATE( Obj % JA )
  ALLOCATE( Obj % Diag( Obj % nrow ) )

  CALL Reallocate( &
    & Obj % IA, Obj % nrow + 1, &
    & Obj % RowSize, Obj % nrow, &
    & Obj % ColSize, Obj % nrow, &
    & Obj % DiagIndx, Obj % nrow )

  ALLOCATE( Obj % Row( Obj % nrow ) )

  IF( PRESENT( MatrixProp ) ) THEN
    Obj % MatrixProp = TRIM( MatrixProp )
  ELSE
    Obj % MatrixProp = 'UNSYM'
  END IF
  CALL setTotalDimension( Obj, 2_I4B )

  IF( PRESENT( StorageFMT ) ) THEN
    Obj%StorageFMT = StorageFMT
  ELSE
    Obj%StorageFMT = Nodes_FMT
  END IF

END PROCEDURE initiate_obj1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj2
END PROCEDURE initiate_obj2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj3
END PROCEDURE initiate_obj3

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj4
  INTEGER( I4B ) :: n
  n = SIZE( Obj % IA ) - 1
  IF( PRESENT( MatrixProp ) ) THEN
    CALL initiate_obj1( Obj = Obj, tdof = 1, tNodes = [n], &
      & MatrixProp = MatrixProp )
  ELSE
    CALL initiate_obj1( Obj = Obj, tdof = 1, tNodes = [n] )
  END IF
  Obj % IA = IA
  Obj % JA = JA
  Obj % A = A
  Obj % nnz = SIZE( JA )
  CALL SetSparsity( Obj )
END PROCEDURE initiate_obj4

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj5
  INTEGER( I4B ) :: n
  n = SIZE( Obj % IA ) - 1
  IF( PRESENT( MatrixProp ) ) THEN
    CALL initiate_obj1( Obj = Obj, tdof = 1, tNodes = [n], &
      & MatrixProp = MatrixProp )
  ELSE
    CALL initiate_obj1( Obj = Obj, tdof = 1, tNodes = [n] )
  END IF
  Obj % IA = IA
  Obj % JA = JA
  Obj % nnz = SIZE( JA )
  CALL Reallocate( Obj % A, Obj % nnz )
  CALL SetSparsity( Obj )
END PROCEDURE initiate_obj5

!----------------------------------------------------------------------------
!                                                                     Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE get_shape
  Ans = [ Obj % nrow,  Obj % ncol ]
END PROCEDURE get_shape

!----------------------------------------------------------------------------
!                                                                       Size
!----------------------------------------------------------------------------

MODULE PROCEDURE get_size
  IF( PRESENT( Dims ) ) THEN
    IF( Dims .EQ. 1 ) THEN
      Ans = Obj % nrow
    ELSE
      Ans = Obj % ncol
    END IF
  ELSE
    Ans = Obj % nrow * Obj % ncol
  END IF
END PROCEDURE get_size

!----------------------------------------------------------------------------
!                                                                      getNNZ
!----------------------------------------------------------------------------

MODULE PROCEDURE get_nnz
  INTEGER( I4B ) :: irow
  Ans = Obj % nnz
END PROCEDURE get_nnz

!----------------------------------------------------------------------------
!                                                               AllocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE Allocate_data
  CALL setTotalDimension( Obj, 2_I4B )
  Obj % tDOF = 1
  Obj % tNodes = Dims( 1 )
  Obj % nrow = Dims( 1 )
  Obj % ncol = Dims( 2 )
  Obj % nnz = 0

  IF( ALLOCATED( Obj % A ) ) DEALLOCATE( Obj % A )
  IF( ALLOCATED( Obj % Diag ) ) DEALLOCATE( Obj % Diag )
  IF( ALLOCATED( Obj % JA ) ) DEALLOCATE( Obj % JA )
  ALLOCATE( Obj % Diag( Obj % nrow ) )

  CALL Reallocate( &
    & Obj % IA, Obj % nrow + 1, &
    & Obj % RowSize, Obj % nrow, &
    & Obj % ColSize, Obj % nrow, &
    & Obj % DiagIndx, Obj % nrow )

  IF( ALLOCATED( Obj % Row ) ) DEALLOCATE( Obj % Row )
  ALLOCATE( Obj % Row( Obj % nrow ) )

  IF( PRESENT( MatrixProp ) ) THEN
    Obj % MatrixProp = TRIM( MatrixProp )
  ELSE
    Obj % MatrixProp = 'UNSYM'
  END IF
END PROCEDURE Allocate_data

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_data
  IF( ALLOCATED( Obj % Row ) ) DEALLOCATE( Obj % Row )
  IF( ALLOCATED( Obj % A ) ) DEALLOCATE( Obj % A )
  IF( ALLOCATED( Obj % IA ) ) DEALLOCATE( Obj % IA )
  IF( ALLOCATED( Obj % JA ) ) DEALLOCATE( Obj % JA )
  IF( ALLOCATED( Obj % RowSize ) ) DEALLOCATE( Obj % RowSize )
  IF( ALLOCATED( Obj % ColSize ) ) DEALLOCATE( Obj % ColSize )
  IF( ALLOCATED( Obj % DiagIndx ) ) DEALLOCATE( Obj % DiagIndx )
  IF( ALLOCATED( Obj % Diag ) ) DEALLOCATE( Obj % Diag )
  Obj % tDOF = 0
  Obj % tNodes = 0
  Obj % nnz = 0
  Obj % nrow = 0
  Obj % ncol = 0
END PROCEDURE Deallocate_data

END SUBMODULE Constructor
