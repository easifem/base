SUBMODULE( SparseMatrix_Method ) Unary
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE aij_convert_dns
  INTEGER( I4B ) :: i, j, nrow
  nrow = SIZE( IA ) - 1
  CALL Reallocate( mat, nrow, nrow )
  DO i = 1, nrow
    DO j = IA( i ), IA( i + 1 ) - 1
      mat( i, JA( j ) ) = A( j )
    END DO
  END DO
END PROCEDURE aij_convert_dns

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_convert_dns
  INTEGER( I4B ) :: i, j
  CALL Reallocate( To, From % nrow, From % ncol )
  DO i = 1, From % nrow
    DO j = From % IA( i ), From % IA( i + 1 ) - 1
      To( i, From % JA( j ) ) = From % A( j )
    END DO
  END DO
END PROCEDURE obj_convert_dns

!----------------------------------------------------------------------------
!                                                                    ColSORT
!----------------------------------------------------------------------------

MODULE PROCEDURE csort_csr
  ASSOCIATE( A => Obj % A, JA => Obj % JA, IA => Obj % IA )
    IF( PRESENT( Values ) ) THEN
      CALL CSORT( Obj % nrow, A, JA, IA, Values )
    ELSE
      CALL CSORT( Obj % nrow, A, JA, IA, .true. )
    END IF
  END ASSOCIATE
END PROCEDURE csort_csr

!----------------------------------------------------------------------------
!                                                           RemoveDuplicates
!----------------------------------------------------------------------------

MODULE PROCEDURE remove_duplicates_csr
  INTEGER( I4B ) :: n
  INTEGER( I4B ), ALLOCATABLE :: INDU( : ), IWK( : )
  n = Obj % nrow
  ALLOCATE( INDU( n ), IWK( n + 1 ) )
  IF( PRESENT( Values ) ) THEN
    IF( Values ) THEN
      CALL CLNCSR( 1, 1, n, Obj % A, Obj % JA, Obj % IA, &
        & INDU, IWK )
    ELSE
      CALL CLNCSR( 1, 0, n, Obj % A, Obj % JA, Obj % IA, &
        & INDU, IWK )
    END IF
  ELSE
    CALL CLNCSR( 1, 1, n, Obj % A, Obj % JA, Obj % IA, &
        & INDU, IWK )
  END IF
  DEALLOCATE( INDU, IWK )
END PROCEDURE remove_duplicates_csr

!----------------------------------------------------------------------------
!                                                                      Clean
!----------------------------------------------------------------------------

MODULE PROCEDURE clean_csr
  INTEGER( I4B ) :: n, val
  INTEGER( I4B ), ALLOCATABLE :: IWK( : )
  !
  n = Obj % nrow; ALLOCATE( IWK( n + 1 ) )
  CALL Reallocate( INDU, n )
  Val = 0; IF( Values ) Val = 1
  CALL CLNCSR( ExtraOption, Val, n, Obj % A, Obj % JA, Obj % IA, &
    & INDU, IWK )
  IF( ALLOCATED( IWK ) ) DEALLOCATE( IWK )
END PROCEDURE clean_csr

!----------------------------------------------------------------------------
!                                                                      Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE copy_csr_csr
  CALL initiate( Obj = To, tdof = From % tdof, tNodes = [From%tNodes], &
    & MatrixProp = From % MatrixProp )

  CALL Reallocate( To % A, From % nnz )

  IF( Present( Values ) .AND. Values ) THEN
    To % A = From % A
  END IF

  To % IA = From % IA
  To % JA = From % JA

  IF( ALLOCATED( From % RowSize ) ) To % RowSize = From % RowSize
  IF( ALLOCATED( From % ColSize ) ) To % ColSize = From % ColSize
  IF( ALLOCATED( From % Diag ) ) To % Diag = From % Diag
  IF( ALLOCATED( From % DiagIndx ) ) To % DiagIndx = From % DiagIndx

END PROCEDURE copy_csr_csr

!----------------------------------------------------------------------------
!                                                                ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE get_scalar_value
  INTEGER( I4B ) :: iadd
  REAL( DFP ), EXTERNAL :: GETELM
  IF( PRESENT( Sorted ) .AND. Sorted ) THEN
    Ans = GETELM( i, j, Obj % A, Obj % JA, Obj % IA, iadd, .TRUE. )
  ELSE
    Ans = GETELM( i, j, Obj % A, Obj % JA, Obj % IA, iadd, .FALSE. )
  END IF
END PROCEDURE get_scalar_value

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Unary