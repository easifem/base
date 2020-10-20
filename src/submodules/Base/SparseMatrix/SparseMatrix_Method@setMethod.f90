SUBMODULE( SparseMatrix_Method ) setMethod
USE BaseMethod
USE MOD_FUNCTIONAL, ONLY: SORT

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                               setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE setSparsity_1
  INTEGER( I4B ) :: n, a, b, m
  INTEGER( I4B ), ALLOCATABLE :: n2ntemp( : )

  SELECT CASE( Obj % tDOF )
  CASE( 1 )
    Obj % nnz = Obj % nnz + SIZE( Col )
    n2ntemp = SORT( Col )
    CALL APPEND( Obj % Row( Row ) % Val, n2ntemp )

  CASE DEFAULT
    IF( Obj%StorageFMT .EQ. NODES_FMT ) THEN
      n = SIZE( Col )
      ALLOCATE( n2ntemp( n * Obj % tDOF ) )
      DO a = 1, n
        DO b = 1, Obj % tDOF
          n2ntemp( ( a - 1 ) * Obj % tDOF + b ) = &
            & ( Col( a ) - 1 ) * Obj % tDOF + b
        END DO
      END DO
      !
      n = SIZE( n2ntemp )
      n2ntemp = SORT( n2ntemp )

      DO b = 1, Obj % tDOF
        Obj % nnz = Obj % nnz + n
        CALL APPEND( Obj % Row( ( Row - 1 ) * Obj % tDOF + b ) % Val, &
          & n2ntemp )
      END DO
      !
      DEALLOCATE( n2ntemp )
    ELSE
      n = SIZE( Col )
      ALLOCATE( n2ntemp( n * Obj % tDOF ) )
      DO b = 1, Obj % tDOF
        DO a = 1, n
          n2ntemp( ( b - 1 ) * n + a ) = &
            & ( b - 1 ) * Obj % tNodes + Col( a )
        END DO
      END DO
      !
      n = SIZE( n2ntemp )
      n2ntemp = SORT( n2ntemp )

      DO b = 1, Obj%tDOF
        Obj%nnz = Obj%nnz + n
        CALL APPEND( Obj%Row( ( b - 1 ) * Obj%tNodes + Row )%Val,n2ntemp )
      END DO
      !
      DEALLOCATE( n2ntemp )
    END IF
  END SELECT

END PROCEDURE setSparsity_1

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE setSparsity_2
  INTEGER( I4B ) :: i
  DO i = 1, SIZE( Row )
    CALL setSparsity( Obj, Row( i ), Col( i ) % Val )
  END DO
END PROCEDURE setSparsity_2

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE setSparsity_3
  !
  INTEGER( I4B ) :: i, j, k

  !<--- Remove duplicate entries in Obj % Row( irow ) % Col
  IF( ALLOCATED( Obj % Row ) ) THEN
    k = 0
    DO i = 1, Obj % nrow
      CALL RemoveDuplicates( Obj % Row( i ) )
      k = k + SIZE( Obj % Row( i ) )
    END DO

    !<--- update nnz: number of non zeros
    Obj % nnz = k
    !<--- allocate Obj % JA and Obj % A
    CALL Reallocate( Obj % A, Obj % nnz, Obj % JA, Obj % nnz )

    !<--- convert data into IA, JA
    Obj % IA( 1 ) = 1
    DO i = 1, Obj % nrow
      Obj % RowSize( i ) = SIZE( Obj % Row( i ) )
      Obj % IA( i + 1 ) = Obj % IA( i ) + Obj % RowSize( i )

      IF( Obj % RowSize( i ) .NE. 0 ) THEN
        Obj % JA( Obj % IA( i ) : Obj % IA( i + 1 ) - 1 ) = Obj % Row( i ) % Val
        DO j = Obj % IA( i ), Obj % IA( i + 1 ) - 1
          Obj % ColSize( Obj % JA( j ) ) = &
            & Obj % ColSize( Obj % JA( j ) ) + 1
          IF( Obj % JA( j ) .EQ. i )Obj % DiagIndx( i ) =j
        END DO
      END IF
    END DO
    DEALLOCATE( Obj % Row )

  ELSE
    j = SIZE( Obj % JA )
    IF( j .GT. Obj % nnz ) THEN
      Obj % ColSize = Obj % JA( 1 : Obj % nnz )
      DEALLOCATE( Obj % A, Obj % JA )
      ALLOCATE( Obj % A( Obj % nnz ), Obj % JA( Obj % nnz ) )
      Obj % JA = Obj % ColSize
      Obj % A = 0.0_DFP
      DEALLOCATE( Obj % ColSize )
      ALLOCATE( Obj % ColSize( Obj % ncol ) )
    END IF

    DO i = 1, Obj % nrow
      Obj % RowSize( i ) = Obj % IA( i + 1 ) - Obj % IA( i )
      IF( Obj % RowSize( i ) .NE. 0 ) THEN
        DO j = 1, Obj % IA( i ), Obj % IA( i + 1 ) - 1
          Obj % ColSize( Obj % JA( j ) ) = &
            & Obj % ColSize( Obj % JA( j ) ) + 1
          IF( Obj % JA( j ) .EQ. i ) THEN
            Obj % DiagIndx( i ) =j
          END IF
        END DO
      END IF
    END DO
  END IF

END PROCEDURE setSparsity_3

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE setValueInternally

  INTEGER( I4B ), ALLOCATABLE :: row( : )
  INTEGER( I4B ) :: i, j, k, n

  n = SIZE( Nptrs )
  ALLOCATE( row( Obj % tDOF * n ) )

  SELECT CASE( Obj%StorageFMT )
  CASE( Nodes_FMT )
    DO i = 1, n
      DO j = 1, Obj % tDOf
        row( ( i - 1 ) * Obj % tDOF + j ) = ( nptrs( i ) - 1 ) * Obj%tDOF + j
      END DO
    END DO
  CASE DEFAULT
    DO j = 1, Obj % tDOf
      DO i = 1, n
        row( ( j - 1 ) * n + i ) = ( j - 1 ) * Obj%tNodes + nptrs( i )
      END DO
    END DO
  END SELECT

  DO i =1, SIZE( row )
    DO k = 1, SIZE( row )
      DO j = Obj % IA( row( i ) ), Obj % IA( row( i ) + 1 ) - 1
        IF( Obj % JA( j ) .EQ. row( k ) ) THEN
          Obj % A( j ) = Val( i, k )
          EXIT
        END IF
      END DO
    END DO
  END DO
  DEALLOCATE( row )

END PROCEDURE setValueInternally

!----------------------------------------------------------------------------
!                                                                 setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE setValue_1
  REAL( DFP ), ALLOCATABLE :: Mat( :, : )

  SELECT CASE( StorageFMT )
  CASE( Nodes_FMT )
    IF( Obj%StorageFMT .EQ. Nodes_FMT ) THEN
      Mat = Val
    ELSE
      CALL Convert( From = Val, To = Mat, Conversion = NodesToDOF, &
        & nns = SIZE( Nptrs ), tDOF = Obj % tDOF )
    END IF
  CASE( DOF_FMT )
    IF( Obj%StorageFMT .EQ. DOF_FMT ) THEN
      Mat=Val
    ELSE
      CALL Convert( From = Val, To = Mat, Conversion = DofToNodes, &
        & nns = SIZE( Nptrs ), tDOF = Obj % tDOF )
    END IF
  END SELECT

  CALL setValueInternally( Obj, Nptrs, Mat )
  IF( ALLOCATED( Mat ) ) DEALLOCATE( Mat )

END PROCEDURE setValue_1

!----------------------------------------------------------------------------
!                                                                 setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE setValue_2
  Obj % A( : ) = 0.0_DFP
END PROCEDURE setValue_2

!----------------------------------------------------------------------------
!                                                            addContribution
!----------------------------------------------------------------------------

MODULE PROCEDURE addContributionInternally
  INTEGER( I4B ), ALLOCATABLE :: row( : )
  INTEGER( I4B ) :: i, j, k, n

  n = SIZE( Nptrs )
  ALLOCATE( row( Obj % tDOF * n ) )

  SELECT CASE( Obj%StorageFMT )
  CASE( Nodes_FMT )
    DO i = 1, n
      DO j = 1, Obj % tDOf
        row( ( i - 1 ) * Obj % tDOF + j ) = ( nptrs( i ) - 1 ) * Obj%tDOF + j
      END DO
    END DO
  CASE DEFAULT
    DO j = 1, Obj % tDOf
      DO i = 1, n
        row( ( j - 1 ) * n + i ) = ( j - 1 ) * Obj%tNodes + nptrs( i )
      END DO
    END DO
  END SELECT

  DO i =1, SIZE( row )
    DO k = 1, SIZE( row )
      DO j = Obj % IA( row( i ) ), Obj % IA( row( i ) + 1 ) - 1
        IF( Obj % JA( j ) .EQ. row( k ) ) THEN
          Obj % A( j ) = Obj % A( j ) + Scale * Val( i, k )
          EXIT
        END IF
      END DO
    END DO
  END DO
  DEALLOCATE( row )
END PROCEDURE addContributionInternally

!----------------------------------------------------------------------------
!                                                           addContribution
!----------------------------------------------------------------------------

MODULE PROCEDURE addContribution_1
  REAL( DFP ), ALLOCATABLE :: Mat( :, : )

  SELECT CASE( StorageFMT )
  CASE( Nodes_FMT )
    IF( Obj%StorageFMT .EQ. Nodes_FMT ) THEN
      Mat = Val
    ELSE
      CALL Convert( From = Val, To = Mat, Conversion = NodesToDOF, &
        & nns = SIZE( Nptrs ), tDOF = Obj % tDOF )
    END IF
  CASE( DOF_FMT )
    IF( Obj%StorageFMT .EQ. DOF_FMT ) THEN
      Mat=Val
    ELSE
      CALL Convert( From = Val, To = Mat, Conversion = DofToNodes, &
        & nns = SIZE( Nptrs ), tDOF = Obj % tDOF )
    END IF
  CASE DEFAULT
    Mat = Val
  END SELECT

  CALL addContributionInternally( Obj, Nptrs, Mat, Scale )
  IF( ALLOCATED( Mat ) ) DEALLOCATE( Mat )

END PROCEDURE addContribution_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE setMethod
