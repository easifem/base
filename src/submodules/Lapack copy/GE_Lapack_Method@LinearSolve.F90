SUBMODULE( GE_Lapack_Method ) LinearSolve
USE BaseMethod
IMPLICIT NONE
CONTAINS


!----------------------------------------------------------------------------
!                                                                      Solve
!----------------------------------------------------------------------------

MODULE PROCEDURE ge_solve_1
  INTEGER( I4B ) :: m, n
  m = SIZE( A, 1 )
  n = SIZE( A, 2 )
  IF( m .EQ. n ) THEN
    CALL GESV( A = A, b = b, x = x )
  ELSE
    SELECT CASE( TRIM( Solver ) )
    CASE( "GELS" )
      CALL GELS( A=A, b=b, x=x )
    CASE( "GELSD" )
      CALL GELSD( A=A, b=b, x=x )
    CASE( "GELSS" )
      CALL GELSS( A=A, b=b, x=x )
    CASE DEFAULT
      CALL GELS( A=A, b=b, x=x )
    END SELECT
  END IF
END PROCEDURE ge_solve_1

!----------------------------------------------------------------------------
!                                                                      Solve
!----------------------------------------------------------------------------

MODULE PROCEDURE ge_solve_2
  INTEGER( I4B ) :: m, n
  m = SIZE( A, 1 )
  n = SIZE( A, 2 )
  IF( m .EQ. n ) THEN
    CALL GESV( A = A, b = b, x = x )
  ELSE
    SELECT CASE( TRIM( Solver ) )
    CASE( "GELS" )
      CALL GELS( A=A, b=b, x=x )
    CASE( "GELSD" )
      CALL GELSD( A=A, b=b, x=x )
    CASE( "GELSS" )
      CALL GELSS( A=A, b=b, x=x )
    CASE DEFAULT
      CALL GELS( A=A, b=b, x=x )
    END SELECT
  END IF
END PROCEDURE ge_solve_2

!----------------------------------------------------------------------------
!                                                                      GESV
!----------------------------------------------------------------------------

MODULE PROCEDURE gesv_1
  ! Define internal variables
  INTEGER( I4B ) :: n
  REAL( DFP ) :: At( SIZE(b),SIZE(b)), bt(SIZE(b),1)
  INTEGER( I4B ) :: ipiv(SIZE(b))
  INTEGER( I4B ) :: n, info, lda, nrhs

  n = SIZE( A, 2 )
  lda = size( A, 1 )
  nrhs = 1
  CALL ASSERT( mat=A, s=[n, n], file=__FILE__, routine="ge_solve_1", &
    & line=__LINE__, msg="A should be a square matrix" )

  At = A
  bt(:,1) = b(:)

  ! LAPACK call
  SELECT CASE( DFP )
  CASE ( Real64 )
    CALL DGESV( n, nrhs, At, lda, ipiv, bt, n, info )
  CASE ( Real32 )
    CALL SGESV(n, nrhs, At, lda, ipiv, bt, n, info)
  END SELECT

  IF( info .NE. 0 ) THEN
    CALL Display( val=info, msg= "DGESV from LAPCK returned info =", unitNo=stdout )
    IF( info < 0 ) THEN
      CALL Display( &
        & msg = trim(str(-info)) // "-th argument had an illegal value!!!", &
        & unitNo = stdout )
    ELSE
      PRINT *, "U(", info, ",", info, ") is exactly zero; The factorization"
      PRINT *, "has been completed, but the factor U is exactly"
      PRINT *, "singular, so the solution could not be computed."
    END IF

    CALL Display( Val=__FILE__, msg="ERROR:: In File ")
    CALL Display( Val= __LINE__, msg="In Line :: " )
    CALL Display( msg="In line routine GESV_1 DGESV error" )
    STOP
  ENDIF
  x( : ) = bt(:,1)
END PROCEDURE gesv_1

!----------------------------------------------------------------------------
!                                                                      GESV
!----------------------------------------------------------------------------

MODULE PROCEDURE gesv_2

  ! Define internal variables
  INTEGER( I4B ) :: n
  REAL( DFP ) :: At( SIZE(A,1),SIZE(A,2)), bt(SIZE(b,1), SIZE(b,2))
  INTEGER( I4B ) :: ipiv(SIZE(b,1))
  INTEGER( I4B ) :: n, info, lda, nrhs

  n = SIZE( A, 2 )
  lda = SIZE( A, 1 )
  nrhs = SIZE(b,2)
  CALL ASSERT( mat=A, s=[n, n], file=__FILE__, routine="ge_solve_1", &
    & line=__LINE__, msg="A should be a square matrix" )
  At = A
  bt = b
  ! LAPACK call
  SELECT CASE( DFP )
  CASE ( Real64 )
    CALL DGESV( n, nrhs, At, lda, ipiv, bt, n, info )
  CASE ( Real32 )
    CALL SGESV(n, nrhs, At, lda, ipiv, bt, n, info)
  END SELECT

  IF( info .NE. 0 ) THEN
    CALL Display( val=info, msg= "dgesv returned info =", unitNo=stdout )
    IF( info < 0 ) THEN
      CALL Display( msg = trim(str(-info)) // "-th argument had an illegal value", &
        & unitNo = stdout )
    ELSE
      PRINT *, "U(", info, ",", info, ") is exactly zero; The factorization"
      PRINT *, "has been completed, but the factor U is exactly"
      PRINT *, "singular, so the solution could not be computed."
    END IF

    CALL Display( Val=__FILE__, msg="Error: In File ")
    CALL Display( Val= __LINE__, msg="In line :: " )
    CALL Display( msg="In line routine GESV_2 DGESV error" )
    STOP
  ENDIF
  x = bt
END PROCEDURE gesv_2

!----------------------------------------------------------------------------
!                                                                      GELS
!----------------------------------------------------------------------------

MODULE PROCEDURE gels_1
  REAL( DFP ) :: At( SIZE( A, 1 ), SIZE( A, 2 ) )
  REAL( DFP ), ALLOCATABLE :: bt( :, : )
  REAL( DFP ), ALLOCATABLE :: WORK( : )

  CHARACTER( LEN = 1 ) :: TRANS

  INTEGER( I4B ) :: M, N, NRHS, LDA, LDB, LWORK, INFO, MN

  M = SIZE( A, 1 )
  N = SIZE( A, 2 )
  LDA = SIZE( A, 1 )
  LDB = MAX( M, N )
  MN = MIN( M, N )
  NRHS = 1
  TRANS = "N"
  LWORK =  -1
  ALLOCATE( Work( 2*MN ) )
  ALLOCATE( bt( LDB, 1 ) )
  At( 1:M, 1:N ) = A( 1:M, 1:N )
  bt( 1:M, 1 ) = b( 1:M )

  ! find size of LWORK
  SELECT CASE( DFP )
  CASE( Real64 )
    CALL DGELS( TRANS, M, N, NRHS, At, LDA, bt, LDB, WORK, LWORK, INFO )
  CASE ( Real32 )
    CALL DGELS( TRANS, M, N, NRHS, At, LDA, bt, LDB, WORK, LWORK, INFO )
  END SELECT

  !! Now LWORK is stored inside the WORK(1)
  !! Reallocate WORK
  LWORK = WORK( 1 )
  CALL Reallocate( WORK, LWORK )

  SELECT CASE( DFP )
  CASE( Real64 )
    CALL DGELS( TRANS, M, N, NRHS, At, LDA, bt, LDB, WORK, LWORK, INFO )
  CASE ( Real32 )
    CALL DGELS( TRANS, M, N, NRHS, At, LDA, bt, LDB, WORK, LWORK, INFO )
  END SELECT

  IF( INFO .NE. 0 ) THEN
    IF( INFO .GT. 0 ) THEN
      CALL Display( __FILE__, msg="In File :: ", unitno=stdout )
      CALL Display( __LINE__, msg= "Line number :: ", unitno=stdout )
      CALL Display( "gels_1()", msg="Subroutine :: ", unitno=stdout )
      CALL Display(  msg = "The " // STR( INFO ) // &
        & "-th diagonal element of the triangular factor of A is zero, &
        & so that A does not have full rank. The least squares solution &
        & could not be computed", unitno=stdout )
    ELSE
      CALL Display( __FILE__, msg="In File :: ", unitno=stdout )
      CALL Display( __LINE__, msg= "Line number :: ", unitno=stdout )
      CALL Display( "gels_1()", msg="Subroutine :: ", unitno=stdout )
      CALL Display(  msg = "The " // STR( -INFO ) // &
        & "-th argument had an illegal value", unitno=stdout )
    END IF
  END IF

  ! If m>n, rows 1:N of bt contains the least squares solution vectors
  ! If m<n, rows 1:N of bt contains the least minimum norm solution vectors
  x( 1 : N ) = bt( 1 : N, 1 )
  DEALLOCATE( WORK, bt )

END PROCEDURE gels_1

!----------------------------------------------------------------------------
!                                                                      GELS
!----------------------------------------------------------------------------

MODULE PROCEDURE gels_2
  REAL( DFP ) :: At( SIZE( A, 1 ), SIZE( A, 2 ) )
  REAL( DFP ), ALLOCATABLE :: bt( :, : )
  REAL( DFP ), ALLOCATABLE :: WORK( : )

  CHARACTER( LEN = 1 ) :: TRANS

  INTEGER( I4B ) :: M, N, NRHS, LDA, LDB, LWORK, INFO, MN

  M = SIZE( A, 1 )
  N = SIZE( A, 2 )
  LDA = SIZE( A, 1 )
  LDB = MAX( M, N )
  MN = MIN( M, N )
  NRHS = SIZE( b, 2 )
  TRANS = "N"
  LWORK =  -1
  ALLOCATE( Work( 2*MN ) )
  ALLOCATE( bt( LDB, NRHS ) )
  At( 1:M, 1:N ) = A( 1:M, 1:N )
  bt( 1:M, 1:NRHS ) = b( 1:M, 1:NRHS )

  ! find size of LWORK
  SELECT CASE( DFP )
  CASE( Real64 )
    CALL DGELS( TRANS, M, N, NRHS, At, LDA, bt, LDB, WORK, LWORK, INFO )
  CASE ( Real32 )
    CALL DGELS( TRANS, M, N, NRHS, At, LDA, bt, LDB, WORK, LWORK, INFO )
  END SELECT

  !! Now LWORK is stored inside the WORK(1)
  !! Reallocate WORK
  LWORK = WORK( 1 )
  CALL Reallocate( WORK, LWORK )

  SELECT CASE( DFP )
  CASE( Real64 )
    CALL DGELS( TRANS, M, N, NRHS, At, LDA, bt, LDB, WORK, LWORK, INFO )
  CASE ( Real32 )
    CALL DGELS( TRANS, M, N, NRHS, At, LDA, bt, LDB, WORK, LWORK, INFO )
  END SELECT

  IF( INFO .NE. 0 ) THEN
    IF( INFO .GT. 0 ) THEN
      CALL Display( __FILE__, msg="In File :: ", unitno=stdout )
      CALL Display( __LINE__, msg= "Line number :: ", unitno=stdout )
      CALL Display( "gels_1()", msg="Subroutine :: ", unitno=stdout )
      CALL Display(  msg = "The " // STR( INFO ) // &
        & "-th diagonal element of the triangular factor of A is zero, &
        & so that A does not have full rank. The least squares solution &
        & could not be computed", unitno=stdout )
    ELSE
      CALL Display( __FILE__, msg="In File :: ", unitno=stdout )
      CALL Display( __LINE__, msg= "Line number :: ", unitno=stdout )
      CALL Display( "gels_1()", msg="Subroutine :: ", unitno=stdout )
      CALL Display(  msg = "The " // STR( -INFO ) // &
        & "-th argument had an illegal value", unitno=stdout )
    END IF
  END IF

  ! If m>n, rows 1:N of bt contains the least squares solution vectors
  ! If m<n, rows 1:N of bt contains the least minimum norm solution vectors
  x( 1 : N, 1:NRHS ) = bt( 1 : N, 1:NRHS )
  DEALLOCATE( WORK, bt )
END PROCEDURE gels_2

!----------------------------------------------------------------------------
!                                                                     GELSD
!----------------------------------------------------------------------------

MODULE PROCEDURE gelsd_1
  REAL( DFP ) :: At( SIZE( A, 1 ), SIZE( A, 2 ) )
  REAL( DFP ), ALLOCATABLE :: bt( :, : )
  REAL( DFP ), ALLOCATABLE :: WORK( : )
  REAL( DFP ), ALLOCATABLE :: S( : )
  INTEGER( I4B ), ALLOCATABLE :: IWORK( : )

  INTEGER( I4B ) :: M, N, NRHS, LDA, LDB, LWORK, INFO, MN, RANK
  REAL( DFP ) :: RCOND

  M = SIZE( A, 1 )
  N = SIZE( A, 2 )
  LDA = SIZE( A, 1 )
  LDB = MAX( M, N )
  MN = MIN( M, N )
  NRHS = 1
  LWORK =  -1
  ALLOCATE( Work( 2 ), bt( LDB, NRHS ), S( MN ), IWORK( 2 ) )

  ! RCOND = -1.0_DFP !! In this case machine precision will be used
  RCOND = 1.0E-8

  At( 1:M, 1:N ) = A( 1:M, 1:N )
  bt( 1:M, 1 ) = b( 1:M )

  ! find size of WORK
  SELECT CASE( DFP )
  CASE( Real64 )
    CALL DGELSD( M, N, NRHS, At, LDA, bt, LDB, S, RCOND, RANK, WORK, LWORK, &
      & IWORK, INFO )
  CASE ( Real32 )
    CALL SGELSD( M, N, NRHS, At, LDA, bt, LDB, S, RCOND, RANK, WORK, LWORK, &
      & IWORK, INFO )
  END SELECT

  !! Now LWORK is stored inside the WORK(1)
  !! Reallocate WORK
  LWORK = WORK( 1 )
  CALL Reallocate( WORK, LWORK, IWORK, IWORK( 1 ) )

  SELECT CASE( DFP )
  CASE( Real64 )
    CALL DGELSD( M, N, NRHS, At, LDA, bt, LDB, S, RCOND, RANK, WORK, LWORK, &
      & IWORK, INFO )
  CASE ( Real32 )
    CALL SGELSD( M, N, NRHS, At, LDA, bt, LDB, S, RCOND, RANK, WORK, LWORK, &
      & IWORK, INFO )
  END SELECT

  IF( INFO .NE. 0 ) THEN
    IF( INFO .GT. 0 ) THEN
      CALL Display( __FILE__, msg="In File :: ", unitno=stdout )
      CALL Display( __LINE__, msg= "Line number :: ", unitno=stdout )
      CALL Display( "gelsd_1()", msg="Subroutine :: ", unitno=stdout )
      CALL Display(  msg = "The algorithm for computing the SVD &
        & failed to converge; " &
        & // STR( INFO ) // &
        & "-th off-diagonal elements of an intermediate bidiagonal form did &
        & not converge to zero", unitno=stdout )
    ELSE
      CALL Display( __FILE__, msg="In File :: ", unitno=stdout )
      CALL Display( __LINE__, msg= "Line number :: ", unitno=stdout )
      CALL Display( "gelsd_1()", msg="Subroutine :: ", unitno=stdout )
      CALL Display(  msg = "The " // STR( -INFO ) // &
        & "-th argument had an illegal value", unitno=stdout )
    END IF
  END IF

  ! If m>n, rows 1:N of bt contains the least squares solution vectors
  ! If m<n, rows 1:N of bt contains the least minimum norm solution vectors
  x( 1 : N ) = bt( 1 : N, 1 )
  DEALLOCATE( WORK, IWORK, bt, S )
END PROCEDURE gelsd_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE gelsd_2
  REAL( DFP ) :: At( SIZE( A, 1 ), SIZE( A, 2 ) )
  REAL( DFP ), ALLOCATABLE :: bt( :, : )
  REAL( DFP ), ALLOCATABLE :: WORK( : )
  REAL( DFP ), ALLOCATABLE :: S( : )
  INTEGER( I4B ), ALLOCATABLE :: IWORK( : )

  INTEGER( I4B ) :: M, N, NRHS, LDA, LDB, LWORK, INFO, MN, RANK
  REAL( DFP ) :: RCOND

  M = SIZE( A, 1 )
  N = SIZE( A, 2 )
  LDA = SIZE( A, 1 )
  LDB = MAX( M, N )
  MN = MIN( M, N )
  NRHS = SIZE( b, 2 )
  LWORK =  -1
  ALLOCATE( Work( 2 ), bt( LDB, NRHS ), S( MN ), IWORK( 2 ) )

  ! RCOND = -1.0_DFP !! In this case machine precision will be used
  RCOND = 1.0E-8

  At( 1:M, 1:N ) = A( 1:M, 1:N )
  bt( 1:M, 1:NRHS ) = b( 1:M, 1:NRHS )

  ! find size of WORK
  SELECT CASE( DFP )
  CASE( Real64 )
    CALL DGELSD( M, N, NRHS, At, LDA, bt, LDB, S, RCOND, RANK, WORK, LWORK, &
      & IWORK, INFO )
  CASE ( Real32 )
    CALL SGELSD( M, N, NRHS, At, LDA, bt, LDB, S, RCOND, RANK, WORK, LWORK, &
      & IWORK, INFO )
  END SELECT

  !! Now LWORK is stored inside the WORK(1)
  !! Reallocate WORK
  LWORK = WORK( 1 )
  CALL Reallocate( WORK, LWORK, IWORK, IWORK( 1 ) )

  SELECT CASE( DFP )
  CASE( Real64 )
    CALL DGELSD( M, N, NRHS, At, LDA, bt, LDB, S, RCOND, RANK, WORK, LWORK, &
      & IWORK, INFO )
  CASE ( Real32 )
    CALL SGELSD( M, N, NRHS, At, LDA, bt, LDB, S, RCOND, RANK, WORK, LWORK, &
      & IWORK, INFO )
  END SELECT

  IF( INFO .NE. 0 ) THEN
    IF( INFO .GT. 0 ) THEN
      CALL Display( __FILE__, msg="In File :: ", unitno=stdout )
      CALL Display( __LINE__, msg= "Line number :: ", unitno=stdout )
      CALL Display( "gelsd_2()", msg="Subroutine :: ", unitno=stdout )
      CALL Display(  msg = "The algorithm for computing the SVD &
        & failed to converge; " &
        & // STR( INFO ) // &
        & "-th off-diagonal elements of an intermediate bidiagonal form did &
        & not converge to zero", unitno=stdout )
    ELSE
      CALL Display( __FILE__, msg="In File :: ", unitno=stdout )
      CALL Display( __LINE__, msg= "Line number :: ", unitno=stdout )
      CALL Display( "gelsd_2()", msg="Subroutine :: ", unitno=stdout )
      CALL Display(  msg = "The " // STR( -INFO ) // &
        & "-th argument had an illegal value", unitno=stdout )
    END IF
  END IF

  ! If m>n, rows 1:N of bt contains the least squares solution vectors
  ! If m<n, rows 1:N of bt contains the least minimum norm solution vectors
  x( 1:N, 1:NRHS ) = bt( 1:N, 1:NRHS )
  DEALLOCATE( WORK, IWORK, bt, S )
END PROCEDURE gelsd_2

!----------------------------------------------------------------------------
!                                                                      GELSS
!----------------------------------------------------------------------------

MODULE PROCEDURE gelss_1
  REAL( DFP ) :: At( SIZE( A, 1 ), SIZE( A, 2 ) )
  REAL( DFP ), ALLOCATABLE :: bt( :, : )
  REAL( DFP ), ALLOCATABLE :: WORK( : )
  REAL( DFP ), ALLOCATABLE :: S( : )

  INTEGER( I4B ) :: M, N, NRHS, LDA, LDB, LWORK, INFO, MN, RANK
  REAL( DFP ) :: RCOND

  M = SIZE( A, 1 )
  N = SIZE( A, 2 )
  LDA = SIZE( A, 1 )
  LDB = MAX( M, N )
  MN = MIN( M, N )
  NRHS = 1
  LWORK =  -1
  ALLOCATE( Work( 2*MN ), bt( LDB, NRHS ), S( MN ) )

  ! RCOND = -1.0_DFP !! In this case machine precision will be used
  RCOND = 1.0E-8

  At( 1:M, 1:N ) = A( 1:M, 1:N )
  bt( 1:M, 1 ) = b( 1:M )

  ! find size of WORK
  SELECT CASE( DFP )
  CASE( Real64 )
    CALL DGELSS( M, N, NRHS, At, LDA, bt, LDB, S, RCOND, RANK, WORK, LWORK, &
      & INFO )
  CASE ( Real32 )
    CALL SGELSS( M, N, NRHS, At, LDA, bt, LDB, S, RCOND, RANK, WORK, LWORK, &
      & INFO )
  END SELECT

  !! Now LWORK is stored inside the WORK(1)
  !! Reallocate WORK
  LWORK = WORK( 1 )
  CALL Reallocate( WORK, LWORK )

  SELECT CASE( DFP )
  CASE( Real64 )
    CALL DGELSS( M, N, NRHS, At, LDA, bt, LDB, S, RCOND, RANK, WORK, LWORK, &
      & INFO )
  CASE ( Real32 )
    CALL SGELSS( M, N, NRHS, At, LDA, bt, LDB, S, RCOND, RANK, WORK, LWORK, &
      & INFO )
  END SELECT

  IF( INFO .NE. 0 ) THEN
    IF( INFO .GT. 0 ) THEN
      CALL Display( __FILE__, msg="In File :: ", unitno=stdout )
      CALL Display( __LINE__, msg= "Line number :: ", unitno=stdout )
      CALL Display( "gelss_1()", msg="Subroutine :: ", unitno=stdout )
      CALL Display(  msg = "The algorithm for computing the SVD &
        & failed to converge; " &
        & // STR( INFO ) // &
        & "-th off-diagonal elements of an intermediate bidiagonal form did &
        & not converge to zero", unitno=stdout )
    ELSE
      CALL Display( __FILE__, msg="In File :: ", unitno=stdout )
      CALL Display( __LINE__, msg= "Line number :: ", unitno=stdout )
      CALL Display( "gelss_1()", msg="Subroutine :: ", unitno=stdout )
      CALL Display(  msg = "The " // STR( -INFO ) // &
        & "-th argument had an illegal value", unitno=stdout )
    END IF
  END IF

  ! If m>n, rows 1:N of bt contains the least squares solution vectors
  ! If m<n, rows 1:N of bt contains the least minimum norm solution vectors
  x( 1 : N ) = bt( 1 : N, 1 )
  DEALLOCATE( WORK, bt, S )
END PROCEDURE gelss_1

!----------------------------------------------------------------------------
!                                                                     GELSS
!----------------------------------------------------------------------------

MODULE PROCEDURE gelss_2
  REAL( DFP ) :: At( SIZE( A, 1 ), SIZE( A, 2 ) )
  REAL( DFP ), ALLOCATABLE :: bt( :, : )
  REAL( DFP ), ALLOCATABLE :: WORK( : )
  REAL( DFP ), ALLOCATABLE :: S( : )

  INTEGER( I4B ) :: M, N, NRHS, LDA, LDB, LWORK, INFO, MN, RANK
  REAL( DFP ) :: RCOND

  M = SIZE( A, 1 )
  N = SIZE( A, 2 )
  LDA = SIZE( A, 1 )
  LDB = MAX( M, N )
  MN = MIN( M, N )
  NRHS = SIZE( b, 2 )
  LWORK =  -1
  ALLOCATE( Work( 2*MN ), bt( LDB, NRHS ), S( MN ) )

  ! RCOND = -1.0_DFP !! In this case machine precision will be used
  RCOND = 1.0E-8

  At( 1:M, 1:N ) = A( 1:M, 1:N )
  bt( 1:M, 1:NRHS ) = b( 1:M, 1:NRHS )

  ! find size of WORK
  SELECT CASE( DFP )
  CASE( Real64 )
    CALL DGELSS( M, N, NRHS, At, LDA, bt, LDB, S, RCOND, RANK, WORK, LWORK, &
      & INFO )
  CASE ( Real32 )
    CALL SGELSS( M, N, NRHS, At, LDA, bt, LDB, S, RCOND, RANK, WORK, LWORK, &
      & INFO )
  END SELECT

  !! Now LWORK is stored inside the WORK(1)
  !! Reallocate WORK
  LWORK = WORK( 1 )
  CALL Reallocate( WORK, LWORK )

  SELECT CASE( DFP )
  CASE( Real64 )
    CALL DGELSS( M, N, NRHS, At, LDA, bt, LDB, S, RCOND, RANK, WORK, LWORK, &
      & INFO )
  CASE ( Real32 )
    CALL SGELSS( M, N, NRHS, At, LDA, bt, LDB, S, RCOND, RANK, WORK, LWORK, &
      & INFO )
  END SELECT

  IF( INFO .NE. 0 ) THEN
    IF( INFO .GT. 0 ) THEN
      CALL Display( __FILE__, msg="In File :: ", unitno=stdout )
      CALL Display( __LINE__, msg= "Line number :: ", unitno=stdout )
      CALL Display( "gelss_2()", msg="Subroutine :: ", unitno=stdout )
      CALL Display(  msg = "The algorithm for computing the SVD &
        & failed to converge; " &
        & // STR( INFO ) // &
        & "-th off-diagonal elements of an intermediate bidiagonal form did &
        & not converge to zero", unitno=stdout )
    ELSE
      CALL Display( __FILE__, msg="In File :: ", unitno=stdout )
      CALL Display( __LINE__, msg= "Line number :: ", unitno=stdout )
      CALL Display( "gelss_2()", msg="Subroutine :: ", unitno=stdout )
      CALL Display(  msg = "The " // STR( -INFO ) // &
        & "-th argument had an illegal value", unitno=stdout )
    END IF
  END IF

  ! If m>n, rows 1:N of bt contains the least squares solution vectors
  ! If m<n, rows 1:N of bt contains the least minimum norm solution vectors
  x( 1:N, 1:NRHS ) = bt( 1:N, 1:NRHS )
  DEALLOCATE( WORK, bt, S )
END PROCEDURE gelss_2

!----------------------------------------------------------------------------
!                                                                     GELSY
!----------------------------------------------------------------------------

MODULE PROCEDURE gelsy_1
  CALL Display( __FILE__, msg="In File :: ", unitno=stdout )
  CALL Display( __LINE__, msg= "Line number :: ", unitno=stdout )
  CALL Display( "gelsy_1()", msg="Subroutine :: ", unitno=stdout )
  CALL Display(  msg = "GELSY has not been implemented yet", unitno=stdout )
  STOP
END PROCEDURE gelsy_1

!----------------------------------------------------------------------------
!                                                                     GELSY
!----------------------------------------------------------------------------

MODULE PROCEDURE gelsy_2
  CALL Display( __FILE__, msg="In File :: ", unitno=stdout )
  CALL Display( __LINE__, msg= "Line number :: ", unitno=stdout )
  CALL Display( "gelss_2()", msg="Subroutine :: ", unitno=stdout )
  CALL Display(  msg = "GELSY has not been implemented yet", unitno=stdout )
  STOP
END PROCEDURE gelsy_2



END SUBMODULE LinearSolve