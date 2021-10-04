SUBMODULE(GE_Lapack_Method) LU
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     getf2
!----------------------------------------------------------------------------

MODULE PROCEDURE getLU_1
  INTEGER( I4B ) :: M, N, LDA, info
  TYPE( String ) :: solvername0


  IF( PRESENT( SolverName ) ) THEN
    solvername0 = String( TRIM(solverName) )
    solvername0 = solvername0%upper()
  ELSE
    solvername0 = String( 'GETRF' )
  END IF

  M = SIZE( A, 1 )
  N = SIZE( A, 2 )
  LDA = M
  LU( :, : ) = A( :, : )

  SELECT CASE( DFP )
  CASE( Real64 )
    SELECT CASE( TRIM(solvername0%chars()) )
    CASE( 'GETF2' )
      CALL DGETF2( M, N, LU, LDA, IPIV, info )
    CASE( 'GETRF' )
      CALL DGETRF( M, N, LU, LDA, IPIV, info )
    CASE( 'GETRF2' )
      CALL DGETRF2( M, N, LU, LDA, IPIV, info )
    END SELECT

  CASE( Real32 )
    SELECT CASE( TRIM(solvername0%chars()) )
    CASE( 'GETF2' )
      CALL SGETF2( M, N, LU, LDA, IPIV, info )
    CASE( 'GETRF' )
      CALL SGETRF( M, N, LU, LDA, IPIV, info )
    CASE( 'GETRF2' )
      CALL SGETRF2( M, N, LU, LDA, IPIV, info )
    END SELECT
  END SELECT

  IF( info .NE. 0 ) THEN
    CALL Display( val=info, msg= "getLU returned info =", unitNo=stdout )
    IF( info < 0 ) THEN
      CALL Display( msg = trim(str(-info)) // "-th argument had an illegal value", &
        & unitNo = stdout )
    ELSE
      CALL Display( &
      & msg = "U(" // TRIM( str( info ) ) // "," // TRIM( str( info ) ) // &
        & " ) is exactly zero; The factorization &
        & has been completed, but the factor U is exactly &
        & singular, and division by zero will occur if it is used &
        & to solve a system of equations", &
      & unitNo = stdout )
    END IF

    CALL Display( Val=__FILE__, msg="Error: In File ")
    CALL Display( Val= __LINE__, msg="In line :: " )
    STOP
  ENDIF

END PROCEDURE getLU_1

END SUBMODULE LU