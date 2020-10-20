SUBMODULE( SparseMatrix_Method ) ILUT
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             Sparsekit_ILUT
!----------------------------------------------------------------------------

MODULE PROCEDURE ilut_csr
  INTEGER( I4B ) :: iwk, fac, nnz, lfil0
  REAL( DFP ) :: droptol0
  INTEGER( I4B ), ALLOCATABLE :: jw( : )
  REAL( DFP ), ALLOCATABLE :: w( : )

  IF( PRESENT( lfil ) ) THEN; lfil0 = lfil; ELSE; lfil0 = 5; END IF
  IF( PRESENT( droptol ) ) THEN; droptol0 = droptol; ELSE; droptol0 = 1.0E-4; END IF
  ALLOCATE( w( Obj % nrow + 1 ), jw( 2 * Obj % nrow ) )
  CALL REALLOCATE( ju, Obj % nrow )

  fac = 2; nnz = getNNZ( Obj )

100   iwk = fac * nnz

  CALL REALLOCATE( alu, iwk  )
  CALL REALLOCATE( jlu, iwk )

  CALL ILUT( Obj % nrow, Obj % A, Obj % JA, Obj % IA, &
    & lfil0, droptol0, alu, jlu, ju, iwk, w, jw, ierr )

  SELECT CASE( ierr )
  CASE( 0 )
    RETURN
  CASE( 1: )
    WRITE( stdout, "(A, I4)" ) "zero pivot encountered at step number = ", ierr
  CASE( -1 )
    WRITE( stdout, "(A)" ) "Error. input matrix may be wrong"
  CASE( -2 )
    fac = fac + 1
    goto 100
  CASE( -3 )
    fac = fac + 1
    goto 100
  CASE( -4 )
    WRITE( stdout, "(A)" ) "Error Illegal value for lfil"
  CASE( -5 )
    WRITE( stdout, "(A)" ) "Error zero row encountered"
  END SELECT
  DEALLOCATE( w, jw )
END PROCEDURE ilut_csr

!----------------------------------------------------------------------------
!                                                            Sparsekit_ILUTP
!----------------------------------------------------------------------------

MODULE PROCEDURE ilutp_csr
  INTEGER( I4B ) :: iwk, fac, nnz, lfil0, mbloc0
  REAL( DFP ) :: droptol0, permtol0
  INTEGER( I4B ), ALLOCATABLE :: jw( : )
  REAL( DFP ), ALLOCATABLE :: w( : )

  IF( PRESENT( lfil ) ) THEN; lfil0 = lfil; ELSE; lfil0 = 5; END IF
  IF( PRESENT( mbloc ) ) THEN; mbloc0 = mbloc; ELSE; mbloc0 = Obj % nrow; END IF
  IF( PRESENT( droptol ) ) THEN; droptol0 = droptol; ELSE; droptol0 = 1.0E-4; END IF
  IF( PRESENT( permtol ) ) THEN; permtol0 = permtol; ELSE; permtol0 = 0.5; END IF

  ALLOCATE( w( Obj % nrow + 1 ), jw( 2 * Obj % nrow ) )
  CALL REALLOCATE( ju, Obj % nrow )
  CALL REALLOCATE( iperm, 2 * Obj % nrow )

  fac = 2; nnz = getNNZ( Obj )

100   iwk = fac * nnz

  CALL REALLOCATE( alu, iwk  )
  CALL REALLOCATE( jlu, iwk )

  CALL ILUTP( Obj % nrow, Obj % A, Obj % JA, Obj % IA, &
    & lfil0, droptol0, permtol0, mbloc0, alu, jlu, ju, &
    & iwk, w, jw, iperm, ierr )

  SELECT CASE( ierr )
  CASE( 0 )
    RETURN
  CASE( 1: )
    WRITE( stdout, "(A, I4)" ) "zero pivot encountered at step number = ", ierr
  CASE( -1 )
    WRITE( stdout, "(A)" ) "Error. input matrix may be wrong"
  CASE( -2 )
    fac = fac + 1
    goto 100
  CASE( -3 )
    fac = fac + 1
    goto 100
  CASE( -4 )
    WRITE( stdout, "(A)" ) "Error Illegal value for lfil"
  CASE( -5 )
    WRITE( stdout, "(A)" ) "Error zero row encountered"
  END SELECT
  DEALLOCATE( w, jw )
END PROCEDURE ilutp_csr

!----------------------------------------------------------------------------
!                                                             Sparsekit_ILUD
!----------------------------------------------------------------------------

MODULE PROCEDURE ilud_csr
  INTEGER( I4B ) :: iwk, fac, nnz
  REAL( DFP ) :: droptol0, alpha0
  INTEGER( I4B ), ALLOCATABLE :: jw( : )
  REAL( DFP ), ALLOCATABLE :: w( : )

  IF( PRESENT( droptol ) ) THEN; droptol0 = droptol; ELSE; droptol0 = 1.0E-4; END IF
  IF( PRESENT( alpha ) ) THEN; alpha0 = alpha; ELSE; alpha0 = 1.0; END IF
  ALLOCATE( w( 2 * Obj % nrow ), jw( 2 * Obj % nrow ) )

  CALL REALLOCATE( ju, Obj % nrow )

  fac = 2; nnz = getNNZ( Obj )

100   iwk = fac * nnz

  CALL REALLOCATE( alu, iwk  )
  CALL REALLOCATE( jlu, iwk )

  CALL ILUD( Obj % nrow, Obj % A, Obj % JA, Obj % IA, &
    & alpha0, droptol0, alu, jlu, ju, iwk, w, jw, ierr )

  SELECT CASE( ierr )
  CASE( 0 )
    RETURN
  CASE( 1: )
    WRITE( stdout, "(A, I4)" ) "zero pivot encountered at step number = ", ierr
  CASE( -1 )
    WRITE( stdout, "(A)" ) "Error. input matrix may be wrong"
  CASE( -2 )
    fac = fac + 1
    goto 100
  CASE( -3 )
    WRITE( stdout, "(A)" ) "Error. zero row encounter"
  END SELECT
  DEALLOCATE( w, jw )
END PROCEDURE ilud_csr

!----------------------------------------------------------------------------
!                                                             Sparsekit_ILUD
!----------------------------------------------------------------------------

MODULE PROCEDURE iludp_csr
  INTEGER( I4B ) :: iwk, fac, nnz, mbloc0
  REAL( DFP ) :: droptol0, permtol0, alpha0
  INTEGER( I4B ), ALLOCATABLE :: jw( : )
  REAL( DFP ), ALLOCATABLE :: w( : )

  IF( PRESENT( mbloc ) ) THEN; mbloc0 = mbloc; ELSE; mbloc0 = Obj % nrow; END IF
  IF( PRESENT( droptol ) ) THEN; droptol0 = droptol; ELSE; droptol0 = 1.0E-4; END IF
  IF( PRESENT( permtol ) ) THEN; permtol0 = permtol; ELSE; permtol0 = 0.5; END IF
  IF( PRESENT( alpha ) ) THEN; alpha0 = alpha; ELSE; alpha0 = 1.0; END IF

  ALLOCATE( w( Obj % nrow ), jw( 2 * Obj % nrow ) )
  CALL REALLOCATE( ju, Obj % nrow )
  CALL REALLOCATE( iperm, 2 * Obj % nrow )

  fac = 2; nnz = getNNZ( Obj )

100   iwk = fac * nnz

  CALL REALLOCATE( alu, iwk  )
  CALL REALLOCATE( jlu, iwk )

  CALL ILUDP( Obj % nrow, Obj % A, Obj % JA, Obj % IA, &
    & alpha0, droptol0, permtol0, mbloc0, alu, jlu, ju, &
    & iwk, w, jw, iperm, ierr )

  SELECT CASE( ierr )
  CASE( 0 )
    RETURN
  CASE( 1: )
    WRITE( stdout, "(A, I4)" ) " &
      & In Sparsematrix_method@CSR >> iludp_csr >> error: &
      & zero pivot encountered at step number = ", ierr
  CASE( -1 )
    WRITE( stdout, "(A)" ) "Error. input matrix may be wrong"
  CASE( -2 )
    fac = fac + 1
    goto 100
  CASE( -3 )
    WRITE( stdout, "(A)" ) "Error. zero row encounter"
  END SELECT
  DEALLOCATE( w, jw )
END PROCEDURE iludp_csr

!----------------------------------------------------------------------------
!                                                           Sparsekit_lusolve
!----------------------------------------------------------------------------

MODULE PROCEDURE lusol_alu
  CALL Reallocate( x, SIZE( y ) )
  CALL LUSOL( SIZE( x ), y, x, alu, jlu, ju )
END PROCEDURE lusol_alu

!----------------------------------------------------------------------------
!                                                           Sparsekit_lutsolve
!----------------------------------------------------------------------------

MODULE PROCEDURE lutsol_alu
  CALL Reallocate( x, SIZE( y ) )
  CALL LUTSOL( SIZE( x ), y, x, alu, jlu, ju )
END PROCEDURE lutsol_alu

END SUBMODULE ILUT