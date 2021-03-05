SUBMODULE( SparseMatrix_Method ) MatVec
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                     Matvec
!----------------------------------------------------------------------------

MODULE PROCEDURE matvec_csr_amux
  CALL Reallocate( y, Obj % nrow )
  SELECT CASE( TRIM( matvectype ) )
  CASE( 'AX', 'AMUX' )
    CALL AMUX( Obj % nrow, x, y, Obj % A, Obj % JA, Obj % IA )
  CASE( 'ATX', 'ATMUX' )
    IF( Obj % nrow .NE. Obj % ncol ) THEN
      CALL ATMUXR( Obj % nrow, Obj % ncol, x, y, Obj % A, Obj % JA, Obj % IA )
    ELSE
      CALL ATMUX( Obj % nrow, x, y, Obj % A, Obj % JA, Obj % IA )
    END IF
  END SELECT
END PROCEDURE matvec_csr_amux

!----------------------------------------------------------------------------
!                                                                     Matmul
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_csr
  INTEGER( I4B ) :: n
  n = Obj % nrow
  SELECT CASE( TRIM( matvectype ) )
  CASE( 'AX', 'AMUX' )
    CALL AMUX( n, x, Ans, Obj % A, Obj % JA, Obj % IA )
  CASE( 'ATX', 'ATMUX' )
    IF( n .NE. Obj % ncol ) THEN
      CALL ATMUXR( n, Obj % ncol, x, Ans, Obj % A, Obj % JA, Obj % IA )
    ELSE
      CALL ATMUX( n, x, Ans, Obj % A, Obj % JA, Obj % IA )
    END IF
  END SELECT
END PROCEDURE matmul_csr

!----------------------------------------------------------------------------
!                                                            Sparsekit_lsolve
!----------------------------------------------------------------------------

MODULE PROCEDURE lsol_csr
  CALL Reallocate( x, Obj % nrow )
  CALL LSOL( Obj % nrow, x, y, Obj % A, Obj % JA, Obj % IA )
END PROCEDURE lsol_csr

!----------------------------------------------------------------------------
!                                                           Sparsekit_usolve
!----------------------------------------------------------------------------

MODULE PROCEDURE usol_csr
  CALL Reallocate( x, Obj % nrow )
  CALL USOL( Obj % nrow, x, y, Obj % A, Obj % JA, Obj % IA )
END PROCEDURE usol_csr

END SUBMODULE MatVec