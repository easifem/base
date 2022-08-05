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

SUBMODULE(GE_Lapack_Method) LinearSolveMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Solve
!----------------------------------------------------------------------------

MODULE PROCEDURE ge_solve_1
  REAL( DFP ), DIMENSION( SIZE( A, 1 ), SIZE( A, 2 ) ) :: At
  CHARACTER( LEN = 1 ) :: TRANS
  !!
  IF( PRESENT( isTranspose ) ) THEN
    IF( isTranspose ) THEN
      TRANS="T"
    ELSE
      TRANS="N"
    END IF
  ELSE
    TRANS = "N"
  END IF
  !!
  At = A
  X = B
  !!
  IF( SIZE( A, 1 ) .EQ. SIZE( A, 2 ) ) THEN
    CALL GESV( A = At, B = X, IPIV=IPIV, info=info )
  ELSE
    IF( PRESENT( SolverName ) ) THEN
      SELECT CASE( TRIM( SolverName ) )
        !!
      CASE( "GELS" )
        CALL GELS( A=At, B=X, TRANS=TRANS, info=info )
        !!
      CASE( "GELSD" )
        CALL GELSD( A=At, B=X, RANK=RANK, RCOND=RCOND, S=S, info=info )
        !!
      CASE( "GELSS" )
        CALL GELSS( A=At, B=X, RANK=RANK, RCOND=RCOND, S=S, info=info)
        !!
      END SELECT
    ELSE
        CALL GELS( A=At, B=X, TRANS=TRANS, info=info )
    END IF
  END IF
  !!
END PROCEDURE ge_solve_1

!----------------------------------------------------------------------------
!                                                                      Solve
!----------------------------------------------------------------------------

MODULE PROCEDURE ge_solve_2
  REAL( DFP ), DIMENSION( SIZE( A, 1 ), SIZE( A, 2 ) ) :: At
  CHARACTER( LEN = 1 ) :: TRANS
  !!
  IF( PRESENT( isTranspose ) ) THEN
    IF( isTranspose ) THEN
      TRANS="T"
    ELSE
      TRANS="N"
    END IF
  ELSE
    TRANS = "N"
  END IF
  !!
  At = A
  X = B
  !!
  IF( SIZE( A, 1 ) .EQ. SIZE( A, 2 ) ) THEN
    CALL GESV( A = At, B = X, IPIV=IPIV, info=info )
  ELSE
    IF( PRESENT( SolverName ) ) THEN
      SELECT CASE( TRIM( SolverName ) )
        !!
      CASE( "GELS" )
        CALL GELS( A=At, B=X, TRANS=TRANS, info=info )
        !!
      CASE( "GELSD" )
        CALL GELSD( A=At, B=X, RANK=RANK, RCOND=RCOND, S=S, info=info )
        !!
      CASE( "GELSS" )
        CALL GELSS( A=At, B=X, RANK=RANK, RCOND=RCOND, S=S, info=info )
        !!
      END SELECT
    ELSE
        CALL GELS( A=At, B=X, TRANS=TRANS, info=info )
    END IF
  END IF
  !!
END PROCEDURE ge_solve_2

!----------------------------------------------------------------------------
!                                                                   LinSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE ge_linsolve_1
  CHARACTER( LEN = 1 ) :: TRANS
  !!
  IF( PRESENT( isTranspose ) ) THEN
    IF( isTranspose ) THEN
      TRANS="T"
    ELSE
      TRANS="N"
    END IF
  ELSE
    TRANS = "N"
  END IF
  !!
  X = B
  !!
  IF( SIZE( A, 1 ) .EQ. SIZE( A, 2 ) ) THEN
    CALL GESV( A = A, B = X, IPIV=IPIV, info=info )
  ELSE
    IF( PRESENT( SolverName ) ) THEN
      SELECT CASE( TRIM( SolverName ) )
        !!
      CASE( "GELS" )
        CALL GELS( A=A, B=X, TRANS=TRANS, info=info )
        !!
      CASE( "GELSD" )
        CALL GELSD( A=A, B=X, RANK=RANK, RCOND=RCOND, S=S, info=info )
        !!
      CASE( "GELSS" )
        CALL GELSS( A=A, B=X, RANK=RANK, RCOND=RCOND, S=S, info=info )
        !!
      END SELECT
    ELSE
        CALL GELS( A=A, B=X, TRANS=TRANS, info=info )
    END IF
  END IF
  !!
END PROCEDURE ge_linsolve_1

!----------------------------------------------------------------------------
!                                                                      Solve
!----------------------------------------------------------------------------

MODULE PROCEDURE ge_linsolve_2
  CHARACTER( LEN = 1 ) :: TRANS
  !!
  IF( PRESENT( isTranspose ) ) THEN
    IF( isTranspose ) THEN
      TRANS="T"
    ELSE
      TRANS="N"
    END IF
  ELSE
    TRANS = "N"
  END IF
  !!
  X = B
  !!
  IF( SIZE( A, 1 ) .EQ. SIZE( A, 2 ) ) THEN
    CALL GESV( A = A, B = X, IPIV=IPIV, info=info )
  ELSE
    IF( PRESENT( SolverName ) ) THEN
      SELECT CASE( TRIM( SolverName ) )
        !!
      CASE( "GELS" )
        CALL GELS( A=A, B=X, TRANS=TRANS, info=info )
        !!
      CASE( "GELSD" )
        CALL GELSD( A=A, B=X, RANK=RANK, RCOND=RCOND, S=S, info=info )
        !!
      CASE( "GELSS" )
        CALL GELSS( A=A, B=X, RANK=RANK, RCOND=RCOND, S=S, info=info )
        !!
      END SELECT
    ELSE
        CALL GELS( A=A, B=X, TRANS=TRANS, info=info )
    END IF
  END IF
  !!
END PROCEDURE ge_linsolve_2

!----------------------------------------------------------------------------
!                                                                   LinSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE ge_linsolve_3
  CHARACTER( LEN = 1 ) :: TRANS
  !!
  IF( PRESENT( isTranspose ) ) THEN
    IF( isTranspose ) THEN
      TRANS="T"
    ELSE
      TRANS="N"
    END IF
  ELSE
    TRANS = "N"
  END IF
  !!
  IF( SIZE( A, 1 ) .EQ. SIZE( A, 2 ) ) THEN
    CALL GESV( A = A, B = B, IPIV=IPIV, info=info )
  ELSE
    IF( PRESENT( SolverName ) ) THEN
      SELECT CASE( TRIM( SolverName ) )
        !!
      CASE( "GELS" )
        CALL GELS( A=A, B=B, TRANS=TRANS, info=info )
        !!
      CASE( "GELSD" )
        CALL GELSD( A=A, B=B, RANK=RANK, RCOND=RCOND, S=S, info=info )
        !!
      CASE( "GELSS" )
        CALL GELSS( A=A, B=B, RANK=RANK, RCOND=RCOND, S=S, info=info )
        !!
      END SELECT
    ELSE
        CALL GELS( A=A, B=B, TRANS=TRANS, info=info )
    END IF
  END IF
  !!
END PROCEDURE ge_linsolve_3

!----------------------------------------------------------------------------
!                                                                      Solve
!----------------------------------------------------------------------------

MODULE PROCEDURE ge_linsolve_4
  CHARACTER( LEN = 1 ) :: TRANS
  !!
  IF( PRESENT( isTranspose ) ) THEN
    IF( isTranspose ) THEN
      TRANS="T"
    ELSE
      TRANS="N"
    END IF
  ELSE
    TRANS = "N"
  END IF
  !!
  IF( SIZE( A, 1 ) .EQ. SIZE( A, 2 ) ) THEN
    CALL GESV( A = A, B = B, IPIV=IPIV, info=info )
  ELSE
    IF( PRESENT( SolverName ) ) THEN
      SELECT CASE( TRIM( SolverName ) )
        !!
      CASE( "GELS" )
        CALL GELS( A=A, B=B, TRANS=TRANS, info=info )
        !!
      CASE( "GELSD" )
        CALL GELSD( A=A, B=B, RANK=RANK, RCOND=RCOND, S=S, info=info )
        !!
      CASE( "GELSS" )
        CALL GELSS( A=A, B=B, RANK=RANK, RCOND=RCOND, S=S, info=info )
        !!
      END SELECT
    ELSE
        CALL GELS( A=A, B=B, TRANS=TRANS, info=info )
    END IF
  END IF
  !!
END PROCEDURE ge_linsolve_4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE LinearSolveMethods