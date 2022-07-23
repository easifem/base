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
  REAL( DFP ), DIMENSION( SIZE( b ) ) :: Bt
  !!
  At = A
  Bt = B
  !!
  IF( SIZE( A, 1 ) .EQ. SIZE( A, 2 ) ) THEN
    CALL GESV( A = At, B = Bt, IPIV=IPIV )
  ELSE
    IF( PRESENT( SolverName ) ) THEN
      SELECT CASE( TRIM( SolverName ) )
        !!
      CASE( "GELS" )
        CALL GELS( A=At, B=Bt, TRANS=TRANS )
        !!
      CASE( "GELSD" )
        CALL GELSD( A=At, B=Bt, RANK=RANK, RCOND=RCOND, S=S )
        !!
      CASE( "GELSS" )
        CALL GELSS( A=At, B=Bt, RANK=RANK, RCOND=RCOND, S=S )
        !!
      END SELECT
    ELSE
        CALL GELS( A=At, B=Bt, TRANS=TRANS )
    END IF
  END IF
  !!
  X = Bt
  !!
END PROCEDURE ge_solve_1

!----------------------------------------------------------------------------
!                                                                      Solve
!----------------------------------------------------------------------------

MODULE PROCEDURE ge_solve_2
  REAL( DFP ), DIMENSION( SIZE( A, 1 ), SIZE( A, 2 ) ) :: At
  REAL( DFP ), DIMENSION( SIZE( B, 1 ), SIZE( B, 2 ) ) :: Bt
  !!
  At = A
  Bt = B
  !!
  IF( SIZE( A, 1 ) .EQ. SIZE( A, 2 ) ) THEN
    CALL GESV( A = At, B = Bt )
  ELSE
    IF( PRESENT( SolverName ) ) THEN
      SELECT CASE( TRIM( SolverName ) )
        !!
      CASE( "GELS" )
        CALL GELS( A=At, B=Bt, TRANS=TRANS )
        !!
      CASE( "GELSD" )
        CALL GELSD( A=At, B=Bt, RANK=RANK, RCOND=RCOND, S=S )
        !!
      CASE( "GELSS" )
        CALL GELSS( A=At, B=Bt, RANK=RANK, RCOND=RCOND, S=S )
        !!
      END SELECT
    ELSE
        CALL GELS( A=At, B=Bt, TRANS=TRANS )
    END IF
  END IF
  !!
  X = Bt
  !!
END PROCEDURE ge_solve_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE LinearSolveMethods