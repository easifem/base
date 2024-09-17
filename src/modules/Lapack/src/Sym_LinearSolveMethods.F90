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
! Linear Solver name
!- SYSV

MODULE Sym_LinearSolveMethods
USE GlobalData, ONLY: DFP, I4B, LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: SymLinSolve

!----------------------------------------------------------------------------
!                                                   Solve@LinearSolveMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: This function solves Ax=b using lapack, A can be square or rect
!
!# Introduction
!
! This function solves Ax=b using LAPACK.
!
! This routine creates a copy of A and B. So do not use it for large
! systems.
!
! When A is a square matrix, then this routine calls GESV routine.
!
!## SYSV
!
! SYSV computes the solution to a real system of linear equations
!
!$$
! Ax = y,
!$$
!
! The LDLt decomposition with partial pivoting and row interchanges is
! used to factor A as (See SYTRF)
!
!@note
! Note that this routine creates a copy of A and b and then find x.
! This is because DGESV modifies the entries of A and b.
! Therefore, when A is large this routine should be avoided.
!@endnote

INTERFACE SymLinSolve
  MODULE SUBROUTINE SymLinSolve_1(X, A, B, preserveA, IPIV, SolverName, &
                                  UPLO, INFO)
    REAL(DFP), INTENT(INOUT) :: X(:)
      !! Unknown vector to be found
    REAL(DFP), INTENT(IN) :: A(:, :)
      !! Symmetric square matrix
    REAL(DFP), INTENT(IN) :: B(:)
      !! RHS of Ax=B
    LOGICAL(LGT), INTENT(IN) :: preserveA
      !! This flag is only for getting a unique interface
      !! it is always set to true
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: IPIV(:)
      !! Used for SYSV
      !! IPIV is INTEGER array, dimension (N)
      !! It is returned by SYTRF
    CHARACTER(*), OPTIONAL, INTENT(IN) :: SolverName
      !! Name of the solver, default is SYSV
    CHARACTER(1), OPTIONAL, INTENT(IN) :: UPLO
      !! "U" or "L", Default is "U"
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: INFO
  END SUBROUTINE SymLinSolve_1
END INTERFACE SymLinSolve

!----------------------------------------------------------------------------
!                                                  Solve@LinearSolveMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: This function solves Ax=b using lapack DGESV x and b are 2d arrays
!
!# Introduction
!
!
! This function solves Ax=b using LAPACK. A is square and symmetric
! matrix. In this case we have several RHS denoted by B matrix.
!
! All other things are same as `ge_solve_1`.

INTERFACE SymLinSolve
  MODULE SUBROUTINE SymLinSolve_2(X, A, B, preserveA, IPIV, SolverName, &
    & UPLO, INFO)
    REAL(DFP), INTENT(INOUT) :: X(:, :)
      !! Unknown vector
    REAL(DFP), INTENT(IN) :: A(:, :)
      !! General square matrix
    REAL(DFP), INTENT(IN) :: B(:, :)
      !! RHS of Ax=B
    LOGICAL(LGT), INTENT(IN) :: preserveA
      !! This flag is only for getting a unique interface
      !! it is always set to true
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: IPIV(:)
      !! inverse of permuation
    CHARACTER(*), OPTIONAL, INTENT(IN) :: SolverName
      !! Name of the solver, when A is not square, default is GELS
    CHARACTER(1), OPTIONAL, INTENT(IN) :: UPLO
      !! "U" or "L", default is "U"
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: INFO
  END SUBROUTINE SymLinSolve_2
END INTERFACE SymLinSolve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Solve Ax=Y
!
!# Introduction
!
! This routine is same as `ge_solve_1` with following difference.
!
! In this subroutine we do not make copy of A. Therefore A will be
! modified on return. Note that B will not be modified as we still
! make a copy of B.

INTERFACE SymLinSolve
  MODULE SUBROUTINE SymLinSolve_3(X, A, B, IPIV, SolverName, UPLO, INFO)
    REAL(DFP), INTENT(INOUT) :: X(:)
      !! Unknown vector solution
    REAL(DFP), INTENT(INOUT) :: A(:, :)
      !! General square sym matrix, it will be modified on return
    REAL(DFP), INTENT(IN) :: B(:)
      !! RHS of Ax=B
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: IPIV(:)
      !! PERMUTATION
    CHARACTER(1), OPTIONAL, INTENT(IN) :: SolverName
      !! Name of the solver, when A is not square, default is SYSV
    CHARACTER(1), OPTIONAL, INTENT(IN) :: UPLO
      !! "U" or "L", default is "U"
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: INFO
  END SUBROUTINE SymLinSolve_3
END INTERFACE SymLinSolve

!----------------------------------------------------------------------------
!                                               LinSolve@LinearSolveMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: This function solves Ax=b using lapack DGESV x and b are 2d arrays

INTERFACE SymLinSolve
  MODULE SUBROUTINE SymLinSolve_4(X, A, B, IPIV, SolverName, UPLO, INFO)
    REAL(DFP), INTENT(INOUT) :: X(:, :)
      !! Unknown vector or solution
    REAL(DFP), INTENT(INOUT) :: A(:, :)
      !! General square sym matrix, its content will be destroyed
    REAL(DFP), INTENT(IN) :: B(:, :)
      !! RHS of Ax=B
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: IPIV(:)
      !! inverse of permuation
    CHARACTER(*), OPTIONAL, INTENT(IN) :: SolverName
      !! Name of the solver, default is SYSV
    CHARACTER(1), OPTIONAL, INTENT(IN) :: UPLO
      !! "U" or "L", default is "U"
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: INFO
  END SUBROUTINE SymLinSolve_4
END INTERFACE SymLinSolve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary:Solve Ax=b
!
!# Introduction
!
! This routine is same as `ge_linsolve_1` with following changes
!
! We do not make any copy of B. The solution is returned in B. This
! means B will be destroyed on return.

INTERFACE SymLinSolve
  MODULE SUBROUTINE SymLinSolve_5(A, B, IPIV, SolverName, UPLO, info)
    REAL(DFP), INTENT(INOUT) :: A(:, :)
      !! General square symmetric matrix, its content will be modified on
      !! return
    REAL(DFP), INTENT(INOUT) :: B(:)
      !! RHS of Ax=B, it will contain the solution on return
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: IPIV(:)
    ! IPIV is INTEGER array, dimension (N)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: SolverName
      !! Name of the solver, when A is not square, default is GELS
    CHARACTER(1), OPTIONAL, INTENT(IN) :: UPLO
      !! "L" or "U", default is "U"
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: INFO
  END SUBROUTINE SymLinSolve_5
END INTERFACE SymLinSolve

!----------------------------------------------------------------------------
!                                               LinSolve@LinearSolveMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Solve Ax=y

INTERFACE SymLinSolve
  MODULE SUBROUTINE SymLinSolve_6(A, B, IPIV, SolverName, UPLO, INFO)
    REAL(DFP), INTENT(INOUT) :: A(:, :)
      !! General square/rectangle matrix, its content will be modifie
      !! on return
    REAL(DFP), INTENT(INOUT) :: B(:, :)
      !! RHS of Ax=B, it will be modified such that it contains solution on
      !! return
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: IPIV(:)
      !! inverse of permuation
    CHARACTER(*), OPTIONAL, INTENT(IN) :: SolverName
      !! Name of the solver, when A is not square, default is GELS
    CHARACTER(1), OPTIONAL, INTENT(IN) :: UPLO
      !! "U" or "L", default is "U"
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: INFO
  END SUBROUTINE SymLinSolve_6
END INTERFACE SymLinSolve

END MODULE Sym_LinearSolveMethods
