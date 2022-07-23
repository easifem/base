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
! - GESV
! - GESVX
! - GESVXX
! - GELS
! - GELSD
! - GELSS
! - GELSY
! - GETSLS

PUBLIC :: Solve

!----------------------------------------------------------------------------
!                                                   Solve@LinearSolveMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: This function solves Ax=b using lapack, A can be square or rect
!
!# Introduction
!
! This function solves Ax=b using LAPACK. A can be a square matrix or
! rectangular matrix
!
! When A is a square matrix, then this routine calls GEGV routine.
! When A is not a square matrix, then this routine calls can call following
! routines depending upon the value of Solver.
!
! - GELS <-- Default
! - GELSD
! - GELSS
!
! Note that this routine creates a copy of A and b and then find x.
! This is because DGESV modifies the entries of A and b.
! Therefore, when A is large
! this routine should be avoided.

INTERFACE
  MODULE SUBROUTINE ge_solve_1(X, A, B, IPIV, SolverName, TRANS, RANK, &
    & RCOND, S)
    REAL( DFP ), INTENT( INOUT ) :: X( : )
      !! Unknown vector
    REAL( DFP ), INTENT( IN ) :: A( :, : )
      !! General square matrix
    REAL( DFP ), INTENT( IN ) :: B( : )
      !! RHS of Ax=B
    INTEGER( I4B ), OPTIONAL, INTENT( OUT ) :: IPIV( : )
      !! inverse of permuation
    CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: SolverName
      !! Name of the solver, when A is not square, default is GELS
    CHARACTER( LEN = 1 ), OPTIONAL, INTENT( IN ) :: TRANS
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: RANK
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: RCOND
    REAL( DFP ), OPTIONAL, INTENT( OUT ) :: S( : )
  END SUBROUTINE ge_solve_1
END INTERFACE

INTERFACE Solve
  MODULE PROCEDURE ge_solve_1
END INTERFACE Solve

!----------------------------------------------------------------------------
!                                                  Solve@LinearSolveMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: This function solves Ax=b using lapack DGESV x and b are 2d arrays
!
!# Introduction
!
!
! This function solves Ax=b using LAPACK. A can be a square or rectangle
! matrix.
!
! When A is a square matrix, then this routine calls GEGV routine.
! When A is not a square matrix, then this routine calls can call following
! routines depending upon the value of Solver.
!
! - GELS <-- Default
! - GELSD
! - GELSS
!
! Note that this routine creates a copy of A and b and then find x.
! This is because DGESV modifies the entries of A and b.
! Therefore, when A is large
! this routine should be avoided.
!
! ## Usage
! ```fortran
! x = Solve( A, b )
! ```

INTERFACE
  MODULE SUBROUTINE ge_solve_2(X, A, B, IPIV, SolverName, TRANS, RANK, &
    & RCOND, S)
    REAL( DFP ), INTENT( INOUT ) :: X( :, : )
      !! Unknown vector
    REAL( DFP ), INTENT( IN ) :: A( :, : )
      !! General square matrix
    REAL( DFP ), INTENT( IN ) :: B( :, : )
      !! RHS of Ax=B
    INTEGER( I4B ), OPTIONAL, INTENT( OUT ) :: IPIV( : )
      !! inverse of permuation
    CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: SolverName
      !! Name of the solver, when A is not square, default is GELS
    CHARACTER( LEN = 1 ), OPTIONAL, INTENT( IN ) :: TRANS
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: RANK
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: RCOND
    REAL( DFP ), OPTIONAL, INTENT( OUT ) :: S( : )
  END SUBROUTINE ge_solve_2
END INTERFACE

INTERFACE Solve
  MODULE PROCEDURE ge_solve_2
END INTERFACE Solve

!----------------------------------------------------------------------------
!                                                                      GESV
!----------------------------------------------------------------------------

!# Introduction
!
! This solves Ax=b using LAPACK DGEGV.
!
! Internally, this routine creates a copy of A and b and then find x
! Therefore, when A is large this routine should be avoided. This is because
! DGESV modifies the entries of A and b.
!
! Documentation of GESV says the following.
!
!@note
!  DGESV computes the solution to a real system of linear equations
!     A * X = B,
!  where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
!
!  The LU decomposition with partial pivoting and row interchanges is
!  used to factor A as
!     A = P * L * U,
!  where P is a permutation matrix, L is unit lower triangular, and U is
!  upper triangular.  The factored form of A is then used to solve the
!  system of equations A * X = B.
!
! note the diagnoal entries of lower triangular matrix is one.
!
! Note that the b will change on return.
!
! A is DOUBLE PRECISION array, dimension (LDA,N)
!           On entry, the N-by-N coefficient matrix A.
!           On exit, the factors L and U from the factorization
!           A = P*L*U; the unit diagonal elements of L are not stored.
!
! B is DOUBLE PRECISION array, dimension (LDB,NRHS)
!           On entry, the N-by-NRHS matrix of right hand side matrix B.
!           On exit, if INFO = 0, the N-by-NRHS solution matrix X.
!
!@endnote

!----------------------------------------------------------------------------
!                                                                      GELS
!----------------------------------------------------------------------------

!# Introduction
!
!  This subroutine solves the under-determined or over determined system
! Ax=b;
!
! - This subroutine calls DGELS from LAPACK, which uses QR and LQ
! factorization of matrix A
! - Shape of matrix A is (m,n)
! - If m>n then system is overdetermined and the routine solves min||b-Ax||
! - If m<n then the system is under-determined and routine finds the
! minimum norm solution of Ax=b
!
!@note
! Documentation of GELS is given below
!
!  DGELS solves overdetermined or underdetermined real linear systems
!  involving an M-by-N matrix A, or its transpose, using a QR or LQ
!  factorization of A.  It is assumed that A has full rank.
!
!  The following options are provided:
!
!  1. If TRANS = 'N' and m >= n:  find the least squares solution of
!     an overdetermined system, i.e., solve the least squares problem
!                  minimize || B - A*X ||.
!
!  2. If TRANS = 'N' and m < n:  find the minimum norm solution of
!     an underdetermined system A * X = B.
!
!  3. If TRANS = 'T' and m >= n:  find the minimum norm solution of
!     an underdetermined system A**T * X = B.
!
!  4. If TRANS = 'T' and m < n:  find the least squares solution of
!     an overdetermined system, i.e., solve the least squares problem
!                  minimize || B - A**T * X ||.
!
!  Several right hand side vectors b and solution vectors x can be
!  handled in a single call; they are stored as the columns of the
!  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
!  matrix X.
!
! A is DOUBLE PRECISION array, dimension (LDA,N)
!           On entry, the M-by-N matrix A.
!           On exit,
!             if M >= N, A is overwritten by details of its QR
!                        factorization as returned by DGEQRF;
!             if M <  N, A is overwritten by details of its LQ
!                        factorization as returned by DGELQF.
!
! B is DOUBLE PRECISION array, dimension (LDB,NRHS)
!           On entry, the matrix B of right hand side vectors, stored
!           columnwise; B is M-by-NRHS if TRANS = 'N', or N-by-NRHS
!           if TRANS = 'T'.
!           On exit, if INFO = 0, B is overwritten by the solution
!           vectors, stored columnwise:
!           if TRANS = 'N' and m >= n, rows 1 to n of B contain the least
!           squares solution vectors; the residual sum of squares for the
!           solution in each column is given by the sum of squares of
!           elements N+1 to M in that column;
!           if TRANS = 'N' and m < n, rows 1 to N of B contain the
!           minimum norm solution vectors;
!           if TRANS = 'T' and m >= n, rows 1 to M of B contain the
!           minimum norm solution vectors;
!           if TRANS = 'T' and m < n, rows 1 to M of B contain the
!           least squares solution vectors; the residual sum of squares
!           for the solution in each column is given by the sum of
!           squares of elements M+1 to N in that column.
!@endnote
!

!----------------------------------------------------------------------------
!                                                                     GELSD
!----------------------------------------------------------------------------

!# Introduction
!
! DGELSD computes the minimum-norm solution to a real linear least
! squares problem:
! minimize 2-norm(| b - A*x |)
!  using the singular value decomposition (SVD) of A. A is an M-by-N
!  matrix which may be rank-deficient.
!
!  Several right hand side vectors b and solution vectors x can be
!  handled in a single call; they are stored as the columns of the
!  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
!  matrix X.
!
!  The problem is solved in three steps:
!  - Reduce the coefficient matrix A to bidiagonal form with
!    Householder transformations, reducing the original problem
!    into a "bidiagonal least squares problem" (BLS)
!  - Solve the BLS using a divide and conquer approach.
!  - Apply back all the Householder transformations to solve
!    the original least squares problem.
!
!  The effective rank of A is determined by treating as zero those
!  singular values which are less than RCOND times the largest singular
!  value.
!
!  The divide and conquer algorithm makes very mild assumptions about
!  floating point arithmetic. It will work on machines with a guard
!  digit in add/subtract, or on those binary machines without guard
!  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
!  Cray-2. It could conceivably fail on hexadecimal or decimal machines
!  without guard digits, but we know of none.

!----------------------------------------------------------------------------
!                                                                     GELSS
!----------------------------------------------------------------------------

!# Introduction
!
! DGELSS computes the minimum norm solution to a real linear least
!  squares problem:
!
!  Minimize 2-norm(| b - A*x |).
!
!  using the singular value decomposition (SVD) of A. A is an M-by-N
!  matrix which may be rank-deficient.
!
!  Several right hand side vectors b and solution vectors x can be
!  handled in a single call; they are stored as the columns of the
!  M-by-NRHS right hand side matrix B and the N-by-NRHS solution matrix
!  X.
!
!  The effective rank of A is determined by treating as zero those
!  singular values which are less than RCOND times the largest singular
!  value

!----------------------------------------------------------------------------
!                                                           GELSY@LinearSolve
!----------------------------------------------------------------------------

!# Introduction
!
! DGELSY computes the minimum-norm solution to a real linear least
!  squares problem:
!      minimize || A * X - B ||
!  using a complete orthogonal factorization of A.  A is an M-by-N
!  matrix which may be rank-deficient.
!
!  Several right hand side vectors b and solution vectors x can be
!  handled in a single call; they are stored as the columns of the
!  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
!  matrix X.
!
!  The routine first computes a QR factorization with column pivoting:
!      A * P = Q * [ R11 R12 ]
!                  [  0  R22 ]
!  with R11 defined as the largest leading submatrix whose estimated
!  condition number is less than 1/RCOND.  The order of R11, RANK,
!  is the effective rank of A.
!
!  Then, R22 is considered to be negligible, and R12 is annihilated
!  by orthogonal transformations from the right, arriving at the
!  complete orthogonal factorization:
!     A * P = Q * [ T11 0 ] * Z
!                 [  0  0 ]
!  The minimum-norm solution is then
!     X = P * Z**T [ inv(T11)*Q1**T*B ]
!                  [        0         ]
!  where Q1 consists of the first RANK columns of Q.
!
!  This routine is basically identical to the original xGELSX except
!  three differences:
!   - The call to the subroutine xGEQPF has been substituted by the
!      the call to the subroutine xGEQP3. This subroutine is a Blas-3
!      version of the QR factorization with column pivoting.
!   - Matrix B (the right hand side) is updated with Blas-3.
!   - The permutation of matrix B (the right hand side) is faster and
!      more simple.