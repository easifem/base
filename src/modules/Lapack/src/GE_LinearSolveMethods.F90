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

MODULE GE_LinearSolveMethods
USE GlobalData, ONLY: DFP, I4B, LGT
IMPLICIT NONE
PRIVATE
PUBLIC :: Solve
PUBLIC :: LinSolve

!----------------------------------------------------------------------------
!                                                   Solve@LinearSolveMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: This function solves Ax=b using lapack, A can be square or rect
!
!# Introduction
!
! This function solves Ax=b using LAPACK. A can be a square matrix or
! rectangular matrix.
!
! This routine creates a copy of A and B. So do not use it for large
! systems.
!
! When A is a square matrix, then this routine calls GESV routine.
!
!## GESV
!
! GESV computes the solution to a real system of linear equations
!
!$$
! Ax = y,
!$$
!
!  The LU decomposition with partial pivoting and row interchanges is
!  used to factor A as
!
!$$
! A = P * L * U,
!$$
!
!  where P is a permutation matrix, L is unit lower triangular, and U is
!  upper triangular.  The factored form of A is then used to solve the
!  system of equations A * X = B.
!
! When A is not a square matrix, then this routine calls can call following
! routines depending upon the value of SolverName.
!
! - GELS <-- Default QR or LQ, (A should have full rank)
! - GELSD <-- When A is rank defincient, SVD
! - GELSS
!
!## GELS
!
! GELS solves overdetermined or underdetermined systems for GE matrices using
! QR or LQ factorization.
!
!@note
! Note that matrix A should have full rank.
!@endnote
!
! If `isTranspose` is false then we solve $Ax=y$.
!
! In this case, if
! the number of rows are greater than number of columns (more equations)
! then we solve a least square problem (by using GEQRF) of
!
!$$
! min \Vert y-Ax \Vert
!$$
!
! When number of rows are lesser than the number of columns we have an
! underdetermined system. And we obtain the minimum norm solution
! of an underdetermined system $Ax=y$ (by using GELQF).
!
! When isTranspose is true then we solve $A^T x = y$. Here if number of rows
! are greater than the number of cols, then we have underdetermined system
! If number of rows of A is lesser than the number of columns of A, then
! we solve a least square system.
!
!## GELSD
!
!  DGELSD computes the minimum-norm solution to a real linear least
!  squares problem:
!
!$$
!  min \Vert b - A*x \Vert_{2}
!$$
!
! by using the singular value decomposition (SVD) of A.
! A is an M-by-N matrix which may be rank-deficient.
!
!
!  The problem is solved in three steps:
!  (1) Reduce the coefficient matrix A to bidiagonal form with
!      Householder transformations, reducing the original problem
!      into a "bidiagonal least squares problem" (BLS)
!  (2) Solve the BLS using a divide and conquer approach.
!  (3) Apply back all the Householder transformations to solve
!      the original least squares problem.
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
!
!## GELSS
!
! GELSS solves overdetermined or underdetermined systems for GE matrices
!
!DGELSS computes the minimum norm solution to a real linear least
!  squares problem:
!
!$$
!  min \Vert b - A*x \Vert_{2}
!$$
!
!  using the singular value decomposition (SVD) of A. A is an M-by-N
!  matrix which may be rank-deficient.
!
!  The effective rank of A is determined by treating as zero those
!  singular values which are less than RCOND times the largest singular
!  value.
!
!@note
! Note that this routine creates a copy of A and b and then find x.
! This is because DGESV modifies the entries of A and b.
! Therefore, when A is large this routine should be avoided.
!@endnote

INTERFACE
  MODULE SUBROUTINE ge_solve_1(X, A, B, IPIV, SolverName, isTranspose, RANK, &
    & RCOND, S, info)
    REAL(DFP), INTENT(INOUT) :: X(:)
      !! Unknown vector
    REAL(DFP), INTENT(IN) :: A(:, :)
      !! General square matrix
    REAL(DFP), INTENT(IN) :: B(:)
      !! RHS of Ax=B
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: IPIV(:)
      !! Used for GESV
    ! IPIV is INTEGER array, dimension (N)
    ! The pivot indices that define the permutation matrix P;
    ! row i of the matrix was interchanged with row IPIV(i).
    CHARACTER(*), OPTIONAL, INTENT(IN) :: SolverName
      !! Name of the solver, when A is not square, default is GELS
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
      !! If true then we solve $A^{T} x = y$
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: RANK
      !! Used in case of GELSD and GELSS
      !! The effective rank of A, i.e., the number of singular values
      !! which are greater than RCOND*S(1).
    REAL(DFP), OPTIONAL, INTENT(IN) :: RCOND
      !! RCOND is used to determine the effective rank of A.
      !! Singular values S(i) <= RCOND*S(1) are treated as zero.
      !! If RCOND < 0, machine precision is used instead.
    REAL(DFP), OPTIONAL, INTENT(OUT) :: S(:)
    !! Used in case of GELSD and GELSS
    !! S is DOUBLE PRECISION array, dimension (min(M,N))
    !! The singular values of A in decreasing order.
    !! The condition number of A in the 2-norm = S(1)/S(min(m,n)).
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
  END SUBROUTINE ge_solve_1
END INTERFACE

INTERFACE Solve
  MODULE PROCEDURE ge_solve_1
END INTERFACE Solve

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
! This function solves Ax=b using LAPACK. A can be a square or rectangle
! matrix. In this case we have several RHS denoted by B matrix.
!
! All other things are same as `ge_solve_1`.

INTERFACE
  MODULE SUBROUTINE ge_solve_2(X, A, B, IPIV, SolverName, isTranspose, RANK, &
    & RCOND, S, info)
    REAL(DFP), INTENT(INOUT) :: X(:, :)
      !! Unknown vector
    REAL(DFP), INTENT(IN) :: A(:, :)
      !! General square matrix
    REAL(DFP), INTENT(IN) :: B(:, :)
      !! RHS of Ax=B
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: IPIV(:)
      !! inverse of permuation
    CHARACTER(*), OPTIONAL, INTENT(IN) :: SolverName
      !! Name of the solver, when A is not square, default is GELS
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
      !! If true then we solve A^T x = y.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: RANK
    REAL(DFP), OPTIONAL, INTENT(IN) :: RCOND
    REAL(DFP), OPTIONAL, INTENT(OUT) :: S(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
  END SUBROUTINE ge_solve_2
END INTERFACE

INTERFACE Solve
  MODULE PROCEDURE ge_solve_2
END INTERFACE Solve

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
!
!
!## GESV
!
! On entry, the N-by-N coefficient matrix A.
! On exit, the factors L and U from the factorization
! A = P*L*U; the unit diagonal elements of L are not stored.
!
!## GELS
!
!           On entry, the M-by-N matrix A.
!           On exit,
!             if M >= N, A is overwritten by details of its QR
!                        factorization as returned by DGEQRF;
!             if M <  N, A is overwritten by details of its LQ
!                        factorization as returned by DGELQF.
!
!## GELSD
!
! On entry, the M-by-N matrix A.
! On exit, A has been destroyed.
!
!## GELSS
!
! On entry, the M-by-N matrix A.
! On exit, the first min(m,n) rows of A are overwritten with
! its right singular vectors, stored rowwise.

INTERFACE
  MODULE SUBROUTINE ge_linsolve_1(X, A, B, IPIV, SolverName, &
    & isTranspose, RANK, RCOND, S, info)
    REAL(DFP), INTENT(INOUT) :: X(:)
      !! Unknown vector solution
    REAL(DFP), INTENT(INOUT) :: A(:, :)
      !! General square/rectangle matrix, it will be modified on return
    REAL(DFP), INTENT(IN) :: B(:)
      !! RHS of Ax=B
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: IPIV(:)
      !! Used for GESV
    ! IPIV is INTEGER array, dimension (N)
    ! The pivot indices that define the permutation matrix P;
    ! row i of the matrix was interchanged with row IPIV(i).
    CHARACTER(*), OPTIONAL, INTENT(IN) :: SolverName
      !! Name of the solver, when A is not square, default is GELS
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
      !! If true then we solve $A^{T} x = y$
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: RANK
      !! Used in case of GELSD and GELSS
      !! The effective rank of A, i.e., the number of singular values
      !! which are greater than RCOND*S(1).
    REAL(DFP), OPTIONAL, INTENT(IN) :: RCOND
      !! RCOND is used to determine the effective rank of A.
      !! Singular values S(i) <= RCOND*S(1) are treated as zero.
      !! If RCOND < 0, machine precision is used instead.
    REAL(DFP), OPTIONAL, INTENT(OUT) :: S(:)
    !! Used in case of GELSD and GELSS
    !! S is DOUBLE PRECISION array, dimension (min(M,N))
    !! The singular values of A in decreasing order.
    !! The condition number of A in the 2-norm = S(1)/S(min(m,n)).
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
  END SUBROUTINE ge_linsolve_1
END INTERFACE

INTERFACE LinSolve
  MODULE PROCEDURE ge_linsolve_1
END INTERFACE LinSolve

!----------------------------------------------------------------------------
!                                               LinSolve@LinearSolveMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: This function solves Ax=b using lapack DGESV x and b are 2d arrays
!
!# Introduction
!
!
! This function solves Ax=b using LAPACK. A can be a square or rectangle
! matrix. In this case we have several RHS denoted by B matrix.
!
! All other things are same as `ge_solve_1`.

INTERFACE
  MODULE SUBROUTINE ge_linsolve_2(X, A, B, IPIV, SolverName, isTranspose, &
    & RANK, RCOND, S, info)
    REAL(DFP), INTENT(INOUT) :: X(:, :)
      !! Unknown vector or solution
    REAL(DFP), INTENT(INOUT) :: A(:, :)
      !! General square/ rectangle matrix, its content will be destroyed
    REAL(DFP), INTENT(IN) :: B(:, :)
      !! RHS of Ax=B
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: IPIV(:)
      !! inverse of permuation
    CHARACTER(*), OPTIONAL, INTENT(IN) :: SolverName
      !! Name of the solver, when A is not square, default is GELS
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
      !! If true then we solve A^T x = y.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: RANK
    REAL(DFP), OPTIONAL, INTENT(IN) :: RCOND
    REAL(DFP), OPTIONAL, INTENT(OUT) :: S(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
  END SUBROUTINE ge_linsolve_2
END INTERFACE

INTERFACE LinSolve
  MODULE PROCEDURE ge_linsolve_2
END INTERFACE LinSolve

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

INTERFACE
  MODULE SUBROUTINE ge_linsolve_3(A, B, IPIV, SolverName, &
    & isTranspose, RANK, RCOND, S, info)
    REAL(DFP), INTENT(INOUT) :: A(:, :)
      !! General square/ rectangle matrix, its content will be modified on
      !! return
    REAL(DFP), INTENT(INOUT) :: B(:)
      !! RHS of Ax=B, it will contain the solution on return
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: IPIV(:)
      !! Used for GESV
    ! IPIV is INTEGER array, dimension (N)
    ! The pivot indices that define the permutation matrix P;
    ! row i of the matrix was interchanged with row IPIV(i).
    CHARACTER(*), OPTIONAL, INTENT(IN) :: SolverName
      !! Name of the solver, when A is not square, default is GELS
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
      !! If true then we solve $A^{T} x = y$
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: RANK
      !! Used in case of GELSD and GELSS
      !! The effective rank of A, i.e., the number of singular values
      !! which are greater than RCOND*S(1).
    REAL(DFP), OPTIONAL, INTENT(IN) :: RCOND
      !! RCOND is used to determine the effective rank of A.
      !! Singular values S(i) <= RCOND*S(1) are treated as zero.
      !! If RCOND < 0, machine precision is used instead.
    REAL(DFP), OPTIONAL, INTENT(OUT) :: S(:)
    !! Used in case of GELSD and GELSS
    !! S is DOUBLE PRECISION array, dimension (min(M,N))
    !! The singular values of A in decreasing order.
    !! The condition number of A in the 2-norm = S(1)/S(min(m,n)).
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
  END SUBROUTINE ge_linsolve_3
END INTERFACE

INTERFACE LinSolve
  MODULE PROCEDURE ge_linsolve_3
END INTERFACE LinSolve

INTERFACE Solve
  MODULE PROCEDURE ge_linsolve_3
END INTERFACE Solve

!----------------------------------------------------------------------------
!                                               LinSolve@LinearSolveMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Solve Ax=y
!
!# Introduction
!
! This routien is same as `ge_linsolve_2` with following difference
!
! In this routine we do not create copy of B, ans sol is returned in B.
! This means B will be changed on return.

INTERFACE
  MODULE SUBROUTINE ge_linsolve_4(A, B, IPIV, SolverName, isTranspose, &
    & RANK, RCOND, S, info)
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
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
      !! If true then we solve A^T x = y.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: RANK
    REAL(DFP), OPTIONAL, INTENT(IN) :: RCOND
    REAL(DFP), OPTIONAL, INTENT(OUT) :: S(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
  END SUBROUTINE ge_linsolve_4
END INTERFACE

INTERFACE LinSolve
  MODULE PROCEDURE ge_linsolve_4
END INTERFACE LinSolve

INTERFACE Solve
  MODULE PROCEDURE ge_linsolve_4
END INTERFACE Solve

END MODULE GE_LinearSolveMethods
