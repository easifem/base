!> authors: Dr. Vikas Sharma
!
! This module contains linear algebra packages for fortran matrix (2D array)
! as well as RealMatrix Defined in EASIFEM
! The matrix is GE
! This module contains 4 submodules
! - @LinearSolve
! - @EigenValue
! - @SingularValue
! - @ComputationalRoutines
! - @AuxiliaryRoutines
!
! https://people.sc.fsu.edu/~jburkardt/f_src/cg/cg.html
! https://people.sc.fsu.edu/~jburkardt/f_src/cg_rc/cg_rc.html
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

MODULE GE_Lapack_Method
! USE BaseType
USE GlobalData
USE BaseType, ONLY: STR, String
IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                          Solve@LinearSolve
!----------------------------------------------------------------------------

INTERFACE
!! This function solves Ax=b using lapack  DGESV

!> authors: Dr. Vikas Sharma
!
! This function solves Ax=b using LAPACK DGEGV
! - Internally this routine creates a copy of A and b and then find x
! - Therefore, when A is large this routine should be avoided.
!
! ## Usage
! ```fortran
! x = Solve( A, b )
! ```

  MODULE FUNCTION ge_solve_1(A, b, Solver) result(x)
    REAL( DFP ), INTENT( IN ) :: A( :, : )
      !! General square matrix
    REAL( DFP ), INTENT( IN ) :: b( : )
      !! RHS of Ax=b
    CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: Solver
      !! Name of the solver
    REAL( DFP ) :: x( SIZE(b) )
      !! Unknown vector
  END FUNCTION ge_solve_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Solve@LinearSolve
!----------------------------------------------------------------------------

INTERFACE
!! This function solves Ax=b using lapack DGESV x and b are 2d arrays

!> authors: Dr. Vikas Sharma
!
! This function solves Ax=b using LAPACK DGEGV, x and b are 2d arrays.
! In otherwords for several right hand side vectors and fixed A matrix
! This function finds the unknown vector given by col of x.
!
! ## Usage
! ```fortran
! x = Solve( A, b )
! ```

  MODULE FUNCTION ge_solve_2(A, b, Solver) result(x)
    REAL( DFP ), INTENT( IN ) :: A( :, : )
      !! General square matrix
    REAL( DFP ), INTENT( IN ) :: b( :, : )
      !! RHS of Ax=b
    CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: Solver
      !! Name of the solver
    REAL( DFP ) :: x( SIZE(b,1), SIZE(b,2) )
      !! Unknown vector
  END FUNCTION ge_solve_2
END INTERFACE

INTERFACE Solve
  MODULE PROCEDURE ge_solve_1, ge_solve_2
END INTERFACE Solve

PUBLIC :: Solve

!----------------------------------------------------------------------------
!                                                           GESV@LinearSolve
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine solves Ax=b using lapack DGESV

!> authors: Dr. Vikas Sharma
!
! This solves Ax=b using LAPACK DGEGV
! - Internally this routine creates a copy of A and b and then find x
! - Therefore, when A is large this routine should be avoided.
!
! ## Usage
! ```fortran
! CALL GESV( A, b, x )
! ```

  MODULE SUBROUTINE gesv_1(A, b, x)
    REAL( DFP ), INTENT( IN ) :: A( :, : )
      !! General square matrix
    REAL( DFP ), INTENT( IN ) :: b( : )
      !! RHS of Ax=b
    REAL( DFP ), INTENT( INOUT ) :: x( : )
      !! Unknown vector
  END SUBROUTINE gesv_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GESV@LinearSolve
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine solves Ax=b using lapack DGESV x and b are 2d arrays

!> authors: Dr. Vikas Sharma
!
! This subroutine solves Ax=b using LAPACK DGEGV, x and b are 2d arrays.
! In otherwords for several right hand side vectors and fixed A matrix
! This subroutine finds the unknown vector given by col of x.
!
! ## Usage
! ```fortran
! CALL GESV( A, b, x )
! ```

  MODULE SUBROUTINE gesv_2(A, b, x)
    REAL( DFP ), INTENT( IN ) :: A( :, : )
      !! General square matrix
    REAL( DFP ), INTENT( IN ) :: b( :, : )
      !! RHS of Ax=b
    REAL( DFP ), INTENT( INOUT) :: x( :, : )
      !! Unknown vector
  END SUBROUTINE gesv_2
END INTERFACE


INTERFACE GESV
  MODULE PROCEDURE gesv_1, gesv_2
END INTERFACE GESV

PUBLIC :: GESV

!----------------------------------------------------------------------------
!                                                           GELS@LinearSolve
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine solves the undetemined or over-determined system
!> authors: Dr. Vikas Sharma
!
!  This subroutine solves the under-determined or over determined system
! Ax=b;
! - This subroutine calls DGELS from LAPACK, which uses QR and LQ
! factorization of matrix A
! - Shape of matrix A is (m,n)
! - If m>n then system is overdetermined and the routine solves min||b-Ax||
! - If m<n then the system is under-determined and routine finds the
! minimum norm solution of Ax=b
!
! ## Usage
! ```fortran
! CALL GELS( A, b, x )
! ```

MODULE SUBROUTINE gels_1( A, b, x )
  REAL( DFP ), INTENT( IN ) :: A( :, : )
  REAL( DFP ), INTENT( IN ) :: b( : )
  REAL( DFP ), INTENT( INOUT) :: x( : )
END SUBROUTINE gels_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GELS@LinearSolve
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine solves the undetemined or over-determined system
!> authors: Dr. Vikas Sharma
!
!  This subroutine solves the under-determined or over determined system
! Ax=b;
! - This subroutine calls DGELS from LAPACK, which uses QR and LQ
! factorization of matrix A
! - Shape of matrix A is (m,n)
! - If m>n then system is overdetermined and the routine solves min||b-Ax||
! - If m<n then the system is under-determined and routine finds the
! minimum norm solution of Ax=b
!
! ## Usage
! ```fortran
! CALL GELS( A, b, x )
! ```

MODULE SUBROUTINE gels_2( A, b, x )
  REAL( DFP ), INTENT( IN ) :: A( :, : )
  REAL( DFP ), INTENT( IN ) :: b( :, : )
  REAL( DFP ), INTENT( INOUT) :: x( :, : )
END SUBROUTINE gels_2
END INTERFACE

INTERFACE GELS
  MODULE PROCEDURE gels_1, gels_2
END INTERFACE GELS

PUBLIC :: GELS

!----------------------------------------------------------------------------
!                                                         GELSD@LinearSolve
!----------------------------------------------------------------------------

INTERFACE
!! DGELSD computes the minimum-norm solution to a real linear least
!! squares problem

!> authors: Dr. Vikas Sharma
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

MODULE SUBROUTINE gelsd_1( A, b, x )
  REAL( DFP ), INTENT( IN ) :: A( :, : )
  REAL( DFP ), INTENT( IN ) :: b( : )
  REAL( DFP ), INTENT( INOUT ) :: x( : )
END SUBROUTINE gelsd_1

MODULE SUBROUTINE gelsd_2( A, b, x )
  REAL( DFP ), INTENT( IN ) :: A( :, : )
  REAL( DFP ), INTENT( IN ) :: b( :, : )
  REAL( DFP ), INTENT( INOUT ) :: x( :, : )
END SUBROUTINE gelsd_2
END INTERFACE

INTERFACE GELSD
  MODULE PROCEDURE gelsd_1, gelsd_2
END INTERFACE GELSD

PUBLIC :: GELSD

!----------------------------------------------------------------------------
!                                                          GELSS@LinearSolve
!----------------------------------------------------------------------------

INTERFACE
!! DGELSS solves overdetermined or underdetermined systems for GE matrices

!> authors: Dr. Vikas Sharma
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

MODULE SUBROUTINE gelss_1( A, b, x )
  REAL( DFP ), INTENT( IN ) :: A( :, : )
  REAL( DFP ), INTENT( IN ) :: b( : )
  REAL( DFP ), INTENT( INOUT) :: x( : )
END SUBROUTINE gelss_1

MODULE SUBROUTINE gelss_2( A, b, x )
  REAL( DFP ), INTENT( IN ) :: A( :, : )
  REAL( DFP ), INTENT( IN ) :: b( :, : )
  REAL( DFP ), INTENT( INOUT) :: x( :, : )
END SUBROUTINE gelss_2

END INTERFACE

INTERFACE GELSS
  MODULE PROCEDURE gelss_1, gelss_2
END INTERFACE GELSS

PUBLIC :: GELSS

!----------------------------------------------------------------------------
!                                                           GELSY@LinearSolve
!----------------------------------------------------------------------------

INTERFACE
!! DGELSY computes the minimum-norm solution to a real linear least
!!  squares problem:

!> authors: Dr. Vikas Sharma
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

MODULE SUBROUTINE gelsy_1( A, b, x )
  REAL( DFP ), INTENT( IN ) :: A( :, : )
  REAL( DFP ), INTENT( IN ) :: b( : )
  REAL( DFP ), INTENT( INOUT ) :: x( : )
END SUBROUTINE gelsy_1

MODULE SUBROUTINE gelsy_2( A, b, x )
  REAL( DFP ), INTENT( IN ) :: A( :, : )
  REAL( DFP ), INTENT( IN ) :: b( :, : )
  REAL( DFP ), INTENT( INOUT ) :: x( :, : )
END SUBROUTINE gelsy_2
END INTERFACE

INTERFACE GELSY
  MODULE PROCEDURE gelsy_1, gelsy_2
END INTERFACE GELSY

PUBLIC :: GELSY

!----------------------------------------------------------------------------
!                                                                   getLU@LU
!----------------------------------------------------------------------------

INTERFACE
!! DGETF2 computes an LU factorization of a general m-by-n matrix A
!  using partial pivoting with row interchanges.

!> authors: Dr. Vikas Sharma
!
! DGETF2 computes an LU factorization of a general m-by-n matrix A
!  using partial pivoting with row interchanges.
!
!  The factorization has the form
!     A = P * L * U
!  where P is a permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and U is upper
!  triangular (upper trapezoidal if m < n).
!
!  This is the right-looking Level 2 BLAS version of the algorithm.
!

MODULE SUBROUTINE getLU_1(A, LU, IPIV, SolverName)
  REAL( DFP ), INTENT( IN ) :: A( :, : )
    !! Matrix to be factored
  REAL( DFP ), INTENT( INOUT ) :: LU( :, : )
    !! LU factorization, the unit diagonal elements of L are not stored.
  INTEGER( I4B ), INTENT( INOUT ) :: IPIV( : )
    !! IPIV is INTEGER array,row i of the matrix was interchanged with row
    !! IPIV(i).
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: SolverName
    !! LAPACK SolverName; GETF2, GETRF, GETRF2
END SUBROUTINE getLU_1
END INTERFACE

INTERFACE getLU
  MODULE PROCEDURE getLU_1
END INTERFACE getLU

PUBLIC :: getLU

!----------------------------------------------------------------------------
!                                                           DGEES@EigenValue
!----------------------------------------------------------------------------

INTERFACE

!> authors: Dr. Vikas Sharma
!
! DGEES computes for an N-by-N real nonsymmetric matrix A, the
!  eigenvalues, the real Schur form T, and, optionally, the matrix of
!  Schur vectors Z.  This gives the Schur factorization A = Z*T*(Z**T).
!
!  Optionally, it also orders the eigenvalues on the diagonal of the
!  real Schur form so that selected eigenvalues are at the top left.
!  The leading columns of Z then form an orthonormal basis for the
!  invariant subspace corresponding to the selected eigenvalues.
!
!  A matrix is in real Schur form if it is upper quasi-triangular with
!  1-by-1 and 2-by-2 blocks. 2-by-2 blocks will be standardized in the
!  form
!          [  a  b  ]
!          [  c  a  ]
!
!  where b*c < 0. The eigenvalues of such a block are a +- sqrt(bc).
!
! JOBVS : JOBVS is CHARACTER*1
! = 'N': Schur vectors are not computed;
! = 'V': Schur vectors are computed.
!
! SORT : SORT is CHARACTER*1
! Specifies whether or not to order the eigenvalues on the
! diagonal of the Schur form.
! = 'N': Eigenvalues are not ordered;
! = 'S': Eigenvalues are ordered (see SELECT).
!
! SELECT:  SELECT is a LOGICAL FUNCTION of two DOUBLE PRECISION arguments
! SELECT must be declared EXTERNAL in the calling subroutine.
! If SORT = 'S', SELECT is used to select eigenvalues to sort
! to the top left of the Schur form.
! If SORT = 'N', SELECT is not referenced.
! An eigenvalue WR(j)+sqrt(-1)*WI(j) is selected if
! SELECT(WR(j),WI(j)) is true; i.e., if either one of a complex
! conjugate pair of eigenvalues is selected, then both complex
! eigenvalues are selected.
! Note that a selected complex eigenvalue may no longer
! satisfy SELECT(WR(j),WI(j)) = .TRUE. after ordering, since
! ordering may change the value of complex eigenvalues
! (especially if the eigenvalue is ill-conditioned); in this
! case INFO is set to N+2 (see INFO below).
!
! N is INTEGER: The order of the matrix A. N >= 0.
!
! A: A is DOUBLE PRECISION array, dimension (LDA,N)
! On entry, the N-by-N matrix A.
! On exit, A has been overwritten by its real Schur form T
!
! LDA: Leading dimension of A
!
! SDIM:

MODULE SUBROUTINE dgees_1( A, WR, WI, SchurForm )
  REAL( DFP ), INTENT( IN ) :: A( :, : )
  REAL( DFP ), INTENT( INOUT ) :: WR( : )
    !! Real part of the eigenvalue
  REAL( DFP ), INTENT( INOUT ) :: WI( : )
    !! Imaginary part of the eigenvalue
  REAL( DFP ), INTENT( INOUT ) :: SchurForm( :, : )
END SUBROUTINE dgees_1
END INTERFACE

END MODULE GE_Lapack_Method