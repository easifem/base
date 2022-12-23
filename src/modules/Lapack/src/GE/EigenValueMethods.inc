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

!----------------------------------------------------------------------------
!                                                          DGEES@EigenValue
!----------------------------------------------------------------------------

!> author: Dr. Vikas Sharma
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

! INTERFACE
!   MODULE SUBROUTINE dgees_1(A, WR, WI, SchurForm)
!     REAL(DFP), INTENT(IN) :: A(:, :)
!     REAL(DFP), INTENT(INOUT) :: WR(:)
!     !! Real part of the eigenvalue
!     REAL(DFP), INTENT(INOUT) :: WI(:)
!     !! Imaginary part of the eigenvalue
!     REAL(DFP), INTENT(INOUT) :: SchurForm(:, :)
!   END SUBROUTINE dgees_1
! END INTERFACE
