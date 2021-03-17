! This program is a part of EASIFEM library,
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D,
!
! This program is free software: you can redistribute it and/or modify,
! it under the terms of the GNU General Public License as published by,
! the Free Software Foundation, either version 3 of the License, or,
! (at your option) any later version.,
!
! This program is distributed in the hope that it will be useful,,
! but WITHOUT ANY WARRANTY; without even the implied warranty of,
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the,
! GNU General Public License for more details.,
!
! You should have received a copy of the GNU General Public License,
! along with this program.  If not, see <https: //www.gnu.org/licenses/>.
!

!----------------------------------------------------------------------------
!                                                                    DSYEVH3
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.,
! date: 13 March 2021,
! summary: Calculates the eigenvalues and normalized eigenvectors of a symmetric 3x3
!
!### Introduction
!
! Calculates the eigenvalues and normalized eigenvectors of a symmetric 3x3
! matrix A using Cardano's method for the eigenvalues and an analytical
! method based on vector cross products for the eigenvectors. However,
! if conditions are such that a large error in the results is to be
! expected, the routine falls back to using the slower, but more
! accurate QL algorithm. Only the diagonal and upper triangular parts of A need
! to contain meaningful values. Access to A is read-only.
!
! - A: The symmetric input matrix
! - Q: Storage buffer for eigenvectors
! - W: Storage buffer for eigenvalues

PURE SUBROUTINE DSYEVH3(A, Q, W)
  REAL( DFP ), INTENT( IN ) :: A(3,3)
  REAL( DFP ), INTENT( INOUT ) :: Q(3,3)
  REAL( DFP ), INTENT( INOUT ) :: W(3)

  ! Internal variables
  REAL( DFP ), PARAMETER :: EPS = 2.2204460492503131D-16
  REAL( DFP ) ::  NORM
  REAL( DFP ) ::  ERROR
  REAL( DFP ) ::  T, U
  INTEGER( I4B ) :: J

  CALL DSYEVC3(A, W)

  ! The rest of this subroutine can be omitted if only the eigenvalues are desired
  ! Prepare calculation of eigenvectors
  ! N1 = A(1, 1)**2 + A(1, 2)**2 + A(1, 3)**2
  ! N2 = A(1, 2)**2 + A(2, 2)**2 + A(2, 3)**2

  T = MAX(ABS(W(1)), ABS(W(2)), ABS(W(3)))
  U = MAX(T, T**2)
  ERROR   = 256.0D0 * EPS * U**2
  Q(1, 2) = A(1, 2) * A(2, 3) - A(1, 3) * A(2, 2)
  Q(2, 2) = A(1, 3) * A(1, 2) - A(2, 3) * A(1, 1)
  Q(3, 2) = A(1, 2)**2

  ! Calculate first eigenvector by the formula
  ! v[0] = (A - lambda[0]).e1 x (A - lambda[0]).e2
  Q(1, 1) = Q(1, 2) + A(1, 3) * W(1)
  Q(2, 1) = Q(2, 2) + A(2, 3) * W(1)
  Q(3, 1) = (A(1,1) - W(1)) * (A(2,2) - W(1)) - Q(3,2)
  NORM    = Q(1, 1)**2 + Q(2, 1)**2 + Q(3, 1)**2

  ! If vectors are nearly linearly dependent, or if there might have
  ! been large cancellations in the calculation of A(I,I) - W(1), fall
  ! back to QL algorithm
  ! Note that this simultaneously ensures that multiple eigenvalues do
  ! not cause problems: If W(1) = W(2), then A - W(1) * I has rank 1,
  ! i.e. all columns of A - W(1) * I are linearly dependent.
  IF (NORM .LE. ERROR) THEN
    CALL DSYEVQ3(A, Q, W)
    RETURN
    ! This is the standard branch
  ELSE
    NORM = SQRT(1.0D0 / NORM)
    DO J = 1, 3
      Q(J, 1) = Q(J, 1) * NORM
    END DO
  END IF

  ! Calculate second eigenvector by the formula
  ! v[1] = (A - lambda[1]).e1 x (A - lambda[1]).e2
  Q(1, 2) = Q(1, 2) + A(1, 3) * W(2)
  Q(2, 2) = Q(2, 2) + A(2, 3) * W(2)
  Q(3, 2) = (A(1,1) - W(2)) * (A(2,2) - W(2)) - Q(3, 2)
  NORM    = Q(1, 2)**2 + Q(2, 2)**2 + Q(3, 2)**2
  IF (NORM .LE. ERROR) THEN
    CALL DSYEVQ3(A, Q, W)
    RETURN
  ELSE
    NORM = SQRT(1.0D0 / NORM)
    DO J = 1, 3
      Q(J, 2) = Q(J, 2) * NORM
    END DO
  END IF

  !Calculate third eigenvector according to
  ! v[2] = v[0] x v[1]
  Q(1, 3) = Q(2, 1) * Q(3, 2) - Q(3, 1) * Q(2, 2)
  Q(2, 3) = Q(3, 1) * Q(1, 2) - Q(1, 1) * Q(3, 2)
  Q(3, 3) = Q(1, 1) * Q(2, 2) - Q(2, 1) * Q(1, 2)
END SUBROUTINE
