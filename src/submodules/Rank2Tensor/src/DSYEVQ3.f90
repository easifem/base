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
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.,
! date: 13 March 2021
! summary: Returns eigenvalue and eigenvector
!
!### Introduction
! Calculates the eigenvalues and normalized eigenvectors of a symmetric 3x3
! matrix A using the QL algorithm with implicit shifts, preceded by a
! Householder reduction to real tridiagonal form.
! The function accesses only the diagonal and upper triangular parts of
! A. The access is read-only.
!
! *   A: The symmetric input matrix
! *   Q: Storage buffer for eigenvectors
! *   W: Storage buffer for eigenvalues

PURE SUBROUTINE DSYEVQ3(A, Q, W)
  REAL( DFP ), INTENT( IN ) :: A(3,3)
  REAL( DFP ), INTENT( INOUT ) :: Q(3,3)
  REAL( DFP ), INTENT( INOUT ) :: W(3)

  INTEGER( I4B ), PARAMETER :: N = 3
  REAL( DFP ) :: E( 3 ), G, R, P, F, B, S, C, T
  INTEGER( I4B ) :: NITER, L, M, I, J, K
  LOGICAL( LGT ) :: FLAG1

  ! Transform A to real tridiagonal form by the Householder method
  CALL DSYTRD3(A, Q, W, E)

  ! Calculate eigensystem of the remaining real symmetric tridiagonal
  ! matrix with the QL method
  ! Loop over all off-diagonal elements
  DO L = 1, N-1
    NITER = 0
    ! Iteration loop
    DO I = 1, 50
      ! Check for convergence and exit iteration loop if off-diagonal
      ! element E(L) is zero
      DO M = L, N-1
        G = ABS(W(M)) + ABS(W(M+1))
        IF (ABS(E(M)) + G .EQ. G) THEN
          IF (M .EQ. L) THEN
            FLAG1 = .TRUE.
          ELSE
            FLAG1 = .FALSE.
          END IF
        END IF
      END DO

      IF( FLAG1 ) EXIT

      NITER = NITER + 1

      ! Calculate G = D(M) - K
      G = (W(L+1) - W(L)) / (2.0D0 * E(L))
      R = SQRT(1.0D0 + G**2)
      IF (G .GE. 0.0D0) THEN
        G = W(M) - W(L) + E(L)/(G + R)
      ELSE
        G = W(M) - W(L) + E(L)/(G - R)
      END IF

      S = 1.0D0
      C = 1.0D0
      P = 0.0D0
      DO J = M - 1, L, -1
        F = S * E(J)
        B = C * E(J)
        IF (ABS(F) .GT. ABS(G)) THEN
          C      = G / F
          R      = SQRT(1.0D0 + C**2)
          E(J+1) = F * R
          S      = 1.0D0 / R
          C      = C * S
        ELSE
          S      = F / G
          R      = SQRT(1.0D0 + S**2)
          E(J+1) = G * R
          C      = 1.0D0 / R
          S      = S * C
        END IF

        G      = W(J+1) - P
        R      = (W(J) - G) * S + 2.0D0 * C * B
        P      = S * R
        W(J+1) = G + P
        G      = C * R - B

        ! Form eigenvectors
        ! This loop can be omitted if only the eigenvalues are desired ---
        DO K = 1, N
          T         = Q(K, J+1)
          Q(K, J+1) = S * Q(K, J) + C * T
          Q(K, J)   = C * Q(K, J) - S * T
        END DO
      END DO
      W(L) = W(L) - P
      E(L) = G
      E(M) = 0.0D0
    END DO
  END DO
END SUBROUTINE DSYEVQ3
