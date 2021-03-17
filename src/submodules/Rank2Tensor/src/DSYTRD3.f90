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


!> authors: Vikas Sharma, Ph. D.,
! date: 13 March 2021
! summary: Reduces a symmetric 3x3 matrix to real tridiagonal form
!
!### Introduction
!
! Reduces a symmetric 3x3 matrix to real tridiagonal form by applying
! (unitary) Householder transformations:
! *            [ D[1]  E[1]       ]
! *    A = Q . [ E[1]  D[2]  E[2] ] . Q^T
! *            [       E[2]  D[3] ]
! * The function accesses only the diagonal and upper triangular parts of

PURE SUBROUTINE DSYTRD3(A, Q, D, E)
  REAL( DFP ), INTENT( IN ) :: A( 3, 3 )
  REAL( DFP ), INTENT( INOUT ) :: Q( 3, 3 )
  REAL( DFP ), INTENT( INOUT ) :: D( 3 )
  REAL( DFP ), INTENT( INOUT ) :: E( 2 )

  ! internal parameters
  INTEGER( I4B ), PARAMETER :: N = 3
  REAL( DFP ) :: U( N ), P( N )
  REAL( DFP ) :: OMEGA, F, K, H, G
  INTEGER( I4B ) :: I, J

  ! Initialize Q to the identitity matrix
  ! This loop can be omitted if only the eigenvalues are desired
  Q = EYE3

  ! Bring first row and column to the desired form
  H = A(1,2)**2 + A(1,3)**2
  IF (A(1,2) .GT. 0.0D0) THEN
    G = -SQRT(H)
  ELSE
    G = SQRT(H)
  END IF
  E(1)  = G
  F     = G * A(1,2)
  U(2)  = A(1,2) - G
  U(3)  = A(1,3)
  OMEGA = H - F
  IF (OMEGA > 0.0D0) THEN
    OMEGA = 1.0D0 / OMEGA
    K     = 0.0D0
    DO I = 2, N
      F    = A(2,I)*U(2) + A(I,3)*U(3)
      P(I) = OMEGA * F
      K    = K + U(I) * F
    END DO
    K = 0.5D0 * K * OMEGA**2

    DO I = 2, N
      P(I) = P(I) - K * U(I)
    END DO

    D(1) = A(1,1)
    D(2) = A(2,2) - 2.0D0 * P(2) * U(2)
    D(3) = A(3,3) - 2.0D0 * P(3) * U(3)

    !Store inverse Householder transformation in Q
    !This loop can be omitted if only the eigenvalues are desired ---
    DO J = 2, N
      F = OMEGA * U(J)
      DO I = 2, N
        Q(I,J) = Q(I,J) - F * U(I)
      END DO
    END DO

    !Calculated updated A(2, 3) and store it in E(2)
    E(2) = A(2, 3) - P(2) * U(3) - U(2) * P(3)
  ELSE
    DO I = 1, N
      D(I) = A(I, I)
    END DO
    E(2) = A(2, 3)
  END IF
END SUBROUTINE DSYTRD3
