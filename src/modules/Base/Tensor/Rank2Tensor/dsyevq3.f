* ----------------------------------------------------------------------------
* Numerical diagonalization of 3x3 matrcies
* Copyright (C) 2006  Joachim Kopp
* ----------------------------------------------------------------------------
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
* ----------------------------------------------------------------------------


* ----------------------------------------------------------------------------
      SUBROUTINE DSYEVQ3(A, Q, W)
* ----------------------------------------------------------------------------
* Calculates the eigenvalues and normalized eigenvectors of a symmetric 3x3
* matrix A using the QL algorithm with implicit shifts, preceded by a
* Householder reduction to real tridiagonal form.
* The function accesses only the diagonal and upper triangular parts of
* A. The access is read-only. 
* ----------------------------------------------------------------------------
* Parameters:
*   A: The symmetric input matrix
*   Q: Storage buffer for eigenvectors
*   W: Storage buffer for eigenvalues
* ----------------------------------------------------------------------------
* Dependencies:
*   DSYTRD3()
* ----------------------------------------------------------------------------
*     .. Arguments ..
      DOUBLE PRECISION A(3,3)
      DOUBLE PRECISION Q(3,3)
      DOUBLE PRECISION W(3)

*     .. Parameters ..
      INTEGER          N
      PARAMETER        ( N = 3 )

*     .. Local Variables ..
      DOUBLE PRECISION E(3)
      DOUBLE PRECISION G, R, P, F, B, S, C, T
      INTEGER          NITER
      INTEGER          L, M, I, J, K

*     .. External Functions ..
      EXTERNAL         DSYTRD3
      
*     Transform A to real tridiagonal form by the Householder method
      CALL DSYTRD3(A, Q, W, E)

*     Calculate eigensystem of the remaining real symmetric tridiagonal
*     matrix with the QL method
*
*     Loop over all off-diagonal elements
      DO 10 L = 1, N-1
        NITER = 0

*       Iteration loop
        DO 11 I = 1, 50
*         Check for convergence and exit iteration loop if off-diagonal
*         element E(L) is zero
          DO 20 M = L, N-1
            G = ABS(W(M)) + ABS(W(M+1))
            IF (ABS(E(M)) + G .EQ. G) THEN
              GO TO 30
            END IF
   20     CONTINUE
   30     IF (M .EQ. L) THEN
            GO TO 10
          END IF

          NITER = NITER + 1
          IF (NITER >= 30) THEN
            PRINT *, 'DSYEVQ3: No convergence.'
            RETURN
          END IF

*         Calculate G = D(M) - K
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
          DO 40 J = M - 1, L, -1
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

*           Form eigenvectors
*           --- This loop can be omitted if only the eigenvalues are desired ---
            DO 50 K = 1, N
              T         = Q(K, J+1)
              Q(K, J+1) = S * Q(K, J) + C * T
              Q(K, J)   = C * Q(K, J) - S * T
   50       CONTINUE
   40     CONTINUE
          W(L) = W(L) - P
          E(L) = G
          E(M) = 0.0D0
   11   CONTINUE
   10 CONTINUE
  
      END SUBROUTINE
* End of subroutine DSYEVQ3

