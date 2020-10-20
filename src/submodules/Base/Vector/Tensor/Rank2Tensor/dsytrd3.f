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
      SUBROUTINE DSYTRD3(A, Q, D, E)
* ----------------------------------------------------------------------------
* Reduces a symmetric 3x3 matrix to real tridiagonal form by applying
* (unitary) Householder transformations:
*            [ D[1]  E[1]       ]
*    A = Q . [ E[1]  D[2]  E[2] ] . Q^T
*            [       E[2]  D[3] ]
* The function accesses only the diagonal and upper triangular parts of
* A. The access is read-only.
* ---------------------------------------------------------------------------
*     .. Arguments ..
      DOUBLE PRECISION A(3,3)
      DOUBLE PRECISION Q(3,3)
      DOUBLE PRECISION D(3)
      DOUBLE PRECISION E(2)

*     .. Parameters ..
      INTEGER          N
      PARAMETER        ( N = 3 )

*     .. Local Variables ..
      DOUBLE PRECISION U(N), P(N)
      DOUBLE PRECISION OMEGA, F
      DOUBLE PRECISION K, H, G
      INTEGER          I, J

*     Initialize Q to the identitity matrix
*     --- This loop can be omitted if only the eigenvalues are desired ---
      DO 10 I = 1, N
        Q(I,I) = 1.0D0
        DO 11, J = 1, I-1
          Q(I, J) = 0.0D0
          Q(J, I) = 0.0D0
   11   CONTINUE
   10 CONTINUE

*     Bring first row and column to the desired form
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
        DO 20 I = 2, N
          F    = A(2,I)*U(2) + A(I,3)*U(3)
          P(I) = OMEGA * F
          K    = K + U(I) * F
  20    CONTINUE
        K = 0.5D0 * K * OMEGA**2

        DO 30 I = 2, N
          P(I) = P(I) - K * U(I)
  30    CONTINUE

        D(1) = A(1,1)
        D(2) = A(2,2) - 2.0D0 * P(2) * U(2)
        D(3) = A(3,3) - 2.0D0 * P(3) * U(3)

*       Store inverse Householder transformation in Q
*       --- This loop can be omitted if only the eigenvalues are desired ---
        DO 40, J = 2, N
          F = OMEGA * U(J)
          DO 41 I = 2, N
            Q(I,J) = Q(I,J) - F * U(I)
   41     CONTINUE
   40   CONTINUE
            
*       Calculated updated A(2, 3) and store it in E(2)
        E(2) = A(2, 3) - P(2) * U(3) - U(2) * P(3)
      ELSE
        DO 50 I = 1, N
          D(I) = A(I, I)
  50    CONTINUE
        E(2) = A(2, 3)
      END IF
      
      END SUBROUTINE
* End of subroutine DSYTRD3

