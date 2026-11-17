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
! along with this program.  If not, see <https://www.gnu.org/licenses/>


!> author: Shion Shimizu
! date: 2026-11-17
! summary: PURE routines on plain 3D Voigt arrays
!
! Moved from SmallStrainVoigtUtility of the elastoplasticity kernel
! (easifem/base#355). The arithmetic is unchanged.

SUBMODULE(VoigtUtility) Methods
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                              VoigtIdentity
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtIdentity
ans = [1.0_DFP, 1.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
END PROCEDURE VoigtIdentity

!----------------------------------------------------------------------------
!                                                                 VoigtTrace
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtTrace
ans = v(1) + v(2) + v(3)
END PROCEDURE VoigtTrace

!----------------------------------------------------------------------------
!                                                                   VoigtDev
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtDev
REAL(DFP) :: p

p = (v(1) + v(2) + v(3)) * (1.0_DFP / 3.0_DFP)
ans = v
ans(1:3) = ans(1:3) - p
END PROCEDURE VoigtDev

!----------------------------------------------------------------------------
!                                                            VoigtStressNorm
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtStressNorm
ans = SQRT(v(1)**2 + v(2)**2 + v(3)**2 + &
           2.0_DFP * (v(4)**2 + v(5)**2 + v(6)**2))
END PROCEDURE VoigtStressNorm

!----------------------------------------------------------------------------
!                                                                 VoigtOuter
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtOuter
INTEGER(I4B) :: jj

DO jj = 1, 6
  ans(:, jj) = a * b(jj)
END DO
END PROCEDURE VoigtOuter

!----------------------------------------------------------------------------
!                                                              VoigtSymEigen
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtSymEigen
REAL(DFP) :: A(3, 3), off, scale
INTEGER(I4B) :: sweep, p, q

A(1, 1) = v(1)
A(2, 2) = v(2)
A(3, 3) = v(3)
A(1, 2) = v(4)
A(2, 1) = v(4)
A(2, 3) = v(5)
A(3, 2) = v(5)
A(1, 3) = v(6)
A(3, 1) = v(6)
vecs = 0.0_DFP
vecs(1, 1) = 1.0_DFP
vecs(2, 2) = 1.0_DFP
vecs(3, 3) = 1.0_DFP

scale = MAXVAL(ABS(A))
sweep_loop: DO sweep = 1, 50
  off = ABS(A(1, 2)) + ABS(A(1, 3)) + ABS(A(2, 3))
  IF (off .LE. 1.0E-15_DFP * scale .OR. off .EQ. 0.0_DFP) EXIT sweep_loop
  DO p = 1, 2
    DO q = p + 1, 3
      CALL JacobiRotate3(A, vecs, p, q)
    END DO
  END DO
END DO sweep_loop

vals = [A(1, 1), A(2, 2), A(3, 3)]
CALL SortEigen3(vals, vecs)
END PROCEDURE VoigtSymEigen

!----------------------------------------------------------------------------
!                                                    VoigtStrainFromGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtStrainFromGradient
ans = 0.0_DFP

SELECT CASE (nsd)
CASE (1)
  ans(1) = grad(1, 1)

CASE (2)
  ans(1) = grad(1, 1)
  ans(2) = grad(2, 2)
  ans(4) = grad(1, 2) + grad(2, 1)

CASE DEFAULT
  ans(1) = grad(1, 1)
  ans(2) = grad(2, 2)
  ans(3) = grad(3, 3)
  ans(4) = grad(1, 2) + grad(2, 1)
  ans(5) = grad(2, 3) + grad(3, 2)
  ans(6) = grad(1, 3) + grad(3, 1)
END SELECT
END PROCEDURE VoigtStrainFromGradient

!----------------------------------------------------------------------------
!                                                              JacobiRotate3
!----------------------------------------------------------------------------

!> Jacobi rotation of the symmetric 3 x 3 matrix A that makes A(p, q)
! zero; the columns p and q of vecs are rotated in the same way.
! Nothing is done if A(p, q) is zero.

PURE SUBROUTINE JacobiRotate3(A, vecs, p, q)
  REAL(DFP), INTENT(INOUT) :: A(3, 3)
  !! symmetric matrix
  REAL(DFP), INTENT(INOUT) :: vecs(3, 3)
  !! accumulated rotation (eigenvectors in columns)
  INTEGER(I4B), INTENT(IN) :: p
  !! row of the entry to be eliminated
  INTEGER(I4B), INTENT(IN) :: q
  !! column of the entry to be eliminated, q > p
  REAL(DFP) :: theta, t, c, s, tau, app, aqq, apq, tmp
  INTEGER(I4B) :: r

  apq = A(p, q)
  IF (ABS(apq) .LE. TINY(1.0_DFP)) RETURN

  app = A(p, p)
  aqq = A(q, q)
  theta = (aqq - app) / (2.0_DFP * apq)
  t = SIGN(1.0_DFP, theta) / (ABS(theta) + SQRT(theta**2 + 1.0_DFP))
  c = 1.0_DFP / SQRT(t**2 + 1.0_DFP)
  s = t * c
  tau = s / (1.0_DFP + c)
  A(p, p) = app - t * apq
  A(q, q) = aqq + t * apq
  A(p, q) = 0.0_DFP
  A(q, p) = 0.0_DFP
  r_loop: DO r = 1, 3
    IF (r .EQ. p .OR. r .EQ. q) CYCLE r_loop
    tmp = A(r, p)
    A(r, p) = tmp - s * (A(r, q) + tau * tmp)
    A(p, r) = A(r, p)
    A(r, q) = A(r, q) + s * (tmp - tau * A(r, q))
    A(q, r) = A(r, q)
  END DO r_loop

  DO r = 1, 3
    tmp = vecs(r, p)
    vecs(r, p) = tmp - s * (vecs(r, q) + tau * tmp)
    vecs(r, q) = vecs(r, q) + s * (tmp - tau * vecs(r, q))
  END DO
END SUBROUTINE JacobiRotate3

!----------------------------------------------------------------------------
!                                                                 SortEigen3
!----------------------------------------------------------------------------

!> Sort three eigenvalues in descending order, with their eigenvectors
! (columns of vecs)

PURE SUBROUTINE SortEigen3(vals, vecs)
  REAL(DFP), INTENT(INOUT) :: vals(3)
  !! eigenvalues
  REAL(DFP), INTENT(INOUT) :: vecs(3, 3)
  !! eigenvectors in columns
  REAL(DFP) :: tmp, col(3)
  INTEGER(I4B) :: i, j, k

  DO i = 1, 2
    k = i
    DO j = i + 1, 3
      IF (vals(j) .GT. vals(k)) k = j
    END DO

    IF (k .NE. i) THEN
      tmp = vals(i)
      vals(i) = vals(k)
      vals(k) = tmp
      col = vecs(:, i)
      vecs(:, i) = vecs(:, k)
      vecs(:, k) = col
    END IF
  END DO
END SUBROUTINE SortEigen3

END SUBMODULE Methods
