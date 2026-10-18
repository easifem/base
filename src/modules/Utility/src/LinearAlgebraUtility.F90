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

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-12
! summary: Routines related to linear algebra.
!
! Description:
!   This module contains the following functions:
!
!   - `HilbertMatrix(n)` returns the Hilbert matrix of size `n x n`,
!   - `InvHilbertMatrix(n)` returns its exact inverse, computed from a
!     closed-form formula instead of a numerical inversion.
!
!   The Hilbert matrix is badly conditioned, so it is a common test
!   matrix for linear solvers and inversion routines.

MODULE LinearAlgebraUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
IMPLICIT NONE

PRIVATE
PUBLIC :: InvHilbertMatrix
PUBLIC :: HilbertMatrix

!----------------------------------------------------------------------------
!                                                   InvHilbertMatrix@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-12
! summary: Returns the inverse of the Hilbert matrix of size n.
!
! Description:
!   Returns the inverse of the `n x n` Hilbert matrix. The entries are
!   computed by a recurrence of the closed-form formula below, so no
!   linear system is solved.
!
!   - All entries of the inverse are integers, and the inverse is
!     symmetric.
!   - The entries grow very fast with `n`, for example the largest entry
!     is about `1.8E+5` for `n = 5` and about `3.5E+12` for `n = 10`.
!     They are exact in `REAL64` up to `n = 12`; for larger `n` they
!     exceed `2**53` and are rounded.
!   - For `n <= 0` the result is an empty matrix.
!
! Arguments:
!   n : INTEGER(I4B) - size of the matrix
!
! Returns:
!   ans : REAL(DFP), rank(2) - inverse of the Hilbert matrix,
!     `SHAPE(ans) = [n, n]`
!
! Formula:
!   $$\left(H^{-1}\right)_{ij} = (-1)^{i+j} (i+j-1)
!     \binom{n+i-1}{n-j} \binom{n+j-1}{n-i} \binom{i+j-2}{i-1}^2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LinearAlgebraUtility, ONLY: InvHilbertMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(1, 1), a2(2, 2), a3(3, 3), a5(5, 5)
! REAL(DFP) :: e2(2, 2), e3(3, 3)
!
! ! n = 1
! a1 = InvHilbertMatrix(1_I4B)
! CALL OK(ABS(a1(1, 1) - 1.0_DFP) .LE. tol, "n = 1")
!
! ! n = 2
! a2 = InvHilbertMatrix(2_I4B)
! e2 = RESHAPE([4.0_DFP, -6.0_DFP, -6.0_DFP, 12.0_DFP], [2, 2])
! CALL OK(ALL(ABS(a2 - e2) .LE. tol), "n = 2")
!
! ! n = 3
! a3 = InvHilbertMatrix(3_I4B)
! e3 = RESHAPE([9.0_DFP, -36.0_DFP, 30.0_DFP, &
!               -36.0_DFP, 192.0_DFP, -180.0_DFP, &
!               30.0_DFP, -180.0_DFP, 180.0_DFP], [3, 3])
! CALL OK(ALL(ABS(a3 - e3) .LE. tol), "n = 3")
!
! ! n = 5, a few entries
! a5 = InvHilbertMatrix(5_I4B)
! CALL OK(ABS(a5(1, 1) - 25.0_DFP) .LE. tol, "n = 5, entry (1,1)")
! CALL OK(ABS(a5(2, 4) - 26880.0_DFP) .LE. tol, "n = 5, entry (2,4)")
! CALL OK(ABS(a5(5, 5) - 44100.0_DFP) .LE. tol, "n = 5, entry (5,5)")
!
! ! the inverse is symmetric
! CALL OK(ALL(ABS(a5 - TRANSPOSE(a5)) .LE. tol), "symmetric")
!
! END PROGRAM main
!```

INTERFACE InvHilbertMatrix
  MODULE PURE FUNCTION InvHilbertMatrix(n) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: Ans(n, n)
  END FUNCTION InvHilbertMatrix
END INTERFACE InvHilbertMatrix

!----------------------------------------------------------------------------
!                                                      HilbertMatrix@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-12
! summary: Returns the Hilbert matrix of size n.
!
! Description:
!   Returns the `n x n` Hilbert matrix, whose entries are `1/(i+j-1)`.
!
!   - The matrix is symmetric and positive definite.
!   - It is badly conditioned: the condition number grows roughly like
!     `EXP(3.5 n)`, so solving with it loses accuracy quickly for
!     `n > 10`. Use `InvHilbertMatrix` to get its exact inverse.
!   - For `n <= 0` the result is an empty matrix.
!
! Arguments:
!   n : INTEGER(I4B) - size of the matrix
!
! Returns:
!   ans : REAL(DFP), rank(2) - the Hilbert matrix,
!     `SHAPE(ans) = [n, n]`
!
! Formula:
!   $$H_{ij} = \frac{1}{i+j-1}, \quad i, j = 1, \dots, n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE LinearAlgebraUtility, ONLY: HilbertMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
! REAL(DFP) :: h(4, 4), h1(1, 1)
! INTEGER(I4B) :: i, j
! LOGICAL(LGT) :: isok
!
! h = HilbertMatrix(4_I4B)
!
! ! entries are 1/(i+j-1)
! isok = .TRUE.
! DO j = 1, 4
!   DO i = 1, 4
!     IF (ABS(h(i, j) - 1.0_DFP / REAL(i + j - 1, DFP)) .GT. tol) THEN
!       isok = .FALSE.
!     END IF
!   END DO
! END DO
! CALL OK(isok, "entries are 1/(i+j-1)")
!
! CALL OK(ABS(h(1, 1) - 1.0_DFP) .LE. tol, "h(1,1) = 1")
! CALL OK(ABS(h(2, 3) - 0.25_DFP) .LE. tol, "h(2,3) = 1/4")
! CALL OK(ABS(h(4, 4) - 1.0_DFP / 7.0_DFP) .LE. tol, "h(4,4) = 1/7")
!
! ! the Hilbert matrix is symmetric
! CALL OK(ALL(ABS(h - TRANSPOSE(h)) .LE. tol), "symmetric")
!
! ! shape of the result
! CALL OK(ALL(SHAPE(HilbertMatrix(3_I4B)) .EQ. [3_I4B, 3_I4B]), &
!         "shape is (n, n)")
!
! ! n = 1
! h1 = HilbertMatrix(1_I4B)
! CALL OK(ABS(h1(1, 1) - 1.0_DFP) .LE. tol, "n = 1")
!
! END PROGRAM main
!```

INTERFACE HilbertMatrix
  MODULE PURE FUNCTION HilbertMatrix(n) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: Ans(n, n)
  END FUNCTION HilbertMatrix
END INTERFACE HilbertMatrix

END MODULE LinearAlgebraUtility
