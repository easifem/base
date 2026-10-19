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
! date: 2021-04-03
! summary: Determinant and inverse of small square matrices.
!
! Description:
!   This module contains the generic function `DET` and the generic
!   subroutine `INV`. They use closed-form (cofactor) formulas, so
!   they are meant for small matrices, such as Jacobians in finite
!   element computations.
!
!   - `DET` uses closed-form formulas for sizes 1 to 5, and Gaussian
!     elimination with partial pivoting for larger sizes.
!   - `INV` uses the adjugate for sizes 1 to 4, and Gauss-Jordan
!     elimination with partial pivoting for larger sizes.
!   - Both accept a single matrix `A(:, :)` or a batch of matrices
!     `A(:, :, :)`, where the last index is the batch index.
!
!   For large matrices an LU decomposition from the LAPACK interface
!   of easifem is faster.

MODULE InvUtility
USE GlobalData, ONLY: DFP
IMPLICIT NONE
PRIVATE

PUBLIC :: DET
PUBLIC :: INV

!----------------------------------------------------------------------------
!                                                         Det@InverseMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Returns the determinant of a small square matrix.
!
! Description:
!   Returns the determinant of the square matrix `A`. Closed-form
!   formulas are used for sizes 1 to 4, and the expansion along the
!   first row is used for size 5. Larger matrices use Gaussian
!   elimination with partial pivoting.
!
!   - Only `SIZE(A, 1)` is used to select the formula. `A` should be
!     square; this is not checked.
!
! Arguments:
!   A : REAL(DFP), rank(2) - square matrix,
!     `SIZE(A, 1) = SIZE(A, 2)`
!
! Returns:
!   ans : REAL(DFP) - determinant of `A`
!
! Formula:
!   $$\mathrm{ans} = \det(A) = \sum_{k=1}^{n} (-1)^{1+k}\,
!     A_{1k}\, M_{1k}$$
!   where $M_{1k}$ is the determinant of `A` without row 1 and
!   column `k`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE InvUtility, ONLY: DET
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(1, 1), a2(2, 2), a3(3, 3), a4(4, 4), eye(4, 4)
! REAL(DFP) :: ans
! INTEGER(I4B) :: i
!
! ! 1 x 1 matrix
! a1 = 5.0_DFP
! ans = DET(a1)
! CALL OK(ABS(ans - 5.0_DFP) .LT. tol, "det of 1x1 matrix")
!
! ! 2 x 2 matrix, det = 3*6 - 8*4 = -14
! a2 = RESHAPE(REAL([3, 8, 4, 6], KIND=DFP), [2, 2], ORDER=[2, 1])
! ans = DET(a2)
! CALL OK(ABS(ans + 14.0_DFP) .LT. tol, "det of 2x2 matrix")
!
! ! 3 x 3 matrix, det = 1
! a3 = RESHAPE(REAL([1, 2, 3, 0, 1, 4, 5, 6, 0], KIND=DFP), [3, 3], &
!              ORDER=[2, 1])
! ans = DET(a3)
! CALL OK(ABS(ans - 1.0_DFP) .LT. tol, "det of 3x3 matrix")
!
! ! 3 x 3 tridiagonal matrix, det = 4
! a3 = RESHAPE(REAL([2, -1, 0, -1, 2, -1, 0, -1, 2], KIND=DFP), &
!              [3, 3], ORDER=[2, 1])
! ans = DET(a3)
! CALL OK(ABS(ans - 4.0_DFP) .LT. tol, "det of 3x3 tridiagonal matrix")
!
! ! 4 x 4 identity matrix
! eye = 0.0_DFP
! DO i = 1, 4
!   eye(i, i) = 1.0_DFP
! END DO
! ans = DET(eye)
! CALL OK(ABS(ans - 1.0_DFP) .LT. tol, "det of 4x4 identity")
!
! ! 4 x 4 Pascal matrix, det = 1
! a4 = RESHAPE(REAL([1, 1, 1, 1, 1, 2, 3, 4, 1, 3, 6, 10, 1, 4, 10, &
!                    20], KIND=DFP), [4, 4], ORDER=[2, 1])
! ans = DET(a4)
! CALL OK(ABS(ans - 1.0_DFP) .LT. tol, "det of 4x4 Pascal matrix")
!
! ! 4 x 4 non-symmetric matrix, det = 16
! a4 = RESHAPE(REAL([1, 2, 0, 1, 0, 1, 3, 0, 2, 0, 1, 1, 1, 1, 0, 2], &
!                   KIND=DFP), [4, 4], ORDER=[2, 1])
! ans = DET(a4)
! CALL OK(ABS(ans - 16.0_DFP) .LT. tol, &
!         "det of 4x4 non-symmetric matrix")
!
! END PROGRAM main
!```

INTERFACE DET
  MODULE PURE FUNCTION det_2D(A) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: A(:, :)
    REAL(DFP) :: Ans
  END FUNCTION det_2D
END INTERFACE DET

!----------------------------------------------------------------------------
!                                                        Det@InverseMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Returns the determinants of a batch of small matrices.
!
! Description:
!   Returns the determinant of each matrix `A(:, :, k)`, where the
!   last index `k` is the batch index. Each determinant is computed
!   by the rank-2 version of `DET`, so the same size limits apply.
!
!   - The result is allocated to `SIZE(A, 3)`.
!   - Each matrix should be square.
!
! Arguments:
!   A : REAL(DFP), rank(3) - batch of square matrices,
!     `SIZE(A, 1) = SIZE(A, 2)`
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE, rank(1) - determinants,
!     `SIZE(ans) = SIZE(A, 3)`
!
! Formula:
!   $$\mathrm{ans}_k = \det\left(A_{:,:,k}\right),
!     \quad k = 1, \ldots, \mathrm{SIZE}(A, 3)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE InvUtility, ONLY: DET
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a(2, 2, 3), b(2, 2, 1)
! REAL(DFP), ALLOCATABLE :: ans(:)
! INTEGER(I4B) :: k
!
! ! the last index is the batch index
! a(:, :, 1) = RESHAPE(REAL([1, 0, 0, 1], KIND=DFP), [2, 2], &
!                      ORDER=[2, 1])
! a(:, :, 2) = RESHAPE(REAL([3, 8, 4, 6], KIND=DFP), [2, 2], &
!                      ORDER=[2, 1])
! a(:, :, 3) = RESHAPE(REAL([1, 2, 3, 4], KIND=DFP), [2, 2], &
!                      ORDER=[2, 1])
!
! ans = DET(a)
! CALL OK(ALLOCATED(ans), "ans is allocated")
! CALL OK(SIZE(ans) .EQ. 3_I4B, "size of ans equals the batch size")
! CALL OK(ABS(ans(1) - 1.0_DFP) .LT. tol, "det of identity")
! CALL OK(ABS(ans(2) + 14.0_DFP) .LT. tol, "det of a(:,:,2)")
! CALL OK(ABS(ans(3) + 2.0_DFP) .LT. tol, "det of a(:,:,3)")
!
! ! each entry equals the determinant of the corresponding slice
! DO k = 1, 3
!   CALL OK(ABS(ans(k) - DET(a(:, :, k))) .LT. tol, &
!           "slice consistency")
! END DO
!
! ! batch of size one
! b(:, :, 1) = a(:, :, 2)
! ans = DET(b)
! CALL OK(SIZE(ans) .EQ. 1_I4B, "size of ans for a single matrix")
! CALL OK(ABS(ans(1) + 14.0_DFP) .LT. tol, "det of a single matrix")
!
! END PROGRAM main
!```

INTERFACE DET
  MODULE PURE FUNCTION det_3D(A) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: A(:, :, :)
    REAL(DFP), ALLOCATABLE :: Ans(:)
  END FUNCTION det_3D
END INTERFACE DET

!----------------------------------------------------------------------------
!                                                        INV@InverseMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-18
! summary: Computes the inverse of a small square matrix.
!
! Description:
!   Computes the inverse of the square matrix `A` and stores it in
!   `invA`, by using the adjugate (transpose of the cofactor matrix)
!   divided by the determinant.
!
!   - The adjugate is used for `SIZE(A, 1)` from 1 to 4. Larger
!     matrices use Gauss-Jordan elimination with partial pivoting.
!   - `invA` should be allocated with the same shape as `A` before
!     the call.
!   - If `ABS(DET(A)) < Zero` (`Zero` from `GlobalData`, about the
!     machine epsilon), then `A` is treated as singular and `invA` is
!     set to zero. No error is raised. The test does not depend on
!     the scale of `A`.
!
! Arguments:
!   invA : REAL(DFP), rank(2), INOUT - inverse of `A`,
!     `SHAPE(invA) = SHAPE(A)`
!   A : REAL(DFP), rank(2) - square matrix,
!     `SIZE(A, 1) = SIZE(A, 2)`
!
! Formula:
!   $$A^{-1} = \frac{1}{\det(A)}\, C^{T}, \quad
!     C_{ij} = (-1)^{i+j} M_{ij}$$
!   where $M_{ij}$ is the determinant of `A` without row `i` and
!   column `j`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE InvUtility, ONLY: INV
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(1, 1), b1(1, 1), a2(2, 2), b2(2, 2), ex2(2, 2)
! REAL(DFP) :: a3(3, 3), b3(3, 3), ex3(3, 3), eye3(3, 3)
! INTEGER(I4B) :: i
!
! ! 1 x 1 matrix
! a1 = 4.0_DFP
! CALL INV(invA=b1, A=a1)
! CALL OK(ABS(b1(1, 1) - 0.25_DFP) .LT. tol, "inverse of 1x1 matrix")
!
! ! 2 x 2 matrix, det = 10
! a2 = RESHAPE(REAL([4, 7, 2, 6], KIND=DFP), [2, 2], ORDER=[2, 1])
! ex2 = RESHAPE([0.6_DFP, -0.7_DFP, -0.2_DFP, 0.4_DFP], [2, 2], &
!               ORDER=[2, 1])
! CALL INV(invA=b2, A=a2)
! CALL OK(MAXVAL(ABS(b2 - ex2)) .LT. tol, "inverse of 2x2 matrix")
!
! ! 3 x 3 matrix, det = 1, the inverse has integer entries
! a3 = RESHAPE(REAL([1, 2, 3, 0, 1, 4, 5, 6, 0], KIND=DFP), [3, 3], &
!              ORDER=[2, 1])
! ex3 = RESHAPE(REAL([-24, 18, 5, 20, -15, -4, -5, 4, 1], KIND=DFP), &
!               [3, 3], ORDER=[2, 1])
! CALL INV(invA=b3, A=a3)
! CALL OK(MAXVAL(ABS(b3 - ex3)) .LT. tol, "inverse of 3x3 matrix")
!
! ! A * inv(A) = inv(A) * A = I
! eye3 = 0.0_DFP
! DO i = 1, 3
!   eye3(i, i) = 1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(MATMUL(a3, b3) - eye3)) .LT. tol, &
!         "A * inv(A) = I")
! CALL OK(MAXVAL(ABS(MATMUL(b3, a3) - eye3)) .LT. tol, &
!         "inv(A) * A = I")
!
! END PROGRAM main
!```

INTERFACE INV
  MODULE PURE SUBROUTINE Inv_2D(invA, A)
    REAL(DFP), INTENT(INOUT) :: invA(:, :)
    REAL(DFP), INTENT(IN) :: A(:, :)
  END SUBROUTINE Inv_2D
END INTERFACE INV

!----------------------------------------------------------------------------
!                                                         INV@InverseMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-18
! summary: Computes the inverses of a batch of small matrices.
!
! Description:
!   Computes the inverse of each matrix `A(:, :, k)` and stores it in
!   `invA(:, :, k)`, where the last index `k` is the batch index.
!   Each inverse is computed by the rank-2 version of `INV`, so the
!   same rules apply, and a singular matrix gives a zero slice.
!
!   - `invA` should be allocated with the same shape as `A` before
!     the call.
!
! Arguments:
!   invA : REAL(DFP), rank(3), INOUT - inverses,
!     `SHAPE(invA) = SHAPE(A)`
!   A : REAL(DFP), rank(3) - batch of square matrices,
!     `SIZE(A, 1) = SIZE(A, 2)`
!
! Formula:
!   $$\mathrm{invA}_{:,:,k} = \left(A_{:,:,k}\right)^{-1},
!     \quad k = 1, \ldots, \mathrm{SIZE}(A, 3)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE InvUtility, ONLY: INV
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a(3, 3, 3), b(3, 3, 3), c(3, 3), eye3(3, 3)
! INTEGER(I4B) :: i, k
!
! eye3 = 0.0_DFP
! DO i = 1, 3
!   eye3(i, i) = 1.0_DFP
! END DO
!
! ! the last index is the batch index
! a(:, :, 1) = 2.0_DFP * eye3
! a(:, :, 2) = RESHAPE(REAL([1, 2, 3, 0, 1, 4, 5, 6, 0], KIND=DFP), &
!                      [3, 3], ORDER=[2, 1])
! a(:, :, 3) = RESHAPE(REAL([2, -1, 0, -1, 2, -1, 0, -1, 2], &
!                           KIND=DFP), [3, 3], ORDER=[2, 1])
!
! CALL INV(invA=b, A=a)
!
! CALL OK(MAXVAL(ABS(b(:, :, 1) - 0.5_DFP * eye3)) .LT. tol, &
!         "inverse of 2*I")
!
! ! each slice is the inverse of the corresponding slice of a
! DO k = 1, 3
!   CALL OK(MAXVAL(ABS(MATMUL(a(:, :, k), b(:, :, k)) - eye3)) &
!           .LT. tol, "A(:,:,k) * invA(:,:,k) = I")
! END DO
!
! ! each slice equals the inverse computed one matrix at a time
! DO k = 1, 3
!   CALL INV(invA=c, A=a(:, :, k))
!   CALL OK(MAXVAL(ABS(b(:, :, k) - c)) .LT. tol, "slice consistency")
! END DO
!
! END PROGRAM main
!```

INTERFACE INV
  MODULE PURE SUBROUTINE Inv_3D(invA, A)
    REAL(DFP), INTENT(INOUT) :: invA(:, :, :)
    REAL(DFP), INTENT(IN) :: A(:, :, :)
  END SUBROUTINE Inv_3D
END INTERFACE INV

END MODULE InvUtility
