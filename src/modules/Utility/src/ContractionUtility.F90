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

!> Module: ContractionUtility
! author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Tensor contraction utilities (ranks 1..4).
!
! Description:
!
!   This module provides pure functions implementing contraction (index
!   summation) operations between real arrays of ranks 1 through 4 using
!   the project's double precision kind (REAL(DFP)). The generic
!   interface `Contraction` dispatches to a specific implementation
!   depending on the ranks of the operands.
!
! The general syntax is given below:
!
! ```fortran
! ans = CONTRACTION(a1, a2)
! ```
!
! where,
!
! - `a1` and `a2` can be a Fortran array of rank 1, 2, 3, and 4.
!
! Let's employ the following notation:
!
! - `r0` rank-0 scalar
! - `r1` rank-1 vector
! - `r2` rank-2 matrix
! - `r3` rank-3
! - `r4` rank-4
!
! Then, the following table explains this operator.
!
! | a1   | a2   | ans  |
! | ---- | ---- | ---- |
! | `r1` | `r1` | `r0` |
! | `r1` | `r2` | `r1` |
! | `r1` | `r3` | `r2` |
! | `r1` | `r4` | `r3` |
! | `r2` | `r1` | `r1` |
! | `r2` | `r2` | `r0` |
! | `r2` | `r3` | `r1` |
! | `r2` | `r4` | `r2` |
! | `r3` | `r1` | `r2` |
! | `r3` | `r2` | `r1` |
! | `r3` | `r3` | `r0` |
! | `r3` | `r4` | `r1` |
! | `r4` | `r1` | `r3` |
! | `r4` | `r2` | `r2` |
! | `r4` | `r3` | `r1` |
! | `r4` | `r4` | `r0` |
! |      |      |      |
!
! Usage:
!
!   B = Contraction(A, v)
!
! Implementations (selected):
! - Contraction_r4_r1(a1(:,:,:,:), a2(:))
!   $\mathrm{ans}(i,j,k)=\sum_{l}a_1(i,j,k,l)\,a_2(l)$
! - Contraction_r4_r2(a1(:,:,:,:), a2(:, :))
! $\mathrm{ans}(i,j)=\sum_{k,l}a_1(i,j,k,l)\,a_2(k,l)$
! - Contraction_r4_r3(a1(:,:,:,:), a2(:, :, :))
! $\mathrm{ans}(i)=\sum_{j,k,l}a_1(i,j,k,l)\,a_2(j,k,l)$
! - Contraction_r4_r4(a1(:,:,:,:), a2(:,:,:,:))
! $\mathrm{ans}=\sum_{i,j,k,l}a_1\,a_2$
! - (and all combinations for ranks 1..4)
!
! Preconditions:
! - Contracted dimensions must have matching extents.
! - Arrays are REAL(DFP) and assumed-shape.
!
! Notes for maintainers:
! - Functions are declared PURE and do not perform explicit shape checks.
! - Follow project style when adding tests or extending the interface.
!
MODULE ContractionUtility
USE GlobalData, ONLY: DFP, I4B, LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> Function: Contraction_r4_r1
! author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contract rank-4 tensor with rank-1 vector -> rank-3 tensor.
!
! Arguments:
!   a1 : REAL(DFP), rank(4) - tensor with indices (i,j,k,l)
!   a2 : REAL(DFP), rank(1) - vector over index l
!
! Returns:
!   ans : REAL(DFP), rank(3) with shape (SIZE(a1,1),SIZE(a1,2),SIZE(a1,3))
!
! Formula:
!
!   $$\mathrm{ans}(i,j,k) = \sum_{l} a_1(i,j,k,l)\, a_2(l)$$
!
! Examples
!```fortran
!
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ContractionUtility, ONLY: Contraction
! USE Test_Method, ONLY: OK
! USE Display_Method, ONLY: Display
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: n1 = 2, n2 = 3, n3 = 4, n4 = 5
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(n1, n2, n3, n4), a2(n4)
! REAL(DFP) :: ans(n1, n2, n3), expected(n1, n2, n3)
! INTEGER(I4B) :: ii, jj, kk, ll
! a1 = RESHAPE([(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, &
!                ii=1, SIZE(a1))], SHAPE(a1))
! a2 = RESHAPE([(REAL(MOD(5 * ii + 3, 11) - 5, DFP) / 2.0_DFP, &
!                ii=1, SIZE(a2))], SHAPE(a2))
! expected = 0.0_DFP
! DO ll = 1, n4
!   DO kk = 1, n3
!     DO jj = 1, n2
!       DO ii = 1, n1
!         expected(ii, jj, kk) = expected(ii, jj, kk) &
!                                + a1(ii, jj, kk, ll) * a2(ll)
!       END DO
!     END DO
!   END DO
! END DO
! ans = Contraction(a1, a2)
! CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "Contraction_r4_r1")
! END PROGRAM main
!```
INTERFACE Contraction
  MODULE PURE FUNCTION Contraction_r4_r1(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2), SIZE(a1, 3))
  END FUNCTION Contraction_r4_r1
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> Function: Contraction_r4_r2
! author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contract rank-4 tensor with rank-2 tensor -> rank-2 tensor.
!
! Arguments:
!   a1 : REAL(DFP), rank(4) - tensor with indices (i,j,k,l)
!   a2 : REAL(DFP), rank(2) - tensor with indices (k,l)
!
! Returns:
!   ans : REAL(DFP), rank(2) with shape (SIZE(a1,1),SIZE(a1,2))
!
! Formula:
!
!   $$\mathrm{ans}(i,j) = \sum_{k,l} a_1(i,j,k,l)\, a_2(k,l)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ContractionUtility, ONLY: Contraction
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: n1 = 2, n2 = 3, n3 = 4, n4 = 5
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(n1, n2, n3, n4), a2(n3, n4)
! REAL(DFP) :: ans(n1, n2), expected(n1, n2)
! INTEGER(I4B) :: ii, jj, kk, ll
! a1 = RESHAPE([(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, &
!                ii=1, SIZE(a1))], SHAPE(a1))
! a2 = RESHAPE([(REAL(MOD(5 * ii + 3, 11) - 5, DFP) / 2.0_DFP, &
!                ii=1, SIZE(a2))], SHAPE(a2))
! expected = 0.0_DFP
! DO ll = 1, n4
!   DO kk = 1, n3
!     DO jj = 1, n2
!       DO ii = 1, n1
!         expected(ii, jj) = expected(ii, jj) &
!                            + a1(ii, jj, kk, ll) * a2(kk, ll)
!       END DO
!     END DO
!   END DO
! END DO
! ans = Contraction(a1, a2)
! CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "Contraction_r4_r2")
! END PROGRAM main
!```
INTERFACE Contraction
  MODULE PURE FUNCTION Contraction_r4_r2(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2))
  END FUNCTION Contraction_r4_r2
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> Function: Contraction_r4_r3
! author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contract rank-4 tensor with rank-3 tensor -> rank-1 vector.
!
! Arguments:
!   a1 : REAL(DFP), rank(4) - tensor with indices (i,j,k,l)
!   a2 : REAL(DFP), rank(3) - tensor with indices (j,k,l)
!
! Returns:
!   ans : REAL(DFP), rank(1) with shape (SIZE(a1,1))
!
! Formula:
!
!   $$\mathrm{ans}(i) = \sum_{j,k,l} a_1(i,j,k,l)\, a_2(j,k,l)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ContractionUtility, ONLY: Contraction
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: n1 = 2, n2 = 3, n3 = 4, n4 = 5
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(n1, n2, n3, n4), a2(n2, n3, n4)
! REAL(DFP) :: ans(n1), expected(n1)
! INTEGER(I4B) :: ii, jj, kk, ll
! a1 = RESHAPE([(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, &
!                ii=1, SIZE(a1))], SHAPE(a1))
! a2 = RESHAPE([(REAL(MOD(5 * ii + 3, 11) - 5, DFP) / 2.0_DFP, &
!                ii=1, SIZE(a2))], SHAPE(a2))
! expected = 0.0_DFP
! DO ll = 1, n4
!   DO kk = 1, n3
!     DO jj = 1, n2
!       DO ii = 1, n1
!         expected(ii) = expected(ii) &
!                        + a1(ii, jj, kk, ll) * a2(jj, kk, ll)
!       END DO
!     END DO
!   END DO
! END DO
! ans = Contraction(a1, a2)
! CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "Contraction_r4_r3")
! END PROGRAM main
!```
INTERFACE Contraction
  MODULE PURE FUNCTION Contraction_r4_r3(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP) :: ans(SIZE(a1, 1))
  END FUNCTION Contraction_r4_r3
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> Function: Contraction_r4_r4
! author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Inner product of two rank-4 tensors -> scalar.
!
! Arguments:
!   a1, a2 : REAL(DFP), rank(4) tensors with matching shapes
!
! Returns:
!   ans : REAL(DFP) scalar
!
! Formula:
!   $$\mathrm{ans} = \sum_{i,j,k,l} a_1(i,j,k,l)\, a_2(i,j,k,l)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ContractionUtility, ONLY: Contraction
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: n1 = 2, n2 = 3, n3 = 4, n4 = 5
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(n1, n2, n3, n4), a2(n1, n2, n3, n4)
! REAL(DFP) :: ans, expected
! INTEGER(I4B) :: ii, jj, kk, ll
! a1 = RESHAPE([(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, &
!                ii=1, SIZE(a1))], SHAPE(a1))
! a2 = RESHAPE([(REAL(MOD(5 * ii + 3, 11) - 5, DFP) / 2.0_DFP, &
!                ii=1, SIZE(a2))], SHAPE(a2))
! expected = 0.0_DFP
! DO ll = 1, n4
!   DO kk = 1, n3
!     DO jj = 1, n2
!       DO ii = 1, n1
!         expected = expected + a1(ii, jj, kk, ll) * a2(ii, jj, kk, ll)
!       END DO
!     END DO
!   END DO
! END DO
! ans = Contraction(a1, a2)
! CALL OK(ABS(ans - expected) .LT. tol, "Contraction_r4_r4")
! END PROGRAM main
!```
INTERFACE Contraction
  MODULE PURE FUNCTION Contraction_r4_r4(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP) :: ans
  END FUNCTION Contraction_r4_r4
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                   Contraction@Contraction
!----------------------------------------------------------------------------

!> Function: Contraction_r3_r1
! author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contract rank-3 tensor with rank-1 vector -> rank-2 tensor.
!
! Arguments:
!   a1 : REAL(DFP), rank(3) - tensor with indices (i,j,k)
!   a2 : REAL(DFP), rank(1) - vector over index k
!
! Returns:
!   ans : REAL(DFP), rank(2) with shape (SIZE(a1,1),SIZE(a1,2))
!
! Formula:
!   $$\mathrm{ans}(i,j) = \sum_{k} a_1(i,j,k)\, a_2(k)$$
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ContractionUtility, ONLY: Contraction
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: n1 = 2, n2 = 3, n3 = 4
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(n1, n2, n3), a2(n3)
! REAL(DFP) :: ans(n1, n2), expected(n1, n2)
! INTEGER(I4B) :: ii, jj, kk
!
! a1 = RESHAPE([(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, &
!                ii=1, SIZE(a1))], SHAPE(a1))
! a2 = RESHAPE([(REAL(MOD(5 * ii + 3, 11) - 5, DFP) / 2.0_DFP, &
!                ii=1, SIZE(a2))], SHAPE(a2))
!
! expected = 0.0_DFP
! DO kk = 1, n3
!   DO jj = 1, n2
!     DO ii = 1, n1
!       expected(ii, jj) = expected(ii, jj) + a1(ii, jj, kk) * a2(kk)
!     END DO
!   END DO
! END DO
!
! ans = Contraction(a1, a2)
!
! CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "Contraction_r3_r1")
! END PROGRAM main
!```
INTERFACE Contraction
  MODULE PURE FUNCTION Contraction_r3_r1(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2))
  END FUNCTION Contraction_r3_r1
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> Function: Contraction_r3_r2
! author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contract rank-3 tensor with rank-2 tensor -> rank-1 vector.
!
! Arguments:
!   a1 : REAL(DFP), rank(3) - tensor with indices (i,j,k)
!   a2 : REAL(DFP), rank(2) - tensor with indices (j,k)
!
! Returns:
!   ans : REAL(DFP), rank(1) with shape (SIZE(a1,1))
!
! Formula:
!   $$\mathrm{ans}(i) = \sum_{j,k} a_1(i,j,k)\, a_2(j,k)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ContractionUtility, ONLY: Contraction
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: n1 = 2, n2 = 3, n3 = 4
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(n1, n2, n3), a2(n2, n3)
! REAL(DFP) :: ans(n1), expected(n1)
! INTEGER(I4B) :: ii, jj, kk
! a1 = RESHAPE([(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, &
!                ii=1, SIZE(a1))], SHAPE(a1))
! a2 = RESHAPE([(REAL(MOD(5 * ii + 3, 11) - 5, DFP) / 2.0_DFP, &
!                ii=1, SIZE(a2))], SHAPE(a2))
! expected = 0.0_DFP
! DO kk = 1, n3
!   DO jj = 1, n2
!     DO ii = 1, n1
!       expected(ii) = expected(ii) + a1(ii, jj, kk) * a2(jj, kk)
!     END DO
!   END DO
! END DO
! ans = Contraction(a1, a2)
! CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "Contraction_r3_r2")
! END PROGRAM main
!```
INTERFACE Contraction
  MODULE PURE FUNCTION Contraction_r3_r2(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :)
    REAL(DFP) :: ans(SIZE(a1, 1))
  END FUNCTION Contraction_r3_r2
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> Function: Contraction_r3_r3
! author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Inner product of two rank-3 tensors -> scalar.
!
! Arguments:
!   a1, a2 : REAL(DFP), rank(3) tensors with matching shapes
!
! Returns:
!   ans : REAL(DFP) scalar
!
! Formula:
!   $$\mathrm{ans} = \sum_{i,j,k} a_1(i,j,k)\, a_2(i,j,k)$$
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ContractionUtility, ONLY: Contraction
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: n1 = 2, n2 = 3, n3 = 4
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(n1, n2, n3), a2(n1, n2, n3)
! REAL(DFP) :: ans, expected
! INTEGER(I4B) :: ii, jj, kk
! a1 = RESHAPE([(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, &
!                ii=1, SIZE(a1))], SHAPE(a1))
! a2 = RESHAPE([(REAL(MOD(5 * ii + 3, 11) - 5, DFP) / 2.0_DFP, &
!                ii=1, SIZE(a2))], SHAPE(a2))
! expected = 0.0_DFP
! DO kk = 1, n3
!   DO jj = 1, n2
!     DO ii = 1, n1
!       expected = expected + a1(ii, jj, kk) * a2(ii, jj, kk)
!     END DO
!   END DO
! END DO
! ans = Contraction(a1, a2)
! CALL OK(ABS(ans - expected) .LT. tol, "Contraction_r3_r3")
! END PROGRAM main
!```
INTERFACE Contraction
  MODULE PURE FUNCTION Contraction_r3_r3(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP) :: ans
  END FUNCTION Contraction_r3_r3
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> Function: Contraction_r3_r4
! author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contract rank-3 tensor with rank-4 tensor -> rank-1 vector.
!
! Arguments:
!   a1 : REAL(DFP), rank(3) - tensor with indices (i,j,k)
!   a2 : REAL(DFP), rank(4) - tensor with indices (i,j,k,l)
!
! Returns:
!   ans : REAL(DFP), rank(1) with shape (SIZE(a2,4))
!
! Formula:
!   $$\mathrm{ans}(l) = \sum_{i,j,k} a_1(i,j,k)\, a_2(i,j,k,l)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ContractionUtility, ONLY: Contraction
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: n1 = 2, n2 = 3, n3 = 4, n4 = 5
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(n1, n2, n3), a2(n1, n2, n3, n4)
! REAL(DFP) :: ans(n4), expected(n4)
! INTEGER(I4B) :: ii, jj, kk, ll
! a1 = RESHAPE([(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, &
!                ii=1, SIZE(a1))], SHAPE(a1))
! a2 = RESHAPE([(REAL(MOD(5 * ii + 3, 11) - 5, DFP) / 2.0_DFP, &
!                ii=1, SIZE(a2))], SHAPE(a2))
! expected = 0.0_DFP
! DO ll = 1, n4
!   DO kk = 1, n3
!     DO jj = 1, n2
!       DO ii = 1, n1
!         expected(ll) = expected(ll) &
!                        + a1(ii, jj, kk) * a2(ii, jj, kk, ll)
!       END DO
!     END DO
!   END DO
! END DO
! ans = Contraction(a1, a2)
! CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "Contraction_r3_r4")
! END PROGRAM main
!```
INTERFACE Contraction
  MODULE PURE FUNCTION Contraction_r3_r4(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP) :: ans(SIZE(a2, 4))
  END FUNCTION Contraction_r3_r4
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                   Contraction@Contraction
!----------------------------------------------------------------------------

!> Function: Contraction_r2_r1
! author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contract rank-2 matrix with vector -> rank-1 vector.
!
! Arguments:
!   a1 : REAL(DFP), rank(2) - matrix with indices (i,j)
!   a2 : REAL(DFP), rank(1) - vector over index j
!
! Returns:
!   ans : REAL(DFP), rank(1) with shape (SIZE(a1,1))
!
! Formula:
!   $$\mathrm{ans}(i) = \sum_{j} a_1(i,j)\, a_2(j)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ContractionUtility, ONLY: Contraction
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: n1 = 3, n2 = 4
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(n1, n2), a2(n2)
! REAL(DFP) :: ans(n1), expected(n1), exact(2)
! INTEGER(I4B) :: ii, jj
! ! hand computed case
! exact = Contraction(RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
!                              5.0_DFP, 6.0_DFP], [2, 3]), &
!                     [1.0_DFP, 0.0_DFP, -1.0_DFP])
! ! [1 3 5; 2 4 6] * [1; 0; -1] = [-4; -4]
! CALL OK(ALL(ABS(exact + 4.0_DFP) .LT. tol), "Contraction_r2_r1 exact")
! a1 = RESHAPE([(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, &
!                ii=1, SIZE(a1))], SHAPE(a1))
! a2 = RESHAPE([(REAL(MOD(5 * ii + 3, 11) - 5, DFP) / 2.0_DFP, &
!                ii=1, SIZE(a2))], SHAPE(a2))
! expected = 0.0_DFP
! DO jj = 1, n2
!   DO ii = 1, n1
!     expected(ii) = expected(ii) + a1(ii, jj) * a2(jj)
!   END DO
! END DO
! ans = Contraction(a1, a2)
! CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "Contraction_r2_r1")
! END PROGRAM main
!```
INTERFACE Contraction
  MODULE PURE FUNCTION Contraction_r2_r1(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP) :: ans(SIZE(a1, 1))
  END FUNCTION Contraction_r2_r1
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> Function: Contraction_r2_r2
! author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Frobenius inner product of two matrices -> scalar.
!
! Arguments:
!   a1, a2 : REAL(DFP), rank(2) matrices with matching shapes
!
! Returns:
!   ans : REAL(DFP) scalar
!
! Formula:
!   $$\mathrm{ans} = \sum_{i,j} a_1(i,j)\, a_2(i,j)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ContractionUtility, ONLY: Contraction
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: n1 = 3, n2 = 4
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(n1, n2), a2(n1, n2)
! REAL(DFP) :: ans, expected
! INTEGER(I4B) :: ii, jj
! ! hand computed case: identity-like inner product
! ans = Contraction(RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], [2, 2]), &
!                   RESHAPE([1.0_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP], [2, 2]))
! CALL OK(ABS(ans - 10.0_DFP) .LT. tol, "Contraction_r2_r2 exact")

! a1 = RESHAPE([(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, &
!                ii=1, SIZE(a1))], SHAPE(a1))
! a2 = RESHAPE([(REAL(MOD(5 * ii + 3, 11) - 5, DFP) / 2.0_DFP, &
!                ii=1, SIZE(a2))], SHAPE(a2))
! expected = 0.0_DFP
! DO jj = 1, n2
!   DO ii = 1, n1
!     expected = expected + a1(ii, jj) * a2(ii, jj)
!   END DO
! END DO
! ans = Contraction(a1, a2)
! CALL OK(ABS(ans - expected) .LT. tol, "Contraction_r2_r2")
! END PROGRAM main
!```
INTERFACE Contraction
  MODULE PURE FUNCTION Contraction_r2_r2(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :)
    REAL(DFP), INTENT(IN) :: a2(:, :)
    REAL(DFP) :: ans
  END FUNCTION Contraction_r2_r2
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> Function: Contraction_r2_r3
! author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contract rank-2 matrix with rank-3 tensor -> rank-1 vector.
!
! Arguments:
!   a1 : REAL(DFP), rank(2) - matrix with indices (i,j)
!   a2 : REAL(DFP), rank(3) - tensor with indices (i,j,k)
!
! Returns:
!   ans : REAL(DFP), rank(1) with shape (SIZE(a2,3))
!
! Formula:
!   $$\mathrm{ans}(k) = \sum_{i,j} a_1(i,j)\, a_2(i,j,k)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ContractionUtility, ONLY: Contraction
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: n1 = 2, n2 = 3, n3 = 4
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(n1, n2), a2(n1, n2, n3)
! REAL(DFP) :: ans(n3), expected(n3)
! INTEGER(I4B) :: ii, jj, kk
! a1 = RESHAPE([(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, &
!                ii=1, SIZE(a1))], SHAPE(a1))
! a2 = RESHAPE([(REAL(MOD(5 * ii + 3, 11) - 5, DFP) / 2.0_DFP, &
!                ii=1, SIZE(a2))], SHAPE(a2))
! expected = 0.0_DFP
! DO kk = 1, n3
!   DO jj = 1, n2
!     DO ii = 1, n1
!       expected(kk) = expected(kk) + a1(ii, jj) * a2(ii, jj, kk)
!     END DO
!   END DO
! END DO
! ans = Contraction(a1, a2)
! CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "Contraction_r2_r3")
! END PROGRAM main
!```
INTERFACE Contraction
  MODULE PURE FUNCTION Contraction_r2_r3(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP) :: ans(SIZE(a2, 3))
  END FUNCTION Contraction_r2_r3
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> Function: Contraction_r2_r4
! author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contract rank-2 matrix with rank-4 tensor -> rank-2 tensor.
!
! Arguments:
!   a1 : REAL(DFP), rank(2) - matrix with indices (i,j)
!   a2 : REAL(DFP), rank(4) - tensor with indices (i,j,k,l)
!
! Returns:
!   ans : REAL(DFP), rank(2) with shape (SIZE(a2,3),SIZE(a2,4))
!
! Formula:
!   $$\mathrm{ans}(k,l) = \sum_{i,j} a_1(i,j)\, a_2(i,j,k,l)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ContractionUtility, ONLY: Contraction
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: n1 = 2, n2 = 3, n3 = 4, n4 = 5
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(n1, n2), a2(n1, n2, n3, n4)
! REAL(DFP) :: ans(n3, n4), expected(n3, n4)
! INTEGER(I4B) :: ii, jj, kk, ll
! a1 = RESHAPE([(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, &
!                ii=1, SIZE(a1))], SHAPE(a1))
! a2 = RESHAPE([(REAL(MOD(5 * ii + 3, 11) - 5, DFP) / 2.0_DFP, &
!                ii=1, SIZE(a2))], SHAPE(a2))
! expected = 0.0_DFP
! DO ll = 1, n4
!   DO kk = 1, n3
!     DO jj = 1, n2
!       DO ii = 1, n1
!         expected(kk, ll) = expected(kk, ll) &
!                            + a1(ii, jj) * a2(ii, jj, kk, ll)
!       END DO
!     END DO
!   END DO
! END DO
! ans = Contraction(a1, a2)
! CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "Contraction_r2_r4")
! END PROGRAM main
!```
INTERFACE Contraction
  MODULE PURE FUNCTION Contraction_r2_r4(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP) :: ans(SIZE(a2, 3), SIZE(a2, 4))
  END FUNCTION Contraction_r2_r4
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                   Contraction@Contraction
!----------------------------------------------------------------------------

!> Function: Contraction_r1_r1
! author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Dot product of two vectors -> scalar.
!
! Arguments:
!   a1, a2 : REAL(DFP), rank(1) vectors with matching length
!
! Returns:
!   ans : REAL(DFP) scalar
!
! Formula:
!   $$\mathrm{ans} = \sum_{i} a_1(i)\, a_2(i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ContractionUtility, ONLY: Contraction
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: n1 = 6
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(n1), a2(n1)
! REAL(DFP) :: ans, expected
! INTEGER(I4B) :: ii
! ! hand computed case: [1,2,3].[4,5,6] = 32
! ans = Contraction([1.0_DFP, 2.0_DFP, 3.0_DFP], [4.0_DFP, 5.0_DFP, 6.0_DFP])
! CALL OK(ABS(ans - 32.0_DFP) .LT. tol, "Contraction_r1_r1 exact")
! a1 = [(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, ii=1, n1)]
! a2 = [(REAL(MOD(5 * ii + 3, 11) - 5, DFP) / 2.0_DFP, ii=1, n1)]
! expected = 0.0_DFP
! DO ii = 1, n1
!   expected = expected + a1(ii) * a2(ii)
! END DO
! ans = Contraction(a1, a2)
! CALL OK(ABS(ans - expected) .LT. tol, "Contraction_r1_r1")
! END PROGRAM main
!```
INTERFACE Contraction
  MODULE PURE FUNCTION Contraction_r1_r1(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP) :: ans
  END FUNCTION Contraction_r1_r1
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> Function: Contraction_r1_r2
! author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Multiply vector into matrix -> rank-1 vector.
!
! Arguments:
!   a1 : REAL(DFP), rank(1) - vector with index (i)
!   a2 : REAL(DFP), rank(2) - matrix with indices (i,j)
!
! Returns:
!   ans : REAL(DFP), rank(1) with shape (SIZE(a2,2))
!
! Formula:
!   $$\mathrm{ans}(j) = \sum_{i} a_1(i)\, a_2(i,j)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ContractionUtility, ONLY: Contraction
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: n1 = 3, n2 = 4
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(n1), a2(n1, n2)
! REAL(DFP) :: ans(n2), expected(n2)
! INTEGER(I4B) :: ii, jj
! a1 = [(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, ii=1, n1)]
! a2 = RESHAPE([(REAL(MOD(5 * ii + 3, 11) - 5, DFP) / 2.0_DFP, &
!                ii=1, SIZE(a2))], SHAPE(a2))
! expected = 0.0_DFP
! DO jj = 1, n2
!   DO ii = 1, n1
!     expected(jj) = expected(jj) + a1(ii) * a2(ii, jj)
!   END DO
! END DO
! ans = Contraction(a1, a2)
! CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "Contraction_r1_r2")
! END PROGRAM main
!```
INTERFACE Contraction
  MODULE PURE FUNCTION Contraction_r1_r2(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:, :)
    REAL(DFP) :: ans(SIZE(a2, 2))
  END FUNCTION Contraction_r1_r2
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> Function: Contraction_r1_r3
! author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contract vector with rank-3 tensor -> rank-2 tensor.
!
! Arguments:
!   a1 : REAL(DFP), rank(1) - vector with index (i)
!   a2 : REAL(DFP), rank(3) - tensor with indices (i,j,k)
!
! Returns:
!   ans : REAL(DFP), rank(2) with shape (SIZE(a2,2),SIZE(a2,3))
!
! Formula:
!   $$\mathrm{ans}(j,k) = \sum_{i} a_1(i)\, a_2(i,j,k)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ContractionUtility, ONLY: Contraction
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: n1 = 2, n2 = 3, n3 = 4
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(n1), a2(n1, n2, n3)
! REAL(DFP) :: ans(n2, n3), expected(n2, n3)
! INTEGER(I4B) :: ii, jj, kk
! a1 = [(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, ii=1, n1)]
! a2 = RESHAPE([(REAL(MOD(5 * ii + 3, 11) - 5, DFP) / 2.0_DFP, &
!                ii=1, SIZE(a2))], SHAPE(a2))
! expected = 0.0_DFP
! DO kk = 1, n3
!   DO jj = 1, n2
!     DO ii = 1, n1
!       expected(jj, kk) = expected(jj, kk) + a1(ii) * a2(ii, jj, kk)
!     END DO
!   END DO
! END DO
! ans = Contraction(a1, a2)
! CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "Contraction_r1_r3")
! END PROGRAM main
!```
INTERFACE Contraction
  MODULE PURE FUNCTION Contraction_r1_r3(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP) :: ans(SIZE(a2, 2), SIZE(a2, 3))
  END FUNCTION Contraction_r1_r3
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> Function: Contraction_r1_r4
! author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Contract vector with rank-4 tensor -> rank-3 tensor.
!
! Arguments:
!   a1 : REAL(DFP), rank(1) - vector with index (i)
!   a2 : REAL(DFP), rank(4) - tensor with indices (i,j,k,l)
!
! Returns:
!   ans : REAL(DFP), rank(3) with shape (SIZE(a2,2),SIZE(a2,3),SIZE(a2,4))
!
! Formula:
!   $$\mathrm{ans}(j,k,l) = \sum_{i} a_1(i)\, a_2(i,j,k,l)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ContractionUtility, ONLY: Contraction
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: n1 = 2, n2 = 3, n3 = 4, n4 = 5
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: a1(n1), a2(n1, n2, n3, n4)
! REAL(DFP) :: ans(n2, n3, n4), expected(n2, n3, n4)
! INTEGER(I4B) :: ii, jj, kk, ll
! a1 = [(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, ii=1, n1)]
! a2 = RESHAPE([(REAL(MOD(5 * ii + 3, 11) - 5, DFP) / 2.0_DFP, &
!                ii=1, SIZE(a2))], SHAPE(a2))
! expected = 0.0_DFP
! DO ll = 1, n4
!   DO kk = 1, n3
!     DO jj = 1, n2
!       DO ii = 1, n1
!         expected(jj, kk, ll) = expected(jj, kk, ll) &
!                                + a1(ii) * a2(ii, jj, kk, ll)
!       END DO
!     END DO
!   END DO
! END DO
! ans = Contraction(a1, a2)
! CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "Contraction_r1_r4")
! END PROGRAM main
!```
INTERFACE Contraction
  MODULE PURE FUNCTION Contraction_r1_r4(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP) :: ans(SIZE(a2, 2), SIZE(a2, 3), SIZE(a2, 4))
  END FUNCTION Contraction_r1_r4
END INTERFACE Contraction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ContractionUtility
