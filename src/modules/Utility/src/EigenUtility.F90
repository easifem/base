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
! date: 2023-06-06
! summary: Eigenvalues and eigenvectors of symmetric matrices.
!
! Description:
!   This module contains routines for computing the eigenvalues and the
!   eigenvectors of real symmetric matrices.
!
!   - `SymEigenValues2by2` and `SymEigenValues3by3` compute the
!     eigenvalues of 2 by 2 and 3 by 3 matrices in closed form.
!   - `SymEigenValuesUpto3` selects the closed form routine for the matrices
!     of size 1, 2 and 3.
!   - `SymEigenValues` is a function which returns the eigenvalues of a
!     matrix of any size.
!   - `GetSymEigenValues` and `GetSymEigenValues_` are the subroutine
!     versions of `SymEigenValues`. The latter overwrites the matrix.
!   - `GetSymEigenJacobi` computes the eigenvalues and the eigenvectors by
!     the Jacobi method.
!
! Symmetry:
!   Only the diagonal and the upper triangle of the input matrix are
!   referenced by the closed form routines and by `GetSymEigenJacobi`.
!   The matrix is not checked for symmetry.
!
! Lapack95:
!   For matrices of size 4 or more, `SymEigenValues`, `GetSymEigenValues`
!   and `GetSymEigenValues_` call `SYEV` of Lapack95. They need the
!   library to be built with `USE_LAPACK95`. Otherwise, an error message is
!   printed and the eigenvalues are not computed.
!
! Ordering:
!   The eigenvalues from the routines which do not compute eigenvectors
!   are in the increasing order. `GetSymEigenJacobi` returns them in the
!   decreasing order.

MODULE EigenUtility
USE GlobalData, ONLY: DFP, I4B, LGT
IMPLICIT NONE
PRIVATE
PUBLIC :: SymEigenValues2by2
PUBLIC :: SymEigenValues3by3
PUBLIC :: SymEigenValuesUpto3
PUBLIC :: SymEigenValues
PUBLIC :: GetSymEigenValues
PUBLIC :: GetSymEigenValues_
PUBLIC :: GetSymEigenJacobi

!----------------------------------------------------------------------------
!                                                                 SymEigen
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-26
! summary: Eigenvalues of a 2 by 2 symmetric matrix.
!
! Description:
!   Computes the two eigenvalues of a symmetric 2 by 2 matrix in closed
!   form. The eigenvalues are returned in the increasing order. Only the
!   entries `mat(1, 1)`, `mat(1, 2)` and `mat(2, 2)` are used.
!
! Arguments:
!   mat : REAL(DFP), shape (2, 2) - symmetric matrix
!
! Returns:
!   ans : REAL(DFP), shape (2) - eigenvalues in the increasing order
!
! Formula:
!   $$\lambda_{1,2} = \frac{a_{11} + a_{22}}{2}
!   \mp \frac{1}{2}\sqrt{(a_{11} - a_{22})^2 + 4\,a_{12}^2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE EigenUtility, ONLY: SymEigenValues2by2
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: mat(2, 2), w(2), exact(2)
!
! ! general matrix, the eigenvalues are 0.5 and 3.0
! mat = RESHAPE([2.5_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP], [2, 2])
! exact = [0.5_DFP, 3.0_DFP]
! w = SymEigenValues2by2(mat)
! CALL OK(ALL(ABS(w - exact) .LE. tol), "general matrix")
! CALL OK(w(1) .LE. w(2), "ascending order")
! CALL OK(ABS(SUM(w) - 3.5_DFP) .LE. tol, "sum is the trace")
! CALL OK(ABS(w(1) * w(2) - 1.5_DFP) .LE. tol, "product is the determinant")
!
! ! diagonal matrix
! mat = RESHAPE([3.0_DFP, 0.0_DFP, 0.0_DFP, -1.0_DFP], [2, 2])
! w = SymEigenValues2by2(mat)
! CALL OK(ALL(ABS(w - [-1.0_DFP, 3.0_DFP]) .LE. tol), "diagonal matrix")
!
! ! repeated eigenvalue
! mat = RESHAPE([2.0_DFP, 0.0_DFP, 0.0_DFP, 2.0_DFP], [2, 2])
! w = SymEigenValues2by2(mat)
! CALL OK(ALL(ABS(w - 2.0_DFP) .LE. tol), "repeated eigenvalue")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION SymEigenValues2by2(mat) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(2, 2)
    REAL(DFP) :: ans(2)
  END FUNCTION SymEigenValues2by2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SymEigen
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-26
! summary: Eigenvalues of a 3 by 3 symmetric matrix.
!
! Description:
!   Computes the three eigenvalues of a symmetric 3 by 3 matrix in closed
!   form by the trigonometric method. The eigenvalues are returned in the
!   increasing order. Only the diagonal and the upper triangle of `mat`
!   are used.
!
!   The method divides by $p$ (see Formula). It fails (the result is NaN)
!   when $p = 0$, that is, when `mat` is a multiple of the identity
!   matrix. In this case all the eigenvalues are equal to `mat(1, 1)`.
!
! Arguments:
!   mat : REAL(DFP), shape (3, 3) - symmetric matrix
!
! Returns:
!   ans : REAL(DFP), shape (3) - eigenvalues in the increasing order
!
! Formula:
!   Let $q = \mathrm{tr}(A)/3$ and
!
!   $$p = \sqrt{\frac{1}{6}\left(\sum_{i=1}^{3}(a_{ii} - q)^2
!   + 2\sum_{i<j} a_{ij}^2\right)}$$
!
!   Let $B = (A - qI)/p$, $r = \det(B)/2$ and
!   $\phi = \arccos(r)/3$, where $r$ is clipped to $[-1, 1]$. Then
!
!   $$\lambda_3 = q + 2p\cos\phi, \qquad
!   \lambda_1 = q + 2p\cos\left(\phi + \frac{2\pi}{3}\right), \qquad
!   \lambda_2 = 3q - \lambda_1 - \lambda_3$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE EigenUtility, ONLY: SymEigenValues3by3
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-9_DFP
! REAL(DFP) :: mat(3, 3), w(3), exact(3), s
!
! s = SQRT(2.0_DFP)
! mat = RESHAPE([ &
!   2.0_DFP, -1.0_DFP, 0.0_DFP, &
!   -1.0_DFP, 2.0_DFP, -1.0_DFP, &
!   0.0_DFP, -1.0_DFP, 2.0_DFP], [3, 3])
! exact = [2.0_DFP - s, 2.0_DFP, 2.0_DFP + s]
!
! w = SymEigenValues3by3(mat)
!
! CALL OK(ALL(ABS(w - exact) .LE. tol), "second difference matrix")
! CALL OK(ALL(w(1:2) .LE. w(2:3)), "ascending order")
! CALL OK(ABS(SUM(w) - 6.0_DFP) .LE. tol, "sum is the trace")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION SymEigenValues3by3(mat) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    REAL(DFP) :: ans(3)
  END FUNCTION SymEigenValues3by3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SymEigen
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-26
! summary: Eigenvalues of a symmetric matrix of size 1, 2 or 3.
!
! Description:
!   Computes the eigenvalues of a symmetric matrix of size 1, 2 or 3.
!
!   - If `n = 1`, the eigenvalue is `mat(1, 1)`.
!   - If `n = 2`, it calls `SymEigenValues2by2`.
!   - If `n = 3`, it calls `SymEigenValues3by3`.
!
!   The size of `mat` must be 1, 2 or 3. For any other size, `ans` is not
!   set. Use `SymEigenValues` if the size is not known in advance.
!
! Arguments:
!   mat : REAL(DFP), shape (n, n) - symmetric matrix, n = 1, 2 or 3
!
! Returns:
!   ans : REAL(DFP), shape (n) - eigenvalues in the increasing order
!
! Formula:
!   $$\lambda_1 \le \lambda_2 \le \cdots \le \lambda_n, \qquad
!   A\,x_i = \lambda_i\,x_i, \qquad n \le 3$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE EigenUtility, ONLY: SymEigenValuesUpto3
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-9_DFP
! REAL(DFP) :: m1(1, 1), m2(2, 2), m3(3, 3)
! REAL(DFP) :: w1(1), w2(2), w3(3), s
!
! ! 1 by 1
! m1(1, 1) = 4.0_DFP
! w1 = SymEigenValuesUpto3(m1)
! CALL OK(ABS(w1(1) - 4.0_DFP) .LE. tol, "1 by 1 matrix")
!
! ! 2 by 2
! m2 = RESHAPE([2.5_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP], [2, 2])
! w2 = SymEigenValuesUpto3(m2)
! CALL OK(ALL(ABS(w2 - [0.5_DFP, 3.0_DFP]) .LE. tol), "2 by 2 matrix")
!
! ! 3 by 3
! s = SQRT(2.0_DFP)
! m3 = RESHAPE([ &
!   2.0_DFP, -1.0_DFP, 0.0_DFP, &
!   -1.0_DFP, 2.0_DFP, -1.0_DFP, &
!   0.0_DFP, -1.0_DFP, 2.0_DFP], [3, 3])
! w3 = SymEigenValuesUpto3(m3)
! CALL OK(ALL(ABS(w3 - [2.0_DFP - s, 2.0_DFP, 2.0_DFP + s]) .LE. tol), &
!   "3 by 3 matrix")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION SymEigenValuesUpto3(mat) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(:, :)
    !! size(mat, 1) = [1,2,3]
    REAL(DFP) :: ans(SIZE(mat, 1))
  END FUNCTION SymEigenValuesUpto3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SymEigen
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-11-14
! summary: Eigenvalues of a symmetric matrix of any size.
!
! Description:
!   Computes the eigenvalues of a symmetric matrix. The input matrix is
!   not changed.
!
!   - If `n = 1`, the eigenvalue is `mat(1, 1)`.
!   - If `n = 2`, it calls `SymEigenValues2by2`.
!   - If `n = 3`, it calls `SymEigenValues3by3`.
!   - If `n >= 4`, it calls `SYEV` from Lapack95. This needs the library
!     to be built with `USE_LAPACK95`.
!
!   This function is not `PURE`, because it can print an error message.
!
! Arguments:
!   mat : REAL(DFP), shape (n, n) - symmetric matrix
!
! Returns:
!   ans : REAL(DFP), shape (n) - eigenvalues in the increasing order
!
! Formula:
!   $$\det(A - \lambda_i I) = 0, \qquad
!   \lambda_1 \le \lambda_2 \le \cdots \le \lambda_n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE EigenUtility, ONLY: SymEigenValues
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-9_DFP
! REAL(DFP) :: m1(1, 1), m2(2, 2), m3(3, 3)
! REAL(DFP) :: w1(1), w2(2), w3(3), s
!
! ! 1 by 1
! m1(1, 1) = 4.0_DFP
! w1 = SymEigenValues(m1)
! CALL OK(ABS(w1(1) - 4.0_DFP) .LE. tol, "1 by 1 matrix")
!
! ! 2 by 2
! m2 = RESHAPE([2.5_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP], [2, 2])
! w2 = SymEigenValues(m2)
! CALL OK(ALL(ABS(w2 - [0.5_DFP, 3.0_DFP]) .LE. tol), "2 by 2 matrix")
!
! ! 3 by 3
! s = SQRT(2.0_DFP)
! m3 = RESHAPE([ &
!   2.0_DFP, -1.0_DFP, 0.0_DFP, &
!   -1.0_DFP, 2.0_DFP, -1.0_DFP, &
!   0.0_DFP, -1.0_DFP, 2.0_DFP], [3, 3])
! w3 = SymEigenValues(m3)
! CALL OK(ALL(ABS(w3 - [2.0_DFP - s, 2.0_DFP, 2.0_DFP + s]) .LE. tol), &
!   "3 by 3 matrix")
!
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION SymEigenValues(mat) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(:, :)
    !! for n=2, we call SymEigenValues2by2
    !! for n=3, we call SymEigenValues3by3
    !! for n>=4, we call Lapack
    REAL(DFP) :: ans(SIZE(mat, 1))
  END FUNCTION SymEigenValues
END INTERFACE

!----------------------------------------------------------------------------
!                                                                GetSymEigen
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-11-14
! summary: Compute the eigenvalues of a symmetric matrix.
!
! Description:
!   Subroutine version of `SymEigenValues`. The input matrix is copied
!   internally, therefore `mat` is not changed. If the copy is not
!   wanted, then use `GetSymEigenValues_`.
!
!   - If `n = 1`, the eigenvalue is `mat(1, 1)`.
!   - If `n = 2`, it calls `SymEigenValues2by2`.
!   - If `n = 3`, it calls `SymEigenValues3by3`.
!   - If `n >= 4`, it calls `SYEV` from Lapack95. This needs the library
!     to be built with `USE_LAPACK95`.
!
! Arguments:
!   mat : REAL(DFP), shape (n, n), IN - symmetric matrix
!   eigenValues : REAL(DFP), shape (n), OUT - eigenvalues in the
!     increasing order. Its size should be exactly `n`.
!
! Formula:
!   $$\det(A - \lambda_i I) = 0, \qquad
!   \lambda_1 \le \lambda_2 \le \cdots \le \lambda_n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE EigenUtility, ONLY: GetSymEigenValues
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-9_DFP
! REAL(DFP) :: m2(2, 2), m2copy(2, 2), w2(2)
! REAL(DFP) :: m3(3, 3), m3copy(3, 3), w3(3), s
!
! ! 2 by 2
! m2 = RESHAPE([2.5_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP], [2, 2])
! m2copy = m2
! CALL GetSymEigenValues(mat=m2, eigenValues=w2)
! CALL OK(ALL(ABS(w2 - [0.5_DFP, 3.0_DFP]) .LE. tol), "2 by 2 eigenvalues")
! CALL OK(ALL(m2 .EQ. m2copy), "2 by 2 mat is not changed")
!
! ! 3 by 3
! s = SQRT(2.0_DFP)
! m3 = RESHAPE([ &
!   2.0_DFP, -1.0_DFP, 0.0_DFP, &
!   -1.0_DFP, 2.0_DFP, -1.0_DFP, &
!   0.0_DFP, -1.0_DFP, 2.0_DFP], [3, 3])
! m3copy = m3
! CALL GetSymEigenValues(mat=m3, eigenValues=w3)
! CALL OK(ALL(ABS(w3 - [2.0_DFP - s, 2.0_DFP, 2.0_DFP + s]) .LE. tol), &
!   "3 by 3 eigenvalues")
! CALL OK(ALL(m3 .EQ. m3copy), "3 by 3 mat is not changed")
!
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE GetSymEigenValues(mat, eigenValues)
    REAL(DFP), INTENT(IN) :: mat(:, :)
    REAL(DFP), INTENT(OUT) :: eigenValues(:)
  END SUBROUTINE GetSymEigenValues
END INTERFACE

!----------------------------------------------------------------------------
!                                                                GetSymEigen
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-11-14
! summary: Compute the eigenvalues of a symmetric matrix, mat is destroyed.
!
! Description:
!   Same as `GetSymEigenValues`, but the matrix is not copied, so it can
!   be overwritten.
!
!   - If `n = 1`, the eigenvalue is `mat(1, 1)`.
!   - If `n = 2`, it calls `SymEigenValues2by2`.
!   - If `n = 3`, it calls `SymEigenValues3by3`.
!   - If `n >= 4`, it calls `SYEV` from Lapack95. This needs the library
!     to be built with `USE_LAPACK95`.
!
!   For `n >= 4`, the diagonal and the upper triangle of `mat` are
!   destroyed on output. Do not use `mat` after the call. If `mat` is
!   needed later, then call `GetSymEigenValues`.
!
! Arguments:
!   mat : REAL(DFP), shape (n, n), INOUT - symmetric matrix, destroyed
!   eigenValues : REAL(DFP), shape (n), OUT - eigenvalues in the
!     increasing order. Its size should be exactly `n`.
!
! Formula:
!   $$\det(A - \lambda_i I) = 0, \qquad
!   \lambda_1 \le \lambda_2 \le \cdots \le \lambda_n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE EigenUtility, ONLY: GetSymEigenValues_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-9_DFP
! REAL(DFP) :: m1(1, 1), m2(2, 2), m3(3, 3)
! REAL(DFP) :: w1(1), w2(2), w3(3), s
!
! ! 1 by 1
! m1(1, 1) = -4.0_DFP
! CALL GetSymEigenValues_(mat=m1, eigenValues=w1)
! CALL OK(ABS(w1(1) + 4.0_DFP) .LE. tol, "1 by 1 matrix")
!
! ! 2 by 2
! m2 = RESHAPE([2.5_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP], [2, 2])
! CALL GetSymEigenValues_(mat=m2, eigenValues=w2)
! CALL OK(ALL(ABS(w2 - [0.5_DFP, 3.0_DFP]) .LE. tol), "2 by 2 matrix")
!
! ! 3 by 3
! s = SQRT(2.0_DFP)
! m3 = RESHAPE([ &
!   2.0_DFP, -1.0_DFP, 0.0_DFP, &
!   -1.0_DFP, 2.0_DFP, -1.0_DFP, &
!   0.0_DFP, -1.0_DFP, 2.0_DFP], [3, 3])
! CALL GetSymEigenValues_(mat=m3, eigenValues=w3)
! CALL OK(ALL(ABS(w3 - [2.0_DFP - s, 2.0_DFP, 2.0_DFP + s]) .LE. tol), &
!   "3 by 3 matrix")
!
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE GetSymEigenValues_(mat, eigenValues)
    REAL(DFP), INTENT(INOUT) :: mat(:, :)
    REAL(DFP), INTENT(OUT) :: eigenValues(:)
  END SUBROUTINE GetSymEigenValues_
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetSymEigenJacobi@LAPACK
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-06
! summary: Eigenvalues and eigenvectors by the Jacobi method.
!
! Description:
!   Computes all the eigenvalues and the eigenvectors of a real symmetric
!   n by n matrix `mat` by the Jacobi method of plane rotations.
!
!   - `mat` is not changed. Only its diagonal and strict upper triangle
!     are referenced.
!   - `eigenValues` returns the eigenvalues of `mat` in the decreasing
!     order. Its size should be `n`.
!   - `eigenVectors` is an `n` by `n` matrix. Its j-th column is the
!     normalized eigenvector of the j-th eigenvalue. The columns are
!     orthonormal.
!   - `maxIter` is the maximum number of sweeps (a sweep rotates every
!     off-diagonal entry once). Each sweep needs $O(n^3)$ operations.
!     Usually less than 10 sweeps are needed, so a value like 50 is
!     safe. If the off-diagonal entries are not small enough after
!     `maxIter` sweeps, then the routine returns the partial result,
!     which is not sorted.
!
!   The algorithm follows Numerical Recipes in Fortran, page 1225.
!
!   TODO: Remove this subroutine, instead call Lapack.
!
! Arguments:
!   mat : REAL(DFP), shape (n, n), IN - symmetric matrix
!   eigenValues : REAL(DFP), shape (n), INOUT - eigenvalues in the
!     decreasing order
!   eigenVectors : REAL(DFP), shape (n, n), INOUT - eigenvectors,
!     stored in columns
!   maxIter : INTEGER(I4B), IN - maximum number of sweeps
!
! Formula:
!   Let $\Lambda = \mathrm{diag}(\lambda_1, \ldots, \lambda_n)$ with
!   $\lambda_1 \ge \lambda_2 \ge \cdots \ge \lambda_n$, and let $V$ be
!   the matrix of the eigenvectors. Then
!
!   $$A\,V = V\,\Lambda, \qquad V^{\top} V = I$$
!
!   Each rotation $J_{pq}$ zeros the entry $a_{pq}$ and the algorithm
!   computes
!
!   $$V = J_{1}\,J_{2}\cdots J_{k}, \qquad
!   \Lambda = V^{\top} A\,V$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE EigenUtility, ONLY: GetSymEigenJacobi
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: n = 5
! REAL(DFP) :: mat(n, n), w(n), v(n, n), exact(n), res(n)
! REAL(DFP) :: eye(n, n)
! INTEGER(I4B) :: jj
! LOGICAL :: isOk
!
! mat = RESHAPE([ &
!   1.96_DFP, -6.49_DFP, -0.47_DFP, -7.20_DFP, -0.65_DFP, &
!   -6.49_DFP, 3.80_DFP, -6.39_DFP, 1.50_DFP, -6.34_DFP, &
!   -0.47_DFP, -6.39_DFP, 4.17_DFP, -1.51_DFP, 2.67_DFP, &
!   -7.20_DFP, 1.50_DFP, -1.51_DFP, 5.70_DFP, 1.80_DFP, &
!   -0.65_DFP, -6.34_DFP, 2.67_DFP, 1.80_DFP, -7.10_DFP], [n, n])
! exact = [16.09_DFP, 8.87_DFP, 0.86_DFP, -6.23_DFP, -11.07_DFP]
!
! w = 0.0_DFP
! v = 0.0_DFP
! CALL GetSymEigenJacobi(mat=mat, eigenValues=w, eigenVectors=v, &
!   maxIter=50_I4B)
!
! CALL OK(ALL(ABS(w - exact) .LE. 1.0E-2_DFP), "eigenvalues")
! CALL OK(ALL(w(1:n - 1) .GE. w(2:n)), "decreasing order")
!
! isOk = .TRUE.
! DO jj = 1, n
!   res = MATMUL(mat, v(:, jj)) - w(jj) * v(:, jj)
!   isOk = isOk .AND. (MAXVAL(ABS(res)) .LE. 1.0E-8_DFP)
! END DO
! CALL OK(isOk, "mat v = lambda v")
!
! eye = MATMUL(TRANSPOSE(v), v)
! DO jj = 1, n
!   eye(jj, jj) = eye(jj, jj) - 1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(eye)) .LE. 1.0E-8_DFP, "orthonormal eigenvectors")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE GetSymEigenJacobi(mat, eigenValues, eigenVectors, &
                                           maxIter)
    REAL(DFP), INTENT(IN) :: mat(:, :)
    REAL(DFP), INTENT(INOUT) :: eigenValues(:)
    REAL(DFP), INTENT(INOUT) :: eigenVectors(:, :)
    INTEGER(I4B), INTENT(IN) :: maxIter
  END SUBROUTINE GetSymEigenJacobi
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE EigenUtility
