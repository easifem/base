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

!> Test: SymEigenValues
! summary: eigenvalues of 4 by 4 and 5 by 5 symmetric matrices
!
! Matrices of size 4 or more are handled by SYEV, so this test needs the
! library to be built with USE_LAPACK95.
!
! The eigenvalues of the n by n second difference matrix are
! 2 - 2 cos(k pi / (n+1)), k = 1, ..., n.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE EigenUtility, ONLY: SymEigenValues
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: pi = ACOS(-1.0_DFP)
INTEGER(I4B), PARAMETER :: n = 4
REAL(DFP) :: mat(n, n), w(n), exact(n), h
REAL(DFP) :: mat5(5, 5), w5(5), exact5(5)
INTEGER(I4B) :: ii

! 4 by 4 second difference matrix
mat = 0.0_DFP
DO ii = 1, n
  mat(ii, ii) = 2.0_DFP
END DO
DO ii = 1, n - 1
  mat(ii, ii + 1) = -1.0_DFP
  mat(ii + 1, ii) = -1.0_DFP
END DO
h = pi / REAL(n + 1, DFP)
DO ii = 1, n
  exact(ii) = 2.0_DFP - 2.0_DFP * COS(REAL(ii, DFP) * h)
END DO

w = SymEigenValues(mat)
CALL OK(ALL(ABS(w - exact) .LE. 1.0E-10_DFP), "4 by 4 matrix")

! 5 by 5 matrix
mat5 = RESHAPE([ &
               1.96_DFP, -6.49_DFP, -0.47_DFP, -7.20_DFP, -0.65_DFP, &
               -6.49_DFP, 3.80_DFP, -6.39_DFP, 1.50_DFP, -6.34_DFP, &
               -0.47_DFP, -6.39_DFP, 4.17_DFP, -1.51_DFP, 2.67_DFP, &
               -7.20_DFP, 1.50_DFP, -1.51_DFP, 5.70_DFP, 1.80_DFP, &
               -0.65_DFP, -6.34_DFP, 2.67_DFP, 1.80_DFP, -7.10_DFP], [5, 5])
exact5 = [-11.07_DFP, -6.23_DFP, 0.86_DFP, 8.87_DFP, 16.09_DFP]

w5 = SymEigenValues(mat5)
CALL OK(ALL(ABS(w5 - exact5) .LE. 1.0E-2_DFP), "5 by 5 matrix")
CALL OK(ALL(w5(1:4) .LE. w5(2:5)), "ascending order")

END PROGRAM main
