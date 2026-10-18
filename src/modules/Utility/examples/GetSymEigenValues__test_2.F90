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

!> Test: GetSymEigenValues_
! summary: eigenvalues of a 5 by 5 matrix, mat is destroyed
!
! Matrices of size 4 or more are handled by SYEV, so this test needs the
! library to be built with USE_LAPACK95.

PROGRAM main
USE GlobalData, ONLY: DFP
USE EigenUtility, ONLY: GetSymEigenValues_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-2_DFP
REAL(DFP) :: mat(5, 5), w(5), exact(5)

mat = RESHAPE([ &
              1.96_DFP, -6.49_DFP, -0.47_DFP, -7.20_DFP, -0.65_DFP, &
              -6.49_DFP, 3.80_DFP, -6.39_DFP, 1.50_DFP, -6.34_DFP, &
              -0.47_DFP, -6.39_DFP, 4.17_DFP, -1.51_DFP, 2.67_DFP, &
              -7.20_DFP, 1.50_DFP, -1.51_DFP, 5.70_DFP, 1.80_DFP, &
              -0.65_DFP, -6.34_DFP, 2.67_DFP, 1.80_DFP, -7.10_DFP], [5, 5])
exact = [-11.07_DFP, -6.23_DFP, 0.86_DFP, 8.87_DFP, 16.09_DFP]

CALL GetSymEigenValues_(mat=mat, eigenValues=w)

CALL OK(ALL(ABS(w - exact) .LE. tol), "eigenvalues")
CALL OK(w(1) .LE. w(5), "ascending order")

END PROGRAM main
