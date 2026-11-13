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

!> Test: SymEigenValues2by2
! summary: one parameter family with known eigenvalues
!
! The matrix [1+3e, 2e; 2e, 1] has the eigenvalues 1-e and 1+4e.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE EigenUtility, ONLY: SymEigenValues2by2
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: eList(3) = [0.5_DFP, 0.01_DFP, 1.0E-4_DFP]
REAL(DFP) :: mat(2, 2), w(2), exact(2), e
INTEGER(I4B) :: ii

DO ii = 1, SIZE(eList)
  e = eList(ii)
  mat(1, 1) = 1.0_DFP + 3.0_DFP * e
  mat(1, 2) = 2.0_DFP * e
  mat(2, 1) = 2.0_DFP * e
  mat(2, 2) = 1.0_DFP
  exact = [1.0_DFP - e, 1.0_DFP + 4.0_DFP * e]
  w = SymEigenValues2by2(mat)
  CALL OK(ALL(ABS(w - exact) .LE. tol), "family member")
END DO

END PROGRAM main
