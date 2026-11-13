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

!> Test: SymEigenValues3by3
! summary: diagonal matrices with different diagonal entries
!
! The eigenvalues of a diagonal matrix are its diagonal entries, sorted
! in the ascending order.

PROGRAM main
USE GlobalData, ONLY: DFP
USE EigenUtility, ONLY: SymEigenValues3by3
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-9_DFP
REAL(DFP) :: mat(3, 3), w(3)

mat = 0.0_DFP
mat(1, 1) = 1.0_DFP
mat(2, 2) = 2.0_DFP
mat(3, 3) = 3.0_DFP
w = SymEigenValues3by3(mat)
CALL OK(ALL(ABS(w - [1.0_DFP, 2.0_DFP, 3.0_DFP]) .LE. tol), "diag(1,2,3)")

mat = 0.0_DFP
mat(1, 1) = -3.0_DFP
mat(2, 2) = 5.0_DFP
mat(3, 3) = 1.0_DFP
w = SymEigenValues3by3(mat)
CALL OK(ALL(ABS(w - [-3.0_DFP, 1.0_DFP, 5.0_DFP]) .LE. tol), &
        "diag(-3,5,1)")

END PROGRAM main
