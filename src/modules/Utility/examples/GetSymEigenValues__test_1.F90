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
! summary: eigenvalues of 1 by 1, 2 by 2 and 3 by 3 matrices
!
! The input matrix is not used after the call, because it can be
! overwritten.

PROGRAM main
USE GlobalData, ONLY: DFP
USE EigenUtility, ONLY: GetSymEigenValues_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-9_DFP
REAL(DFP) :: m1(1, 1), m2(2, 2), m3(3, 3)
REAL(DFP) :: w1(1), w2(2), w3(3), s

! 1 by 1
m1(1, 1) = -4.0_DFP
CALL GetSymEigenValues_(mat=m1, eigenValues=w1)
CALL OK(ABS(w1(1) + 4.0_DFP) .LE. tol, "1 by 1 matrix")

! 2 by 2
m2 = RESHAPE([2.5_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP], [2, 2])
CALL GetSymEigenValues_(mat=m2, eigenValues=w2)
CALL OK(ALL(ABS(w2 - [0.5_DFP, 3.0_DFP]) .LE. tol), "2 by 2 matrix")

! 3 by 3
s = SQRT(2.0_DFP)
m3 = RESHAPE([ &
             2.0_DFP, -1.0_DFP, 0.0_DFP, &
             -1.0_DFP, 2.0_DFP, -1.0_DFP, &
             0.0_DFP, -1.0_DFP, 2.0_DFP], [3, 3])
CALL GetSymEigenValues_(mat=m3, eigenValues=w3)
CALL OK(ALL(ABS(w3 - [2.0_DFP - s, 2.0_DFP, 2.0_DFP + s]) .LE. tol), &
        "3 by 3 matrix")

END PROGRAM main
