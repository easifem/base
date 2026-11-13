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

!> Test: GetSymEigenValues
! summary: eigenvalues of 2 by 2 and 3 by 3 matrices, mat is preserved

PROGRAM main
USE GlobalData, ONLY: DFP
USE EigenUtility, ONLY: GetSymEigenValues
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-9_DFP
REAL(DFP) :: m2(2, 2), m2copy(2, 2), w2(2)
REAL(DFP) :: m3(3, 3), m3copy(3, 3), w3(3), s

! 2 by 2
m2 = RESHAPE([2.5_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP], [2, 2])
m2copy = m2
CALL GetSymEigenValues(mat=m2, eigenValues=w2)
CALL OK(ALL(ABS(w2 - [0.5_DFP, 3.0_DFP]) .LE. tol), "2 by 2 eigenvalues")
CALL OK(ALL(m2 .EQ. m2copy), "2 by 2 mat is not changed")

! 3 by 3
s = SQRT(2.0_DFP)
m3 = RESHAPE([ &
             2.0_DFP, -1.0_DFP, 0.0_DFP, &
             -1.0_DFP, 2.0_DFP, -1.0_DFP, &
             0.0_DFP, -1.0_DFP, 2.0_DFP], [3, 3])
m3copy = m3
CALL GetSymEigenValues(mat=m3, eigenValues=w3)
CALL OK(ALL(ABS(w3 - [2.0_DFP - s, 2.0_DFP, 2.0_DFP + s]) .LE. tol), &
        "3 by 3 eigenvalues")
CALL OK(ALL(m3 .EQ. m3copy), "3 by 3 mat is not changed")

END PROGRAM main
