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

!> Test: Eye (identity matrix of DFP reals, DataType is not given)
! summary: ans(i,j) = 1 if i == j, 0 otherwise

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE EyeUtility, ONLY: Eye
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 5
REAL(DFP) :: a(n, n), expected(n, n)
INTEGER(I4B) :: ii

expected = 0.0_DFP
DO ii = 1, n
  expected(ii, ii) = 1.0_DFP
END DO

a = Eye(n)

CALL OK(ALL(SHAPE(a) .EQ. [n, n]), "Eye(m) shape")
CALL OK(ALL(a .EQ. expected), "Eye(m) values")
CALL OK(SUM(a) .EQ. REAL(n, DFP), "Eye(m) sum is n")
CALL OK(KIND(a) .EQ. DFP, "Eye(m) kind")

END PROGRAM main
