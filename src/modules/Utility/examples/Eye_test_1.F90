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

!> Test: Eye (identity matrix of integers)
! summary: ans(i,j) = 1 if i == j, 0 otherwise

PROGRAM main
USE GlobalData, ONLY: I4B
USE EyeUtility, ONLY: Eye
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 5
INTEGER(I4B) :: a(n, n), expected(n, n)
INTEGER(I4B) :: ii

expected = 0_I4B
DO ii = 1, n
  expected(ii, ii) = 1_I4B
END DO

a = Eye(n, 1_I4B)

CALL OK(ALL(SHAPE(a) .EQ. [n, n]), "Eye(I4B) shape")
CALL OK(ALL(a .EQ. expected), "Eye(I4B) values")
CALL OK(SUM(a) .EQ. n, "Eye(I4B) sum is n")
CALL OK(KIND(a) .EQ. I4B, "Eye(I4B) kind")

END PROGRAM main
