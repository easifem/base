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

!> Test: Contraction_r1_r2
! summary: ans(j) = sum_i a1(i) a2(i,j)

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ContractionUtility, ONLY: Contraction
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n1 = 3, n2 = 4
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: a1(n1), a2(n1, n2)
REAL(DFP) :: ans(n2), expected(n2)
INTEGER(I4B) :: ii, jj

a1 = [(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, ii=1, n1)]
a2 = RESHAPE([(REAL(MOD(5 * ii + 3, 11) - 5, DFP) / 2.0_DFP, &
               ii=1, SIZE(a2))], SHAPE(a2))

expected = 0.0_DFP
DO jj = 1, n2
  DO ii = 1, n1
    expected(jj) = expected(jj) + a1(ii) * a2(ii, jj)
  END DO
END DO

ans = Contraction(a1, a2)

CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "Contraction_r1_r2")
END PROGRAM main
