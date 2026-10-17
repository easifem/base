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

!> Test: Contraction_r1_r1
! summary: ans = sum_i a1(i) a2(i)

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ContractionUtility, ONLY: Contraction
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n1 = 6
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: a1(n1), a2(n1)
REAL(DFP) :: ans, expected
INTEGER(I4B) :: ii

! hand computed case: [1,2,3].[4,5,6] = 32
ans = Contraction([1.0_DFP, 2.0_DFP, 3.0_DFP], [4.0_DFP, 5.0_DFP, 6.0_DFP])
CALL OK(ABS(ans - 32.0_DFP) .LT. tol, "Contraction_r1_r1 exact")

a1 = [(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, ii=1, n1)]
a2 = [(REAL(MOD(5 * ii + 3, 11) - 5, DFP) / 2.0_DFP, ii=1, n1)]

expected = 0.0_DFP
DO ii = 1, n1
  expected = expected + a1(ii) * a2(ii)
END DO

ans = Contraction(a1, a2)

CALL OK(ABS(ans - expected) .LT. tol, "Contraction_r1_r1")
END PROGRAM main
