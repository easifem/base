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

!> Test: Contraction_r2_r1
! summary: ans(i) = sum_j a1(i,j) a2(j)

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ContractionUtility, ONLY: Contraction
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n1 = 3, n2 = 4
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: a1(n1, n2), a2(n2)
REAL(DFP) :: ans(n1), expected(n1), exact(2)
INTEGER(I4B) :: ii, jj

! hand computed case
exact = Contraction(RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
                             5.0_DFP, 6.0_DFP], [2, 3]), &
                    [1.0_DFP, 0.0_DFP, -1.0_DFP])
! [1 3 5; 2 4 6] * [1; 0; -1] = [-4; -4]
CALL OK(ALL(ABS(exact + 4.0_DFP) .LT. tol), "Contraction_r2_r1 exact")

a1 = RESHAPE([(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, &
               ii=1, SIZE(a1))], SHAPE(a1))
a2 = RESHAPE([(REAL(MOD(5 * ii + 3, 11) - 5, DFP) / 2.0_DFP, &
               ii=1, SIZE(a2))], SHAPE(a2))

expected = 0.0_DFP
DO jj = 1, n2
  DO ii = 1, n1
    expected(ii) = expected(ii) + a1(ii, jj) * a2(jj)
  END DO
END DO

ans = Contraction(a1, a2)

CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "Contraction_r2_r1")
END PROGRAM main
