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

!> Test: ConvertSafe (rank-2)
! summary: ConvertSafe writes into a pre-sized matrix. The result is
! compared with the permutation formula to(j1,j2) = from(p(j1),p(j2)).

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DofToNodes
USE GlobalData, ONLY: NodesToDOF
USE ConvertUtility, ONLY: ConvertSafe
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
INTEGER(I4B), PARAMETER :: nns = 3, tdof = 2, m = nns * tdof
INTEGER(I4B), PARAMETER :: pD2N(m) = [1, 4, 2, 5, 3, 6]
INTEGER(I4B), PARAMETER :: pN2D(m) = [1, 3, 5, 2, 4, 6]
REAL(DFP) :: from(m, m), to(m, m), expected(m, m)
INTEGER(I4B) :: ii, jj

DO jj = 1, m
  DO ii = 1, m
    from(ii, jj) = REAL(10 * ii + jj, DFP)
  END DO
END DO

! DofToNodes
DO jj = 1, m
  DO ii = 1, m
    expected(ii, jj) = from(pD2N(ii), pD2N(jj))
  END DO
END DO

to = 0.0_DFP
CALL ConvertSafe(from=from, to=to, conversion=DofToNodes, nns=nns, &
                 tdof=tdof)
CALL OK(MAXVAL(ABS(to - expected)) .LT. tol, "ConvertSafe DofToNodes")

! NodesToDOF
DO jj = 1, m
  DO ii = 1, m
    expected(ii, jj) = from(pN2D(ii), pN2D(jj))
  END DO
END DO

to = 0.0_DFP
CALL ConvertSafe(from=from, to=to, conversion=NodesToDOF, nns=nns, &
                 tdof=tdof)
CALL OK(MAXVAL(ABS(to - expected)) .LT. tol, "ConvertSafe NodesToDOF")

END PROGRAM main
