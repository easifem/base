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

!> Test: SetTriDiag with vectors of REAL(DFP)
! summary: Sets the main (d), super (da) and sub (db) diagonals and leaves
! all other entries unchanged.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE DiagUtility, ONLY: SetTriDiag
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 5
REAL(DFP), PARAMETER :: base = 7.0_DFP
REAL(DFP) :: mat(n, n), expected(n, n)
REAL(DFP) :: d(n), da(n - 1), db(n - 1)
INTEGER(I4B) :: ii

DO ii = 1, n
  d(ii) = REAL(10 + ii, DFP)
END DO
DO ii = 1, n - 1
  da(ii) = REAL(20 + ii, DFP)
  db(ii) = REAL(30 + ii, DFP)
END DO

mat = base
CALL SetTriDiag(mat=mat, d=d, da=da, db=db)

expected = base
DO ii = 1, n
  expected(ii, ii) = d(ii)
END DO
DO ii = 1, n - 1
  expected(ii, ii + 1) = da(ii)
  expected(ii + 1, ii) = db(ii)
END DO

CALL OK(ALL(mat .EQ. expected), "SetTriDiag values")

! entries away from the three diagonals are untouched
CALL OK(mat(1, 3) .EQ. base .AND. mat(3, 1) .EQ. base &
        .AND. mat(1, 5) .EQ. base .AND. mat(5, 1) .EQ. base, &
        "SetTriDiag untouched entries")

! the super-diagonal goes to (i, i+1), the sub-diagonal to (i+1, i)
CALL OK(mat(1, 2) .EQ. 21.0_DFP .AND. mat(2, 1) .EQ. 31.0_DFP, &
        "SetTriDiag da is above, db is below")

END PROGRAM main
