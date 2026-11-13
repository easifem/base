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

!> Test: TriDiag with diagNo = 2
! summary: The super and sub diagonals are shifted to the second diagonals.
! ans(i,i) = d(i), ans(i,i+2) = da(i), ans(i+2,i) = db(i).

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE DiagUtility, ONLY: TriDiag
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 5, k = 2
REAL(DFP) :: d(n), da(n - k), db(n - k)
REAL(DFP) :: amat(n, n), expected(n, n)
INTEGER(I4B) :: ii

DO ii = 1, n
  d(ii) = REAL(ii, DFP)
END DO
DO ii = 1, n - k
  da(ii) = REAL(10 + ii, DFP)
  db(ii) = REAL(20 + ii, DFP)
END DO

expected = 0.0_DFP
DO ii = 1, n
  expected(ii, ii) = d(ii)
END DO
DO ii = 1, n - k
  expected(ii, ii + k) = da(ii)
  expected(ii + k, ii) = db(ii)
END DO

amat = TriDiag(d=d, da=da, db=db, diagNo=k)
CALL OK(ALL(amat .EQ. expected), "TriDiag diagNo = 2")

! hand check: first row is [1, 0, 11, 0, 0], first column is
! [1, 0, 21, 0, 0]
CALL OK(amat(1, 1) .EQ. 1.0_DFP .AND. amat(1, 2) .EQ. 0.0_DFP &
        .AND. amat(1, 3) .EQ. 11.0_DFP .AND. amat(3, 1) .EQ. 21.0_DFP &
        .AND. amat(2, 1) .EQ. 0.0_DFP, "TriDiag diagNo = 2, hand check")

! constant off-diagonals
amat = TriDiag(d=d, da=[1.0_DFP], db=[-1.0_DFP], diagNo=k)
expected = 0.0_DFP
DO ii = 1, n
  expected(ii, ii) = d(ii)
END DO
DO ii = 1, n - k
  expected(ii, ii + k) = 1.0_DFP
  expected(ii + k, ii) = -1.0_DFP
END DO
CALL OK(ALL(amat .EQ. expected), "TriDiag diagNo = 2, constant da, db")

END PROGRAM main
