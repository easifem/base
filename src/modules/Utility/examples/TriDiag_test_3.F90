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

!> Test: TriDiag, optional diagNo
! summary: The default diagNo is 1, and only ABS(diagNo) is used, so
! diagNo = -2 gives the same matrix as diagNo = 2.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE DiagUtility, ONLY: TriDiag
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 6
REAL(DFP) :: d(n), da(n), db(n)
REAL(DFP) :: a1(n, n), a2(n, n)
INTEGER(I4B) :: ii

DO ii = 1, n
  d(ii) = REAL(ii, DFP)
  da(ii) = REAL(10 + ii, DFP)
  db(ii) = REAL(20 + ii, DFP)
END DO

! da, db have n entries, only the first n - diagNo are used
a1 = TriDiag(d=d, da=da, db=db)
a2 = TriDiag(d=d, da=da, db=db, diagNo=1_I4B)
CALL OK(ALL(a1 .EQ. a2), "TriDiag default diagNo is 1")
CALL OK(a1(1, 2) .EQ. 11.0_DFP .AND. a1(2, 1) .EQ. 21.0_DFP, &
        "TriDiag default diagNo, hand check")

a1 = TriDiag(d=d, da=da, db=db, diagNo=2_I4B)
a2 = TriDiag(d=d, da=da, db=db, diagNo=-2_I4B)
CALL OK(ALL(a1 .EQ. a2), "TriDiag diagNo = -2 equals diagNo = 2")

END PROGRAM main
