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

!> Test: Convert_ (rank-2), no allocation
! summary: The result is written only in to(1:nrow, 1:ncol) with
! nrow = ncol = nns*tdof. Entries outside that block must stay untouched.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DofToNodes
USE GlobalData, ONLY: NodesToDOF
USE ConvertUtility, ONLY: Convert
USE ConvertUtility, ONLY: Convert_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: sentinel = 99.0_DFP
INTEGER(I4B), PARAMETER :: nns = 4, tdof = 3, m = nns * tdof
INTEGER(I4B), PARAMETER :: ld = m + 3, lc = m + 5
REAL(DFP) :: from(m, m), to(ld, lc)
REAL(DFP), ALLOCATABLE :: expected(:, :)
INTEGER(I4B) :: ii, nrow, ncol

from = RESHAPE([(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, &
                 ii=1, SIZE(from))], SHAPE(from))

! DofToNodes
to = sentinel
nrow = -1
ncol = -1
CALL Convert_(from=from, to=to, conversion=DofToNodes, nns=nns, &
              tdof=tdof, nrow=nrow, ncol=ncol)
CALL Convert(from=from, to=expected, conversion=DofToNodes, nns=nns, &
             tdof=tdof)

CALL OK(nrow .EQ. m, "Convert_ rank2 nrow")
CALL OK(ncol .EQ. m, "Convert_ rank2 ncol")
CALL OK(MAXVAL(ABS(to(1:nrow, 1:ncol) - expected)) .LT. tol, &
        "Convert_ rank2 DofToNodes values")
CALL OK(ALL(to(nrow + 1:, :) .EQ. sentinel), &
        "Convert_ rank2 rows beyond nrow untouched")
CALL OK(ALL(to(:, ncol + 1:) .EQ. sentinel), &
        "Convert_ rank2 cols beyond ncol untouched")

! NodesToDOF
to = sentinel
CALL Convert_(from=from, to=to, conversion=NodesToDOF, nns=nns, &
              tdof=tdof, nrow=nrow, ncol=ncol)
CALL Convert(from=from, to=expected, conversion=NodesToDOF, nns=nns, &
             tdof=tdof)

CALL OK(MAXVAL(ABS(to(1:nrow, 1:ncol) - expected)) .LT. tol, &
        "Convert_ rank2 NodesToDOF values")

END PROGRAM main
