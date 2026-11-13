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

!> Test: Convert (rank-2) round trip
! summary: NodesToDOF followed by DofToNodes (and vice versa) must
! recover the original matrix, because the two maps are inverse
! permutations.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DofToNodes
USE GlobalData, ONLY: NodesToDOF
USE ConvertUtility, ONLY: Convert
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
INTEGER(I4B), PARAMETER :: nns = 4, tdof = 3, m = nns * tdof
REAL(DFP) :: from(m, m)
REAL(DFP), ALLOCATABLE :: mid(:, :), back(:, :)
INTEGER(I4B) :: ii

from = RESHAPE([(REAL(MOD(11 * ii, 17) - 8, DFP) / 3.0_DFP, &
                 ii=1, SIZE(from))], SHAPE(from))

CALL Convert(from=from, to=mid, conversion=NodesToDOF, nns=nns, tdof=tdof)
CALL Convert(from=mid, to=back, conversion=DofToNodes, nns=nns, tdof=tdof)

CALL OK(MAXVAL(ABS(back - from)) .LT. tol, "Convert_roundtrip N2D->D2N")
CALL OK(MAXVAL(ABS(mid - from)) .GT. tol, "Convert_intermediate differs")

CALL Convert(from=from, to=mid, conversion=DofToNodes, nns=nns, tdof=tdof)
CALL Convert(from=mid, to=back, conversion=NodesToDOF, nns=nns, tdof=tdof)

CALL OK(MAXVAL(ABS(back - from)) .LT. tol, "Convert_roundtrip D2N->N2D")

END PROGRAM main
