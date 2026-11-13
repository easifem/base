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

!> Test: Convert, Convert_ and ConvertSafe with an unknown conversion
! summary: When conversion is neither DofToNodes nor NodesToDOF the
! permutation matrix is the identity, so the output is a copy of from.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: NONE
USE ConvertUtility, ONLY: Convert
USE ConvertUtility, ONLY: Convert_
USE ConvertUtility, ONLY: ConvertSafe
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
INTEGER(I4B), PARAMETER :: nns = 3, tdof = 2, m = nns * tdof
REAL(DFP) :: from(m, m), safe(m, m), big(m + 2, m + 2)
REAL(DFP), ALLOCATABLE :: to(:, :)
INTEGER(I4B) :: ii, nrow, ncol

from = RESHAPE([(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, &
                 ii=1, SIZE(from))], SHAPE(from))

CALL Convert(from=from, to=to, conversion=NONE, nns=nns, tdof=tdof)
CALL OK(ALL(SHAPE(to) .EQ. [m, m]), "Convert default shape")
CALL OK(MAXVAL(ABS(to - from)) .LT. tol, "Convert default is copy")

safe = 0.0_DFP
CALL ConvertSafe(from=from, to=safe, conversion=NONE, nns=nns, tdof=tdof)
CALL OK(MAXVAL(ABS(safe - from)) .LT. tol, "ConvertSafe default is copy")

big = 0.0_DFP
CALL Convert_(from=from, to=big, conversion=NONE, nns=nns, tdof=tdof, &
              nrow=nrow, ncol=ncol)
CALL OK(nrow .EQ. m .AND. ncol .EQ. m, "Convert_ default nrow, ncol")
CALL OK(MAXVAL(ABS(big(1:nrow, 1:ncol) - from)) .LT. tol, &
        "Convert_ default is copy")

END PROGRAM main
