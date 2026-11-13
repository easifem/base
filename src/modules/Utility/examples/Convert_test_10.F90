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

!> Test: Convert_ (rank-6 to rank-4), no allocation
! summary: Same mapping as Convert, but written into a pre-allocated,
! larger array. dim1 = n1*n3, dim2 = n2*n4, dim3 = n5, dim4 = n6, and
! entries outside to(1:dim1, 1:dim2, 1:dim3, 1:dim4) stay untouched.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ConvertUtility, ONLY: Convert
USE ConvertUtility, ONLY: Convert_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: sentinel = 99.0_DFP
INTEGER(I4B), PARAMETER :: n1 = 2, n2 = 3, n3 = 2, n4 = 4, n5 = 3, n6 = 2
REAL(DFP) :: from(n1, n2, n3, n4, n5, n6)
REAL(DFP) :: to(n1 * n3 + 2, n2 * n4 + 3, n5 + 1, n6 + 2)
REAL(DFP), ALLOCATABLE :: expected(:, :, :, :)
INTEGER(I4B) :: ii, dim1, dim2, dim3, dim4

from = RESHAPE([(REAL(MOD(5 * ii + 2, 17) - 8, DFP) / 3.0_DFP, &
                 ii=1, SIZE(from))], SHAPE(from))

to = sentinel
dim1 = -1
dim2 = -1
dim3 = -1
dim4 = -1
CALL Convert_(from=from, to=to, dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4)
CALL Convert(from=from, to=expected)

CALL OK(dim1 .EQ. n1 * n3, "Convert_ rank6 dim1")
CALL OK(dim2 .EQ. n2 * n4, "Convert_ rank6 dim2")
CALL OK(dim3 .EQ. n5, "Convert_ rank6 dim3")
CALL OK(dim4 .EQ. n6, "Convert_ rank6 dim4")

CALL OK(MAXVAL(ABS(to(1:dim1, 1:dim2, 1:dim3, 1:dim4) - expected)) &
        .LT. tol, "Convert_ rank6 values")

CALL OK(ALL(to(dim1 + 1:, :, :, :) .EQ. sentinel), &
        "Convert_ rank6 dim1 padding untouched")
CALL OK(ALL(to(:, dim2 + 1:, :, :) .EQ. sentinel), &
        "Convert_ rank6 dim2 padding untouched")
CALL OK(ALL(to(:, :, dim3 + 1:, :) .EQ. sentinel), &
        "Convert_ rank6 dim3 padding untouched")
CALL OK(ALL(to(:, :, :, dim4 + 1:) .EQ. sentinel), &
        "Convert_ rank6 dim4 padding untouched")

END PROGRAM main
