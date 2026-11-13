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

!> Test: Eye (properties of the identity matrix)
! summary: I*A = A*I = A, I*x = x, and the sizes 1 and 0

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE EyeUtility, ONLY: Eye
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(3, 3), b(3, 3), x(3), y(3), c(1, 1), d(0, 0)

a = RESHAPE([ &
            1.5_DFP, -2.0_DFP, 0.25_DFP, &
            4.0_DFP, 3.5_DFP, -1.0_DFP, &
            0.5_DFP, 8.0_DFP, -6.25_DFP], [3, 3])
x = [2.0_DFP, -3.0_DFP, 0.5_DFP]

! left and right multiplication
b = MATMUL(Eye(3), a)
CALL OK(ALL(b .EQ. a), "I * A = A")

b = MATMUL(a, Eye(3))
CALL OK(ALL(b .EQ. a), "A * I = A")

! matrix-vector product
y = MATMUL(Eye(3), x)
CALL OK(ALL(y .EQ. x), "I * x = x")

! I * I = I
b = MATMUL(Eye(3), Eye(3))
CALL OK(ALL(b .EQ. Eye(3)), "I * I = I")

! size 1
c = Eye(1_I4B)
CALL OK(SIZE(c) .EQ. 1_I4B, "size 1: number of entries")
CALL OK(c(1, 1) .EQ. 1.0_DFP, "size 1: value")

! size 0, empty array
d = Eye(0_I4B)
CALL OK(SIZE(d) .EQ. 0, "size 0: empty matrix")

END PROGRAM main
