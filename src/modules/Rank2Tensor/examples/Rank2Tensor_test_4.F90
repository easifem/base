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


!> Test: J2, J3 and Lode angle
! summary: uniaxial stress and pure shear

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE GlobalData, ONLY: SineLode
USE GlobalData, ONLY: CosineLode
USE BaseType, ONLY: Rank2Tensor_
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: J2
USE Rank2Tensor_Method, ONLY: J3
USE Rank2Tensor_Method, ONLY: LodeAngle
USE Rank2Tensor_Method, ONLY: Dev
USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: pi = 4.0_DFP * ATAN(1.0_DFP)
REAL(DFP), PARAMETER :: s = 3.0_DFP
TYPE(Rank2Tensor_) :: A
REAL(DFP) :: a0(3, 3)

! Uniaxial stress: J2 = s^2/3, J3 = 2 s^3/27
a0 = 0.0_DFP
a0(1, 1) = s
a0(2, 2) = 0.0_DFP
A = Rank2Tensor(a0 + 5.0_DFP * Eye3, .TRUE.)

CALL OK(ABS(J2(A) - s**2 / 3.0_DFP) .LT. tol, "J2 uniaxial")
CALL OK(ABS(J2(Dev(A), isDeviatoric=.TRUE.) - s**2 / 3.0_DFP) .LT. tol, &
        "J2 deviatoric branch")
CALL OK(ABS(J3(A) - 2.0_DFP * s**3 / 27.0_DFP) .LT. tol, "J3 uniaxial")
CALL OK(ABS(LodeAngle(A, CosineLode)) .LT. 1.0E-6_DFP, &
        "Lode (cos) uniaxial tension = 0")
CALL OK(ABS(LodeAngle(A, SineLode) + pi / 6.0_DFP) .LT. 1.0E-6_DFP, &
        "Lode (sin) uniaxial tension = -pi/6")

! Pure shear: J3 = 0, cos Lode angle = pi/6
a0 = 0.0_DFP
a0(1, 2) = s
a0(2, 1) = s
A = Rank2Tensor(a0, .TRUE.)
CALL OK(ABS(J2(A) - s**2) .LT. tol, "J2 shear")
CALL OK(ABS(J3(A)) .LT. tol, "J3 shear")
CALL OK(ABS(LodeAngle(A, CosineLode) - pi / 6.0_DFP) .LT. tol, &
        "Lode (cos) shear = pi/6")

! Zero deviator gives zero, not NaN
A = Rank2Tensor(2.0_DFP * Eye3, .TRUE.)
CALL OK(ABS(LodeAngle(A, CosineLode)) .LT. tol, "Lode of iso tensor")

END PROGRAM main
