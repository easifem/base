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


!> Test: stress invariants
! summary: MeanStress, EquivalentStress, HaighWestergaard

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE GlobalData, ONLY: SineLode
USE BaseType, ONLY: Rank2Tensor_
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: MeanStress
USE Rank2Tensor_Method, ONLY: EquivalentStress
USE Rank2Tensor_Method, ONLY: HaighWestergaard
USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: pi = 4.0_DFP * ATAN(1.0_DFP)
REAL(DFP), PARAMETER :: s = 6.0_DFP
TYPE(Rank2Tensor_) :: A
REAL(DFP) :: a0(3, 3), hw(3)

a0 = 0.0_DFP
a0(1, 1) = s
A = Rank2Tensor(a0, .TRUE.)

CALL OK(ABS(MeanStress(A) - s / 3.0_DFP) .LT. tol, "mean stress")
CALL OK(ABS(EquivalentStress(A) - s) .LT. tol, "equivalent stress")

hw = HaighWestergaard(A)
CALL OK(ABS(hw(1) - s / SQRT(3.0_DFP)) .LT. tol, "xi = I1/sqrt(3)")
CALL OK(ABS(hw(2) - SQRT(2.0_DFP / 3.0_DFP) * s) .LT. tol, &
        "rho = sqrt(2 J2)")
CALL OK(ABS(hw(3)) .LT. 1.0E-6_DFP, "theta (cos) = 0")

hw = HaighWestergaard(A, SineLode)
CALL OK(ABS(hw(3) + pi / 6.0_DFP) .LT. 1.0E-6_DFP, "theta (sin)")

! Hydrostatic stress has zero equivalent stress
A = Rank2Tensor(3.0_DFP * Eye3, .TRUE.)
CALL OK(ABS(EquivalentStress(A)) .LT. tol, "hydrostatic: sigma_bar = 0")

END PROGRAM main
