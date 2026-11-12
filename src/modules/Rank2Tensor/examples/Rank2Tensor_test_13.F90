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


!> Test: derivatives of stress invariants
! summary: compare with central finite differences

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Eye3
USE BaseType, ONLY: Rank2Tensor_
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: MeanStress
USE Rank2Tensor_Method, ONLY: EquivalentStress
USE Rank2Tensor_Method, ONLY: J2
USE Rank2Tensor_Method, ONLY: J3
USE Rank2Tensor_Method, ONLY: MeanStressDerivative
USE Rank2Tensor_Method, ONLY: EquivalentStressDerivative
USE Rank2Tensor_Method, ONLY: J2Derivative
USE Rank2Tensor_Method, ONLY: J3Derivative
USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
REAL(DFP), PARAMETER :: tol = 1.0E-6_DFP
TYPE(Rank2Tensor_) :: sig, sp, sm, D
REAL(DFP) :: s0(3, 3), E(3, 3), fd
INTEGER(I4B) :: k

s0 = RESHAPE([10.0_DFP, 2.0_DFP, 3.0_DFP, &
              2.0_DFP, -20.0_DFP, 4.0_DFP, &
              3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
E = RESHAPE([0.3_DFP, -0.2_DFP, 0.5_DFP, &
             -0.2_DFP, 0.7_DFP, 0.1_DFP, &
             0.5_DFP, 0.1_DFP, -0.4_DFP], [3, 3])

sig = Rank2Tensor(s0, .TRUE.)
sp = Rank2Tensor(s0 + h * E, .TRUE.)
sm = Rank2Tensor(s0 - h * E, .TRUE.)

DO k = 1, 4
  SELECT CASE (k)
  CASE (1)
    D = MeanStressDerivative(sig)
    fd = (MeanStress(sp) - MeanStress(sm)) / (2.0_DFP * h)
  CASE (2)
    D = J2Derivative(sig)
    fd = (J2(sp) - J2(sm)) / (2.0_DFP * h)
  CASE (3)
    D = J3Derivative(sig)
    fd = (J3(sp) - J3(sm)) / (2.0_DFP * h)
  CASE DEFAULT
    D = EquivalentStressDerivative(sig)
    fd = (EquivalentStress(sp) - EquivalentStress(sm)) / (2.0_DFP * h)
  END SELECT
  CALL OK(ABS(SUM(D%T * E) - fd) .LT. tol * MAX(1.0_DFP, ABS(fd)), &
          "derivative vs finite difference")
  CALL OK(D%isSym .AND. MAXVAL(ABS(D%T - TRANSPOSE(D%T))) .LT. tol, &
          "derivative is symmetric")
END DO

! Zero equivalent stress gives a zero derivative, not NaN
sig = Rank2Tensor(5.0_DFP * Eye3, .TRUE.)
D = EquivalentStressDerivative(sig)
CALL OK(MAXVAL(ABS(D%T)) .LE. 0.0_DFP, "zero-stress guard")

END PROGRAM main
