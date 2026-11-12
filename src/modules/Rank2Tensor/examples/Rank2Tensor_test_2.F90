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


!> Test: Contraction of Rank2Tensor and VoigtRank2Tensor
! summary: all four combinations equal SUM(A*B)

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: StressTypeVoigt
USE GlobalData, ONLY: StrainTypeVoigt
USE BaseType, ONLY: Rank2Tensor_
USE BaseType, ONLY: VoigtRank2Tensor_
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: Contraction
USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(Rank2Tensor_) :: A, B
TYPE(VoigtRank2Tensor_) :: vA, vB
REAL(DFP) :: a0(3, 3), b0(3, 3), expected
INTEGER(I4B) :: ta, tb, types(2)

a0 = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, &
              2.0_DFP, 4.0_DFP, 5.0_DFP, &
              3.0_DFP, 5.0_DFP, 6.0_DFP], [3, 3])
b0 = RESHAPE([-1.0_DFP, 0.5_DFP, 2.0_DFP, &
              0.5_DFP, 3.0_DFP, -4.0_DFP, &
              2.0_DFP, -4.0_DFP, 7.0_DFP], [3, 3])
expected = SUM(a0 * b0)
types = [StressTypeVoigt, StrainTypeVoigt]

A = Rank2Tensor(a0, .TRUE.)
B = Rank2Tensor(b0, .TRUE.)
CALL OK(ABS(Contraction(A, B) - expected) .LT. tol, "r2 : r2")

DO tb = 1, 2
  vB = VoigtRank2Tensor(b0, types(tb))
  CALL OK(ABS(Contraction(A, vB) - expected) .LT. tol, "r2 : voigt")
  CALL OK(ABS(Contraction(vB, A) - expected) .LT. tol, "voigt : r2")
  DO ta = 1, 2
    vA = VoigtRank2Tensor(a0, types(ta))
    CALL OK(ABS(Contraction(vA, vB) - expected) .LT. tol, &
            "voigt : voigt")
  END DO
END DO

! Non-symmetric rank-2 with a symmetric Voigt tensor
a0(1, 2) = 10.0_DFP
A = Rank2Tensor(a0)
vB = VoigtRank2Tensor(b0, StrainTypeVoigt)
CALL OK(ABS(Contraction(A, vB) - SUM(a0 * b0)) .LT. tol, &
        "non-symmetric r2 : voigt")

END PROGRAM main
