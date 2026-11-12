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


!> Test: VoigtRank2Tensor stress-like round trip
! summary: T -> Voigt (StressTypeVoigt) -> T, shear entries are T_ij

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StressTypeVoigt
USE BaseType, ONLY: VoigtRank2Tensor_
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
USE VoigtRank2Tensor_Method, ONLY: Display
USE VoigtRank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(VoigtRank2Tensor_) :: obj
REAL(DFP) :: T(3, 3), T2(3, 3), vec(6)

T = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
             4.0_DFP, 2.0_DFP, 5.0_DFP, &
             6.0_DFP, 5.0_DFP, 3.0_DFP], [3, 3])

obj = VoigtRank2Tensor(T, StressTypeVoigt)
CALL Display(obj, "obj: ")

vec = obj
CALL OK(MAXVAL(ABS(vec - [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
                          5.0_DFP, 6.0_DFP])) .LT. tol, &
        "stress: Voigt vector")
CALL OK(ABS(obj%scale - 1.0_DFP) .LT. tol, "stress: scale")

T2 = obj
CALL OK(MAXVAL(ABS(T2 - T)) .LT. tol, "stress: round trip")

END PROGRAM main
