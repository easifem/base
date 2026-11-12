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


!> Test: reduced 2D Voigt forms
! summary: VoigtSize, GetVoigtVector, VoigtRank2Tensor(vec, type, form)

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StressTypeVoigt
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: VoigtForm3D
USE GlobalData, ONLY: VoigtFormPlaneStrain
USE GlobalData, ONLY: VoigtFormPlaneStress
USE GlobalData, ONLY: VoigtFormAxisymmetric
USE BaseType, ONLY: VoigtRank2Tensor_
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
USE VoigtRank2Tensor_Method, ONLY: VoigtSize
USE VoigtRank2Tensor_Method, ONLY: GetVoigtVector
USE VoigtRank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(VoigtRank2Tensor_) :: obj, obj2
REAL(DFP) :: v6(6), T(3, 3)
REAL(DFP), ALLOCATABLE :: v(:)

CALL OK(VoigtSize(VoigtForm3D) .EQ. 6 .AND. &
        VoigtSize(VoigtFormPlaneStrain) .EQ. 3 .AND. &
        VoigtSize(VoigtFormPlaneStress) .EQ. 3 .AND. &
        VoigtSize(VoigtFormAxisymmetric) .EQ. 4, "VoigtSize")

v6 = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
obj = VoigtRank2Tensor(v6, StressTypeVoigt)

v = GetVoigtVector(obj, VoigtForm3D)
CALL OK(MAXVAL(ABS(v - v6)) .LT. tol, "3D vector")
v = GetVoigtVector(obj, VoigtFormPlaneStrain)
CALL OK(SIZE(v) .EQ. 3 .AND. MAXVAL(ABS(v - [1.0_DFP, 2.0_DFP, &
        4.0_DFP])) .LT. tol, "plane strain vector [11,22,12]")
v = GetVoigtVector(obj, VoigtFormAxisymmetric)
CALL OK(SIZE(v) .EQ. 4 .AND. MAXVAL(ABS(v - [1.0_DFP, 2.0_DFP, &
        4.0_DFP, 3.0_DFP])) .LT. tol, "axisymmetric [11,22,12,33]")

! Round trip of a 2D tensor (out-of-plane entries zero)
obj2 = VoigtRank2Tensor([1.0_DFP, 2.0_DFP, 4.0_DFP, 3.0_DFP], &
                        StrainTypeVoigt, VoigtFormAxisymmetric)
v = GetVoigtVector(obj2, VoigtFormAxisymmetric)
CALL OK(MAXVAL(ABS(v - [1.0_DFP, 2.0_DFP, 4.0_DFP, 3.0_DFP])) .LT. tol, &
        "axisymmetric round trip")
T = obj2
CALL OK(ABS(T(1, 2) - 2.0_DFP) .LT. tol .AND. ABS(T(3, 3) - 3.0_DFP) &
        .LT. tol .AND. ABS(T(2, 3)) .LT. tol, "strain-like 2D to tensor")

obj2 = VoigtRank2Tensor([1.0_DFP, 2.0_DFP, 4.0_DFP], StressTypeVoigt, &
                        VoigtFormPlaneStress)
T = obj2
CALL OK(ABS(T(3, 3)) .LT. tol .AND. ABS(T(1, 2) - 4.0_DFP) .LT. tol, &
        "plane stress from vector")

END PROGRAM main
