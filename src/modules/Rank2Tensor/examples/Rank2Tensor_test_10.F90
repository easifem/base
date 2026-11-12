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


!> Test: Stress_ constructors and stress type
! summary: Stress(mat), Stress(Voigt), Get/SetStressType, assignment

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StressMeasureCauchy
USE GlobalData, ONLY: StressMeasureKirchhoff
USE GlobalData, ONLY: StressMeasurePK1
USE GlobalData, ONLY: StressMeasurePK2
USE GlobalData, ONLY: StressMeasureMandel
USE GlobalData, ONLY: StressTypeVoigt
USE BaseType, ONLY: Rank2Tensor_
USE BaseType, ONLY: Stress_
USE Rank2Tensor_Method, ONLY: Stress
USE Rank2Tensor_Method, ONLY: Stress_Pointer
USE Rank2Tensor_Method, ONLY: Initiate
USE Rank2Tensor_Method, ONLY: GetStressType
USE Rank2Tensor_Method, ONLY: SetStressType
USE Rank2Tensor_Method, ONLY: Display
USE Rank2Tensor_Method, ONLY: Trace
USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(Stress_) :: sig, sig2
CLASS(Stress_), POINTER :: psig
TYPE(Rank2Tensor_) :: A
REAL(DFP) :: s0(3, 3), p0(3, 3)

s0 = RESHAPE([10.0_DFP, 2.0_DFP, 3.0_DFP, &
              2.0_DFP, 20.0_DFP, 4.0_DFP, &
              3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
p0 = s0
p0(1, 2) = 7.0_DFP

sig = Stress(s0, StressMeasureCauchy)
CALL Display(sig, "sig: ")
CALL OK(GetStressType(sig) .EQ. StressMeasureCauchy, "stress type")
CALL OK(sig%isSym, "symmetric matrix gives isSym")
CALL OK(MAXVAL(ABS(sig%T - s0)) .LT. tol, "components")

sig = Stress(p0, StressMeasurePK1)
CALL OK(.NOT. sig%isSym, "non-symmetric PK1 is kept")
CALL OK(ABS(sig%T(1, 2) - 7.0_DFP) .LT. tol, "PK1 component kept")

! Assignment between Stress_ keeps the measure
sig2 = sig
CALL OK(GetStressType(sig2) .EQ. StressMeasurePK1, "= keeps stressType")

CALL SetStressType(sig2, StressMeasureMandel)
CALL OK(GetStressType(sig2) .EQ. StressMeasureMandel, "SetStressType")

sig = Stress(VoigtRank2Tensor(s0, StressTypeVoigt), StressMeasurePK2)
CALL OK(sig%isSym .AND. MAXVAL(ABS(sig%T - s0)) .LT. tol &
        .AND. GetStressType(sig) .EQ. StressMeasurePK2, "Stress(Voigt)")

A = sig
sig2 = Stress(A, StressMeasureKirchhoff)
CALL OK(MAXVAL(ABS(sig2%T - s0)) .LT. tol .AND. &
        GetStressType(sig2) .EQ. StressMeasureKirchhoff, "Stress(T)")

CALL Initiate(sig2, p0, StressMeasureCauchy)
CALL OK(.NOT. sig2%isSym .AND. GetStressType(sig2) &
        .EQ. StressMeasureCauchy, "Initiate(Stress_)")

! Rank2Tensor generics work on Stress_
CALL OK(ABS(Trace(sig2) - 60.0_DFP) .LT. tol, "Trace(Stress_)")

psig => Stress_Pointer(s0, StressMeasureKirchhoff)
CALL OK(GetStressType(psig) .EQ. StressMeasureKirchhoff, &
        "Stress_Pointer")
DEALLOCATE (psig)

END PROGRAM main
