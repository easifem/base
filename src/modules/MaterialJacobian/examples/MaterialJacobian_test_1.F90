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


!> Test: MaterialJacobian constructors, tags and Cijkl
! summary: Initiate, MaterialJacobian, tags, assignment, Cijkl, Display

PROGRAM main
USE GlobalData, ONLY: DFP
USE BaseType, ONLY: MaterialJacobian_
USE GlobalData, ONLY: StressMeasureCauchy
USE GlobalData, ONLY: StressMeasurePK2
USE GlobalData, ONLY: StrainMeasureSmall
USE GlobalData, ONLY: StrainMeasureGreen
USE GlobalData, ONLY: StressRateNone
USE GlobalData, ONLY: StressRateJaumann
USE MaterialJacobian_Method, ONLY: MaterialJacobian
USE MaterialJacobian_Method, ONLY: MaterialJacobian_Pointer
USE MaterialJacobian_Method, ONLY: GetStressType
USE MaterialJacobian_Method, ONLY: GetStrainType
USE MaterialJacobian_Method, ONLY: GetRateType
USE MaterialJacobian_Method, ONLY: SetMeasureTypes
USE MaterialJacobian_Method, ONLY: Cijkl
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE MaterialJacobian_Method, ONLY: Display
USE MaterialJacobian_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(MaterialJacobian_) :: obj, obj2
CLASS(MaterialJacobian_), POINTER :: pobj
REAL(DFP) :: C(6, 6)

obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP))
CALL Display(obj, "obj: ")
CALL OK(GetStressType(obj) .EQ. StressMeasureCauchy .AND. &
        GetStrainType(obj) .EQ. StrainMeasureSmall .AND. &
        GetRateType(obj) .EQ. StressRateNone, "default tags")

CALL OK(ABS(Cijkl(obj, 1, 1, 1, 1) - 8.0_DFP) .LT. tol, "C1111")
CALL OK(ABS(Cijkl(obj, 1, 1, 2, 2) - 2.0_DFP) .LT. tol, "C1122")
CALL OK(ABS(Cijkl(obj, 1, 2, 1, 2) - 3.0_DFP) .LT. tol, "C1212 = mu")
CALL OK(ABS(Cijkl(obj, 2, 1, 1, 2) - Cijkl(obj, 1, 2, 2, 1)) .LT. tol, &
        "minor symmetry")

CALL SetMeasureTypes(obj, stressType=StressMeasurePK2, &
                     strainType=StrainMeasureGreen)
obj2 = obj
CALL OK(GetStressType(obj2) .EQ. StressMeasurePK2 .AND. &
        GetStrainType(obj2) .EQ. StrainMeasureGreen, "= keeps tags")

C = obj
CALL OK(MAXVAL(ABS(C - IsotropicTangent(2.0_DFP, 3.0_DFP))) .LT. tol, &
        "mat = obj")

pobj => MaterialJacobian_Pointer(C, rateType=StressRateJaumann)
CALL OK(GetRateType(pobj) .EQ. StressRateJaumann, "pointer and rate tag")
DEALLOCATE (pobj)

END PROGRAM main
