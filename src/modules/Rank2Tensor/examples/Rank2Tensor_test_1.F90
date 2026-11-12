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


!> Test: Rank2Tensor constructors, assignment and Voigt conversion
! summary: Initiate, Rank2Tensor, =, Sym, SkewSym, Inv, isDeviatoric

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE GlobalData, ONLY: StressTypeVoigt
USE GlobalData, ONLY: StrainTypeVoigt
USE BaseType, ONLY: Rank2Tensor_
USE BaseType, ONLY: VoigtRank2Tensor_
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: Initiate
USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE Rank2Tensor_Method, ONLY: Sym
USE Rank2Tensor_Method, ONLY: SkewSym
USE Rank2Tensor_Method, ONLY: Inv
USE Rank2Tensor_Method, ONLY: isDeviatoric
USE Rank2Tensor_Method, ONLY: Dev
USE Rank2Tensor_Method, ONLY: Display
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(Rank2Tensor_) :: A, B, C
TYPE(VoigtRank2Tensor_) :: v
REAL(DFP) :: mat(3, 3), mat2(3, 3)

mat = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
               -1.0_DFP, 3.0_DFP, 0.25_DFP, &
               0.0_DFP, 1.5_DFP, 4.0_DFP], [3, 3])

A = mat
CALL Display(A, "A: ")
CALL OK(.NOT. A%isSym, "A = mat is not symmetric")
mat2 = A
CALL OK(MAXVAL(ABS(mat2 - mat)) .LT. tol, "mat = A")

B = Sym(A)
C = SkewSym(A)
CALL OK(B%isSym .AND. .NOT. C%isSym, "Sym/SkewSym flags")
CALL OK(MAXVAL(ABS(B%T + C%T - mat)) .LT. tol, "Sym + SkewSym = A")
CALL OK(MAXVAL(ABS(B%T - TRANSPOSE(B%T))) .LT. tol, "Sym is symmetric")
CALL OK(MAXVAL(ABS(C%T + TRANSPOSE(C%T))) .LT. tol, "SkewSym is skew")

CALL Inv(A, B)
CALL OK(MAXVAL(ABS(MATMUL(B%T, A%T) - Eye3)) .LT. tol, "Inv")

CALL OK(.NOT. isDeviatoric(A), "A is not deviatoric")
CALL OK(isDeviatoric(Dev(A)), "Dev(A) is deviatoric")
C = Dev(A)
CALL OK(isDeviatoric(Rank2Tensor(1.0E+6_DFP * C%T)), &
        "isDeviatoric is scale independent")
C = Sym(A)

! Rank2Tensor -> Voigt (stress) -> Rank2Tensor gives Sym(A)
v = A
B = v
CALL OK(B%isSym, "Rank2Tensor = Voigt is symmetric")
CALL OK(MAXVAL(ABS(B%T - C%T)) .LT. tol, "stress Voigt round trip")

CALL Initiate(v, A, StrainTypeVoigt)
B = Rank2Tensor(v)
CALL OK(MAXVAL(ABS(B%T - C%T)) .LT. tol, "strain Voigt round trip")

B = Rank2Tensor(VoigtRank2Tensor(C%T, StressTypeVoigt))
CALL OK(MAXVAL(ABS(B%T - C%T)) .LT. tol, "Rank2Tensor(Voigt)")

END PROGRAM main
