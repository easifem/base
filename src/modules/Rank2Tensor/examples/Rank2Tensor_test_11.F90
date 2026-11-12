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


!> Test: stress measure conversions
! summary: round trips, F = I, M = C S, P = F S, tau = J sigma

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Eye3
USE GlobalData, ONLY: StressMeasureCauchy
USE GlobalData, ONLY: StressMeasurePK2
USE GlobalData, ONLY: LGT
USE BaseType, ONLY: DeformationGradient_
USE BaseType, ONLY: Stress_
USE Rank2Tensor_Method, ONLY: Stress
USE Rank2Tensor_Method, ONLY: DeformationGradient
USE Rank2Tensor_Method, ONLY: ConvertStress
USE Rank2Tensor_Method, ONLY: CauchyStress
USE Rank2Tensor_Method, ONLY: KirchhoffStress
USE Rank2Tensor_Method, ONLY: PK1Stress
USE Rank2Tensor_Method, ONLY: PK2Stress
USE Rank2Tensor_Method, ONLY: MandelStress
USE Rank2Tensor_Method, ONLY: Jacobian
USE Rank2Tensor_Method, ONLY: GetStressType
USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
TYPE(DeformationGradient_) :: F, Id
TYPE(Stress_) :: sig, tau, P, S, M, X, Y, Z
REAL(DFP) :: s0(3, 3)
INTEGER(I4B) :: a, b
LOGICAL(LGT) :: isok

s0 = RESHAPE([10.0_DFP, 2.0_DFP, 3.0_DFP, &
              2.0_DFP, 20.0_DFP, 4.0_DFP, &
              3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
F = DeformationGradient(RESHAPE([1.2_DFP, 0.1_DFP, 0.0_DFP, &
                                 0.3_DFP, 0.9_DFP, 0.2_DFP, &
                                 0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3]))
Id = DeformationGradient(Eye3)
sig = Stress(s0, StressMeasureCauchy)

tau = KirchhoffStress(sig, F)
P = PK1Stress(sig, F)
S = PK2Stress(sig, F)
M = MandelStress(sig, F)

CALL OK(MAXVAL(ABS(tau%T - Jacobian(F) * s0)) .LT. tol, "tau = J sigma")
CALL OK(MAXVAL(ABS(P%T - MATMUL(F%T, S%T))) .LT. tol, "P = F S")
CALL OK(MAXVAL(ABS(M%T - MATMUL(MATMUL(TRANSPOSE(F%T), F%T), S%T))) &
        .LT. tol, "M = C S")
CALL OK(MAXVAL(ABS(M%T - MATMUL(TRANSPOSE(F%T), P%T))) .LT. tol, &
        "M = F^T P")
CALL OK(tau%isSym .AND. S%isSym .AND. .NOT. P%isSym .AND. &
        .NOT. M%isSym, "isSym of measures")
CALL OK(GetStressType(S) .EQ. StressMeasurePK2, "result stress type")

X = CauchyStress(S, F)
CALL OK(MAXVAL(ABS(X%T - s0)) .LT. tol .AND. X%isSym, "S -> sigma")

! All 5 x 5 round trips A -> B -> A
isok = .TRUE.
DO a = 1, 5
  X = ConvertStress(sig, F, a)
  DO b = 1, 5
    Y = ConvertStress(X, F, b)
    Z = ConvertStress(Y, F, a)
    isok = isok .AND. MAXVAL(ABS(Z%T - X%T)) .LT. tol * MAXVAL(ABS(X%T))
  END DO
END DO
CALL OK(isok, "all round trips")

! With F = I all measures are the same
isok = .TRUE.
DO a = 1, 5
  X = ConvertStress(sig, Id, a)
  isok = isok .AND. MAXVAL(ABS(X%T - s0)) .LT. tol
END DO
CALL OK(isok, "F = I gives equal measures")

END PROGRAM main
