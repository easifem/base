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


!> Test: Exp and Otimes
! summary: exp(0) = I, exp of diagonal, exp(sA) exp(tA) = exp((s+t)A)

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Eye3
USE BaseType, ONLY: Rank2Tensor_
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: Exp
USE Rank2Tensor_Method, ONLY: Otimes
USE Rank2Tensor_Method, ONLY: OPERATOR(*)
USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
TYPE(Rank2Tensor_) :: A, E, E1, E2
REAL(DFP) :: a0(3, 3), expected(3, 3), series(3, 3), term(3, 3)
REAL(DFP) :: C4(3, 3, 3, 3)
INTEGER(I4B) :: k

A = Rank2Tensor(0.0_DFP * Eye3)
E = Exp(A)
CALL OK(MAXVAL(ABS(E%T - Eye3)) .LT. tol, "exp(0) = I")

a0 = 0.0_DFP
a0(1, 1) = 1.0_DFP
a0(2, 2) = -2.0_DFP
a0(3, 3) = 5.0_DFP
A = Rank2Tensor(a0, .TRUE.)
E = Exp(A)
expected = 0.0_DFP
expected(1, 1) = EXP(1.0_DFP)
expected(2, 2) = EXP(-2.0_DFP)
expected(3, 3) = EXP(5.0_DFP)
CALL OK(MAXVAL(ABS(E%T - expected) / MAXVAL(ABS(expected))) .LT. tol, &
        "exp of diagonal")

! Non-symmetric, compare with the Taylor series
a0 = RESHAPE([0.1_DFP, 0.2_DFP, -0.3_DFP, &
              0.4_DFP, -0.5_DFP, 0.6_DFP, &
              0.7_DFP, 0.0_DFP, 0.2_DFP], [3, 3])
A = Rank2Tensor(a0)
series = Eye3
term = Eye3
DO k = 1, 30
  term = MATMUL(term, a0) / REAL(k, DFP)
  series = series + term
END DO
E = Exp(A)
CALL OK(MAXVAL(ABS(E%T - series)) .LT. tol, "exp vs Taylor series")

! Large norm triggers scaling and squaring
E1 = Exp(Rank2Tensor(4.0_DFP * a0))
E2 = Exp(Rank2Tensor(6.0_DFP * a0))
E = Exp(Rank2Tensor(10.0_DFP * a0))
CALL OK(MAXVAL(ABS(MATMUL(E1%T, E2%T) - E%T)) &
        .LT. tol * MAXVAL(ABS(E%T)), "exp(4A) exp(6A) = exp(10A)")

! Otimes
A = Otimes([1.0_DFP, 2.0_DFP, 3.0_DFP], [4.0_DFP, 5.0_DFP, 6.0_DFP])
CALL OK(ABS(A%T(2, 3) - 12.0_DFP) .LT. tol .AND. &
        ABS(A%T(3, 1) - 12.0_DFP) .LT. tol, "Otimes vec vec")

E = Rank2Tensor(a0)
C4 = Otimes(A, E)
CALL OK(ABS(C4(2, 3, 1, 3) - A%T(2, 3) * a0(1, 3)) .LT. tol, &
        "Otimes r2 r2")

! * is element-wise
E = A * Rank2Tensor(a0)
CALL OK(MAXVAL(ABS(E%T - A%T * a0)) .LT. tol, "* is element-wise")

END PROGRAM main
