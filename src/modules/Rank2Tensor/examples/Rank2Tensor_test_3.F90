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


!> Test: Trace, Det, Invariants
! summary: tr(A^n), principal invariants and Cayley-Hamilton theorem

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Eye3
USE BaseType, ONLY: Rank2Tensor_
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: Trace
USE Rank2Tensor_Method, ONLY: Det
USE Rank2Tensor_Method, ONLY: Invariants
USE Rank2Tensor_Method, ONLY: J2
USE Rank2Tensor_Method, ONLY: Dev
USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
TYPE(Rank2Tensor_) :: A, S
REAL(DFP) :: a0(3, 3), P(3, 3), inv(3), CH(3, 3), tr
INTEGER(I4B) :: n

a0 = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
              -1.0_DFP, 3.0_DFP, 0.25_DFP, &
              0.0_DFP, 1.5_DFP, 4.0_DFP], [3, 3])
A = Rank2Tensor(a0)

CALL OK(ABS(Trace(A) - 9.0_DFP) .LT. tol, "Trace")
CALL OK(ABS(Trace(A, power=0) - 3.0_DFP) .LT. tol, "Trace power 0")

P = a0
DO n = 2, 5
  P = MATMUL(P, a0)
  tr = P(1, 1) + P(2, 2) + P(3, 3)
  CALL OK(ABS(Trace(A, power=n) - tr) .LT. tol * ABS(tr), &
          "Trace power n")
END DO

inv = Invariants(A)
P = MATMUL(a0, a0)
CALL OK(ABS(inv(1) - Trace(A)) .LT. tol, "I1")
CALL OK(ABS(inv(2) - 0.5_DFP * (inv(1)**2 - Trace(A, power=2))) &
        .LT. tol, "I2")
CALL OK(ABS(inv(3) - Det(A)) .LT. tol, "I3")

! Cayley-Hamilton: A^3 - I1 A^2 + I2 A - I3 I = 0
CH = MATMUL(P, a0) - inv(1) * P + inv(2) * a0 - inv(3) * Eye3
CALL OK(MAXVAL(ABS(CH)) .LT. tol, "Cayley-Hamilton")

! The deviatoric branch gives the same principal invariants, I2 = -J2
S = Dev(A)
inv = Invariants(S, isDeviatoric=.TRUE.)
CALL OK(MAXVAL(ABS(inv - Invariants(S))) .LT. tol, &
        "Invariants deviatoric branch")
CALL OK(ABS(inv(2) + J2(S, isDeviatoric=.TRUE.)) .LT. tol, "I2 = -J2")

END PROGRAM main
