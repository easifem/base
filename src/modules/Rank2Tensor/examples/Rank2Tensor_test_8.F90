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


!> Test: Pullback and PushForward
! summary: PushForward(Pullback(T)) = T for every index variance

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE BaseType, ONLY: Rank2Tensor_
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: Pullback
USE Rank2Tensor_Method, ONLY: PushForward
USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
CHARACTER(9), PARAMETER :: names(3) = ["CONTRAVAR", "COVAR    ", &
                                       "NA       "]
TYPE(Rank2Tensor_) :: F, S, P, Q
REAL(DFP) :: v(3), w(3)
INTEGER(I4B) :: ii, jj

F = Rank2Tensor(RESHAPE([1.2_DFP, 0.1_DFP, 0.0_DFP, &
                         0.3_DFP, 0.9_DFP, 0.2_DFP, &
                         0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3]))
S = Rank2Tensor(RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, &
                         4.0_DFP, 5.0_DFP, 6.0_DFP, &
                         7.0_DFP, 8.0_DFP, 10.0_DFP], [3, 3]))

DO ii = 1, 3
  DO jj = 1, 3
    P = Pullback(S, F, TRIM(names(ii)), TRIM(names(jj)))
    Q = PushForward(P, F, TRIM(names(ii)), TRIM(names(jj)))
    CALL OK(MAXVAL(ABS(Q%T - S%T)) .LT. tol, &
            "push(pull(T)) "//TRIM(names(ii))//" "//TRIM(names(jj)))
  END DO
  v = [1.0_DFP, -2.0_DFP, 0.5_DFP]
  w = PushForward(Pullback(v, F, TRIM(names(ii))), F, TRIM(names(ii)))
  CALL OK(MAXVAL(ABS(w - v)) .LT. tol, "vector "//TRIM(names(ii)))
END DO

! Contravariant pull back is F^{-1} S F^{-T}
P = Pullback(S, F, "CONTRAVAR", "CONTRAVAR")
Q = PushForward(P, F, "CONTRAVAR", "CONTRAVAR")
CALL OK(MAXVAL(ABS(MATMUL(F%T, MATMUL(P%T, TRANSPOSE(F%T))) - S%T)) &
        .LT. tol, "F P F^T = S")

END PROGRAM main
