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


!> Test: PolarDecomp and RotationPart
! summary: A = R U = V R, R proper orthogonal, U and V symmetric SPD

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Eye3
USE BaseType, ONLY: Rank2Tensor_
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: PolarDecomp
USE Rank2Tensor_Method, ONLY: RotationPart
USE Rank2Tensor_Method, ONLY: PrincipalValues
USE Rank2Tensor_Method, ONLY: Det
USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
TYPE(Rank2Tensor_) :: A, R, U, V, R2
REAL(DFP) :: a0(3, 3)
INTEGER(I4B) :: icase

DO icase = 1, 3
  SELECT CASE (icase)
  CASE (1)
    a0 = RESHAPE([1.0_DFP, -0.333_DFP, 0.959_DFP, &
                  0.495_DFP, 1.0_DFP, 0.0_DFP, &
                  0.5_DFP, -0.247_DFP, 1.5_DFP], [3, 3])
  CASE (2)
    a0 = RESHAPE([0.0_DFP, 2.0_DFP, 0.0_DFP, &
                  -1.0_DFP, 0.0_DFP, 0.0_DFP, &
                  0.0_DFP, 0.0_DFP, 3.0_DFP], [3, 3])
  CASE DEFAULT
    a0 = RESHAPE([-2.0_DFP, 0.1_DFP, 0.3_DFP, &
                  0.2_DFP, -1.0_DFP, 0.4_DFP, &
                  0.0_DFP, 0.5_DFP, 4.0_DFP], [3, 3])
  END SELECT

  A = Rank2Tensor(a0)
  CALL PolarDecomp(A, R, U, V)

  CALL OK(MAXVAL(ABS(MATMUL(TRANSPOSE(R%T), R%T) - Eye3)) .LT. tol, &
          "R^T R = I")
  CALL OK(ABS(Det(R) - 1.0_DFP) .LT. tol, "det R = 1")
  CALL OK(MAXVAL(ABS(MATMUL(R%T, U%T) - a0)) .LT. tol, "R U = A")
  CALL OK(MAXVAL(ABS(MATMUL(V%T, R%T) - a0)) .LT. tol, "V R = A")
  CALL OK(MAXVAL(ABS(U%T - TRANSPOSE(U%T))) .LT. tol, "U symmetric")
  CALL OK(MINVAL(PrincipalValues(U)) .GT. 0.0_DFP, "U positive")
  CALL OK(MINVAL(PrincipalValues(V)) .GT. 0.0_DFP, "V positive")
  R2 = RotationPart(A)
  CALL OK(MAXVAL(ABS(R2%T - R%T)) .LT. tol, "RotationPart")
END DO

END PROGRAM main
