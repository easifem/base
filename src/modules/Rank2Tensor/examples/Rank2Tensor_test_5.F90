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


!> Test: Eigen, PrincipalValues and SpectralRadius
! summary: A q = w q for symmetric and non-symmetric tensors

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE BaseType, ONLY: Rank2Tensor_
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: Eigen
USE Rank2Tensor_Method, ONLY: PrincipalValues
USE Rank2Tensor_Method, ONLY: SpectralRadius
USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
TYPE(Rank2Tensor_) :: A
REAL(DFP) :: a0(3, 3), QR(3, 3), QI(3, 3), WR(3), WI(3), pv(3)
COMPLEX(KIND(1.0_DFP)) :: q(3), r(3)
INTEGER(I4B) :: k

! Symmetric: eigenvalues 5, 10, -15 (see docblock of Eigen)
a0 = 0.0_DFP
a0(1, 1) = 5.0_DFP
a0(2:3, 2) = [-6.0_DFP, -12.0_DFP]
a0(2:3, 3) = [-12.0_DFP, 1.0_DFP]
A = Rank2Tensor(a0, .TRUE.)

CALL Eigen(A, QR, WR)
DO k = 1, 3
  CALL OK(MAXVAL(ABS(MATMUL(a0, QR(:, k)) - WR(k) * QR(:, k))) .LT. tol, &
          "symmetric: A q = w q")
END DO

pv = PrincipalValues(A)
CALL OK(MAXVAL(ABS(pv - [10.0_DFP, 5.0_DFP, -15.0_DFP])) .LT. tol, &
        "PrincipalValues sorted")
CALL OK(ABS(SpectralRadius(A) - 15.0_DFP) .LT. tol, "SpectralRadius")

! Non-symmetric with a complex pair: rotation about z by 90 deg, scaled
a0 = 0.0_DFP
a0(1, 2) = -2.0_DFP
a0(2, 1) = 2.0_DFP
a0(3, 3) = 1.0_DFP
A = Rank2Tensor(a0)

CALL Eigen(A, QR, WR, QI, WI)
DO k = 1, 3
  q = CMPLX(QR(:, k), QI(:, k), KIND(1.0_DFP))
  r = MATMUL(a0, q) - CMPLX(WR(k), WI(k), KIND(1.0_DFP)) * q
  CALL OK(MAXVAL(ABS(r)) .LT. tol, "non-symmetric: A q = w q")
END DO
CALL OK(ABS(SpectralRadius(A) - 2.0_DFP) .LT. tol, &
        "SpectralRadius complex")

END PROGRAM main
