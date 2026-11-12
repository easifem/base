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


!> Test: MaterialJacobian and rank-4 arrays
! summary: Initiate(C4) -> GetRank4, Otimes(I, I)

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Eye3
USE BaseType, ONLY: MaterialJacobian_
USE BaseType, ONLY: Rank2Tensor_
USE MaterialJacobian_Method, ONLY: MaterialJacobian
USE MaterialJacobian_Method, ONLY: GetRank4
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE MaterialJacobian_Method, ONLY: Rank4IdentityOtimesIdentity
USE MaterialJacobian_Method, ONLY: Rank4SymIdentity
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: Otimes
USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(MaterialJacobian_) :: obj, obj2
TYPE(Rank2Tensor_) :: I2
REAL(DFP) :: C4(3, 3, 3, 3), S4(3, 3, 3, 3)
INTEGER(I4B) :: i, j, k, l

! Rank-4 round trip of an isotropic tangent
obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP))
C4 = GetRank4(obj)
CALL OK(ABS(C4(1, 2, 2, 1) - 3.0_DFP) .LT. tol .AND. &
        ABS(C4(3, 3, 1, 1) - 2.0_DFP) .LT. tol, "GetRank4 entries")
obj2 = MaterialJacobian(C4)
CALL OK(MAXVAL(ABS(obj2%C - obj%C)) .LT. tol, "C4 round trip")

! Otimes(I, I) from Rank2Tensor
I2 = Rank2Tensor(Eye3, .TRUE.)
obj = MaterialJacobian(Otimes(I2, I2))
CALL OK(MAXVAL(ABS(obj%C - Rank4IdentityOtimesIdentity())) .LT. tol, &
        "Otimes(I, I) = I x I")

! Symmetric identity built from Kronecker deltas
DO l = 1, 3
  DO k = 1, 3
    DO j = 1, 3
      DO i = 1, 3
        S4(i, j, k, l) = 0.5_DFP * (Eye3(i, k) * Eye3(j, l) &
                                    + Eye3(i, l) * Eye3(j, k))
      END DO
    END DO
  END DO
END DO
obj = MaterialJacobian(S4)
CALL OK(MAXVAL(ABS(obj%C - Rank4SymIdentity())) .LT. tol, &
        "Isym from deltas")

! Only the minor-symmetric part of a general C4 is kept
S4(1, 2, 1, 1) = 1.0_DFP
S4(2, 1, 1, 1) = 0.0_DFP
obj = MaterialJacobian(S4)
CALL OK(ABS(obj%C(4, 1) - 0.5_DFP) .LT. tol, "minor-symmetric average")

END PROGRAM main
