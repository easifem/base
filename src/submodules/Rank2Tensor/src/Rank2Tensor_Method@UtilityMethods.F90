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

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: General tensor utilities for [[Rank2Tensor_]]
!
! Ported from the old `Tensor_Class` (`OperatorOverloading/Otimes`,
! `Tensor_PrincipalValue`, `Tensor_SpectralRadius`, `RotationPart`,
! `Tensor_Exp`).

SUBMODULE(Rank2Tensor_Method) UtilityMethods
USE GlobalData, ONLY: Eye3
USE InvUtility, ONLY: Util_Inv => Inv
IMPLICIT NONE

! Order of the diagonal Pade approximant used by Exp
INTEGER(I4B), PARAMETER :: padeOrder = 6

CONTAINS

!----------------------------------------------------------------------------
!                                                                     Otimes
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_OtimesVecVec
INTEGER(I4B) :: jj

DO jj = 1, 3
  ans%T(:, jj) = a * b(jj)
END DO
ans%isSym = .FALSE.
END PROCEDURE Rank2Tensor_OtimesVecVec

!----------------------------------------------------------------------------
!                                                                     Otimes
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_OtimesObjObj
INTEGER(I4B) :: kk, ll

DO ll = 1, 3
  DO kk = 1, 3
    ans(:, :, kk, ll) = obj1%T * obj2%T(kk, ll)
  END DO
END DO
END PROCEDURE Rank2Tensor_OtimesObjObj

!----------------------------------------------------------------------------
!                                                            PrincipalValues
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_PrincipalValues
REAL(DFP) :: QR(3, 3)

CALL Eigen(obj=obj, QR=QR, WR=ans)
CALL SortDescending(ans)
END PROCEDURE Rank2Tensor_PrincipalValues

!----------------------------------------------------------------------------
!                                                             SpectralRadius
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_SpectralRadius
REAL(DFP) :: QR(3, 3), WR(3), WI(3)

CALL Eigen(obj=obj, QR=QR, WR=WR, WI=WI)
ans = MAXVAL(SQRT(WR**2 + WI**2))
END PROCEDURE Rank2Tensor_SpectralRadius

!----------------------------------------------------------------------------
!                                                               RotationPart
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_RotationPart
TYPE(Rank2Tensor_) :: U, V

CALL PolarDecomp(obj=obj, R=ans, U=U, V=V)
END PROCEDURE Rank2Tensor_RotationPart

!----------------------------------------------------------------------------
!                                                                        Exp
!----------------------------------------------------------------------------

! Scaling and squaring with a diagonal Pade approximant, see
! C. Moler and C. Van Loan, "Nineteen dubious ways to compute the
! exponential of a matrix, twenty-five years later", SIAM Review 45
! (2003). This is the algorithm of MATLAB's original expm.

MODULE PROCEDURE Rank2Tensor_Exp
REAL(DFP) :: A(3, 3), X(3, 3), N(3, 3), D(3, 3), invD(3, 3), E(3, 3)
REAL(DFP) :: aNorm, c
INTEGER(I4B) :: s, k
LOGICAL(LGT) :: positive

! Scale A so that its infinity norm is at most 1/2
aNorm = MAXVAL(SUM(ABS(obj%T), DIM=2))
s = 0
IF (aNorm .GT. 0.0_DFP) s = MAX(0, EXPONENT(aNorm) + 1)
A = SCALE(obj%T, -s)

! Pade approximant N/D of exp(A)
c = 0.5_DFP
X = A
N = Eye3 + c * A
D = Eye3 - c * A
positive = .TRUE.

DO k = 2, padeOrder
  c = c * REAL(padeOrder - k + 1, DFP) &
      / REAL(k * (2 * padeOrder - k + 1), DFP)
  X = MATMUL(A, X)
  N = N + c * X
  IF (positive) THEN
    D = D + c * X
  ELSE
    D = D - c * X
  END IF
  positive = .NOT. positive
END DO

CALL Util_Inv(invA=invD, A=D)
E = MATMUL(invD, N)

! Undo the scaling by repeated squaring
DO k = 1, s
  E = MATMUL(E, E)
END DO

ans%T = E
ans%isSym = obj%isSym
IF (ans%isSym) ans%T = 0.5_DFP * (E + TRANSPOSE(E))
END PROCEDURE Rank2Tensor_Exp

!----------------------------------------------------------------------------
!                                                             SortDescending
!----------------------------------------------------------------------------

! Sorts three values in descending order

PURE SUBROUTINE SortDescending(v)
  REAL(DFP), INTENT(INOUT) :: v(3)
  !! values, sorted in descending order on return

  IF (v(1) .LT. v(2)) CALL Swap(v(1), v(2))
  IF (v(2) .LT. v(3)) CALL Swap(v(2), v(3))
  IF (v(1) .LT. v(2)) CALL Swap(v(1), v(2))
END SUBROUTINE SortDescending

!----------------------------------------------------------------------------
!                                                                       Swap
!----------------------------------------------------------------------------

! Swaps two values

PURE SUBROUTINE Swap(a, b)
  REAL(DFP), INTENT(INOUT) :: a
  !! first value, b on return
  REAL(DFP), INTENT(INOUT) :: b
  !! second value, a on return

  REAL(DFP) :: tmp

  tmp = a
  a = b
  b = tmp
END SUBROUTINE Swap

END SUBMODULE UtilityMethods
