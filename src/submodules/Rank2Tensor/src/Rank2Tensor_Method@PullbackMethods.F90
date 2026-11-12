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
! date: 2021-03-17
! summary: Pull back of tensors and vectors
!
! Description:
!   Each index is mapped by a matrix M that depends on its variance:
!   `"CONTRAVAR"` uses $F^{-1}$, `"COVAR"` uses $F^{T}$ and `"NA"`
!   (or any other value) uses $I$. Then the pull back of a tensor is
!   $M_1 T M_2^{T}$ and of a vector is $M_1 v$.

SUBMODULE(Rank2Tensor_Method) PullbackMethods
USE GlobalData, ONLY: Eye3
USE InvUtility, ONLY: Util_Inv => Inv
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Pullback
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Pullback1
REAL(DFP) :: M1(3, 3), M2(3, 3)

M1 = PullbackMap(F%T, indx1)
M2 = PullbackMap(F%T, indx2)
ans%T = MATMUL(M1, MATMUL(T%T, TRANSPOSE(M2)))
ans%isSym = .FALSE.
END PROCEDURE Rank2Tensor_Pullback1

!----------------------------------------------------------------------------
!                                                                   Pullback
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Pullback2
ans = MATMUL(PullbackMap(F%T, indx1), vec)
END PROCEDURE Rank2Tensor_Pullback2

!----------------------------------------------------------------------------
!                                                                PullbackMap
!----------------------------------------------------------------------------

! Returns the map of one index for the pull back

PURE FUNCTION PullbackMap(F, indx) RESULT(ans)
  REAL(DFP), INTENT(IN) :: F(3, 3)
  CHARACTER(*), INTENT(IN) :: indx
  REAL(DFP) :: ans(3, 3)

  SELECT CASE (TRIM(ADJUSTL(indx)))
  CASE ("CONTRAVAR")
    CALL Util_Inv(invA=ans, A=F)
  CASE ("COVAR")
    ans = TRANSPOSE(F)
  CASE DEFAULT
    ans = Eye3
  END SELECT
END FUNCTION PullbackMap

END SUBMODULE PullbackMethods
