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
! summary: Methods for [[DeformationGradient_]]
!
! Description:
!   Ported from the old `DeformationGradient_Class` (`Constructor`,
!   `Components`: `.J.`, `.U.`, `.V.`, `.R.`, `.EigenValues.`).

SUBMODULE(Rank2Tensor_Method) DeformationGradientMethods
USE EigenUtility, ONLY: GetSymEigenJacobi
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: maxJacobiIter = 100

CONTAINS

!----------------------------------------------------------------------------
!                                                        DeformationGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_DeformationGradient1
IF (PRESENT(obj)) CALL Initiate(obj=ans, obj2=obj)
END PROCEDURE Rank2Tensor_DeformationGradient1

!----------------------------------------------------------------------------
!                                                        DeformationGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_DeformationGradient2
CALL Initiate(obj=ans, mat=mat)
END PROCEDURE Rank2Tensor_DeformationGradient2

!----------------------------------------------------------------------------
!                                                DeformationGradient_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_DeformationGradientPtr1
ALLOCATE (ans)
IF (PRESENT(obj)) CALL Initiate(obj=ans, obj2=obj)
END PROCEDURE Rank2Tensor_DeformationGradientPtr1

!----------------------------------------------------------------------------
!                                                DeformationGradient_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_DeformationGradientPtr2
ALLOCATE (ans)
CALL Initiate(obj=ans, mat=mat)
END PROCEDURE Rank2Tensor_DeformationGradientPtr2

!----------------------------------------------------------------------------
!                                                                   Jacobian
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Jacobian
ans = Det(F)
END PROCEDURE Rank2Tensor_Jacobian

!----------------------------------------------------------------------------
!                                                               RightStretch
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_RightStretch
TYPE(Rank2Tensor_) :: R, V

CALL PolarDecomp(obj=F, R=R, U=ans, V=V)
END PROCEDURE Rank2Tensor_RightStretch

!----------------------------------------------------------------------------
!                                                                LeftStretch
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_LeftStretch
TYPE(Rank2Tensor_) :: R, U

CALL PolarDecomp(obj=F, R=R, U=U, V=ans)
END PROCEDURE Rank2Tensor_LeftStretch

!----------------------------------------------------------------------------
!                                                                   Rotation
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Rotation
TYPE(Rank2Tensor_) :: U, V

CALL PolarDecomp(obj=F, R=ans, U=U, V=V)
END PROCEDURE Rank2Tensor_Rotation

!----------------------------------------------------------------------------
!                                                           PrincipalStretch
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_PrincipalStretch
REAL(DFP) :: C(3, 3), Q(3, 3), tmp
INTEGER(I4B) :: ii, jj

C = MATMUL(TRANSPOSE(F%T), F%T)
CALL GetSymEigenJacobi(mat=C, eigenValues=ans, eigenVectors=Q, &
                       maxIter=maxJacobiIter)
ans = SQRT(MAX(ans, 0.0_DFP))

DO ii = 1, 2
  DO jj = ii + 1, 3
    IF (ans(ii) .LT. ans(jj)) THEN
      tmp = ans(ii)
      ans(ii) = ans(jj)
      ans(jj) = tmp
    END IF
  END DO
END DO
END PROCEDURE Rank2Tensor_PrincipalStretch

END SUBMODULE DeformationGradientMethods
