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
! date: 2021-03-22
! summary: Methods for [[LeftCauchyGreen_]] and [[RightCauchyGreen_]]
!
! Ported from the old `LeftCauchyGreen_Class`, `RightCauchyGreen_Class`
! and `DeformationGradient/DeformationTensor.part`.

SUBMODULE(Rank2Tensor_Method) CauchyGreenMethods
USE GlobalData, ONLY: Eye3
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                            LeftCauchyGreen
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_LeftCauchyGreen
IF (PRESENT(F)) THEN
  ans%T = MATMUL(F%T, TRANSPOSE(F%T))
ELSE IF (PRESENT(V)) THEN
  ans%T = MATMUL(V%T, V%T)
ELSE
  ans%T = Eye3
END IF
ans%isSym = .TRUE.
END PROCEDURE Rank2Tensor_LeftCauchyGreen

!----------------------------------------------------------------------------
!                                                    LeftCauchyGreen_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_LeftCauchyGreenPtr
ALLOCATE (ans)
CALL Initiate(obj=ans, obj2=LeftCauchyGreen(F=F, V=V))
END PROCEDURE Rank2Tensor_LeftCauchyGreenPtr

!----------------------------------------------------------------------------
!                                                           RightCauchyGreen
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_RightCauchyGreen
IF (PRESENT(F)) THEN
  ans%T = MATMUL(TRANSPOSE(F%T), F%T)
ELSE IF (PRESENT(U)) THEN
  ans%T = MATMUL(U%T, U%T)
ELSE
  ans%T = Eye3
END IF
ans%isSym = .TRUE.
END PROCEDURE Rank2Tensor_RightCauchyGreen

!----------------------------------------------------------------------------
!                                                   RightCauchyGreen_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_RightCauchyGreenPtr
ALLOCATE (ans)
CALL Initiate(obj=ans, obj2=RightCauchyGreen(F=F, U=U))
END PROCEDURE Rank2Tensor_RightCauchyGreenPtr

END SUBMODULE CauchyGreenMethods
