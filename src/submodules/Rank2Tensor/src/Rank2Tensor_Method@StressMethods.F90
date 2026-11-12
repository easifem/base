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
! summary: Constructors and measure conversions for [[Stress_]]
!
! Description:
!   Ported from the old `Stress_Class` (`Constructor`, `StressType`,
!   `getStress`, `CauchyStress`, `Pk1Stress`, `Pk2Stress`,
!   `KirchhoffStress` and `EshelbyStress`). The old class stored a
!   symmetric Voigt vector, which lost the non-symmetric PK1 and Mandel
!   stresses. Here the full 3x3 components are kept.

SUBMODULE(Rank2Tensor_Method) StressMethods
USE GlobalData, ONLY: stdout
USE GlobalData, ONLY: StressMeasureCauchy
USE GlobalData, ONLY: StressMeasureKirchhoff
USE GlobalData, ONLY: StressMeasurePK1
USE GlobalData, ONLY: StressMeasurePK2
USE GlobalData, ONLY: StressMeasureMandel
USE InvUtility, ONLY: Util_Inv => Inv
USE ErrorHandling, ONLY: Errormsg
IMPLICIT NONE

! Relative tolerance used to detect a symmetric matrix
REAL(DFP), PARAMETER :: symTol = 1.0E-12_DFP

CONTAINS

!----------------------------------------------------------------------------
!                                                                     Stress
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Stress1
CALL Initiate(obj=ans, mat=mat, stressType=stressType)
END PROCEDURE Rank2Tensor_Stress1

!----------------------------------------------------------------------------
!                                                                     Stress
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Stress2
CALL Initiate(obj=ans, mat=T%T, stressType=stressType)
END PROCEDURE Rank2Tensor_Stress2

!----------------------------------------------------------------------------
!                                                                     Stress
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Stress3
CALL Initiate(obj=ans, V=V)
ans%stressType = stressType
END PROCEDURE Rank2Tensor_Stress3

!----------------------------------------------------------------------------
!                                                             Stress_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_StressPtr1
ALLOCATE (ans)
CALL Initiate(obj=ans, mat=mat, stressType=stressType)
END PROCEDURE Rank2Tensor_StressPtr1

!----------------------------------------------------------------------------
!                                                             Stress_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_StressPtr2
ALLOCATE (ans)
CALL Initiate(obj=ans, mat=T%T, stressType=stressType)
END PROCEDURE Rank2Tensor_StressPtr2

!----------------------------------------------------------------------------
!                                                             Stress_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_StressPtr3
ALLOCATE (ans)
CALL Initiate(obj=ans, V=V)
ans%stressType = stressType
END PROCEDURE Rank2Tensor_StressPtr3

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_InitiateStress
obj%T = mat
obj%isSym = IsSymmetricMat(mat)
obj%stressType = stressType
END PROCEDURE Rank2Tensor_InitiateStress

!----------------------------------------------------------------------------
!                                                              GetStressType
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_GetStressType
ans = obj%stressType
END PROCEDURE Rank2Tensor_GetStressType

!----------------------------------------------------------------------------
!                                                              SetStressType
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_SetStressType
obj%stressType = stressType
END PROCEDURE Rank2Tensor_SetStressType

!----------------------------------------------------------------------------
!                                                              ConvertStress
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_ConvertStress
CHARACTER(*), PARAMETER :: myName = "Rank2Tensor_ConvertStress()"
REAL(DFP) :: tau(3, 3), invF(3, 3), J

J = Det(F)
CALL Util_Inv(invA=invF, A=F%T)

! Kirchhoff stress from the input measure
SELECT CASE (obj%stressType)
CASE (StressMeasureCauchy)
  tau = J * obj%T
CASE (StressMeasureKirchhoff)
  tau = obj%T
CASE (StressMeasurePK1)
  tau = MATMUL(obj%T, TRANSPOSE(F%T))
CASE (StressMeasurePK2)
  tau = MATMUL(F%T, MATMUL(obj%T, TRANSPOSE(F%T)))
CASE (StressMeasureMandel)
  tau = MATMUL(TRANSPOSE(invF), MATMUL(obj%T, TRANSPOSE(F%T)))
CASE DEFAULT
  CALL Errormsg(msg="[ARG ERROR] :: unknown stress measure of obj", &
                file=__FILE__, routine=myName, line=__LINE__, &
                unitno=stdout)
  STOP
END SELECT

! Target measure from the Kirchhoff stress
SELECT CASE (stressType)
CASE (StressMeasureCauchy)
  ans%T = tau / J
CASE (StressMeasureKirchhoff)
  ans%T = tau
CASE (StressMeasurePK1)
  ans%T = MATMUL(tau, TRANSPOSE(invF))
CASE (StressMeasurePK2)
  ans%T = MATMUL(invF, MATMUL(tau, TRANSPOSE(invF)))
CASE (StressMeasureMandel)
  ans%T = MATMUL(TRANSPOSE(F%T), MATMUL(tau, TRANSPOSE(invF)))
CASE DEFAULT
  CALL Errormsg(msg="[ARG ERROR] :: unknown target stress measure", &
                file=__FILE__, routine=myName, line=__LINE__, &
                unitno=stdout)
  STOP
END SELECT

ans%stressType = stressType

SELECT CASE (stressType)
CASE (StressMeasurePK1, StressMeasureMandel)
  ans%isSym = .FALSE.
CASE DEFAULT
  ans%isSym = IsSymmetricMat(ans%T)
END SELECT
END PROCEDURE Rank2Tensor_ConvertStress

!----------------------------------------------------------------------------
!                                                               CauchyStress
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_CauchyStress
ans = ConvertStress(obj=obj, F=F, stressType=StressMeasureCauchy)
END PROCEDURE Rank2Tensor_CauchyStress

!----------------------------------------------------------------------------
!                                                            KirchhoffStress
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_KirchhoffStress
ans = ConvertStress(obj=obj, F=F, stressType=StressMeasureKirchhoff)
END PROCEDURE Rank2Tensor_KirchhoffStress

!----------------------------------------------------------------------------
!                                                                  PK1Stress
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_PK1Stress
ans = ConvertStress(obj=obj, F=F, stressType=StressMeasurePK1)
END PROCEDURE Rank2Tensor_PK1Stress

!----------------------------------------------------------------------------
!                                                                  PK2Stress
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_PK2Stress
ans = ConvertStress(obj=obj, F=F, stressType=StressMeasurePK2)
END PROCEDURE Rank2Tensor_PK2Stress

!----------------------------------------------------------------------------
!                                                               MandelStress
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_MandelStress
ans = ConvertStress(obj=obj, F=F, stressType=StressMeasureMandel)
END PROCEDURE Rank2Tensor_MandelStress

!----------------------------------------------------------------------------
!                                                             IsSymmetricMat
!----------------------------------------------------------------------------

! True if mat is symmetric within a relative tolerance

PURE FUNCTION IsSymmetricMat(mat) RESULT(ans)
  REAL(DFP), INTENT(IN) :: mat(3, 3)
  LOGICAL(LGT) :: ans

  ans = MAXVAL(ABS(mat - TRANSPOSE(mat))) .LE. symTol * MAXVAL(ABS(mat))
END FUNCTION IsSymmetricMat

END SUBMODULE StressMethods
