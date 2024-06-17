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
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This module contains method to construct finite element matrices

MODULE StiffnessMatrix_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: StiffnessMatrix

!----------------------------------------------------------------------------
!                                     StiffnessMatrix@StiffnessMatrixMethods
!----------------------------------------------------------------------------

INTERFACE StiffnessMatrix
  MODULE PURE FUNCTION obj_StiffnessMatrix1(test, trial, Cijkl) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    CLASS(FEVariable_), INTENT(IN) :: Cijkl
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_StiffnessMatrix1
END INTERFACE StiffnessMatrix

!----------------------------------------------------------------------------
!                                     StiffnessMatrix@StiffnessMatrixMethods
!----------------------------------------------------------------------------

INTERFACE StiffnessMatrix
  MODULE PURE FUNCTION obj_StiffnessMatrix2(test, trial, lambda, mu,  &
    & isLambdaYoungsModulus) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    !! Shape function data
    CLASS(FEVariable_), INTENT(IN) :: lambda, mu
    !! Two elastic parameters
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isLambdaYoungsModulus
    !! if it is true then lambda is YoungsModulus
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_StiffnessMatrix2
END INTERFACE StiffnessMatrix

!----------------------------------------------------------------------------
!                                     StiffnessMatrix@StiffnessMatrixMethods
!----------------------------------------------------------------------------

INTERFACE StiffnessMatrix
  MODULE PURE FUNCTION obj_StiffnessMatrix3(test, trial, lambda,  &
    & mu) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: lambda, mu
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_StiffnessMatrix3
END INTERFACE StiffnessMatrix

!----------------------------------------------------------------------------
!                                     StiffnessMatrix@StiffnessMatrixMethods
!----------------------------------------------------------------------------

INTERFACE StiffnessMatrix
  MODULE PURE FUNCTION obj_StiffnessMatrix4(test, trial, Cijkl) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: Cijkl(:, :)
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_StiffnessMatrix4
END INTERFACE StiffnessMatrix

!----------------------------------------------------------------------------
!                                     StiffnessMatrix@StiffnessMatrixMethods
!----------------------------------------------------------------------------

INTERFACE StiffnessMatrix
  MODULE PURE FUNCTION obj_StiffnessMatrix5(test, trial, lambda, mu) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: lambda(:)
    REAL(DFP), INTENT(IN) :: mu(:)
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_StiffnessMatrix5
END INTERFACE StiffnessMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE StiffnessMatrix_Method
