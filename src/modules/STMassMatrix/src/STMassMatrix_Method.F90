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

MODULE STMassMatrix_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: STMassMatrix

!----------------------------------------------------------------------------
!                                              MassMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine makes mass matrix in space domain

INTERFACE STMassMatrix
  MODULE PURE FUNCTION mat4_STMassMatrix_1(test, trial, term1, term2, opt) &
    & RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    INTEGER(I4B), INTENT(IN) :: term1
    !! del_t, del_none
    INTEGER(I4B), INTENT(IN) :: term2
    !! del_t, del_none
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! ncopy
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STMassMatrix_1
END INTERFACE STMassMatrix

!----------------------------------------------------------------------------
!                                              MassMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine makes mass matrix in space domain

INTERFACE STMassMatrix
  MODULE PURE FUNCTION mat4_STMassMatrix_2(test, trial, term1, term2, &
    & rho, rhorank, opt) RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    INTEGER(I4B), INTENT(IN) :: term1
    !! del_t, del_none
    INTEGER(I4B), INTENT(IN) :: term2
    !! del_t, del_none
    CLASS(FEVariable_), INTENT(IN) :: rho
    TYPE(FEVariableScalar_), INTENT(IN) :: rhorank
    !! Scalar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! ncopy
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STMassMatrix_2
END INTERFACE STMassMatrix

!----------------------------------------------------------------------------
!                                              MassMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine makes mass matrix in space domain

INTERFACE STMassMatrix
  MODULE PURE FUNCTION mat4_STMassMatrix_3(test, trial, term1, term2, &
    & rho, rhorank, opt) RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    INTEGER(I4B), INTENT(IN) :: term1
    !! del_t, del_none
    INTEGER(I4B), INTENT(IN) :: term2
    !! del_t, del_none
    CLASS(FEVariable_), INTENT(IN) :: rho
    TYPE(FEVariableVector_), INTENT(IN) :: rhorank
    !! Vector
    INTEGER(I4B), INTENT(IN) :: opt
    !! 1, 2, 3, 4
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STMassMatrix_3
END INTERFACE STMassMatrix

!----------------------------------------------------------------------------
!                                              MassMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine makes mass matrix in space domain

INTERFACE STMassMatrix
  MODULE PURE FUNCTION mat4_STMassMatrix_4(test, trial, term1, term2, &
    & rho, rhorank) RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    INTEGER(I4B), INTENT(IN) :: term1
    !! del_t, del_none
    INTEGER(I4B), INTENT(IN) :: term2
    !! del_t, del_none
    CLASS(FEVariable_), INTENT(IN) :: rho
    TYPE(FEVariableMatrix_), INTENT(IN) :: rhorank
    !! Matrix
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STMassMatrix_4
END INTERFACE STMassMatrix

!----------------------------------------------------------------------------
!                                              MassMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine makes mass matrix in space domain

INTERFACE STMassMatrix
  MODULE PURE FUNCTION mat4_STMassMatrix_5(test, trial, term1, term2, &
      & c1, c1rank, c2, c2rank, opt) RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    INTEGER(I4B), INTENT(IN) :: term1
    !! del_t, del_none
    INTEGER(I4B), INTENT(IN) :: term2
    !! del_t, del_none
    CLASS(FEVariable_), INTENT(IN) :: c1
    CLASS(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    !! Scalar
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    !! Scalar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! ncopy
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STMassMatrix_5
END INTERFACE STMassMatrix

!----------------------------------------------------------------------------
!                                              MassMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine makes mass matrix in space domain

INTERFACE STMassMatrix
  MODULE PURE FUNCTION mat4_STMassMatrix_6(test, trial, term1, term2, &
      & c1, c1rank, c2, c2rank, opt) RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    INTEGER(I4B), INTENT(IN) :: term1
    !! del_t, del_none
    INTEGER(I4B), INTENT(IN) :: term2
    !! del_t, del_none
    CLASS(FEVariable_), INTENT(IN) :: c1
    CLASS(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    !! Scalar
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    !! Vector
    INTEGER(I4B), INTENT(IN) :: opt
    !! 1,2,3,4
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STMassMatrix_6
END INTERFACE STMassMatrix

!----------------------------------------------------------------------------
!                                              MassMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine makes mass matrix in space domain

INTERFACE STMassMatrix
  MODULE PURE FUNCTION mat4_STMassMatrix_7(test, trial, term1, term2, &
    & c1, c1rank, c2, c2rank) RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    INTEGER(I4B), INTENT(IN) :: term1
    !! del_t, del_none
    INTEGER(I4B), INTENT(IN) :: term2
    !! del_t, del_none
    CLASS(FEVariable_), INTENT(IN) :: c1
    CLASS(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    !! Scalar
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    !! Matrix
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STMassMatrix_7
END INTERFACE STMassMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE STMassMatrix_Method
