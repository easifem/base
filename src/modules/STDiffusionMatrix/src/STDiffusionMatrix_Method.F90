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

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This module contains method to construct finite element matrices

MODULE STDiffusionMatrix_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: STDiffusionMatrix

!----------------------------------------------------------------------------
!                                                         STDiffusionMatrix
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-17
! update: 2021-12-17
! summary: Space-time diffusion matrix

INTERFACE
  MODULE PURE FUNCTION mat4_STDiffusionMatrix_1(test, trial, opt) &
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STDiffusionMatrix_1
END INTERFACE

INTERFACE STDiffusionMatrix
  MODULE PROCEDURE mat4_STDiffusionMatrix_1
END INTERFACE STDiffusionMatrix

!----------------------------------------------------------------------------
!                                                          STDiffusionMatrix
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-17
! update: 2021-12-17
! summary: Space-time diffusion matrix

INTERFACE
  MODULE PURE FUNCTION mat4_STDiffusionMatrix_2(test, trial, k, krank, opt) &
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    CLASS(FEVariable_), INTENT(IN) :: k
    TYPE(FEVariableScalar_), INTENT( IN ) :: krank
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STDiffusionMatrix_2
END INTERFACE

INTERFACE STDiffusionMatrix
  MODULE PROCEDURE mat4_STDiffusionMatrix_2
END INTERFACE STDiffusionMatrix

!----------------------------------------------------------------------------
!                                                          STDiffusionMatrix
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-17
! update: 2021-12-17
! summary: Space-time diffusion matrix

INTERFACE
  MODULE PURE FUNCTION mat4_STDiffusionMatrix_3(test, trial, k, krank, opt) &
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    CLASS(FEVariable_), INTENT(IN) :: k
    TYPE(FEVariableVector_), INTENT( IN ) :: krank
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STDiffusionMatrix_3
END INTERFACE

INTERFACE STDiffusionMatrix
  MODULE PROCEDURE mat4_STDiffusionMatrix_3
END INTERFACE STDiffusionMatrix

!----------------------------------------------------------------------------
!                                                          STDiffusionMatrix
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-17
! update: 2021-12-17
! summary: Space-time diffusion matrix

INTERFACE
  MODULE PURE FUNCTION mat4_STDiffusionMatrix_4(test, trial, k, krank, opt) &
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    CLASS(FEVariable_), INTENT(IN) :: k
    TYPE(FEVariableMatrix_), INTENT( IN ) :: krank
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STDiffusionMatrix_4
END INTERFACE

INTERFACE STDiffusionMatrix
  MODULE PROCEDURE mat4_STDiffusionMatrix_4
END INTERFACE STDiffusionMatrix

!----------------------------------------------------------------------------
!                                                          STDiffusionMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION mat4_STDiffusionMatrix_5(test, trial, c1, c2, &
    & c1rank, c2rank, opt) &
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    CLASS(FEVariable_), INTENT(IN) :: c1
    CLASS(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT( IN ) :: c1rank
    TYPE(FEVariableScalar_), INTENT( IN ) :: c2rank
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STDiffusionMatrix_5
END INTERFACE

INTERFACE STDiffusionMatrix
  MODULE PROCEDURE mat4_STDiffusionMatrix_5
END INTERFACE STDiffusionMatrix

!----------------------------------------------------------------------------
!                                                          STDiffusionMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION mat4_STDiffusionMatrix_6(test, trial, c1, c2, &
    & c1rank, c2rank, opt) &
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    CLASS(FEVariable_), INTENT(IN) :: c1
    CLASS(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT( IN ) :: c1rank
    TYPE(FEVariableVector_), INTENT( IN ) :: c2rank
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STDiffusionMatrix_6
END INTERFACE

INTERFACE STDiffusionMatrix
  MODULE PROCEDURE mat4_STDiffusionMatrix_6
END INTERFACE STDiffusionMatrix

!----------------------------------------------------------------------------
!                                                          STDiffusionMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION mat4_STDiffusionMatrix_7(test, trial, c1, c2, &
    & c1rank, c2rank, opt) &
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    CLASS(FEVariable_), INTENT(IN) :: c1
    CLASS(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT( IN ) :: c1rank
    TYPE(FEVariableMatrix_), INTENT( IN ) :: c2rank
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STDiffusionMatrix_7
END INTERFACE

INTERFACE STDiffusionMatrix
  MODULE PROCEDURE mat4_STDiffusionMatrix_7
END INTERFACE STDiffusionMatrix

!----------------------------------------------------------------------------
!                                                          STDiffusionMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION mat4_STDiffusionMatrix_8(test, trial, c1, c2, &
    & c1rank, c2rank, opt) &
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    CLASS(FEVariable_), INTENT(IN) :: c1
    CLASS(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableVector_), INTENT( IN ) :: c1rank
    TYPE(FEVariableScalar_), INTENT( IN ) :: c2rank
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STDiffusionMatrix_8
END INTERFACE

INTERFACE STDiffusionMatrix
  MODULE PROCEDURE mat4_STDiffusionMatrix_8
END INTERFACE STDiffusionMatrix

!----------------------------------------------------------------------------
!                                                          STDiffusionMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION mat4_STDiffusionMatrix_9(test, trial, c1, c2, &
    & c1rank, c2rank, opt) &
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    CLASS(FEVariable_), INTENT(IN) :: c1
    CLASS(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableVector_), INTENT( IN ) :: c1rank
    TYPE(FEVariableVector_), INTENT( IN ) :: c2rank
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STDiffusionMatrix_9
END INTERFACE

INTERFACE STDiffusionMatrix
  MODULE PROCEDURE mat4_STDiffusionMatrix_9
END INTERFACE STDiffusionMatrix

!----------------------------------------------------------------------------
!                                                          STDiffusionMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION mat4_STDiffusionMatrix_10(test, trial, c1, c2, &
    & c1rank, c2rank, opt) &
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    CLASS(FEVariable_), INTENT(IN) :: c1
    CLASS(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableVector_), INTENT( IN ) :: c1rank
    TYPE(FEVariableMatrix_), INTENT( IN ) :: c2rank
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STDiffusionMatrix_10
END INTERFACE

INTERFACE STDiffusionMatrix
  MODULE PROCEDURE mat4_STDiffusionMatrix_10
END INTERFACE STDiffusionMatrix

!----------------------------------------------------------------------------
!                                                          STDiffusionMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION mat4_STDiffusionMatrix_11(test, trial, c1, c2, &
    & c1rank, c2rank, opt) &
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    CLASS(FEVariable_), INTENT(IN) :: c1
    CLASS(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableMatrix_), INTENT( IN ) :: c1rank
    TYPE(FEVariableScalar_), INTENT( IN ) :: c2rank
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STDiffusionMatrix_11
END INTERFACE

INTERFACE STDiffusionMatrix
  MODULE PROCEDURE mat4_STDiffusionMatrix_11
END INTERFACE STDiffusionMatrix

!----------------------------------------------------------------------------
!                                                          STDiffusionMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION mat4_STDiffusionMatrix_12(test, trial, c1, c2, &
    & c1rank, c2rank, opt) &
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    CLASS(FEVariable_), INTENT(IN) :: c1
    CLASS(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableMatrix_), INTENT( IN ) :: c1rank
    TYPE(FEVariableVector_), INTENT( IN ) :: c2rank
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STDiffusionMatrix_12
END INTERFACE

INTERFACE STDiffusionMatrix
  MODULE PROCEDURE mat4_STDiffusionMatrix_12
END INTERFACE STDiffusionMatrix

!----------------------------------------------------------------------------
!                                                          STDiffusionMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION mat4_STDiffusionMatrix_13(test, trial, c1, c2, &
    & c1rank, c2rank, opt) &
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    CLASS(FEVariable_), INTENT(IN) :: c1
    CLASS(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableMatrix_), INTENT( IN ) :: c1rank
    TYPE(FEVariableMatrix_), INTENT( IN ) :: c2rank
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STDiffusionMatrix_13
END INTERFACE

INTERFACE STDiffusionMatrix
  MODULE PROCEDURE mat4_STDiffusionMatrix_13
END INTERFACE STDiffusionMatrix

!----------------------------------------------------------------------------
!                                                          STDiffusionMatrix
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-17
! update: 2021-12-17
! summary: Space-time diffusion matrix

INTERFACE
  MODULE PURE FUNCTION mat4_STDiffusionMatrix_14(test, trial, k, krank, &
    & opt) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    CLASS(FEVariable_), INTENT(IN) :: k
    TYPE(FEVariableScalar_), INTENT( IN ) :: krank
    !! scalar
    INTEGER( I4B ), INTENT( IN ) :: opt(1)
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STDiffusionMatrix_14
END INTERFACE

INTERFACE STDiffusionMatrix
  MODULE PROCEDURE mat4_STDiffusionMatrix_14
END INTERFACE STDiffusionMatrix

!----------------------------------------------------------------------------
!                                                          STDiffusionMatrix
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-17
! update: 2021-12-17
! summary: Space-time diffusion matrix

INTERFACE
  MODULE PURE FUNCTION mat4_STDiffusionMatrix_15(test, trial, k, krank, &
    & opt) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    CLASS(FEVariable_), INTENT(IN) :: k
    TYPE(FEVariableVector_), INTENT( IN ) :: krank
    !! Vector
    INTEGER( I4B ), INTENT( IN ) :: opt(1)
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STDiffusionMatrix_15
END INTERFACE

INTERFACE STDiffusionMatrix
  MODULE PROCEDURE mat4_STDiffusionMatrix_15
END INTERFACE STDiffusionMatrix

!----------------------------------------------------------------------------
!                                                          STDiffusionMatrix
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-17
! update: 2021-12-17
! summary: Space-time diffusion matrix

INTERFACE
  MODULE PURE FUNCTION mat4_STDiffusionMatrix_16(test, trial, c1, c2, &
    & c1rank, c2rank, opt) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    CLASS(FEVariable_), INTENT(IN) :: c1
    CLASS(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT( IN ) :: c1rank
    !! scalar
    TYPE(FEVariableScalar_), INTENT( IN ) :: c2rank
    !! scalar
    INTEGER( I4B ), INTENT( IN ) :: opt(1)
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STDiffusionMatrix_16
END INTERFACE

INTERFACE STDiffusionMatrix
  MODULE PROCEDURE mat4_STDiffusionMatrix_16
END INTERFACE STDiffusionMatrix

!----------------------------------------------------------------------------
!                                                          STDiffusionMatrix
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-17
! update: 2021-12-17
! summary: Space-time diffusion matrix

INTERFACE
  MODULE PURE FUNCTION mat4_STDiffusionMatrix_17(test, trial, c1, c2, &
    & c1rank, c2rank, opt) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    CLASS(FEVariable_), INTENT(IN) :: c1
    CLASS(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT( IN ) :: c1rank
    !! Scalar
    TYPE(FEVariableVector_), INTENT( IN ) :: c2rank
    !! Vector
    INTEGER( I4B ), INTENT( IN ) :: opt(1)
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION mat4_STDiffusionMatrix_17
END INTERFACE

INTERFACE STDiffusionMatrix
  MODULE PROCEDURE mat4_STDiffusionMatrix_17
END INTERFACE STDiffusionMatrix

!----------------------------------------------------------------------------
!                                                          DiffusionMatrix
!----------------------------------------------------------------------------

END MODULE STDiffusionMatrix_Method
