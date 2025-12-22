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

MODULE STForceVector_Method
USE BaseType, ONLY: ElemShapeData_, STElemShapeData_, FEVariable_
USE BaseType, ONLY: FEVariableScalar_, FEVariableVector_, FEVariableMatrix_
USE GlobalData, ONLY: I4B, DFP, LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: STForceVector
PUBLIC :: STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector1(test) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_STForceVector1
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector1
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_1(test, ans, nrow, ncol)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_STForceVector_1
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_1
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_22(testSpace, testTime, ans, &
                                              nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: testSpace
    CLASS(ElemshapeData_), INTENT(IN) :: testTime
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_STForceVector_22
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_22
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector2(test, c, crank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableScalar_), INTENT(IN) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_STForceVector2
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector2
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_2(test, c, crank, ans, nrow, ncol)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableScalar_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_STForceVector_2
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_2
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_23( &
    testSpace, testTime, c, crank, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: testSpace
    CLASS(ElemshapeData_), INTENT(IN) :: testTime
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableScalar_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_STForceVector_23
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_23
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector3(test, c, crank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION obj_STForceVector3
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector3
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_3( &
    test, c, crank, ans, dim1, dim2, dim3)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE obj_STForceVector_3
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_3
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_24( &
    testSpace, testTime, c, crank, ans, &
    dim1, dim2, dim3)
    CLASS(ElemshapeData_), INTENT(IN) :: testSpace
    CLASS(ElemshapeData_), INTENT(IN) :: testTime
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE obj_STForceVector_24
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_24
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector4(test, c, crank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableMatrix_), INTENT(IN) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION obj_STForceVector4
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector4
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_4( &
    test, c, crank, ans, dim1, dim2, dim3, dim4)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableMatrix_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  END SUBROUTINE obj_STForceVector_4
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_4
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector5(test, c1, c1rank, c2, c2rank) &
    RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_STForceVector5
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector5
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_5( &
    test, c1, c1rank, c2, c2rank, ans, nrow, ncol)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_STForceVector_5
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_5
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector6(test, c1, c1rank, c2, c2rank) &
    RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION obj_STForceVector6
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector6
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_6( &
    test, c1, c1rank, c2, c2rank, ans, dim1, dim2, dim3)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE obj_STForceVector_6
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_6
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector7(test, c1, c1rank, c2, c2rank) &
    RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION obj_STForceVector7
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector7
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_7( &
    test, c1, c1rank, c2, c2rank, ans, dim1, dim2, dim3, dim4)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  END SUBROUTINE obj_STForceVector_7
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_7
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector8(test, term1) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_STForceVector8
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector8
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_8(test, term1, ans, nrow, ncol)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_STForceVector_8
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_8
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector9(test, term1, c, crank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableScalar_), INTENT(IN) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_STForceVector9
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector9
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_9( &
    test, term1, c, crank, ans, nrow, ncol)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableScalar_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_STForceVector_9
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_9
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector10(test, term1, c, crank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION obj_STForceVector10
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector10
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_10( &
    test, term1, c, crank, ans, dim1, dim2, dim3)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE obj_STForceVector_10
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_10
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector11(test, term1, c, crank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableMatrix_), INTENT(IN) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION obj_STForceVector11
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector11
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_11( &
    test, term1, c, crank, ans, dim1, dim2, dim3, dim4)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableMatrix_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  END SUBROUTINE obj_STForceVector_11
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_11
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector12( &
    test, term1, c1, c1rank, c2, c2rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_STForceVector12
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector12
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_12( &
    test, term1, c1, c1rank, c2, c2rank, ans, nrow, ncol)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_STForceVector_12
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_12
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector13( &
    test, term1, c1, c1rank, c2, c2rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION obj_STForceVector13
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector13
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_13( &
    test, term1, c1, c1rank, c2, c2rank, ans, dim1, dim2, dim3)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE obj_STForceVector_13
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_13
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector14( &
    test, term1, c1, c1rank, c2, c2rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION obj_STForceVector14
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector14
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_14( &
    test, term1, c1, c1rank, c2, c2rank, ans, dim1, dim2, dim3, dim4)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  END SUBROUTINE obj_STForceVector_14
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_14
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector15(test, projection, c, crank) &
    RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_STForceVector15
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector15
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Space time force vector
!

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_15( &
    test, projection, c, crank, ans, nrow, ncol, temp)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), INTENT(INOUT) :: temp(:, :)
    !! temp array to keep projection data at ips and ipt
    !! size should be at least (nns x nnt)
  END SUBROUTINE obj_STForceVector_15
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_15
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector16( &
    test, projection, c1, c1rank, c2, c2rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_STForceVector16
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector16
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_16( &
    test, projection, c1, c1rank, c2, c2rank, ans, nrow, ncol, temp)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), INTENT(INOUT) :: temp(:, :)
    !! temp array to keep projection data at ips and ipt
    !! size should be at least (nns x nnt)
  END SUBROUTINE obj_STForceVector_16
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_16
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector17( &
    test, projection, c1, c1rank, c2, c2rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! projection is made on c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    !!
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION obj_STForceVector17
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector17
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_17( &
    test, projection, c1, c1rank, c2, c2rank, ans, dim1, dim2, dim3, temp)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! projection is made on c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    !! c2 force vector
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    REAL(DFP), INTENT(INOUT) :: temp(:, :)
  END SUBROUTINE obj_STForceVector_17
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_17
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector18( &
    test, projection, c1, c1rank, c2, c2rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION obj_STForceVector18
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector18
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_18( &
    test, projection, c1, c1rank, c2, c2rank, ans, dim1, dim2, dim3, dim4, &
    temp)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! projection vector
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
    REAL(DFP), INTENT(INOUT) :: temp(:, :)
  END SUBROUTINE obj_STForceVector_18
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_18
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector19( &
    test, projection, c1, c1rank, c2, c2rank, c3, c3rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariable_), INTENT(IN) :: c3
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c3rank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_STForceVector19
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector19
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_19( &
    test, projection, c1, c1rank, c2, c2rank, c3, c3rank, ans, nrow, ncol, &
    temp)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariable_), INTENT(IN) :: c3
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c3rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), INTENT(INOUT) :: temp(:, :)
  END SUBROUTINE obj_STForceVector_19
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_19
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector20( &
    test, projection, c1, c1rank, c2, c2rank, c3, c3rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariable_), INTENT(IN) :: c3
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    TYPE(FEVariableVector_), INTENT(IN) :: c3rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION obj_STForceVector20
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector20
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_20( &
    test, projection, c1, c1rank, c2, c2rank, c3, c3rank, ans, &
    dim1, dim2, dim3, temp)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariable_), INTENT(IN) :: c3
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    !! projection on c1
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    TYPE(FEVariableVector_), INTENT(IN) :: c3rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    REAL(DFP), INTENT(INOUT) :: temp(:, :)
  END SUBROUTINE obj_STForceVector_20
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_20
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION obj_STForceVector21( &
    test, projection, c1, c1rank, c2, c2rank, c3, c3rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariable_), INTENT(IN) :: c3
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    TYPE(FEVariableMatrix_), INTENT(IN) :: c3rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION obj_STForceVector21
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE obj_STForceVector21
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE SUBROUTINE obj_STForceVector_21( &
    test, projection, c1, c1rank, c2, c2rank, c3, c3rank, ans, dim1, dim2, &
    dim3, dim4, temp)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariable_), INTENT(IN) :: c3
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    TYPE(FEVariableMatrix_), INTENT(IN) :: c3rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
    REAL(DFP), INTENT(INOUT) :: temp(:, :)
  END SUBROUTINE obj_STForceVector_21
END INTERFACE

INTERFACE STForceVector_
  MODULE PROCEDURE obj_STForceVector_21
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE STForceVector_Method

