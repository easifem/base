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
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_1(test) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION STForceVector_1
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_1
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_2(test, c, crank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    TYPE(FEVariable_), INTENT( IN ) :: c
    TYPE(FEVariableScalar_), INTENT( IN ) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION STForceVector_2
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_2
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_3(test, c, crank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    TYPE(FEVariable_), INTENT( IN ) :: c
    TYPE(FEVariableVector_), INTENT( IN ) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION STForceVector_3
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_3
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_4(test, c, crank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    TYPE(FEVariable_), INTENT( IN ) :: c
    TYPE(FEVariableMatrix_), INTENT( IN ) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION STForceVector_4
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_4
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_5(test, c1, c1rank, c2, c2rank) &
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    TYPE(FEVariable_), INTENT( IN ) :: c1
    TYPE(FEVariable_), INTENT( IN ) :: c2
    TYPE(FEVariableScalar_), INTENT( IN ) :: c1rank
    TYPE(FEVariableScalar_), INTENT( IN ) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION STForceVector_5
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_5
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_6(test, c1, c1rank, c2, c2rank) &
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    TYPE(FEVariable_), INTENT( IN ) :: c1
    TYPE(FEVariable_), INTENT( IN ) :: c2
    TYPE(FEVariableScalar_), INTENT( IN ) :: c1rank
    TYPE(FEVariableVector_), INTENT( IN ) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION STForceVector_6
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_6
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_7(test, c1, c1rank, c2, c2rank) &
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    TYPE(FEVariable_), INTENT( IN ) :: c1
    TYPE(FEVariable_), INTENT( IN ) :: c2
    TYPE(FEVariableScalar_), INTENT( IN ) :: c1rank
    TYPE(FEVariableMatrix_), INTENT( IN ) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION STForceVector_7
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_7
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_8(test, term1) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    INTEGER( I4B ), INTENT( IN ) :: term1
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION STForceVector_8
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_8
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_9(test, term1, c, crank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    INTEGER( I4B ), INTENT( IN ) :: term1
    TYPE(FEVariable_), INTENT( IN ) :: c
    TYPE(FEVariableScalar_), INTENT( IN ) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION STForceVector_9
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_9
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_10(test, term1, c, crank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    INTEGER( I4B ), INTENT( IN ) :: term1
    TYPE(FEVariable_), INTENT( IN ) :: c
    TYPE(FEVariableVector_), INTENT( IN ) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION STForceVector_10
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_10
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_11(test, term1, c, crank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    INTEGER( I4B ), INTENT( IN ) :: term1
    TYPE(FEVariable_), INTENT( IN ) :: c
    TYPE(FEVariableMatrix_), INTENT( IN ) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION STForceVector_11
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_11
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_12(test, term1, c1, c1rank, c2, c2rank)&
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    INTEGER( I4B ), INTENT( IN ) :: term1
    TYPE(FEVariable_), INTENT( IN ) :: c1
    TYPE(FEVariable_), INTENT( IN ) :: c2
    TYPE(FEVariableScalar_), INTENT( IN ) :: c1rank
    TYPE(FEVariableScalar_), INTENT( IN ) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION STForceVector_12
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_12
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_13(test, term1, c1, c1rank, c2, c2rank)&
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    INTEGER( I4B ), INTENT( IN ) :: term1
    TYPE(FEVariable_), INTENT( IN ) :: c1
    TYPE(FEVariable_), INTENT( IN ) :: c2
    TYPE(FEVariableScalar_), INTENT( IN ) :: c1rank
    TYPE(FEVariableVector_), INTENT( IN ) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION STForceVector_13
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_13
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_14(test, term1, c1, c1rank, c2, c2rank)&
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    INTEGER( I4B ), INTENT( IN ) :: term1
    TYPE(FEVariable_), INTENT( IN ) :: c1
    TYPE(FEVariable_), INTENT( IN ) :: c2
    TYPE(FEVariableScalar_), INTENT( IN ) :: c1rank
    TYPE(FEVariableMatrix_), INTENT( IN ) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION STForceVector_14
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_14
END INTERFACE STForceVector


!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_15(test, projecton, c, crank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    CHARACTER( LEN = * ), INTENT( IN ) :: projecton
    TYPE(FEVariable_), INTENT( IN ) :: c
    TYPE(FEVariableVector_), INTENT( IN ) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION STForceVector_15
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_15
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_16(test, projecton, c1, c1rank, &
    & c2, c2rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    CHARACTER( LEN = * ), INTENT( IN ) :: projecton
    TYPE(FEVariable_), INTENT( IN ) :: c1
    TYPE(FEVariable_), INTENT( IN ) :: c2
    TYPE(FEVariableVector_), INTENT( IN ) :: c1rank
    TYPE(FEVariableScalar_), INTENT( IN ) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION STForceVector_16
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_16
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_17(test, projecton, &
    & c1, c1rank, c2, c2rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    CHARACTER( LEN = * ), INTENT( IN ) :: projecton
    TYPE(FEVariable_), INTENT( IN ) :: c1
    TYPE(FEVariable_), INTENT( IN ) :: c2
    TYPE(FEVariableVector_), INTENT( IN ) :: c1rank
    TYPE(FEVariableVector_), INTENT( IN ) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION STForceVector_17
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_17
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_18(test, projecton, &
    & c1, c1rank, c2, c2rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    CHARACTER( LEN = * ), INTENT( IN ) :: projecton
    TYPE(FEVariable_), INTENT( IN ) :: c1
    TYPE(FEVariable_), INTENT( IN ) :: c2
    TYPE(FEVariableVector_), INTENT( IN ) :: c1rank
    TYPE(FEVariableMatrix_), INTENT( IN ) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION STForceVector_18
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_18
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_19(test, projecton, &
    & c1, c1rank, c2, c2rank, c3, c3rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    CHARACTER( LEN = * ), INTENT( IN ) :: projecton
    TYPE(FEVariable_), INTENT( IN ) :: c1
    TYPE(FEVariable_), INTENT( IN ) :: c2
    TYPE(FEVariable_), INTENT( IN ) :: c3
    TYPE(FEVariableVector_), INTENT( IN ) :: c1rank
    TYPE(FEVariableScalar_), INTENT( IN ) :: c2rank
    TYPE(FEVariableScalar_), INTENT( IN ) :: c3rank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION STForceVector_19
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_19
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_20(test, projecton, c1, c1rank, c2, &
    & c2rank, c3, c3rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    CHARACTER( LEN = * ), INTENT( IN ) :: projecton
    TYPE(FEVariable_), INTENT( IN ) :: c1
    TYPE(FEVariable_), INTENT( IN ) :: c2
    TYPE(FEVariable_), INTENT( IN ) :: c3
    TYPE(FEVariableVector_), INTENT( IN ) :: c1rank
    TYPE(FEVariableScalar_), INTENT( IN ) :: c2rank
    TYPE(FEVariableVector_), INTENT( IN ) :: c3rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION STForceVector_20
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_20
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE PURE FUNCTION STForceVector_21(test, projecton, c1, c1rank, c2, &
    & c2rank, c3, c3rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test( : )
    CHARACTER( LEN = * ), INTENT( IN ) :: projecton
    TYPE(FEVariable_), INTENT( IN ) :: c1
    TYPE(FEVariable_), INTENT( IN ) :: c2
    TYPE(FEVariable_), INTENT( IN ) :: c3
    TYPE(FEVariableVector_), INTENT( IN ) :: c1rank
    TYPE(FEVariableScalar_), INTENT( IN ) :: c2rank
    TYPE(FEVariableMatrix_), INTENT( IN ) :: c3rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION STForceVector_21
END INTERFACE

INTERFACE STForceVector
  MODULE PROCEDURE STForceVector_21
END INTERFACE STForceVector

END MODULE STForceVector_Method