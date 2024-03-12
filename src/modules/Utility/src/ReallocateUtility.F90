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

MODULE ReallocateUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_logical(Mat, row)
    LOGICAL(LGT), ALLOCATABLE, INTENT(INOUT) :: Mat(:)
    INTEGER(I4B), INTENT(IN) :: row
  END SUBROUTINE Reallocate_logical
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R1(Mat, row)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Mat(:)
    INTEGER(I4B), INTENT(IN) :: row
  END SUBROUTINE Reallocate_Real64_R1
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R1b(Mat, s)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Mat(:)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Real64_R1b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R1(Mat, row)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Mat(:)
    INTEGER(I4B), INTENT(IN) :: row
  END SUBROUTINE Reallocate_Real32_R1
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R1b(Mat, s)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Mat(:)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Real32_R1b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R2(Mat, row, col)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :)
    INTEGER(I4B), INTENT(IN) :: row, col
  END SUBROUTINE Reallocate_Real64_R2
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R2b(Mat, s)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Real64_R2b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R2(Mat, row, col)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :)
    INTEGER(I4B), INTENT(IN) :: row, col
  END SUBROUTINE Reallocate_Real32_R2
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R2b(Mat, s)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Real32_R2b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R3(Mat, i1, i2, i3)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3
  END SUBROUTINE Reallocate_Real64_R3
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R3b(Mat, s)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Real64_R3b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R3(Mat, i1, i2, i3)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3
  END SUBROUTINE Reallocate_Real32_R3
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R3b(Mat, s)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Real32_R3b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R4(Mat, i1, i2, i3, i4)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4
  END SUBROUTINE Reallocate_Real64_R4
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R4b(Mat, s)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Real64_R4b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R4(Mat, i1, i2, i3, i4)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4
  END SUBROUTINE Reallocate_Real32_R4
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R4b(Mat, s)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Real32_R4b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R5(Mat, i1, i2, i3, i4, i5)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5
  END SUBROUTINE Reallocate_Real64_R5
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R5b(Mat, s)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Real64_R5b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R5(Mat, i1, i2, i3, i4, i5)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5
  END SUBROUTINE Reallocate_Real32_R5
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R5b(Mat, s)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Real32_R5b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R6(Mat, i1, i2, i3, i4, i5, i6)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6
  END SUBROUTINE Reallocate_Real64_R6
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R6b(Mat, s)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Real64_R6b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R6(Mat, i1, i2, i3, i4, i5, i6)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6
  END SUBROUTINE Reallocate_Real32_R6
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R6b(Mat, s)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Real32_R6b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R7(Mat, i1, i2, i3, i4, i5, &
    & i6, i7)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6, i7
  END SUBROUTINE Reallocate_Real64_R7
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R7b(Mat, s)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Real64_R7b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R7(Mat, i1, i2, i3, i4, i5, i6, i7)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6, i7
  END SUBROUTINE Reallocate_Real32_R7
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R7b(Mat, s)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Real32_R7b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R1(Mat, row)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: Mat(:)
    INTEGER(I4B), INTENT(IN) :: row
  END SUBROUTINE Reallocate_Int64_R1
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE Reallocate_Int64_R1b(Mat, s)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: Mat(:)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Int64_R1b
END INTERFACE

INTERFACE Reallocate
  MODULE PROCEDURE Reallocate_Int64_R1b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R1(Mat, row)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: Mat(:)
    INTEGER(I4B), INTENT(IN) :: row
  END SUBROUTINE Reallocate_Int32_R1
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R1b(Mat, s)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: Mat(:)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Int32_R1b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int16_R1(Mat, row)
    INTEGER(INT16), ALLOCATABLE, INTENT(INOUT) :: Mat(:)
    INTEGER(I4B), INTENT(IN) :: row
  END SUBROUTINE Reallocate_Int16_R1
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int16_R1b(Mat, s)
    INTEGER(INT16), ALLOCATABLE, INTENT(INOUT) :: Mat(:)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Int16_R1b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int8_R1(Mat, row)
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: Mat(:)
    INTEGER(I4B), INTENT(IN) :: row
  END SUBROUTINE Reallocate_Int8_R1
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int8_R1b(Mat, s)
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: Mat(:)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Int8_R1b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R2(Mat, row, col)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :)
    INTEGER(I4B), INTENT(IN) :: row, col
  END SUBROUTINE Reallocate_Int64_R2

  MODULE PURE SUBROUTINE Reallocate_Int64_R2b(Mat, s)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Int64_R2b

  MODULE PURE SUBROUTINE Reallocate_Int32_R2(Mat, row, col)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :)
    INTEGER(I4B), INTENT(IN) :: row, col
  END SUBROUTINE Reallocate_Int32_R2

  MODULE PURE SUBROUTINE Reallocate_Int32_R2b(Mat, s)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Int32_R2b

  MODULE PURE SUBROUTINE Reallocate_Int16_R2(Mat, row, col)
    INTEGER(INT16), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :)
    INTEGER(I4B), INTENT(IN) :: row, col
  END SUBROUTINE Reallocate_Int16_R2

  MODULE PURE SUBROUTINE Reallocate_Int16_R2b(Mat, s)
    INTEGER(INT16), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Int16_R2b

  MODULE PURE SUBROUTINE Reallocate_Int8_R2(Mat, row, col)
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :)
    INTEGER(I4B), INTENT(IN) :: row, col
  END SUBROUTINE Reallocate_Int8_R2

  MODULE PURE SUBROUTINE Reallocate_Int8_R2b(Mat, s)
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Int8_R2b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R3(Mat, i1, i2, i3)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3
  END SUBROUTINE Reallocate_Int64_R3
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R3b(Mat, s)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Int64_R3b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R3(Mat, i1, i2, i3)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3
  END SUBROUTINE Reallocate_Int32_R3
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R3b(Mat, s)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Int32_R3b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R4(Mat, i1, i2, i3, i4)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4
  END SUBROUTINE Reallocate_Int64_R4
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R4b(Mat, s)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Int64_R4b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R4(Mat, i1, i2, i3, i4)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4
  END SUBROUTINE Reallocate_Int32_R4
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R4b(Mat, s)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Int32_R4b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R5(Mat, i1, i2, i3, i4, i5)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5
  END SUBROUTINE Reallocate_Int64_R5
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R5b(Mat, s)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Int64_R5b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R5(Mat, i1, i2, i3, i4, i5)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5
  END SUBROUTINE Reallocate_Int32_R5
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R5b(Mat, s)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Int32_R5b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R6(Mat, i1, i2, i3, i4, i5, i6)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6
  END SUBROUTINE Reallocate_Int64_R6
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R6b(Mat, s)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Int64_R6b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R6(Mat, i1, i2, i3, i4, i5, i6)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6
  END SUBROUTINE Reallocate_Int32_R6
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R6b(Mat, s)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Int32_R6b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R7(Mat, i1, i2, i3, i4, i5, &
    & i6, i7)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6, i7
  END SUBROUTINE Reallocate_Int64_R7
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R7b(Mat, s)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Int64_R7b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R7(Mat, i1, i2, i3, i4, i5, i6, i7)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6, i7
  END SUBROUTINE Reallocate_Int32_R7
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R7b(Mat, s)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE Reallocate_Int32_R7b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R1_6(Vec1, n1, Vec2, n2, Vec3, &
    & n3, Vec4, n4, Vec5, n5, Vec6, n6)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: Vec1(:), Vec2(:)
    INTEGER(I4B), ALLOCATABLE, OPTIONAL, INTENT(INOUT) :: Vec3(:), &
      & Vec4(:), Vec5(:), Vec6(:)
    INTEGER(I4B), INTENT(IN) :: n1, n2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: n3, n4, n5, n6
  END SUBROUTINE Reallocate_Int32_R1_6
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R1_6(Vec1, n1, Vec2, &
    & n2, Vec3, n3, Vec4, n4, Vec5, n5, Vec6, n6)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Vec1(:), Vec2(:)
    REAL(REAL64), ALLOCATABLE, OPTIONAL, INTENT(INOUT) :: Vec3(:), &
      & Vec4(:), Vec5(:), Vec6(:)
    INTEGER(I4B), INTENT(IN) :: n1, n2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: n3, n4, n5, n6
  END SUBROUTINE Reallocate_Real64_R1_6
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R1_6(Vec1, n1, Vec2, &
    & n2, Vec3, n3, Vec4, n4, Vec5, n5, Vec6, n6)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Vec1(:), Vec2(:)
    REAL(REAL32), ALLOCATABLE, OPTIONAL, INTENT(INOUT) :: Vec3(:), &
      & Vec4(:), Vec5(:), Vec6(:)
    INTEGER(I4B), INTENT(IN) :: n1, n2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: n3, n4, n5, n6
  END SUBROUTINE Reallocate_Real32_R1_6
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_AIJ(A, nA, IA, nIA, JA, nJA)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: IA(:), JA(:)
    INTEGER(I4B), INTENT(IN) :: nA, nIA, nJA
  END SUBROUTINE Reallocate_Real64_AIJ
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_AIJ(A, nA, IA, nIA, JA, nJA)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: IA(:), JA(:)
    INTEGER(I4B), INTENT(IN) :: nA, nIA, nJA
  END SUBROUTINE Reallocate_Real32_AIJ
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_AI(A, nA, IA, nIA)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: IA(:)
    INTEGER(I4B), INTENT(IN) :: nA, nIA
  END SUBROUTINE Reallocate_Real64_AI
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_AI(A, nA, IA, nIA)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: IA(:)
    INTEGER(I4B), INTENT(IN) :: nA, nIA
  END SUBROUTINE Reallocate_Real32_AI
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

END MODULE ReallocateUtility
