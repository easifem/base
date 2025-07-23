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
USE GlobalData, ONLY: DFP, LGT, I4B, REAL32, REAL64, REAL128, &
                      INT8, INT16, INT32, INT64

IMPLICIT NONE

PRIVATE

PUBLIC :: Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_logical(mat, row, isExpand, expandFactor)
    LOGICAL(LGT), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: row
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size if more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor time row
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
    !! expand factor, used when isExpand is true.
  END SUBROUTINE Reallocate_logical
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
 MODULE PURE SUBROUTINE Reallocate_Real64_R1(mat, row, isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: row
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R1
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R1b(mat, s, isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R1b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
 MODULE PURE SUBROUTINE Reallocate_Real32_R1(mat, row, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: row
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R1
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R1b(mat, s, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R1b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R2(mat, row, col, isExpand, &
                                              expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: row, col
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R2
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R2b(mat, s, isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R2b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R2(mat, row, col, isExpand, &
                                              expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: row, col
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R2
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R2b(mat, s, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R2b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R3(mat, i1, i2, i3, isExpand, &
                                              expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R3
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R3b(mat, s, isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R3b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R3(mat, i1, i2, i3, isExpand, &
                                              expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R3
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R3b(mat, s, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R3b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R4(mat, i1, i2, i3, i4, isExpand, &
                                              expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R4
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R4b(mat, s, isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R4b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R4(mat, i1, i2, i3, i4, isExpand, &
                                              expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R4
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R4b(mat, s, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R4b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R5(mat, i1, i2, i3, i4, i5, &
                                              isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R5
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R5b(mat, s, isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R5b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R5(mat, i1, i2, i3, i4, i5, &
                                              isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R5
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R5b(mat, s, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R5b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R6(mat, i1, i2, i3, i4, i5, i6, &
                                              isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R6
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R6b(mat, s, isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R6b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R6(mat, i1, i2, i3, i4, i5, i6, &
                                              isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R6
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R6b(mat, s, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R6b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R7(mat, i1, i2, i3, i4, i5, &
    & i6, i7, isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6, i7
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R7
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R7b(mat, s, isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R7b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R7(mat, i1, i2, i3, i4, i5, i6, &
                                              i7, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6, i7
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R7
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R7b(mat, s, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R7b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R1(mat, row, isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: row
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R1
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE Reallocate_Int64_R1b(mat, s, isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R1b
END INTERFACE

INTERFACE Reallocate
  MODULE PROCEDURE Reallocate_Int64_R1b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R1(mat, row, isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: row
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R1
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R1b(mat, s, isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R1b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int16_R1(mat, row, isExpand, expandFactor)
    INTEGER(INT16), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: row
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int16_R1
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int16_R1b(mat, s, isExpand, expandFactor)
    INTEGER(INT16), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int16_R1b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int8_R1(mat, row, isExpand, expandFactor)
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: row
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int8_R1
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int8_R1b(mat, s, isExpand, expandFactor)
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: mat(:)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int8_R1b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R2(mat, row, col, isExpand, &
                                             expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: row, col
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R2

  MODULE PURE SUBROUTINE Reallocate_Int64_R2b(mat, s, isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R2b

  MODULE PURE SUBROUTINE Reallocate_Int32_R2(mat, row, col, isExpand, &
                                             expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: row, col
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R2

  MODULE PURE SUBROUTINE Reallocate_Int32_R2b(mat, s, isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R2b

  MODULE PURE SUBROUTINE Reallocate_Int16_R2(mat, row, col, isExpand, &
                                             expandFactor)
    INTEGER(INT16), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: row, col
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int16_R2

  MODULE PURE SUBROUTINE Reallocate_Int16_R2b(mat, s, isExpand, expandFactor)
    INTEGER(INT16), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int16_R2b

  MODULE PURE SUBROUTINE Reallocate_Int8_R2(mat, row, col, isExpand, &
                                            expandFactor)
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: row, col
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int8_R2

  MODULE PURE SUBROUTINE Reallocate_Int8_R2b(mat, s, isExpand, expandFactor)
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int8_R2b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R3(mat, i1, i2, i3, isExpand, &
                                             expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R3
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R3b(mat, s, isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R3b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R3(mat, i1, i2, i3, isExpand, &
                                             expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R3
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R3b(mat, s, isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R3b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R4(mat, i1, i2, i3, i4, isExpand, &
                                             expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R4
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R4b(mat, s, isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R4b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R4(mat, i1, i2, i3, i4, isExpand, &
                                             expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R4
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R4b(mat, s, isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R4b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R5(mat, i1, i2, i3, i4, i5, &
                                             isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R5
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R5b(mat, s, isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R5b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R5(mat, i1, i2, i3, i4, i5, &
                                             isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R5
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R5b(mat, s, isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R5b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R6(mat, i1, i2, i3, i4, i5, i6, &
                                             isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R6
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R6b(mat, s, isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R6b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R6(mat, i1, i2, i3, i4, i5, i6, &
                                             isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R6
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R6b(mat, s, isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R6b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R7(mat, i1, i2, i3, i4, i5, &
                                             i6, i7, isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6, i7
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R7
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int64_R7b(mat, s, isExpand, expandFactor)
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int64_R7b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R7(mat, i1, i2, i3, i4, i5, i6, &
                                             i7, isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: i1, i2, i3, i4, i5, i6, i7
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R7
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R7b(mat, s, isExpand, expandFactor)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:, :, :, :, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R7b
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Int32_R1_6(vec1, n1, vec2, n2, vec3, &
                                               n3, vec4, n4, vec5, n5, vec6, &
                                               n6, isExpand, expandFactor)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: vec1(:), vec2(:)
    INTEGER(I4B), ALLOCATABLE, OPTIONAL, INTENT(INOUT) :: vec3(:), &
                                                     vec4(:), vec5(:), vec6(:)
    INTEGER(I4B), INTENT(IN) :: n1, n2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: n3, n4, n5, n6
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Int32_R1_6
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_R1_6(vec1, n1, vec2, &
                                                n2, vec3, n3, vec4, n4, &
                                                vec5, n5, vec6, n6, &
                                                isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: vec1(:), vec2(:)
    REAL(REAL64), ALLOCATABLE, OPTIONAL, INTENT(INOUT) :: vec3(:), &
                                                     vec4(:), vec5(:), vec6(:)
    INTEGER(I4B), INTENT(IN) :: n1, n2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: n3, n4, n5, n6
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_R1_6
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_R1_6(vec1, n1, vec2, &
                                                n2, vec3, n3, vec4, &
                                                n4, vec5, n5, vec6, &
                                                n6, isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: vec1(:), vec2(:)
    REAL(REAL32), ALLOCATABLE, OPTIONAL, INTENT(INOUT) :: vec3(:), &
                                                     vec4(:), vec5(:), vec6(:)
    INTEGER(I4B), INTENT(IN) :: n1, n2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: n3, n4, n5, n6
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_R1_6
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_AIJ(A, nA, IA, nIA, JA, nJA, &
                                               isExpand, expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: IA(:), JA(:)
    INTEGER(I4B), INTENT(IN) :: nA, nIA, nJA
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_AIJ
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_AIJ(A, nA, IA, nIA, JA, nJA, &
                                               isExpand, expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: IA(:), JA(:)
    INTEGER(I4B), INTENT(IN) :: nA, nIA, nJA
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_AIJ
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real64_AI(A, nA, IA, nIA, isExpand, &
                                              expandFactor)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: IA(:)
    INTEGER(I4B), INTENT(IN) :: nA, nIA
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real64_AI
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                               Reallocate@ReallocateMethods
!----------------------------------------------------------------------------

INTERFACE Reallocate
  MODULE PURE SUBROUTINE Reallocate_Real32_AI(A, nA, IA, nIA, isExpand, &
                                              expandFactor)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: IA(:)
    INTEGER(I4B), INTENT(IN) :: nA, nIA
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isExpand
    !! if true then we do not allocate if current size is more than required
    !! in this case if the size is not enough then the new size
    !! is expandFactor times required size
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: expandFactor
  END SUBROUTINE Reallocate_Real32_AI
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

END MODULE ReallocateUtility
