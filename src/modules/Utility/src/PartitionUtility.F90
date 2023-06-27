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
! date: 2023-06-27
! summary: Partition methods for quicksorting and quickselect
!
!# Introduction
!
! This module contains Hoare's style partitioning algorithm used
! for quicksorting and quickselect routines.
!
! Reference:
!
! https://github.com/leonfoks/coretran/blob/master/src/core/m_partition.f90

MODULE PartitionUtility
USE GlobalData
IMPLICIT NONE
PRIVATE
PUBLIC :: partition
PUBLIC :: argPartition

!----------------------------------------------------------------------------
!                                                                 Partition
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-27
! summary:  Partitioning used for quickSort and quickSelect routines

INTERFACE partition
  MODULE PURE SUBROUTINE partition_Real32(this, left, right, iPivot)
    REAL(REAL32), INTENT(inout) :: this(:)
    !! 1D array
    INTEGER(I4B), INTENT(in) :: left
    !! Left index
    INTEGER(I4B), INTENT(in) :: right
    !! Right index
    INTEGER(I4B), INTENT(inout) :: iPivot
    !! Pivoting index
  END SUBROUTINE

  MODULE PURE SUBROUTINE partition_Real64(this, left, right, iPivot)
    REAL(REAL64), INTENT(inout) :: this(:)
    !! 1D array
    INTEGER(I4B), INTENT(in) :: left
    !! Left index
    INTEGER(I4B), INTENT(in) :: right
    !! Right index
    INTEGER(I4B), INTENT(inout) :: iPivot
    !! Pivoting index
  END SUBROUTINE

  MODULE PURE SUBROUTINE partition_Int8(this, left, right, iPivot)
    INTEGER(INT8), INTENT(inout) :: this(:)
    !! 1D array
    INTEGER(I4B), INTENT(in) :: left
    !! Left index
    INTEGER(I4B), INTENT(in) :: right
    !! Right index
    INTEGER(I4B), INTENT(inout) :: iPivot
    !! Pivoting index
  END SUBROUTINE

  MODULE PURE SUBROUTINE partition_Int16(this, left, right, iPivot)
    INTEGER(INT16), INTENT(inout) :: this(:)
    !! 1D array
    INTEGER(I4B), INTENT(in) :: left
    !! Left index
    INTEGER(I4B), INTENT(in) :: right
    !! Right index
    INTEGER(I4B), INTENT(inout) :: iPivot
    !! Pivoting index
  END SUBROUTINE

  MODULE PURE SUBROUTINE partition_Int32(this, left, right, iPivot)
    INTEGER(INT32), INTENT(inout) :: this(:)
    !! 1D array
    INTEGER(I4B), INTENT(in) :: left
    !! Left index
    INTEGER(I4B), INTENT(in) :: right
    !! Right index
    INTEGER(I4B), INTENT(inout) :: iPivot
    !! Pivoting index
  END SUBROUTINE

  MODULE PURE SUBROUTINE partition_int64(this, left, right, iPivot)
    INTEGER(INT64), INTENT(inout) :: this(:)
    !! 1D array
    INTEGER(I4B), INTENT(in) :: left
    !! Left index
    INTEGER(I4B), INTENT(in) :: right
    !! Right index
    INTEGER(I4B), INTENT(inout) :: iPivot
    !! Pivoting index
  END SUBROUTINE

END INTERFACE

!----------------------------------------------------------------------------
!                                                             argPartition
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-27
! summary:  Partitioning used for argQuicksort routines

INTERFACE argPartition
  MODULE PURE SUBROUTINE argPartition_Real32(this, idx, left, right, i)
    REAL(REAL32), INTENT(in) :: this(:)
    INTEGER(I4B), INTENT(inout) :: idx(:)
    INTEGER(I4B), INTENT(in) :: left
    INTEGER(I4B), INTENT(in) :: right
    INTEGER(I4B), INTENT(inout) :: i
  END SUBROUTINE

  MODULE PURE SUBROUTINE argPartition_Real64(this, idx, left, right, i)
    REAL(REAL64), INTENT(in) :: this(:)
    INTEGER(I4B), INTENT(inout) :: idx(:)
    INTEGER(I4B), INTENT(in) :: left
    INTEGER(I4B), INTENT(in) :: right
    INTEGER(I4B), INTENT(inout) :: i
  END SUBROUTINE

  MODULE PURE SUBROUTINE argPartition_Int8(this, idx, left, right, i)
    INTEGER(INT8), INTENT(in) :: this(:)
    INTEGER(I4B), INTENT(inout) :: idx(:)
    INTEGER(I4B), INTENT(in) :: left
    INTEGER(I4B), INTENT(in) :: right
    INTEGER(I4B), INTENT(inout) :: i
  END SUBROUTINE

  MODULE PURE SUBROUTINE argPartition_Int16(this, idx, left, right, i)
    INTEGER(INT16), INTENT(in) :: this(:)
    INTEGER(I4B), INTENT(inout) :: idx(:)
    INTEGER(I4B), INTENT(in) :: left
    INTEGER(I4B), INTENT(in) :: right
    INTEGER(I4B), INTENT(inout) :: i
  END SUBROUTINE

  MODULE PURE SUBROUTINE argPartition_Int32(this, idx, left, right, i)
    INTEGER(INT32), INTENT(in) :: this(:)
    INTEGER(I4B), INTENT(inout) :: idx(:)
    INTEGER(I4B), INTENT(in) :: left
    INTEGER(I4B), INTENT(in) :: right
    INTEGER(I4B), INTENT(inout) :: i
  END SUBROUTINE

  MODULE PURE SUBROUTINE argPartition_Int64(this, idx, left, right, i)
    INTEGER(INT64), INTENT(in) :: this(:)
    INTEGER(I4B), INTENT(inout) :: idx(:)
    INTEGER(I4B), INTENT(in) :: left
    INTEGER(I4B), INTENT(in) :: right
    INTEGER(I4B), INTENT(inout) :: i
  END SUBROUTINE

END INTERFACE argPartition

END MODULE PartitionUtility
