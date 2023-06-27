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

SUBMODULE(PartitionUtility) Methods
USE BaseMethod, ONLY: SWAP
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Partition
!----------------------------------------------------------------------------

MODULE PROCEDURE partition_Real32
INTEGER(I4B) :: lo, hi
REAL(REAL32) :: pivot
#include "./Partition/Partition.inc"
END PROCEDURE partition_Real32

!----------------------------------------------------------------------------
!                                                                 Partition
!----------------------------------------------------------------------------

MODULE PROCEDURE partition_Real64
INTEGER(I4B) :: lo, hi
REAL(REAL64) :: pivot
#include "./Partition/Partition.inc"
END PROCEDURE partition_Real64

!----------------------------------------------------------------------------
!                                                                 Partition
!----------------------------------------------------------------------------

MODULE PROCEDURE partition_Int8
INTEGER(I4B) :: lo, hi
INTEGER(INT8) :: pivot
#include "./Partition/Partition.inc"
END PROCEDURE partition_Int8

!----------------------------------------------------------------------------
!                                                                 Partition
!----------------------------------------------------------------------------

MODULE PROCEDURE partition_Int16
INTEGER(I4B) :: lo, hi
INTEGER(INT16) :: pivot
#include "./Partition/Partition.inc"
END PROCEDURE partition_Int16

!----------------------------------------------------------------------------
!                                                                 Partition
!----------------------------------------------------------------------------

MODULE PROCEDURE partition_Int32
INTEGER(I4B) :: lo, hi
INTEGER(INT32) :: pivot
#include "./Partition/Partition.inc"
END PROCEDURE partition_Int32

!----------------------------------------------------------------------------
!                                                                 Partition
!----------------------------------------------------------------------------

MODULE PROCEDURE partition_Int64
INTEGER(I4B) :: lo, hi
INTEGER(INT64) :: pivot
#include "./Partition/Partition.inc"
END PROCEDURE partition_Int64

!----------------------------------------------------------------------------
!                                                             ArgPartition
!----------------------------------------------------------------------------

MODULE PROCEDURE argPartition_Int8
INTEGER(I4B) :: lo, hi
INTEGER(INT8) :: pivot
#include "./Partition/ArgPartition.inc"
END PROCEDURE argPartition_Int8

!----------------------------------------------------------------------------
!                                                             ArgPartition
!----------------------------------------------------------------------------

MODULE PROCEDURE argPartition_Int16
INTEGER(I4B) :: lo, hi
INTEGER(INT16) :: pivot
#include "./Partition/ArgPartition.inc"
END PROCEDURE argPartition_Int16

!----------------------------------------------------------------------------
!                                                             ArgPartition
!----------------------------------------------------------------------------

MODULE PROCEDURE argPartition_Int32
INTEGER(I4B) :: lo, hi
INTEGER(INT32) :: pivot
#include "./Partition/ArgPartition.inc"
END PROCEDURE argPartition_Int32

!----------------------------------------------------------------------------
!                                                             ArgPartition
!----------------------------------------------------------------------------

MODULE PROCEDURE argPartition_Int64
INTEGER(I4B) :: lo, hi
INTEGER(INT64) :: pivot
#include "./Partition/ArgPartition.inc"
END PROCEDURE argPartition_Int64

!----------------------------------------------------------------------------
!                                                             ArgPartition
!----------------------------------------------------------------------------

MODULE PROCEDURE argPartition_Real32
INTEGER(I4B) :: lo, hi
REAL(REAL32) :: pivot
#include "./Partition/ArgPartition.inc"
END PROCEDURE argPartition_Real32

!----------------------------------------------------------------------------
!                                                             ArgPartition
!----------------------------------------------------------------------------

MODULE PROCEDURE argPartition_Real64
INTEGER(I4B) :: lo, hi
REAL(REAL64) :: pivot
#include "./Partition/ArgPartition.inc"
END PROCEDURE argPartition_Real64

END SUBMODULE
