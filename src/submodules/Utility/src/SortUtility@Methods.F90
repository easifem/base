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
! date:         22 March 2021
! summary:         This submodule contains the sorting routine

SUBMODULE(SortUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 HeapSort
!----------------------------------------------------------------------------

MODULE PROCEDURE HeapSort_Int8
INTEGER(I4B) :: n, i, k, j, l
INTEGER(Int8) :: t
#include "./HeapSort/HeapSort.inc"
END PROCEDURE HeapSort_Int8

MODULE PROCEDURE HeapSort_Int16
INTEGER(I4B) :: n, i, k, j, l
INTEGER(Int16) :: t
#include "./HeapSort/HeapSort.inc"
END PROCEDURE HeapSort_Int16

MODULE PROCEDURE HeapSort_Int32
INTEGER(I4B) :: n, i, k, j, l
INTEGER(Int32) :: t
#include "./HeapSort/HeapSort.inc"
END PROCEDURE HeapSort_Int32

MODULE PROCEDURE HeapSort_Int64
INTEGER(I4B) :: n, i, k, j, l
INTEGER(Int64) :: t
#include "./HeapSort/HeapSort.inc"
END PROCEDURE HeapSort_Int64

!----------------------------------------------------------------------------
!                                                                   HeapSort
!----------------------------------------------------------------------------

MODULE PROCEDURE HeapSort_Real32
INTEGER(I4B) :: n, i, k, j, l
REAL(Real32) :: t
#include "./HeapSort/HeapSort.inc"
END PROCEDURE HeapSort_Real32
MODULE PROCEDURE HeapSort_Real64
INTEGER(I4B) :: n, i, k, j, l
REAL(Real64) :: t
#include "./HeapSort/HeapSort.inc"
END PROCEDURE HeapSort_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort1vectReal32
INTEGER(I4B) i, iPivot
#include "./QuickSort/QuickSort1Vec.inc"
END PROCEDURE QuickSort1vectReal32

MODULE PROCEDURE QuickSort1vectReal64
INTEGER(I4B) i, iPivot
#include "./QuickSort/QuickSort1Vec.inc"
END PROCEDURE QuickSort1vectReal64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort1vectInt8
INTEGER(I4B) i, iPivot
#include "./QuickSort/QuickSort1Vec.inc"
END PROCEDURE QuickSort1vectInt8

MODULE PROCEDURE QuickSort1vectInt16
INTEGER(I4B) i, iPivot
#include "./QuickSort/QuickSort1Vec.inc"
END PROCEDURE QuickSort1vectInt16

MODULE PROCEDURE QuickSort1vectInt32
INTEGER(I4B) i, iPivot
#include "./QuickSort/QuickSort1Vec.inc"
END PROCEDURE QuickSort1vectInt32

MODULE PROCEDURE QuickSort1vectInt64
INTEGER(I4B) i, iPivot
#include "./QuickSort/QuickSort1Vec.inc"
END PROCEDURE QuickSort1vectInt64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort2vectIR
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort2vectII
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort2vectRI
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort2vectRR
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort3vectIII
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort3vectIIR
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort3vectIRR
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort3vectIRI
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort3vectRRR
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort3vectRRI
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort3vectRIR
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort3vectRII
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort4vectIIII
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect4(i), vect4(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    CALL Swap(vect4(iPivot - 1), vect4(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, vect4, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, vect4, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort4vectIIIR
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect4(i), vect4(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    CALL Swap(vect4(iPivot - 1), vect4(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, vect4, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, vect4, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort4vectIIRI
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect4(i), vect4(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    CALL Swap(vect4(iPivot - 1), vect4(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, vect4, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, vect4, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort4vectIIRR
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect4(i), vect4(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    CALL Swap(vect4(iPivot - 1), vect4(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, vect4, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, vect4, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort4vectIRRR
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect4(i), vect4(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    CALL Swap(vect4(iPivot - 1), vect4(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, vect4, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, vect4, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort4vectIRRI
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect4(i), vect4(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    CALL Swap(vect4(iPivot - 1), vect4(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, vect4, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, vect4, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort4vectIRIR
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect4(i), vect4(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    CALL Swap(vect4(iPivot - 1), vect4(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, vect4, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, vect4, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort4vectIRII
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect4(i), vect4(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    CALL Swap(vect4(iPivot - 1), vect4(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, vect4, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, vect4, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort4vectRRRR
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect4(i), vect4(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    CALL Swap(vect4(iPivot - 1), vect4(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, vect4, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, vect4, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort4vectRRRI
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect4(i), vect4(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    CALL Swap(vect4(iPivot - 1), vect4(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, vect4, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, vect4, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort4vectRRIR
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect4(i), vect4(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    CALL Swap(vect4(iPivot - 1), vect4(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, vect4, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, vect4, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort4vectRRII
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect4(i), vect4(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    CALL Swap(vect4(iPivot - 1), vect4(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, vect4, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, vect4, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort4vectRIRR
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect4(i), vect4(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    CALL Swap(vect4(iPivot - 1), vect4(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, vect4, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, vect4, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort4vectRIRI
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect4(i), vect4(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    CALL Swap(vect4(iPivot - 1), vect4(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, vect4, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, vect4, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort4vectRIIR
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect4(i), vect4(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    CALL Swap(vect4(iPivot - 1), vect4(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, vect4, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, vect4, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort4vectRIII
INTEGER(I4B) i, iPivot
iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect4(i), vect4(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    CALL Swap(vect4(iPivot - 1), vect4(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, vect4, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, vect4, iPivot + 1, high)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Sort_Int
INTEGER(I4B), DIMENSION(SIZE(x) - 1) :: rest
INTEGER(I4B) :: pivot

IF (SIZE(x) > 1) THEN
  pivot = HEAD(SPLIT(x, 2))
  rest = [SPLIT(x, 1), TAIL(split(x, 2))]
  ANS = [Sort_Int(PACK(rest, rest < pivot)), pivot, &
          & Sort_Int(PACK(rest, rest >= pivot))]
ELSE
  ANS = x
END IF
END PROCEDURE Sort_Int

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Sort_Real
REAL(DFP), DIMENSION(SIZE(x) - 1) :: rest
REAL(DFP) :: pivot
!
IF (SIZE(x) > 1) THEN
  pivot = HEAD(SPLIT(x, 2))
  rest = [SPLIT(x, 1), TAIL(split(x, 2))]
  ANS = [Sort_Real(pack(rest, rest < pivot)), pivot, &
          & Sort_Real(pack(rest, rest >= pivot))]
ELSE
  ANS = x
END IF
END PROCEDURE Sort_Real

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
