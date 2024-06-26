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
USE SwapUtility, ONLY: Swap

USE StringUtility, ONLY: UpperCase

USE ArangeUtility, ONLY: Arange

USE MedianUtility, ONLY: Median, ArgMedian

USE PartitionUtility, ONLY: Partition, ArgPartition

IMPLICIT NONE

INTEGER(I4B), PARAMETER :: minimumLengthForInsertion = 16

CONTAINS

!----------------------------------------------------------------------------
!                                                           IntroSort_Int8
!----------------------------------------------------------------------------

#define _Recursive_IntroSort_ Recursive_IntroSort_Int8
MODULE PROCEDURE IntroSort_Int8
INTEGER(I4B) :: low, high
INTEGER(I4B) :: maxDepth
low = 1
high = SIZE(array)
maxDepth = 2 * idnint(LOG(DBLE(high)))
CALL _Recursive_IntroSort_(array, low, high, maxDepth)
END PROCEDURE IntroSort_Int8
RECURSIVE PURE SUBROUTINE _Recursive_IntroSort_(this, left, right, maxDepth)
  INTEGER(INT8), INTENT(INOUT) :: this(:)
  INTEGER(I4B), INTENT(IN) :: left, right, maxDepth
  INTEGER(I4B) :: imid, iPivot, N
#include "./IntroSort/Recursive_IntroSort.inc"
END SUBROUTINE _Recursive_IntroSort_
#undef _Recursive_IntroSort_

!----------------------------------------------------------------------------
!                                                           IntroSort_Int16
!----------------------------------------------------------------------------

#define _Recursive_IntroSort_ Recursive_IntroSort_Int16
MODULE PROCEDURE IntroSort_Int16
INTEGER(I4B) :: low, high
INTEGER(I4B) :: maxDepth
low = 1
high = SIZE(array)
maxDepth = 2 * idnint(LOG(DBLE(high)))
CALL _Recursive_IntroSort_(array, low, high, maxDepth)
END PROCEDURE IntroSort_Int16
RECURSIVE PURE SUBROUTINE _Recursive_IntroSort_(this, left, right, maxDepth)
  INTEGER(INT16), INTENT(INOUT) :: this(:)
  INTEGER(I4B), INTENT(IN) :: left, right, maxDepth
  INTEGER(I4B) :: imid, iPivot, N
#include "./IntroSort/Recursive_IntroSort.inc"
END SUBROUTINE _Recursive_IntroSort_
#undef _Recursive_IntroSort_

!----------------------------------------------------------------------------
!                                                           IntroSort_Int32
!----------------------------------------------------------------------------

#define _Recursive_IntroSort_ Recursive_IntroSort_Int32
MODULE PROCEDURE IntroSort_Int32
INTEGER(I4B) :: low, high
INTEGER(I4B) :: maxDepth
low = 1
high = SIZE(array)
maxDepth = 2 * idnint(LOG(DBLE(high)))
CALL _Recursive_IntroSort_(array, low, high, maxDepth)
END PROCEDURE IntroSort_Int32
RECURSIVE PURE SUBROUTINE _Recursive_IntroSort_(this, left, right, maxDepth)
  INTEGER(INT32), INTENT(INOUT) :: this(:)
  INTEGER(I4B), INTENT(IN) :: left, right, maxDepth
  INTEGER(I4B) :: imid, iPivot, N
#include "./IntroSort/Recursive_IntroSort.inc"
END SUBROUTINE _Recursive_IntroSort_
#undef _Recursive_IntroSort_

!----------------------------------------------------------------------------
!                                                           IntroSort_Int8
!----------------------------------------------------------------------------

#define _Recursive_IntroSort_ Recursive_IntroSort_Int64
MODULE PROCEDURE IntroSort_Int64
INTEGER(I4B) :: low, high
INTEGER(I4B) :: maxDepth
low = 1
high = SIZE(array)
maxDepth = 2 * idnint(LOG(DBLE(high)))
CALL _Recursive_IntroSort_(array, low, high, maxDepth)
END PROCEDURE IntroSort_Int64
RECURSIVE PURE SUBROUTINE _Recursive_IntroSort_(this, left, right, maxDepth)
  INTEGER(INT64), INTENT(INOUT) :: this(:)
  INTEGER(I4B), INTENT(IN) :: left, right, maxDepth
  INTEGER(I4B) :: imid, iPivot, N
#include "./IntroSort/Recursive_IntroSort.inc"
END SUBROUTINE _Recursive_IntroSort_
#undef _Recursive_IntroSort_

!----------------------------------------------------------------------------
!                                                           IntroSort_Real32
!----------------------------------------------------------------------------

#define _Recursive_IntroSort_ Recursive_IntroSort_Real32
MODULE PROCEDURE IntroSort_Real32
INTEGER(I4B) :: low, high
INTEGER(I4B) :: maxDepth
low = 1
high = SIZE(array)
maxDepth = 2 * idnint(LOG(DBLE(high)))
CALL _Recursive_IntroSort_(array, low, high, maxDepth)
END PROCEDURE IntroSort_Real32
RECURSIVE PURE SUBROUTINE _Recursive_IntroSort_(this, left, right, maxDepth)
  REAL(REAL32), INTENT(INOUT) :: this(:)
  INTEGER(I4B), INTENT(IN) :: left, right, maxDepth
  INTEGER(I4B) :: imid, iPivot, N
#include "./IntroSort/Recursive_IntroSort.inc"
END SUBROUTINE _Recursive_IntroSort_
#undef _Recursive_IntroSort_

!----------------------------------------------------------------------------
!                                                           IntroSort_Real64
!----------------------------------------------------------------------------

#define _Recursive_IntroSort_ Recursive_IntroSort_Real64
MODULE PROCEDURE IntroSort_Real64
INTEGER(I4B) :: low, high
INTEGER(I4B) :: maxDepth
low = 1
high = SIZE(array)
maxDepth = 2 * idnint(LOG(DBLE(high)))
CALL _Recursive_IntroSort_(array, low, high, maxDepth)
END PROCEDURE IntroSort_Real64
RECURSIVE PURE SUBROUTINE _Recursive_IntroSort_(this, left, right, maxDepth)
  REAL(REAL64), INTENT(INOUT) :: this(:)
  INTEGER(I4B), INTENT(IN) :: left, right, maxDepth
  INTEGER(I4B) :: imid, iPivot, N
#include "./IntroSort/Recursive_IntroSort.inc"
END SUBROUTINE _Recursive_IntroSort_
#undef _Recursive_IntroSort_

!----------------------------------------------------------------------------
!                                                           IntroSort_Int8
!----------------------------------------------------------------------------

#define _Recursive_ArgIntroSort_ Recursive_ArgIntroSort_Int8
MODULE PROCEDURE ArgIntroSort_Int8
INTEGER(I4B) :: low, high
INTEGER(I4B) :: maxDepth
low = 1
high = SIZE(array)
maxDepth = 2 * idnint(LOG(DBLE(high)))
CALL _Recursive_ArgIntroSort_(array, arg, low, high, maxDepth)
END PROCEDURE ArgIntroSort_Int8
RECURSIVE PURE SUBROUTINE _Recursive_ArgIntroSort_(this, idx, &
  & left, right, maxDepth)
  INTEGER(INT8), INTENT(IN) :: this(:)
  INTEGER(I4B), INTENT(INOUT) :: idx(:)
  INTEGER(I4B), INTENT(IN) :: left, right, maxDepth
  INTEGER(I4B) :: imid, iPivot, N
#include "./IntroSort/Recursive_ArgIntroSort.inc"
END SUBROUTINE _Recursive_ArgIntroSort_
#undef _Recursive_ArgIntroSort_

!----------------------------------------------------------------------------
!                                                           IntroSort_Int16
!----------------------------------------------------------------------------

#define _Recursive_ArgIntroSort_ Recursive_ArgIntroSort_Int16
MODULE PROCEDURE ArgIntroSort_Int16
INTEGER(I4B) :: low, high
INTEGER(I4B) :: maxDepth
low = 1
high = SIZE(array)
maxDepth = 2 * idnint(LOG(DBLE(high)))
CALL _Recursive_ArgIntroSort_(array, arg, low, high, maxDepth)
END PROCEDURE ArgIntroSort_Int16
RECURSIVE PURE SUBROUTINE _Recursive_ArgIntroSort_(this, idx, &
  & left, right, maxDepth)
  INTEGER(INT16), INTENT(IN) :: this(:)
  INTEGER(I4B), INTENT(INOUT) :: idx(:)
  INTEGER(I4B), INTENT(IN) :: left, right, maxDepth
  INTEGER(I4B) :: imid, iPivot, N
#include "./IntroSort/Recursive_ArgIntroSort.inc"
END SUBROUTINE _Recursive_ArgIntroSort_
#undef _Recursive_ArgIntroSort_

!----------------------------------------------------------------------------
!                                                           IntroSort_Int32
!----------------------------------------------------------------------------

#define _Recursive_ArgIntroSort_ Recursive_ArgIntroSort_Int32
MODULE PROCEDURE ArgIntroSort_Int32
INTEGER(I4B) :: low, high
INTEGER(I4B) :: maxDepth
low = 1
high = SIZE(array)
maxDepth = 2 * idnint(LOG(DBLE(high)))
CALL _Recursive_ArgIntroSort_(array, arg, low, high, maxDepth)
END PROCEDURE ArgIntroSort_Int32
RECURSIVE PURE SUBROUTINE _Recursive_ArgIntroSort_(this, idx, &
  & left, right, maxDepth)
  INTEGER(INT32), INTENT(IN) :: this(:)
  INTEGER(I4B), INTENT(INOUT) :: idx(:)
  INTEGER(I4B), INTENT(IN) :: left, right, maxDepth
  INTEGER(I4B) :: imid, iPivot, N
#include "./IntroSort/Recursive_ArgIntroSort.inc"
END SUBROUTINE _Recursive_ArgIntroSort_
#undef _Recursive_ArgIntroSort_

!----------------------------------------------------------------------------
!                                                           IntroSort_Int64
!----------------------------------------------------------------------------

#define _Recursive_ArgIntroSort_ Recursive_ArgIntroSort_Int64
MODULE PROCEDURE ArgIntroSort_Int64
INTEGER(I4B) :: low, high
INTEGER(I4B) :: maxDepth
low = 1
high = SIZE(array)
maxDepth = 2 * idnint(LOG(DBLE(high)))
CALL _Recursive_ArgIntroSort_(array, arg, low, high, maxDepth)
END PROCEDURE ArgIntroSort_Int64
RECURSIVE PURE SUBROUTINE _Recursive_ArgIntroSort_(this, idx, &
  & left, right, maxDepth)
  INTEGER(INT64), INTENT(IN) :: this(:)
  INTEGER(I4B), INTENT(INOUT) :: idx(:)
  INTEGER(I4B), INTENT(IN) :: left, right, maxDepth
  INTEGER(I4B) :: imid, iPivot, N
#include "./IntroSort/Recursive_ArgIntroSort.inc"
END SUBROUTINE _Recursive_ArgIntroSort_
#undef _Recursive_ArgIntroSort_

!----------------------------------------------------------------------------
!                                                           IntroSort_Real32
!----------------------------------------------------------------------------

#define _Recursive_ArgIntroSort_ Recursive_ArgIntroSort_Real32
MODULE PROCEDURE ArgIntroSort_Real32
INTEGER(I4B) :: low, high
INTEGER(I4B) :: maxDepth
low = 1
high = SIZE(array)
maxDepth = 2 * idnint(LOG(DBLE(high)))
CALL _Recursive_ArgIntroSort_(array, arg, low, high, maxDepth)
END PROCEDURE ArgIntroSort_Real32
RECURSIVE PURE SUBROUTINE _Recursive_ArgIntroSort_(this, idx, &
  & left, right, maxDepth)
  REAL(REAL32), INTENT(IN) :: this(:)
  INTEGER(I4B), INTENT(INOUT) :: idx(:)
  INTEGER(I4B), INTENT(IN) :: left, right, maxDepth
  INTEGER(I4B) :: imid, iPivot, N
#include "./IntroSort/Recursive_ArgIntroSort.inc"
END SUBROUTINE _Recursive_ArgIntroSort_
#undef _Recursive_ArgIntroSort_

!----------------------------------------------------------------------------
!                                                           IntroSort_Real32
!----------------------------------------------------------------------------

#define _Recursive_ArgIntroSort_ Recursive_ArgIntroSort_Real64
MODULE PROCEDURE ArgIntroSort_Real64
INTEGER(I4B) :: low, high
INTEGER(I4B) :: maxDepth
low = 1
high = SIZE(array)
maxDepth = 2 * idnint(LOG(DBLE(high)))
CALL _Recursive_ArgIntroSort_(array, arg, low, high, maxDepth)
END PROCEDURE ArgIntroSort_Real64
RECURSIVE PURE SUBROUTINE _Recursive_ArgIntroSort_(this, idx, &
  & left, right, maxDepth)
  REAL(REAL64), INTENT(IN) :: this(:)
  INTEGER(I4B), INTENT(INOUT) :: idx(:)
  INTEGER(I4B), INTENT(IN) :: left, right, maxDepth
  INTEGER(I4B) :: imid, iPivot, N
#include "./IntroSort/Recursive_ArgIntroSort.inc"
END SUBROUTINE _Recursive_ArgIntroSort_
#undef _Recursive_ArgIntroSort_

!----------------------------------------------------------------------------
!                                                             InsertionSort
!----------------------------------------------------------------------------

MODULE PROCEDURE InsertionSort_Int8
#include "./InsertionSort/InsertionSort.inc"
END PROCEDURE InsertionSort_Int8

MODULE PROCEDURE InsertionSort_Int16
#include "./InsertionSort/InsertionSort.inc"
END PROCEDURE InsertionSort_Int16

MODULE PROCEDURE InsertionSort_Int32
#include "./InsertionSort/InsertionSort.inc"
END PROCEDURE InsertionSort_Int32

MODULE PROCEDURE InsertionSort_Int64
#include "./InsertionSort/InsertionSort.inc"
END PROCEDURE InsertionSort_Int64

MODULE PROCEDURE InsertionSort_Real32
#include "./InsertionSort/InsertionSort.inc"
END PROCEDURE InsertionSort_Real32

MODULE PROCEDURE InsertionSort_Real64
#include "./InsertionSort/InsertionSort.inc"
END PROCEDURE InsertionSort_Real64

!----------------------------------------------------------------------------
!                                                            ArgInsertionSort
!----------------------------------------------------------------------------

MODULE PROCEDURE ArgInsertionSort_Int8
#include "./InsertionSort/ArgInsertionSort.inc"
END PROCEDURE ArgInsertionSort_Int8

MODULE PROCEDURE ArgInsertionSort_Int16
#include "./InsertionSort/ArgInsertionSort.inc"
END PROCEDURE ArgInsertionSort_Int16

MODULE PROCEDURE ArgInsertionSort_Int32
#include "./InsertionSort/ArgInsertionSort.inc"
END PROCEDURE ArgInsertionSort_Int32

MODULE PROCEDURE ArgInsertionSort_Int64
#include "./InsertionSort/ArgInsertionSort.inc"
END PROCEDURE ArgInsertionSort_Int64

MODULE PROCEDURE ArgInsertionSort_Real32
#include "./InsertionSort/ArgInsertionSort.inc"
END PROCEDURE ArgInsertionSort_Real32

MODULE PROCEDURE ArgInsertionSort_Real64
#include "./InsertionSort/ArgInsertionSort.inc"
END PROCEDURE ArgInsertionSort_Real64

!----------------------------------------------------------------------------
!                                                                 HeapSort
!----------------------------------------------------------------------------

MODULE PROCEDURE HeapSort_Int8
INTEGER(INT8) :: t
#include "./HeapSort/HeapSort.inc"
END PROCEDURE HeapSort_Int8

MODULE PROCEDURE HeapSort_Int16
INTEGER(INT16) :: t
#include "./HeapSort/HeapSort.inc"
END PROCEDURE HeapSort_Int16

MODULE PROCEDURE HeapSort_Int32
INTEGER(INT32) :: t
#include "./HeapSort/HeapSort.inc"
END PROCEDURE HeapSort_Int32

MODULE PROCEDURE HeapSort_Int64
INTEGER(INT64) :: t
#include "./HeapSort/HeapSort.inc"
END PROCEDURE HeapSort_Int64

MODULE PROCEDURE HeapSort_Real32
REAL(REAL32) :: t
#include "./HeapSort/HeapSort.inc"
END PROCEDURE HeapSort_Real32

MODULE PROCEDURE HeapSort_Real64
REAL(REAL64) :: t
#include "./HeapSort/HeapSort.inc"
END PROCEDURE HeapSort_Real64

!----------------------------------------------------------------------------
!                                                              ArgHeapSort
!----------------------------------------------------------------------------

MODULE PROCEDURE ArgHeapSort_Int8
#include "./HeapSort/ArgHeapSort.inc"
END PROCEDURE ArgHeapSort_Int8

MODULE PROCEDURE ArgHeapSort_Int16
#include "./HeapSort/ArgHeapSort.inc"
END PROCEDURE ArgHeapSort_Int16

MODULE PROCEDURE ArgHeapSort_Int32
#include "./HeapSort/ArgHeapSort.inc"
END PROCEDURE ArgHeapSort_Int32

MODULE PROCEDURE ArgHeapSort_Int64
#include "./HeapSort/ArgHeapSort.inc"
END PROCEDURE ArgHeapSort_Int64

MODULE PROCEDURE ArgHeapSort_Real32
#include "./HeapSort/ArgHeapSort.inc"
END PROCEDURE ArgHeapSort_Real32

MODULE PROCEDURE ArgHeapSort_Real64
#include "./HeapSort/ArgHeapSort.inc"
END PROCEDURE ArgHeapSort_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort1vectReal32
#include "./QuickSort/QuickSort1Vec.inc"
END PROCEDURE QuickSort1vectReal32

MODULE PROCEDURE QuickSort1vectReal64
#include "./QuickSort/QuickSort1Vec.inc"
END PROCEDURE QuickSort1vectReal64

MODULE PROCEDURE QuickSort1vectInt8
#include "./QuickSort/QuickSort1Vec.inc"
END PROCEDURE QuickSort1vectInt8

MODULE PROCEDURE QuickSort1vectInt16
#include "./QuickSort/QuickSort1Vec.inc"
END PROCEDURE QuickSort1vectInt16

MODULE PROCEDURE QuickSort1vectInt32
#include "./QuickSort/QuickSort1Vec.inc"
END PROCEDURE QuickSort1vectInt32

MODULE PROCEDURE QuickSort1vectInt64
#include "./QuickSort/QuickSort1Vec.inc"
END PROCEDURE QuickSort1vectInt64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort2vectIR
#include "./QuickSort/QuickSort2Vec.inc"
END PROCEDURE QuickSort2vectIR

MODULE PROCEDURE QuickSort2vectII
#include "./QuickSort/QuickSort2Vec.inc"
END PROCEDURE QuickSort2vectII

MODULE PROCEDURE QuickSort2vectRI
#include "./QuickSort/QuickSort2Vec.inc"
END PROCEDURE QuickSort2vectRI

MODULE PROCEDURE QuickSort2vectRR
#include "./QuickSort/QuickSort2Vec.inc"
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort3vectIII
#include "./QuickSort/QuickSort3Vec.inc"
END PROCEDURE QuickSort3vectIII

MODULE PROCEDURE QuickSort3vectIIR
#include "./QuickSort/QuickSort3Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort3vectIRR
#include "./QuickSort/QuickSort3Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort3vectIRI
#include "./QuickSort/QuickSort3Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort3vectRRR
#include "./QuickSort/QuickSort3Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort3vectRRI
#include "./QuickSort/QuickSort3Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort3vectRIR
#include "./QuickSort/QuickSort3Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort3vectRII
#include "./QuickSort/QuickSort3Vec.inc"
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuickSort4vectIIII
#include "./QuickSort/QuickSort4Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort4vectIIIR
#include "./QuickSort/QuickSort4Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort4vectIIRI
#include "./QuickSort/QuickSort4Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort4vectIIRR
#include "./QuickSort/QuickSort4Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort4vectIRRR
#include "./QuickSort/QuickSort4Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort4vectIRRI
#include "./QuickSort/QuickSort4Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort4vectIRIR
#include "./QuickSort/QuickSort4Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort4vectIRII
#include "./QuickSort/QuickSort4Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort4vectRRRR
#include "./QuickSort/QuickSort4Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort4vectRRRI
#include "./QuickSort/QuickSort4Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort4vectRRIR
#include "./QuickSort/QuickSort4Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort4vectRRII
#include "./QuickSort/QuickSort4Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort4vectRIRR
#include "./QuickSort/QuickSort4Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort4vectRIRI
#include "./QuickSort/QuickSort4Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort4vectRIIR
#include "./QuickSort/QuickSort4Vec.inc"
END PROCEDURE

MODULE PROCEDURE QuickSort4vectRIII
#include "./QuickSort/QuickSort4Vec.inc"
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Sort_Int8
#include "./Sort/Sort.inc"
END PROCEDURE Sort_Int8
MODULE PROCEDURE Sort_Int16
#include "./Sort/Sort.inc"
END PROCEDURE Sort_Int16
MODULE PROCEDURE Sort_Int32
#include "./Sort/Sort.inc"
END PROCEDURE Sort_Int32
MODULE PROCEDURE Sort_Int64
#include "./Sort/Sort.inc"
END PROCEDURE Sort_Int64
MODULE PROCEDURE Sort_Real32
#include "./Sort/Sort.inc"
END PROCEDURE Sort_Real32
MODULE PROCEDURE Sort_Real64
#include "./Sort/Sort.inc"
END PROCEDURE Sort_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ArgSort_Int8
#include "./Sort/ArgSort.inc"
END PROCEDURE ArgSort_Int8
MODULE PROCEDURE ArgSort_Int16
#include "./Sort/ArgSort.inc"
END PROCEDURE ArgSort_Int16
MODULE PROCEDURE ArgSort_Int32
#include "./Sort/ArgSort.inc"
END PROCEDURE ArgSort_Int32
MODULE PROCEDURE ArgSort_Int64
#include "./Sort/ArgSort.inc"
END PROCEDURE ArgSort_Int64
MODULE PROCEDURE ArgSort_Real32
#include "./Sort/ArgSort.inc"
END PROCEDURE ArgSort_Real32
MODULE PROCEDURE ArgSort_Real64
#include "./Sort/ArgSort.inc"
END PROCEDURE ArgSort_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
