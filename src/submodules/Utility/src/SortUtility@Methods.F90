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
USE BaseMethod, ONLY: Swap, Split, Head, Tail, arange
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 HeapSort
!----------------------------------------------------------------------------

MODULE PROCEDURE HeapSort_Int8
INTEGER(Int8) :: t
#include "./HeapSort/HeapSort.inc"
END PROCEDURE HeapSort_Int8

MODULE PROCEDURE HeapSort_Int16
INTEGER(Int16) :: t
#include "./HeapSort/HeapSort.inc"
END PROCEDURE HeapSort_Int16

MODULE PROCEDURE HeapSort_Int32
INTEGER(Int32) :: t
#include "./HeapSort/HeapSort.inc"
END PROCEDURE HeapSort_Int32

MODULE PROCEDURE HeapSort_Int64
INTEGER(Int64) :: t
#include "./HeapSort/HeapSort.inc"
END PROCEDURE HeapSort_Int64

MODULE PROCEDURE HeapSort_Real32
REAL(Real32) :: t
#include "./HeapSort/HeapSort.inc"
END PROCEDURE HeapSort_Real32

MODULE PROCEDURE HeapSort_Real64
REAL(Real64) :: t
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
