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
USE BaseMethod, ONLY: Swap, UpperCase, arange
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
