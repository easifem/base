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

MODULE SortUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: HeapSort
PUBLIC :: ArgHeapSort
PUBLIC :: QuickSort
PUBLIC :: Sort
PUBLIC :: ArgSort

!----------------------------------------------------------------------------
!                                                              HeapSort@Sort
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Heap Sort

INTERFACE
  MODULE PURE SUBROUTINE HeapSort_Int8(array)
    INTEGER(Int8), INTENT(INOUT) :: array(:)
  END SUBROUTINE HeapSort_Int8
  MODULE PURE SUBROUTINE HeapSort_Int16(array)
    INTEGER(Int16), INTENT(INOUT) :: array(:)
  END SUBROUTINE HeapSort_Int16
  MODULE PURE SUBROUTINE HeapSort_Int32(array)
    INTEGER(Int32), INTENT(INOUT) :: array(:)
  END SUBROUTINE HeapSort_Int32
  MODULE PURE SUBROUTINE HeapSort_Int64(array)
    INTEGER(Int64), INTENT(INOUT) :: array(:)
  END SUBROUTINE HeapSort_Int64
  MODULE PURE SUBROUTINE HeapSort_Real32(array)
    REAL(Real32), INTENT(INOUT) :: array(:)
  END SUBROUTINE HeapSort_Real32
  MODULE PURE SUBROUTINE HeapSort_Real64(array)
    REAL(Real64), INTENT(INOUT) :: array(:)
  END SUBROUTINE HeapSort_Real64
END INTERFACE

INTERFACE HeapSort
  MODULE PROCEDURE HeapSort_Int8, HeapSort_Int16, HeapSort_Int32, &
    & HeapSort_Int64, HeapSort_Real32, HeapSort_Real64
END INTERFACE HeapSort

!----------------------------------------------------------------------------
!                                                           ArgHeapSort@Sort
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Heap Sort

INTERFACE
  MODULE PURE SUBROUTINE ArgHeapSort_Int8(array, arg)
    INTEGER(Int8), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(OUT) :: arg(0:)
  END SUBROUTINE ArgHeapSort_Int8

  MODULE PURE SUBROUTINE ArgHeapSort_Int16(array, arg)
    INTEGER(Int16), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(OUT) :: arg(0:)
  END SUBROUTINE ArgHeapSort_Int16

  MODULE PURE SUBROUTINE ArgHeapSort_Int32(array, arg)
    INTEGER(Int32), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(OUT) :: arg(0:)
  END SUBROUTINE ArgHeapSort_Int32

  MODULE PURE SUBROUTINE ArgHeapSort_Int64(array, arg)
    INTEGER(Int64), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(OUT) :: arg(0:)
  END SUBROUTINE ArgHeapSort_Int64

  MODULE PURE SUBROUTINE ArgHeapSort_Real32(array, arg)
    REAL(Real32), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(OUT) :: arg(0:)
  END SUBROUTINE ArgHeapSort_Real32

  MODULE PURE SUBROUTINE ArgHeapSort_Real64(array, arg)
    REAL(Real64), INTENT(IN) :: array(:)
    INTEGER(I4B), INTENT(OUT) :: arg(0:)
  END SUBROUTINE ArgHeapSort_Real64
END INTERFACE

INTERFACE ArgHeapSort
  MODULE PROCEDURE ArgHeapSort_Int8, ArgHeapSort_Int16, ArgHeapSort_Int32, &
    & ArgHeapSort_Int64, ArgHeapSort_Real32, ArgHeapSort_Real64
END INTERFACE ArgHeapSort

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE RECURSIVE PURE SUBROUTINE QuickSort1vectInt8(vect1, low, high)
    INTEGER(Int8), INTENT(INOUT) :: vect1(:)
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort1vectInt8
  MODULE RECURSIVE PURE SUBROUTINE QuickSort1vectInt16(vect1, low, high)
    INTEGER(Int16), INTENT(INOUT) :: vect1(:)
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort1vectInt16
  MODULE RECURSIVE PURE SUBROUTINE QuickSort1vectInt32(vect1, low, high)
    INTEGER(Int32), INTENT(INOUT) :: vect1(:)
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort1vectInt32
  MODULE RECURSIVE PURE SUBROUTINE QuickSort1vectInt64(vect1, low, high)
    INTEGER(Int64), INTENT(INOUT) :: vect1(:)
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort1vectInt64
  MODULE RECURSIVE PURE SUBROUTINE QuickSort1vectReal32(vect1, low, high)
    REAL(Real32), INTENT(INOUT) :: vect1(:)
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort1vectReal32
  MODULE RECURSIVE PURE SUBROUTINE QuickSort1vectReal64(vect1, low, high)
    REAL(Real64), INTENT(INOUT) :: vect1(:)
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE QuickSort1vectReal64
END INTERFACE

INTERFACE QuickSort
  MODULE PROCEDURE QuickSort1vectInt8, QuickSort1vectInt16, &
    & QuickSort1vectInt32, QuickSort1vectInt64
  MODULE PROCEDURE QuickSort1vectReal32, QuickSort1vectReal64
END INTERFACE QuickSort

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE RECURSIVE PURE SUBROUTINE QuickSort2vectIR(vect1, vect2, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort2vectII(vect1, vect2, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort2vectRI(vect1, vect2, low, high)
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort2vectRR(vect1, vect2, low, high)
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort3vectIII(vect1, vect2, vect3, &
    & low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort3vectIIR(vect1, vect2, vect3, &
    & low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect2
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort3vectIRR(vect1, vect2, vect3, &
    & low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort3vectIRI(vect1, vect2, vect3, &
    & low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort3vectRRR(vect1, vect2, vect3, &
    & low, high)
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort3vectRRI(vect1, vect2, vect3, &
    & low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort3vectRIR(vect1, vect2, vect3, &
    & low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort3vectRII(vect1, vect2, vect3, &
    & low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2, vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectIIII(vect1, vect2, vect3, &
      & vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectIIIR(vect1, vect2, vect3, &
      & vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectIIRI(vect1, vect2, vect3, &
    & vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectIIRR(vect1, vect2, vect3, &
      & vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect2
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect3, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectIRRR(vect1, vect2, vect3, &
      & vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2, vect3, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectIRRI(vect1, vect2, vect3, &
      & vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectIRIR(vect1, vect2, vect3, &
      & vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectIRII(vect1, vect2, vect3, &
    & vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect3, vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectRRRR(vect1, vect2, vect3, &
    & vect4, low, high)
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectRRRI(vect1, vect2, &
    & vect3, vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectRRIR(vect1, vect2, &
  & vect3, vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectRRII(vect1, vect2, vect3, &
    & vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect3, vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectRIRR(vect1, vect2, vect3, &
    & vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect3, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectRIRI(vect1, vect2, vect3, &
    & vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2, vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectRIIR(vect1, vect2, vect3, &
    & vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2, vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE QuickSort4vectRIII(vect1, vect2, vect3, &
    & vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2, vect3, vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

INTERFACE QuickSort
  MODULE PROCEDURE QuickSort2vectII, &
    & QuickSort2vectIR, QuickSort2vectRR, QuickSort2vectRI, &
    & QuickSort3vectIII, QuickSort3vectIIR, QuickSort3vectIRI, &
    & QuickSort3vectIRR, QuickSort3vectRRR, QuickSort3vectRRI, &
    & QuickSort3vectRIR, QuickSort3vectRII, QuickSort4vectIIII, &
    & QuickSort4vectIIIR, QuickSort4vectIIRI, QuickSort4vectIIRR, &
    & QuickSort4vectIRII, QuickSort4vectIRIR, QuickSort4vectIRRI, &
    & QuickSort4vectIRRR, QuickSort4vectRIII, QuickSort4vectRIIR, &
    & QuickSort4vectRIRI, QuickSort4vectRIRR, QuickSort4vectRRII, &
    & QuickSort4vectRRIR, QuickSort4vectRRRI, QuickSort4vectRRRR
END INTERFACE QuickSort

!----------------------------------------------------------------------------
!                                                                 Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Sort_Int8(x, name) RESULT(ans)
    INTEGER(Int8), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(Int8) :: ans(SIZE(x))
  END FUNCTION Sort_Int8
  MODULE PURE FUNCTION Sort_Int16(x, name) RESULT(ans)
    INTEGER(Int16), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(Int16) :: ans(SIZE(x))
  END FUNCTION Sort_Int16
  MODULE PURE FUNCTION Sort_Int32(x, name) RESULT(ans)
    INTEGER(Int32), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(Int32) :: ans(SIZE(x))
  END FUNCTION Sort_Int32
  MODULE PURE FUNCTION Sort_Int64(x, name) RESULT(ans)
    INTEGER(Int64), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(Int64) :: ans(SIZE(x))
  END FUNCTION Sort_Int64
  MODULE PURE FUNCTION Sort_Real32(x, name) RESULT(ans)
    Real(Real32), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    Real(Real32) :: ans(SIZE(x))
  END FUNCTION Sort_Real32
  MODULE PURE FUNCTION Sort_Real64(x, name) RESULT(ans)
    Real(Real64), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    Real(Real64) :: ans(SIZE(x))
  END FUNCTION Sort_Real64
END INTERFACE

INTERFACE Sort
  MODULE PROCEDURE Sort_Int8, Sort_Int16, Sort_Int32, Sort_Int64
  MODULE PROCEDURE Sort_Real32, Sort_Real64
END INTERFACE Sort

!----------------------------------------------------------------------------
!                                                                    ArgSort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ArgSort_Int8(x, name) RESULT(ans)
    INTEGER(Int8), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(I4B) :: ans(SIZE(x))
  END FUNCTION ArgSort_Int8
  MODULE PURE FUNCTION ArgSort_Int16(x, name) RESULT(ans)
    INTEGER(Int16), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(I4B) :: ans(SIZE(x))
  END FUNCTION ArgSort_Int16
  MODULE PURE FUNCTION ArgSort_Int32(x, name) RESULT(ans)
    INTEGER(Int32), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(I4B) :: ans(SIZE(x))
  END FUNCTION ArgSort_Int32
  MODULE PURE FUNCTION ArgSort_Int64(x, name) RESULT(ans)
    INTEGER(Int64), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(I4B) :: ans(SIZE(x))
  END FUNCTION ArgSort_Int64
  MODULE PURE FUNCTION ArgSort_Real32(x, name) RESULT(ans)
    Real(Real32), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(I4B) :: ans(SIZE(x))
  END FUNCTION ArgSort_Real32
  MODULE PURE FUNCTION ArgSort_Real64(x, name) RESULT(ans)
    Real(Real64), INTENT(IN) :: x(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(I4B) :: ans(SIZE(x))
  END FUNCTION ArgSort_Real64
END INTERFACE

INTERFACE ArgSort
  MODULE PROCEDURE ArgSort_Int8, ArgSort_Int16, ArgSort_Int32, ArgSort_Int64
  MODULE PROCEDURE ArgSort_Real32, ArgSort_Real64
END INTERFACE ArgSort

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE SortUtility
