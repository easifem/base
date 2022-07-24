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

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	This submodule contains the sorting routine

SUBMODULE(Utility) SORTMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 HEAPSORT
!----------------------------------------------------------------------------

MODULE PROCEDURE HEAPSORT_Int8
  INTEGER( I4B ) :: n, i,k,j,l
  INTEGER( Int8 ) :: t
#include "./HeapSort.inc"
END PROCEDURE HEAPSORT_Int8
MODULE PROCEDURE HEAPSORT_Int16
  INTEGER( I4B ) :: n, i,k,j,l
  INTEGER( Int16 ) :: t
#include "./HeapSort.inc"
END PROCEDURE HEAPSORT_Int16
MODULE PROCEDURE HEAPSORT_Int32
  INTEGER( I4B ) :: n, i,k,j,l
  INTEGER( Int32 ) :: t
#include "./HeapSort.inc"
END PROCEDURE HEAPSORT_Int32
MODULE PROCEDURE HEAPSORT_Int64
  INTEGER( I4B ) :: n, i,k,j,l
  INTEGER( Int64 ) :: t
#include "./HeapSort.inc"
END PROCEDURE HEAPSORT_Int64

!----------------------------------------------------------------------------
!                                                                   HeapSort
!----------------------------------------------------------------------------

MODULE PROCEDURE HEAPSORT_Real32
  INTEGER( I4B ) :: n, i,k,j,l
  REAL( Real32 ) :: t
#include "./HeapSort.inc"
END PROCEDURE HEAPSORT_Real32
MODULE PROCEDURE HEAPSORT_Real64
  INTEGER( I4B ) :: n, i,k,j,l
  REAL( Real64 ) :: t
#include "./HeapSort.inc"
END PROCEDURE HEAPSORT_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


MODULE PROCEDURE quickSort1vectReal32
  INTEGER( I4B ) i, iPivot
#include "./QuickSort1Vec.inc"
END PROCEDURE quickSort1vectReal32

MODULE PROCEDURE quickSort1vectReal64
  INTEGER( I4B ) i, iPivot
#include "./QuickSort1Vec.inc"
END PROCEDURE quickSort1vectReal64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort1vectInt8
  INTEGER( I4B ) i, iPivot
#include "./QuickSort1Vec.inc"
END PROCEDURE quickSort1vectInt8
MODULE PROCEDURE quickSort1vectInt16
  INTEGER( I4B ) i, iPivot
#include "./QuickSort1Vec.inc"
END PROCEDURE quickSort1vectInt16
MODULE PROCEDURE quickSort1vectInt32
  INTEGER( I4B ) i, iPivot
#include "./QuickSort1Vec.inc"
END PROCEDURE quickSort1vectInt32
MODULE PROCEDURE quickSort1vectInt64
  INTEGER( I4B ) i, iPivot
#include "./QuickSort1Vec.inc"
END PROCEDURE quickSort1vectInt64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort2vectIR
INTEGER( I4B ) i, iPivot
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, low, iPivot-1)
  call quickSort(vect1, vect2, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort2vectII
INTEGER( I4B ) i, iPivot
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, low, iPivot-1)
  call quickSort(vect1, vect2, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort2vectRI
INTEGER( I4B ) i, iPivot
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, low, iPivot-1)
  call quickSort(vect1, vect2, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort2vectRR
INTEGER( I4B ) i, iPivot
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, low, iPivot-1)
  call quickSort(vect1, vect2, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort3vectIII
INTEGER( I4B ) i, iPivot
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort3vectIIR
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort3vectIRR
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort3vectIRI
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort3vectRRR
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort3vectRRI
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort3vectRIR
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort3vectRII
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort4vectIIII
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect4(i), vect4(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    call swap(vect4(iPivot-1), vect4(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort4vectIIIR
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect4(i), vect4(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    call swap(vect4(iPivot-1), vect4(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort4vectIIRI
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect4(i), vect4(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    call swap(vect4(iPivot-1), vect4(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort4vectIIRR
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect4(i), vect4(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    call swap(vect4(iPivot-1), vect4(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort4vectIRRR
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect4(i), vect4(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    call swap(vect4(iPivot-1), vect4(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort4vectIRRI
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect4(i), vect4(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    call swap(vect4(iPivot-1), vect4(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort4vectIRIR
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect4(i), vect4(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    call swap(vect4(iPivot-1), vect4(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort4vectIRII
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect4(i), vect4(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    call swap(vect4(iPivot-1), vect4(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort4vectRRRR
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect4(i), vect4(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    call swap(vect4(iPivot-1), vect4(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort4vectRRRI
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect4(i), vect4(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    call swap(vect4(iPivot-1), vect4(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort4vectRRIR
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect4(i), vect4(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    call swap(vect4(iPivot-1), vect4(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort4vectRRII
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect4(i), vect4(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    call swap(vect4(iPivot-1), vect4(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort4vectRIRR
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect4(i), vect4(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    call swap(vect4(iPivot-1), vect4(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort4vectRIRI
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect4(i), vect4(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    call swap(vect4(iPivot-1), vect4(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort4vectRIIR
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect4(i), vect4(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    call swap(vect4(iPivot-1), vect4(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE quickSort4vectRIII
INTEGER( I4B ) i, iPivot, j
iPivot = high
i = low
do while(iPivot > i)
  if (vect1(i) > vect1(iPivot)) then
    call swap(vect1(i), vect1(iPivot-1))
    call swap(vect2(i), vect2(iPivot-1))
    call swap(vect3(i), vect3(iPivot-1))
    call swap(vect4(i), vect4(iPivot-1))
    call swap(vect1(iPivot-1), vect1(iPivot))
    call swap(vect2(iPivot-1), vect2(iPivot))
    call swap(vect3(iPivot-1), vect3(iPivot))
    call swap(vect4(iPivot-1), vect4(iPivot))
    iPivot = iPivot - 1
  else
    i=i+1
  end if
end do
if (low < high) then
  call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
  call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
end if
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SORT_INT
  INTEGER( I4B ), DIMENSION( SIZE(x)-1 ) :: rest
  INTEGER( I4B ) :: pivot

  IF ( SIZE(x) > 1 ) THEN
    pivot = HEAD( SPLIT( x, 2 ) )
    rest = [ SPLIT( x, 1 ), TAIL( split(x, 2 ) ) ]
    ANS = [ SORT_INT( PACK( rest, rest < pivot ) ), pivot, &
            & SORT_INT( PACK( rest, rest >= pivot ) ) ]
  ELSE
    ANS = x
  ENDIF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SORT_REAL
  REAL( DFP ), DIMENSION(SIZE(x)-1) :: rest
  REAL( DFP ) :: pivot
  !
  IF( SIZE(x) > 1 ) THEN
    pivot = HEAD( SPLIT( x, 2 ) )
    rest = [ SPLIT(x, 1), TAIL(split(x, 2)) ]
    ANS = [ SORT_REAL( pack( rest, rest < pivot ) ), pivot, &
            & SORT_REAL( pack(rest, rest >= pivot ) ) ]
  ELSE
    ANS = x
  END IF
END PROCEDURE SORT_REAL

END SUBMODULE SORTMethods
