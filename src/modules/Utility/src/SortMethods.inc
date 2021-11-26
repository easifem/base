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


!----------------------------------------------------------------------------
!                                                              HeapSort@Sort
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Heap sort

INTERFACE
  MODULE PURE SUBROUTINE HEAPSORT_INT(array)
    INTEGER(I4B), INTENT(INOUT) :: array(:)
  END SUBROUTINE HEAPSORT_INT
END INTERFACE

!----------------------------------------------------------------------------
!                                                             HeapSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE HEAPSORT_REAL(array)
    REAL(DFP), INTENT(INOUT) :: array(:)
  END SUBROUTINE HEAPSORT_REAL
END INTERFACE

INTERFACE HeapSort
  MODULE PROCEDURE HEAPSORT_INT, HEAPSORT_REAL
END INTERFACE HeapSort

PUBLIC :: HeapSort

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE RECURSIVE SUBROUTINE quickSort1vectR(vect1, low, high)
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE quickSort1vectR
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE RECURSIVE SUBROUTINE quickSort1vectI(vect1, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE quickSort1vectI
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE RECURSIVE SUBROUTINE quickSort2vectIR(vect1, vect2, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE RECURSIVE SUBROUTINE quickSort2vectII(vect1, vect2, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE RECURSIVE SUBROUTINE quickSort2vectRI(vect1, vect2, low, high)
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
  MODULE RECURSIVE SUBROUTINE quickSort2vectRR(vect1, vect2, low, high)
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
 MODULE RECURSIVE SUBROUTINE quickSort3vectIII(vect1, vect2, vect3, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
 MODULE RECURSIVE SUBROUTINE quickSort3vectIIR(vect1, vect2, vect3, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect2
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
 MODULE RECURSIVE SUBROUTINE quickSort3vectIRR(vect1, vect2, vect3, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
 MODULE RECURSIVE SUBROUTINE quickSort3vectIRI(vect1, vect2, vect3, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
 MODULE RECURSIVE SUBROUTINE quickSort3vectRRR(vect1, vect2, vect3, low, high)
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
 MODULE RECURSIVE SUBROUTINE quickSort3vectRRI(vect1, vect2, vect3, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
 MODULE RECURSIVE SUBROUTINE quickSort3vectRIR(vect1, vect2, vect3, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
 MODULE RECURSIVE SUBROUTINE quickSort3vectRII(vect1, vect2, vect3, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2, vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectIIII(vect1, vect2, vect3, vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectIIIR(vect1, vect2, vect3, vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectIIRI(vect1, vect2, vect3, vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectIIRR(vect1, vect2, vect3, vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect2
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect3, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectIRRR(vect1, vect2, vect3, vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2, vect3, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectIRRI(vect1, vect2, vect3, vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectIRIR(vect1, vect2, vect3, vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectIRII(vect1, vect2, vect3, vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect1, vect3, vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectRRRR(vect1, vect2, vect3, vect4, low, high)
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectRRRI(vect1, vect2, vect3, vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectRRIR(vect1, vect2, vect3, vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect2, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectRRII(vect1, vect2, vect3, vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect3, vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect2
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectRIRR(vect1, vect2, vect3, vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect3, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectRIRI(vect1, vect2, vect3, vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2, vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect3
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectRIIR(vect1, vect2, vect3, vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2, vect3
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1, vect4
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             QuickSort@Sort
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE quickSort4vectRIII(vect1, vect2, vect3, vect4, low, high)
    INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: vect2, vect3, vect4
    REAL(DFP), DIMENSION(:), INTENT(INOUT) :: vect1
    INTEGER(I4B), INTENT(IN) :: low, high
  END SUBROUTINE
END INTERFACE

INTERFACE QUICKSORT
  MODULE PROCEDURE quickSort1vectI, quickSort1vectR, quickSort2vectII, &
    & quickSort2vectIR, quickSort2vectRR, quickSort2vectRI, &
    & quickSort3vectIII, quickSort3vectIIR, quickSort3vectIRI, &
    & quickSort3vectIRR, quickSort3vectRRR, quickSort3vectRRI, &
    & quickSort3vectRIR, quickSort3vectRII, quickSort4vectIIII, &
    & quickSort4vectIIIR, quickSort4vectIIRI, quickSort4vectIIRR, &
    & quickSort4vectIRII, quickSort4vectIRIR, quickSort4vectIRRI, &
    & quickSort4vectIRRR, quickSort4vectRIII, quickSort4vectRIIR, &
    & quickSort4vectRIRI, quickSort4vectRIRR, quickSort4vectRRII, &
    & quickSort4vectRRIR, quickSort4vectRRRI, quickSort4vectRRRR
END INTERFACE QUICKSORT

PUBLIC :: QUICKSORT

!----------------------------------------------------------------------------
!                                                                 SORT@SORT
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Recursive quicksort using binary tree pivot.

INTERFACE
  MODULE PURE RECURSIVE FUNCTION SORT_INT(x) RESULT(Ans)
    INTEGER(I4B), DIMENSION(:), INTENT(IN) :: x
    INTEGER(I4B), DIMENSION(SIZE(x)) :: Ans
  END FUNCTION SORT_INT
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SORT@SORT
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Recursive quicksort using binary tree pivot.

INTERFACE
  MODULE PURE RECURSIVE FUNCTION SORT_REAL(x) RESULT(Ans)
    REAL(DFP), DIMENSION(:), INTENT(IN) :: x
    REAL(DFP), DIMENSION(SIZE(x)) :: Ans
  END FUNCTION SORT_REAL
END INTERFACE

INTERFACE SORT
  MODULE PROCEDURE SORT_INT, SORT_REAL
END INTERFACE SORT

PUBLIC :: SORT