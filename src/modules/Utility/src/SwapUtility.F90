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

MODULE SwapUtility
USE GlobalData, ONLY: INT8, INT16, INT32, INT64, REAL32, REAL64, &
                      DFPC, LGT, I4B

#ifdef USE_BLAS95
USE F95_BLAS, ONLY: SWAP
#endif

IMPLICIT NONE

PRIVATE

PUBLIC :: Swap
PUBLIC :: Swap_

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Swap two integer

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_Int8(a, b)
    INTEGER(INT8), INTENT(INOUT) :: a, b
  END SUBROUTINE Swap_Int8
END INTERFACE swap

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary:  Swap two integer

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_Int16(a, b)
    INTEGER(INT16), INTENT(INOUT) :: a, b
  END SUBROUTINE Swap_Int16
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: Swap two integer

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_Int32(a, b)
    INTEGER(INT32), INTENT(INOUT) :: a, b
  END SUBROUTINE Swap_Int32
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: Swap two integer

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_Int64(a, b)
    INTEGER(INT64), INTENT(INOUT) :: a, b
  END SUBROUTINE Swap_Int64
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Swap two real

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_r32(a, b)
    REAL(REAL32), INTENT(INOUT) :: a, b
  END SUBROUTINE Swap_r32
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Swap two real

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_r64(a, b)
    REAL(REAL64), INTENT(INOUT) :: a, b
  END SUBROUTINE Swap_r64
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Swap two vectors of real, if blas95 is used then ignore it.

#ifndef USE_BLAS95
INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_r32v(a, b)
    REAL(REAL32), INTENT(INOUT) :: a(:), b(:)
  END SUBROUTINE Swap_r32v
END INTERFACE Swap
#endif

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Swap two vectors of real, if blas95 is used then ignore it.

#ifndef USE_BLAS95
INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_r64v(a, b)
    REAL(REAL64), INTENT(INOUT) :: a(:), b(:)
  END SUBROUTINE Swap_r64v
END INTERFACE Swap
#endif

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-27
! summary:  Swap two integer vectors

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_Int8v(a, b)
    INTEGER(INT8), INTENT(INOUT) :: a(:), b(:)
  END SUBROUTINE Swap_Int8v
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Swap two vectors of real, if blas95 is used then ignore it.

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_Int16v(a, b)
    INTEGER(INT16), INTENT(INOUT) :: a(:), b(:)
  END SUBROUTINE Swap_Int16v
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Swap two vectors of real, if blas95 is used then ignore it.

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_Int32v(a, b)
    INTEGER(INT32), INTENT(INOUT) :: a(:), b(:)
  END SUBROUTINE Swap_Int32v
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Swap two vectors of real, if blas95 is used then ignore it.

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_Int64v(a, b)
    INTEGER(INT64), INTENT(INOUT) :: a(:), b(:)
  END SUBROUTINE Swap_Int64v
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Swap two vectors of real, if blas95 is used then ignore it.

#ifdef USE_Int128
INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_Int128v(a, b)
    INTEGER(INT128), INTENT(INOUT) :: a(:), b(:)
  END SUBROUTINE Swap_Int128v
END INTERFACE Swap
#endif

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Subroutine for interchanging two complex numbers

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_c(a, b)
    COMPLEX(DFPC), INTENT(INOUT) :: a, b
  END SUBROUTINE Swap_c
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Swap two vectors of complex numbers, if blas95 is used ignore it.

#ifndef USE_BLAS95
INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_cv(a, b)
    COMPLEX(DFPC), INTENT(INOUT) :: a(:), b(:)
  END SUBROUTINE Swap_cv
END INTERFACE Swap
#endif

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Swap two matrix

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_cm(a, b)
    COMPLEX(DFPC), INTENT(INOUT) :: a(:, :), b(:, :)
  END SUBROUTINE Swap_cm
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Swap two matrix of real numbers

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_r32m(a, b)
    REAL(REAL32), INTENT(INOUT) :: a(:, :), b(:, :)
  END SUBROUTINE Swap_r32m
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary:  Swap two real matrix

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_r64m(a, b)
    REAL(REAL64), INTENT(INOUT) :: a(:, :), b(:, :)
  END SUBROUTINE Swap_r64m
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-27
! summary:  Swap two integer matrix

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_Int8m(a, b)
    INTEGER(INT8), INTENT(INOUT) :: a(:, :), b(:, :)
  END SUBROUTINE Swap_Int8m
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary:  Swap two matrix

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_Int16m(a, b)
    INTEGER(INT16), INTENT(INOUT) :: a(:, :), b(:, :)
  END SUBROUTINE Swap_Int16m
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary:  Swap two matrix

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_Int32m(a, b)
    INTEGER(INT32), INTENT(INOUT) :: a(:, :), b(:, :)
  END SUBROUTINE Swap_Int32m
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary:  Swap two matrix

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_Int64m(a, b)
    INTEGER(INT64), INTENT(INOUT) :: a(:, :), b(:, :)
  END SUBROUTINE Swap_Int64m
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: Swap two matrix

#ifdef USE_Int128
INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_Int128m(a, b)
    INTEGER(Int128), INTENT(INOUT) :: a(:, :), b(:, :)
  END SUBROUTINE Swap_Int128m
END INTERFACE Swap
#endif

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Swap two scalars with masking

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_r32s(a, b, mask)
    REAL(REAL32), INTENT(INOUT) :: a, b
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE masked_Swap_r32s
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-27
! summary:  Swap two scalars with masking

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_r64s(a, b, mask)
    REAL(REAL64), INTENT(INOUT) :: a, b
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE masked_Swap_r64s
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-27
! summary:  Swap two scalars with masking

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_Int8s(a, b, mask)
    INTEGER(INT8), INTENT(INOUT) :: a, b
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE masked_Swap_Int8s
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-27
! summary:  Swap two scalars with masking

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_Int16s(a, b, mask)
    INTEGER(INT16), INTENT(INOUT) :: a, b
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE masked_Swap_Int16s
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-27
! summary:  Swap two scalars with masking

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_Int32s(a, b, mask)
    INTEGER(INT32), INTENT(INOUT) :: a, b
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE masked_Swap_Int32s
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-27
! summary:  Swap two scalars with masking

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_Int64s(a, b, mask)
    INTEGER(INT64), INTENT(INOUT) :: a, b
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE masked_Swap_Int64s
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-27
! summary:  Swap two scalars with masking

#ifdef USE_Int128
INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_Int128s(a, b, mask)
    INTEGER(Int128), INTENT(INOUT) :: a, b
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE masked_Swap_Int128s
END INTERFACE Swap
#endif

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-27
! summary:  Swap two vectors with masking

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_r32v(a, b, mask)
    REAL(REAL32), INTENT(INOUT) :: a(:), b(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE masked_Swap_r32v
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                           Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-27
! summary:  Swap two vectors with masking

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_r64v(a, b, mask)
    REAL(REAL64), INTENT(INOUT) :: a(:), b(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE masked_Swap_r64v
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                           Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-27
! summary:  Swap two vectors with masking

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_Int8v(a, b, mask)
    INTEGER(INT8), INTENT(INOUT) :: a(:), b(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE masked_Swap_Int8v
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: Swap two vectors with masking

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_Int16v(a, b, mask)
    INTEGER(INT16), INTENT(INOUT) :: a(:), b(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE masked_Swap_Int16v
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: Swap two vectors with masking

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_Int32v(a, b, mask)
    INTEGER(INT32), INTENT(INOUT) :: a(:), b(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE masked_Swap_Int32v
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: Swap two vectors with masking

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_Int64v(a, b, mask)
    INTEGER(INT64), INTENT(INOUT) :: a(:), b(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE masked_Swap_Int64v
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-27
! summary:  Swap two vectors with masking

#ifdef USE_Int128
INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_Int128v(a, b, mask)
    INTEGER(Int128), INTENT(INOUT) :: a(:), b(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE masked_Swap_Int128v
END INTERFACE Swap
#endif

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-27
! summary:  Swap two matrices with masking

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_r32m(a, b, mask)
    REAL(REAL32), INTENT(INOUT) :: a(:, :), b(:, :)
    LOGICAL(LGT), INTENT(IN) :: mask(:, :)
  END SUBROUTINE masked_Swap_r32m
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-27
! summary:  Swap two matrices with masking

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_r64m(a, b, mask)
    REAL(REAL64), INTENT(INOUT) :: a(:, :), b(:, :)
    LOGICAL(LGT), INTENT(IN) :: mask(:, :)
  END SUBROUTINE masked_Swap_r64m
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-27
! summary:  Swap two matrices with masking

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_Int8m(a, b, mask)
    INTEGER(INT8), INTENT(INOUT) :: a(:, :), b(:, :)
    LOGICAL(LGT), INTENT(IN) :: mask(:, :)
  END SUBROUTINE masked_Swap_Int8m
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: Swap two matrices with masking

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_Int16m(a, b, mask)
    INTEGER(INT16), INTENT(INOUT) :: a(:, :), b(:, :)
    LOGICAL(LGT), INTENT(IN) :: mask(:, :)
  END SUBROUTINE masked_Swap_Int16m
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: Swap two matrices with masking

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_Int32m(a, b, mask)
    INTEGER(INT32), INTENT(INOUT) :: a(:, :), b(:, :)
    LOGICAL(LGT), INTENT(IN) :: mask(:, :)
  END SUBROUTINE masked_Swap_Int32m
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                               Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: Swap two matrices with masking

INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_Int64m(a, b, mask)
    INTEGER(INT64), INTENT(INOUT) :: a(:, :), b(:, :)
    LOGICAL(LGT), INTENT(IN) :: mask(:, :)
  END SUBROUTINE masked_Swap_Int64m
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-27
! summary:  Swap two matrices with masking

#ifdef USE_Int128
INTERFACE Swap
  MODULE PURE SUBROUTINE masked_Swap_Int128m(a, b, mask)
    INTEGER(Int128), INTENT(INOUT) :: a(:, :), b(:, :)
    LOGICAL(LGT), INTENT(IN) :: mask(:, :)
  END SUBROUTINE masked_Swap_Int128m
END INTERFACE Swap
#endif

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-20
! summary: Swap the index, it is like taking transpose.
!
!# Introduction
!
! - This routine returns an matrix by chaning the dimensions of input matrix
! `b`.
! - This routine does not check the shape, so make sure the shape of
! `a` and `b` are appropriate,.
!

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_index1(a, b, i1, i2)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: a(:, :)
      !! the returned array
    REAL(REAL32), INTENT(IN) :: b(:, :)
      !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
      !! index 1 is Swapped with index `i1`
      !! make sure i1 is lesser than or equal to 2
    INTEGER(I4B), INTENT(IN) :: i2
      !! index 2 is Swapped with index `i2`
      !! make sure i2 is less than or equal to 2
  END SUBROUTINE Swap_index1
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-20
! update: 2021-11-20
! summary: Swap the index, it is like taking transpose.
!
!# Introduction
!
! - This routine returns an matrix by chaning the dimensions of input matrix
! `b`.
! - This routine does not check the shape, so make sure the shape of
! `a` and `b` are appropriate,.
!

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_index2(a, b, i1, i2)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: a(:, :)
    !! the returned array
    REAL(REAL64), INTENT(IN) :: b(:, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! index 1 is Swapped with index `i1`
    !! make sure i1 is lesser than or equal to 2
    INTEGER(I4B), INTENT(IN) :: i2
    !! index 2 is Swapped with index `i2`
    !! make sure i2 is less than or equal to 2
  END SUBROUTINE Swap_index2
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-20
! summary: Swap the index, it is like taking transpose.
!
!# Introduction
!
! - This routine returns an matrix by chaning the dimensions of input matrix
! `b`.
! - This routine does not check the shape, so make sure the shape of
! `a` and `b` are appropriate,.
!

INTERFACE Swap_
  MODULE PURE SUBROUTINE Swap_index_1(a, b, i1, i2)
    REAL(REAL32), INTENT(INOUT) :: a(:, :)
      !! the returned array
    REAL(REAL32), INTENT(IN) :: b(:, :)
      !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
      !! index 1 is Swapped with index `i1`
      !! make sure i1 is lesser than or equal to 2
    INTEGER(I4B), INTENT(IN) :: i2
      !! index 2 is Swapped with index `i2`
      !! make sure i2 is less than or equal to 2
  END SUBROUTINE Swap_index_1
END INTERFACE Swap_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE Swap_
  MODULE PURE SUBROUTINE Swap_index_2(a, b, i1, i2)
    REAL(REAL64), INTENT(INOUT) :: a(:, :)
      !! the returned array
    REAL(REAL64), INTENT(IN) :: b(:, :)
      !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
      !! index 1 is Swapped with index `i1`
      !! make sure i1 is lesser than or equal to 2
    INTEGER(I4B), INTENT(IN) :: i2
      !! index 2 is Swapped with index `i2`
      !! make sure i2 is less than or equal to 2
  END SUBROUTINE Swap_index_2
END INTERFACE Swap_

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-20
! update: 2021-11-20
! summary: Swap the index, it is like taking transpose.
!
!# Introduction
!
! - This routine returns an matrix by chaning the dimensions of input matrix
! `b`.
! - This routine does not check the shape, so make sure the shape of
! `a` and `b` are appropriate,.
!

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_index3(a, b, i1, i2, i3)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: a(:, :, :)
    !! the returned array
    REAL(REAL32), INTENT(IN) :: b(:, :, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! index 1 is Swapped with index `i1`
    !! make sure i1 is lesser than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i2
    !! index 2 is Swapped with index `i2`
    !! make sure i2 is less than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i3
    !! index 3 is Swapped with index `i3`
    !! make sure i3 is less than or equal to 3
  END SUBROUTINE Swap_index3
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-20
! update: 2021-11-20
! summary: Swap the index, it is like taking transpose.
!
!# Introduction
!
! - This routine returns an matrix by chaning the dimensions of input matrix
! `b`.
! - This routine does not check the shape, so make sure the shape of
! `a` and `b` are appropriate,.
!

INTERFACE Swap_
  MODULE PURE SUBROUTINE Swap_index_3(a, b, i1, i2, i3)
    REAL(REAL32), INTENT(INOUT) :: a(:, :, :)
    !! the returned array
    REAL(REAL32), INTENT(IN) :: b(:, :, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! index 1 is Swapped with index `i1`
    !! make sure i1 is lesser than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i2
    !! index 2 is Swapped with index `i2`
    !! make sure i2 is less than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i3
    !! index 3 is Swapped with index `i3`
    !! make sure i3 is less than or equal to 3
  END SUBROUTINE Swap_index_3
END INTERFACE Swap_

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-20
! update: 2021-11-20
! summary: Swap the index, it is like taking transpose.
!
!# Introduction
!
! - This routine returns an matrix by chaning the dimensions of input matrix
! `b`.
! - This routine does not check the shape, so make sure the shape of
! `a` and `b` are appropriate,.

INTERFACE Swap_
  MODULE PURE SUBROUTINE Swap_index_4(a, b, i1, i2, i3)
    REAL(REAL64), INTENT(INOUT) :: a(:, :, :)
    !! the returned array
    REAL(REAL64), INTENT(IN) :: b(:, :, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! index 1 is Swapped with index `i1`
    !! make sure i1 is lesser than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i2
    !! index 2 is Swapped with index `i2`
    !! make sure i2 is less than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i3
    !! index 3 is Swapped with index `i3`
    !! make sure i3 is less than or equal to 3
  END SUBROUTINE Swap_index_4
END INTERFACE Swap_

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-20
! update: 2021-11-20
! summary: Swap the index, it is like taking transpose.
!
!# Introduction
!
! - This routine returns an matrix by chaning the dimensions of input matrix
! `b`.
! - This routine does not check the shape, so make sure the shape of
! `a` and `b` are appropriate,.

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_index4(a, b, i1, i2, i3)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: a(:, :, :)
    !! the returned array
    REAL(REAL64), INTENT(IN) :: b(:, :, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! index 1 is Swapped with index `i1`
    !! make sure i1 is lesser than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i2
    !! index 2 is Swapped with index `i2`
    !! make sure i2 is less than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i3
    !! index 3 is Swapped with index `i3`
    !! make sure i3 is less than or equal to 3
  END SUBROUTINE Swap_index4
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-20
! update: 2021-11-20
! summary: Swap the index, it is like taking transpose.
!
!# Introduction
!
! - This routine returns an matrix by chaning the dimensions of input matrix
! `b`.
! - This routine does not check the shape, so make sure the shape of
! `a` and `b` are appropriate,.

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_index5(a, b, i1, i2, i3, i4)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: a(:, :, :, :)
    !! the returned array
    REAL(REAL64), INTENT(IN) :: b(:, :, :, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! index 1 is Swapped with index `i1`
    !! make sure i1 is lesser than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i2
    !! index 2 is Swapped with index `i2`
    !! make sure i2 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i3
    !! index 3 is Swapped with index `i3`
    !! make sure i3 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i4
    !! index 4 is Swapped with index `i4`
    !! make sure i4 is less than or equal to 4
  END SUBROUTINE Swap_index5
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-20
! update: 2021-11-20
! summary: Swap the index, it is like taking transpose.
!
!# Introduction
!
! - This routine returns an matrix by chaning the dimensions of input matrix
! `b`.
! - This routine does not check the shape, so make sure the shape of
! `a` and `b` are appropriate,.

INTERFACE Swap
  MODULE PURE SUBROUTINE Swap_index6(a, b, i1, i2, i3, i4)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: a(:, :, :, :)
    !! the returned array
    REAL(REAL32), INTENT(IN) :: b(:, :, :, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! index 1 is Swapped with index `i1`
    !! make sure i1 is lesser than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i2
    !! index 2 is Swapped with index `i2`
    !! make sure i2 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i3
    !! index 3 is Swapped with index `i3`
    !! make sure i3 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i4
    !! index 4 is Swapped with index `i4`
    !! make sure i4 is less than or equal to 4
  END SUBROUTINE Swap_index6
END INTERFACE Swap

!----------------------------------------------------------------------------
!                                                           Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-20
! update: 2021-11-20
! summary: Swap the index, it is like taking transpose.
!
!# Introduction
!
! - This routine returns an matrix by chaning the dimensions of input matrix
! `b`.
! - This routine does not check the shape, so make sure the shape of
! `a` and `b` are appropriate,.

INTERFACE Swap_
  MODULE PURE SUBROUTINE Swap_index_5(a, b, i1, i2, i3, i4)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: a(:, :, :, :)
    !! the returned array
    REAL(REAL32), INTENT(IN) :: b(:, :, :, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! index 1 is Swapped with index `i1`
    !! make sure i1 is lesser than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i2
    !! index 2 is Swapped with index `i2`
    !! make sure i2 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i3
    !! index 3 is Swapped with index `i3`
    !! make sure i3 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i4
    !! index 4 is Swapped with index `i4`
    !! make sure i4 is less than or equal to 4
  END SUBROUTINE Swap_index_5
END INTERFACE Swap_

!----------------------------------------------------------------------------
!                                                                Swap@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-20
! summary: Swap the index, it is like taking transpose.

INTERFACE Swap_
  MODULE PURE SUBROUTINE Swap_index_6(a, b, i1, i2, i3, i4)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: a(:, :, :, :)
    !! the returned array
    REAL(REAL64), INTENT(IN) :: b(:, :, :, :)
    !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
    !! index 1 is Swapped with index `i1`
    !! make sure i1 is lesser than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i2
    !! index 2 is Swapped with index `i2`
    !! make sure i2 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i3
    !! index 3 is Swapped with index `i3`
    !! make sure i3 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i4
    !! index 4 is Swapped with index `i4`
    !! make sure i4 is less than or equal to 4
  END SUBROUTINE Swap_index_6
END INTERFACE Swap_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE SwapUtility
