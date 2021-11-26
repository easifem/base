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
!                                                           SWAP@SWAPMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Swap two integer

INTERFACE
  MODULE PURE SUBROUTINE swap_i(a, b)
    INTEGER(I4B), INTENT(INOUT) :: a, b
  END SUBROUTINE swap_i
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE swap_i
END INTERFACE

PUBLIC :: SWAP

!----------------------------------------------------------------------------
!                                                            SWAP@SWAPMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Swap two real

INTERFACE
  MODULE PURE SUBROUTINE swap_r32(a, b)
    REAL(Real32), INTENT(INOUT) :: a, b
  END SUBROUTINE swap_r32
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE swap_r32
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SWAP@SWAPMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Swap two real

INTERFACE
  MODULE PURE SUBROUTINE swap_r64(a, b)
    REAL(Real64), INTENT(INOUT) :: a, b
  END SUBROUTINE swap_r64
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE swap_r64
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SWAP@SWAPMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Swap two vectors of real, if blas95 is used then ignore it.

#ifndef USE_BLAS95
INTERFACE
  MODULE PURE SUBROUTINE swap_r32v(a, b)
    REAL(Real32), INTENT(INOUT) :: a(:), b(:)
  END SUBROUTINE swap_r32v
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE swap_r32v
END INTERFACE SWAP
#endif

!----------------------------------------------------------------------------
!                                                            SWAP@SWAPMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Swap two vectors of real

#ifndef USE_BLAS95
INTERFACE
  MODULE PURE SUBROUTINE swap_r64v(a, b)
    REAL(Real64), INTENT(INOUT) :: a(:), b(:)
  END SUBROUTINE swap_r64v
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE swap_r64v
END INTERFACE SWAP
#endif

!----------------------------------------------------------------------------
!                                                            SWAP@SWAPMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Subroutine for interchanging two complex numbers

INTERFACE
  MODULE PURE SUBROUTINE swap_c(a, b)
    COMPLEX(DFPC), INTENT(INOUT) :: a, b
  END SUBROUTINE swap_c
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE swap_c
END INTERFACE

!----------------------------------------------------------------------------
!                                                           SWAP@SWAPMethods
!----------------------------------------------------------------------------

#ifndef USE_BLAS95
INTERFACE
  MODULE PURE SUBROUTINE swap_cv(a, b)
    COMPLEX(DFPC), INTENT(INOUT) :: a(:), b(:)
  END SUBROUTINE swap_cv
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE swap_cv
END INTERFACE SWAP
#endif

!----------------------------------------------------------------------------
!                                                           SWAP@SWAPMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE swap_cm(a, b)
    COMPLEX(DFPC), INTENT(INOUT) :: a(:, :), b(:, :)
  END SUBROUTINE swap_cm
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE swap_cm
END INTERFACE

!----------------------------------------------------------------------------
!                                                           SWAP@SWAPMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE masked_swap_r32s(a, b, mask)
    REAL(Real32), INTENT(INOUT) :: a, b
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE masked_swap_r32s
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE masked_swap_r32s
END INTERFACE

!----------------------------------------------------------------------------
!                                                           SWAP@SWAPMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE masked_swap_r64s(a, b, mask)
    REAL(Real64), INTENT(INOUT) :: a, b
    LOGICAL(LGT), INTENT(IN) :: mask
  END SUBROUTINE masked_swap_r64s
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE masked_swap_r64s
END INTERFACE

!----------------------------------------------------------------------------
!                                                           SWAP@SWAPMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE masked_swap_r32v(a, b, mask)
    REAL(Real32), INTENT(INOUT) :: a(:), b(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE masked_swap_r32v
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE masked_swap_r32v
END INTERFACE

!----------------------------------------------------------------------------
!                                                           SWAP@SWAPMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE masked_swap_r64v(a, b, mask)
    REAL(Real64), INTENT(INOUT) :: a(:), b(:)
    LOGICAL(LGT), INTENT(IN) :: mask(:)
  END SUBROUTINE masked_swap_r64v
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE masked_swap_r64v
END INTERFACE

!----------------------------------------------------------------------------
!                                                           SWAP@SWAPMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE masked_swap_r32m(a, b, mask)
    REAL(Real32), INTENT(INOUT) :: a(:, :), b(:, :)
    LOGICAL(LGT), INTENT(IN) :: mask(:, :)
  END SUBROUTINE masked_swap_r32m
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE masked_swap_r32m
END INTERFACE

!----------------------------------------------------------------------------
!                                                           SWAP@SWAPMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE masked_swap_r64m(a, b, mask)
    REAL(Real64), INTENT(INOUT) :: a(:, :), b(:, :)
    LOGICAL(LGT), INTENT(IN) :: mask(:, :)
  END SUBROUTINE masked_swap_r64m
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE masked_swap_r64m
END INTERFACE

!----------------------------------------------------------------------------
!                                                           SWAP@SWAPMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
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

INTERFACE
  MODULE PURE SUBROUTINE swap_index1(a, b, i1, i2)
    REAL(Real32), ALLOCATABLE, INTENT(INOUT) :: a(:, :)
     !! the returned array
    REAL(Real32), INTENT(IN) :: b(:, :)
     !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
     !! index 1 is swapped with index `i1`
     !! make sure i1 is lesser than or equal to 2
    INTEGER(I4B), INTENT(IN) :: i2
     !! index 2 is swapped with index `i2`
     !! make sure i2 is less than or equal to 2
  END SUBROUTINE swap_index1
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE swap_index1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           SWAP@SWAPMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
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

INTERFACE
  MODULE PURE SUBROUTINE swap_index2(a, b, i1, i2)
    REAL(Real64), ALLOCATABLE, INTENT(INOUT) :: a(:, :)
     !! the returned array
    REAL(Real64), INTENT(IN) :: b(:, :)
     !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
     !! index 1 is swapped with index `i1`
     !! make sure i1 is lesser than or equal to 2
    INTEGER(I4B), INTENT(IN) :: i2
     !! index 2 is swapped with index `i2`
     !! make sure i2 is less than or equal to 2
  END SUBROUTINE swap_index2
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE swap_index2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           SWAP@SWAPMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
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

INTERFACE
  MODULE PURE SUBROUTINE swap_index3(a, b, i1, i2, i3)
    REAL(Real32), ALLOCATABLE, INTENT(INOUT) :: a(:, :, :)
     !! the returned array
    REAL(Real32), INTENT(IN) :: b(:, :, :)
     !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
     !! index 1 is swapped with index `i1`
     !! make sure i1 is lesser than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i2
     !! index 2 is swapped with index `i2`
     !! make sure i2 is less than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i3
     !! index 3 is swapped with index `i3`
     !! make sure i3 is less than or equal to 3
  END SUBROUTINE swap_index3
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE swap_index3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           SWAP@SWAPMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
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

INTERFACE
  MODULE PURE SUBROUTINE swap_index4(a, b, i1, i2, i3)
    REAL(Real64), ALLOCATABLE, INTENT(INOUT) :: a(:, :, :)
     !! the returned array
    REAL(Real64), INTENT(IN) :: b(:, :, :)
     !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
     !! index 1 is swapped with index `i1`
     !! make sure i1 is lesser than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i2
     !! index 2 is swapped with index `i2`
     !! make sure i2 is less than or equal to 3
    INTEGER(I4B), INTENT(IN) :: i3
     !! index 3 is swapped with index `i3`
     !! make sure i3 is less than or equal to 3
  END SUBROUTINE swap_index4
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE swap_index4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           SWAP@SWAPMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
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

INTERFACE
  MODULE PURE SUBROUTINE swap_index5(a, b, i1, i2, i3, i4)
    REAL(Real64), ALLOCATABLE, INTENT(INOUT) :: a(:, :, :, :)
     !! the returned array
    REAL(Real64), INTENT(IN) :: b(:, :, :, :)
     !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
     !! index 1 is swapped with index `i1`
     !! make sure i1 is lesser than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i2
     !! index 2 is swapped with index `i2`
     !! make sure i2 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i3
     !! index 3 is swapped with index `i3`
     !! make sure i3 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i4
     !! index 4 is swapped with index `i4`
     !! make sure i4 is less than or equal to 4
  END SUBROUTINE swap_index5
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE swap_index5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           SWAP@SWAPMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
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

INTERFACE
  MODULE PURE SUBROUTINE swap_index6(a, b, i1, i2, i3, i4)
    REAL(Real32), ALLOCATABLE, INTENT(INOUT) :: a(:, :, :, :)
     !! the returned array
    REAL(Real32), INTENT(IN) :: b(:, :, :, :)
     !! input array, it will be untouched
    INTEGER(I4B), INTENT(IN) :: i1
     !! index 1 is swapped with index `i1`
     !! make sure i1 is lesser than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i2
     !! index 2 is swapped with index `i2`
     !! make sure i2 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i3
     !! index 3 is swapped with index `i3`
     !! make sure i3 is less than or equal to 4
    INTEGER(I4B), INTENT(IN) :: i4
     !! index 4 is swapped with index `i4`
     !! make sure i4 is less than or equal to 4
  END SUBROUTINE swap_index6
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE swap_index6
END INTERFACE