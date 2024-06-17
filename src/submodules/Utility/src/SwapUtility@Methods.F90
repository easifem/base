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
! summary:         This submodule contains method for swaping

SUBMODULE(SwapUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_Int8
INTEGER(INT8) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_Int8

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_Int16
INTEGER(INT16) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_Int16

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_Int32
INTEGER(INT32) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_Int32

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_Int64
INTEGER(INT64) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_Int64

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_r32
REAL(REAL32) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_r32

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_r64
REAL(REAL64) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_r64

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

#ifndef USE_BLAS95
MODULE PROCEDURE swap_r32v
REAL(REAL32), DIMENSION(SIZE(a)) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_r32v

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_r64v
REAL(REAL64), DIMENSION(SIZE(a)) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_r64v
#endif

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_Int8v
INTEGER(INT8), DIMENSION(SIZE(a)) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_Int8v

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_Int16v
INTEGER(INT16), DIMENSION(SIZE(a)) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_Int16v

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_Int32v
INTEGER(INT32), DIMENSION(SIZE(a)) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_Int32v

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_Int64v
INTEGER(INT64), DIMENSION(SIZE(a)) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_Int64v

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

#ifdef USE_Int128
MODULE PROCEDURE swap_Int128v
INTEGER(Int128), DIMENSION(SIZE(a)) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_Int128v
#endif

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_c
COMPLEX(DFPC) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_c

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

#ifndef USE_BLAS95
MODULE PROCEDURE swap_cv
COMPLEX(DFPC), DIMENSION(SIZE(a)) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_cv
#endif

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_cm
COMPLEX(DFPC), DIMENSION(SIZE(a, 1), SIZE(a, 2)) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_cm

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_r32m
REAL(REAL32), DIMENSION(SIZE(a, 1), SIZE(a, 2)) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_r32m

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_r64m
REAL(REAL64), DIMENSION(SIZE(a, 1), SIZE(a, 2)) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_r64m

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_Int8m
INTEGER(INT8), DIMENSION(SIZE(a, 1), SIZE(a, 2)) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_Int8m

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_Int16m
INTEGER(INT16), DIMENSION(SIZE(a, 1), SIZE(a, 2)) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_Int16m

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_Int32m
INTEGER(INT32), DIMENSION(SIZE(a, 1), SIZE(a, 2)) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_Int32m

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_Int64m
INTEGER(INT64), DIMENSION(SIZE(a, 1), SIZE(a, 2)) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_Int64m

!----------------------------------------------------------------------------
!                                                                     SWAP
!----------------------------------------------------------------------------

#ifdef USE_Int128
MODULE PROCEDURE swap_Int128m
INTEGER(Int128), DIMENSION(SIZE(a, 1), SIZE(a, 2)) :: dum
dum = a
a = b
b = dum
END PROCEDURE swap_Int128m
#endif

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_r32s
REAL(REAL32) :: swp
IF (mask) THEN
  swp = a
  a = b
  b = swp
END IF
END PROCEDURE masked_swap_r32s

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_r64s
REAL(REAL64) :: swp
IF (mask) THEN
  swp = a
  a = b
  b = swp
END IF
END PROCEDURE masked_swap_r64s

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_Int8s
INTEGER(INT8) :: swp
IF (mask) THEN
  swp = a
  a = b
  b = swp
END IF
END PROCEDURE masked_swap_Int8s

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_Int16s
INTEGER(INT16) :: swp
IF (mask) THEN
  swp = a
  a = b
  b = swp
END IF
END PROCEDURE masked_swap_Int16s

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_Int32s
INTEGER(INT32) :: swp
IF (mask) THEN
  swp = a
  a = b
  b = swp
END IF
END PROCEDURE masked_swap_Int32s

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_Int64s
INTEGER(INT64) :: swp
IF (mask) THEN
  swp = a
  a = b
  b = swp
END IF
END PROCEDURE masked_swap_Int64s

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

#ifdef USE_Int128
MODULE PROCEDURE masked_swap_Int128s
INTEGER(Int128) :: swp
IF (mask) THEN
  swp = a
  a = b
  b = swp
END IF
END PROCEDURE masked_swap_Int128s
#endif

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_r32v
REAL(REAL32), DIMENSION(SIZE(a)) :: swp
WHERE (mask)
  swp = a
  a = b
  b = swp
END WHERE
END PROCEDURE masked_swap_r32v

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_r64v
REAL(REAL64), DIMENSION(SIZE(a)) :: swp
WHERE (mask)
  swp = a
  a = b
  b = swp
END WHERE
END PROCEDURE masked_swap_r64v

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_Int8v
INTEGER(INT8), DIMENSION(SIZE(a)) :: swp
WHERE (mask)
  swp = a
  a = b
  b = swp
END WHERE
END PROCEDURE masked_swap_Int8v

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_Int16v
INTEGER(INT16), DIMENSION(SIZE(a)) :: swp
WHERE (mask)
  swp = a
  a = b
  b = swp
END WHERE
END PROCEDURE masked_swap_Int16v

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_Int32v
INTEGER(INT32), DIMENSION(SIZE(a)) :: swp
WHERE (mask)
  swp = a
  a = b
  b = swp
END WHERE
END PROCEDURE masked_swap_Int32v

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_Int64v
INTEGER(INT64), DIMENSION(SIZE(a)) :: swp
WHERE (mask)
  swp = a
  a = b
  b = swp
END WHERE
END PROCEDURE masked_swap_Int64v

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

#ifdef USE_Int128
MODULE PROCEDURE masked_swap_Int128v
INTEGER(Int128), DIMENSION(SIZE(a)) :: swp
WHERE (mask)
  swp = a
  a = b
  b = swp
END WHERE
END PROCEDURE masked_swap_Int128v
#endif

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_r32m
REAL(REAL32), DIMENSION(SIZE(a, 1), SIZE(a, 2)) :: swp
WHERE (mask)
  swp = a
  a = b
  b = swp
END WHERE
END PROCEDURE masked_swap_r32m

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_r64m
REAL(REAL64), DIMENSION(SIZE(a, 1), SIZE(a, 2)) :: swp
WHERE (mask)
  swp = a
  a = b
  b = swp
END WHERE
END PROCEDURE masked_swap_r64m

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_Int8m
INTEGER(INT8), DIMENSION(SIZE(a, 1), SIZE(a, 2)) :: swp
WHERE (mask)
  swp = a
  a = b
  b = swp
END WHERE
END PROCEDURE masked_swap_Int8m

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_Int16m
INTEGER(INT16), DIMENSION(SIZE(a, 1), SIZE(a, 2)) :: swp
WHERE (mask)
  swp = a
  a = b
  b = swp
END WHERE
END PROCEDURE masked_swap_Int16m

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_Int32m
INTEGER(INT32), DIMENSION(SIZE(a, 1), SIZE(a, 2)) :: swp
WHERE (mask)
  swp = a
  a = b
  b = swp
END WHERE
END PROCEDURE masked_swap_Int32m

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_Int64m
INTEGER(INT64), DIMENSION(SIZE(a, 1), SIZE(a, 2)) :: swp
WHERE (mask)
  swp = a
  a = b
  b = swp
END WHERE
END PROCEDURE masked_swap_Int64m

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

#ifdef USE_Int128
MODULE PROCEDURE masked_swap_Int128m
INTEGER(Int128), DIMENSION(SIZE(a, 1), SIZE(a, 2)) :: swp
WHERE (mask)
  swp = a
  a = b
  b = swp
END WHERE
END PROCEDURE masked_swap_Int128m
#endif

!----------------------------------------------------------------------------
!                                                                  SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_index1
INTEGER(I4B) :: IJ(2), s(2), i, j
!! main
IF (ANY([i1, i2] .GT. 2) .OR. ANY([i1, i2] .LE. 0) .OR. i1 .EQ. i2) THEN
  s = SHAPE(b)
  CALL Reallocate(a, s(1), s(2))
  a = b
ELSE
  s = SHAPE(b)
  CALL Reallocate(a, s(i1), s(i2))
  DO j = 1, s(2)
    DO i = 1, s(1)
      ij = [i, j]
      a(ij(i1), ij(i2)) = b(i, j)
    END DO
  END DO
END IF
END PROCEDURE swap_index1

!----------------------------------------------------------------------------
!                                                                  SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_index_1
INTEGER(I4B) :: ij(2), s(2), i, j
!! main
s = SHAPE(b)
DO j = 1, s(2)
  DO i = 1, s(1)
    ij(1) = i; ij(2) = j
    a(ij(i1), ij(i2)) = b(i, j)
  END DO
END DO
END PROCEDURE swap_index_1

!----------------------------------------------------------------------------
!                                                                  SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_index_2
INTEGER(I4B) :: ij(2), s(2), i, j
!! main
s = SHAPE(b)
DO j = 1, s(2)
  DO i = 1, s(1)
    ij(1) = i; ij(2) = j
    a(ij(i1), ij(i2)) = b(i, j)
  END DO
END DO
END PROCEDURE swap_index_2

!----------------------------------------------------------------------------
!                                                                  SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_index2
INTEGER(I4B) :: IJ(2), s(2), i, j
!! main
IF (ANY([i1, i2] .GT. 2) .OR. ANY([i1, i2] .LE. 0) .OR. i1 .EQ. i2) THEN
  s = SHAPE(b)
  CALL Reallocate(a, s(1), s(2))
  a = b
ELSE
  s = SHAPE(b)
  CALL Reallocate(a, s(i1), s(i2))
  DO j = 1, s(2)
    DO i = 1, s(1)
      ij = [i, j]
      a(ij(i1), ij(i2)) = b(i, j)
    END DO
  END DO
END IF
END PROCEDURE swap_index2

!----------------------------------------------------------------------------
!                                                                  SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_index3
INTEGER(I4B) :: ijk(3), s(3), i, j, k
!! main
IF (ANY([i1, i2, i3] .GT. 3) .OR. ANY([i1, i2, i3] .LE. 0)) THEN
  s = SHAPE(b)
  CALL Reallocate(a, s(1), s(2), s(3))
  a = b
ELSE
  s = SHAPE(b)
  CALL Reallocate(a, s(i1), s(i2), s(i3))
  DO k = 1, s(3)
    DO j = 1, s(2)
      DO i = 1, s(1)
        ijk = [i, j, k]
        a(ijk(i1), ijk(i2), ijk(i3)) = b(i, j, k)
      END DO
    END DO
  END DO
END IF
END PROCEDURE swap_index3

!----------------------------------------------------------------------------
!                                                                  SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_index4
INTEGER(I4B) :: ijk(3), s(3), i, j, k
!! main
IF (ANY([i1, i2, i3] .GT. 3) .OR. ANY([i1, i2, i3] .LE. 0)) THEN
  s = SHAPE(b)
  CALL Reallocate(a, s(1), s(2), s(3))
  a = b
ELSE
  s = SHAPE(b)
  CALL Reallocate(a, s(i1), s(i2), s(i3))
  DO k = 1, s(3)
    DO j = 1, s(2)
      DO i = 1, s(1)
        ijk = [i, j, k]
        a(ijk(i1), ijk(i2), ijk(i3)) = b(i, j, k)
      END DO
    END DO
  END DO
END IF
END PROCEDURE swap_index4

!----------------------------------------------------------------------------
!                                                                  SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_index_3
INTEGER(I4B) :: ijk(3), s(3), i, j, k
!! main
s = SHAPE(b)
DO k = 1, s(3)
  DO j = 1, s(2)
    DO i = 1, s(1)
      ijk = [i, j, k]
      a(ijk(i1), ijk(i2), ijk(i3)) = b(i, j, k)
    END DO
  END DO
END DO
END PROCEDURE swap_index_3

!----------------------------------------------------------------------------
!                                                                  SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_index_4
INTEGER(I4B) :: ijk(3), s(3), i, j, k
!! main
s = SHAPE(b)
DO k = 1, s(3)
  DO j = 1, s(2)
    DO i = 1, s(1)
      ijk = [i, j, k]
      a(ijk(i1), ijk(i2), ijk(i3)) = b(i, j, k)
    END DO
  END DO
END DO
END PROCEDURE swap_index_4

!----------------------------------------------------------------------------
!                                                                  SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_index5
INTEGER(I4B) :: indx(4), s(4), i, j, k, l
!! main
s = SHAPE(b)
CALL Reallocate(a, s(i1), s(i2), s(i3), s(i4))
DO l = 1, s(4)
  DO k = 1, s(3)
    DO j = 1, s(2)
      DO i = 1, s(1)
        indx = [i, j, k, l]
        a(indx(i1), indx(i2), indx(i3), indx(i4)) = b(i, j, k, l)
      END DO
    END DO
  END DO
END DO
END PROCEDURE swap_index5

!----------------------------------------------------------------------------
!                                                                  SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_index6
INTEGER(I4B) :: indx(4), s(4), i, j, k, l
!! main
s = SHAPE(b)
CALL Reallocate(a, s(i1), s(i2), s(i3), s(i4))
DO l = 1, s(4)
  DO k = 1, s(3)
    DO j = 1, s(2)
      DO i = 1, s(1)
        indx = [i, j, k, l]
        a(indx(i1), indx(i2), indx(i3), indx(i4)) = b(i, j, k, l)
      END DO
    END DO
  END DO
END DO
END PROCEDURE swap_index6

!----------------------------------------------------------------------------
!                                                                  SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_index_5
INTEGER(I4B) :: indx(4), s(4), i, j, k, l
!! main
s = SHAPE(b)
DO l = 1, s(4)
  DO k = 1, s(3)
    DO j = 1, s(2)
      DO i = 1, s(1)
        indx = [i, j, k, l]
        a(indx(i1), indx(i2), indx(i3), indx(i4)) = b(i, j, k, l)
      END DO
    END DO
  END DO
END DO
END PROCEDURE swap_index_5

!----------------------------------------------------------------------------
!                                                                  SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_index_6
INTEGER(I4B) :: indx(4), s(4), i, j, k, l
!! main
s = SHAPE(b)
DO l = 1, s(4)
  DO k = 1, s(3)
    DO j = 1, s(2)
      DO i = 1, s(1)
        indx = [i, j, k, l]
        a(indx(i1), indx(i2), indx(i3), indx(i4)) = b(i, j, k, l)
      END DO
    END DO
  END DO
END DO
END PROCEDURE swap_index_6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
