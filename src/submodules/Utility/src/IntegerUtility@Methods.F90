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

SUBMODULE(IntegerUtility) Methods
USE AppendUtility, ONLY: OPERATOR(.ROWCONCAT.), &
                         OPERATOR(.COLCONCAT.), &
                         Expand
USE SortUtility, ONLY: QuickSort
USE BinomUtility, ONLY: Binom
USE OnesUtility, ONLY: ones

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Size
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size1
ans = INT(Binom(n + d, d, 1.0_DFP), KIND=I4B)
END PROCEDURE obj_Size1

!----------------------------------------------------------------------------
!                                                                 Size
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size2
INTEGER(I4B) :: ii
ans = 0_I4B
DO ii = 0, n
  ans = ans + SIZE(n=ii, d=d)
END DO
END PROCEDURE obj_Size2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMultiIndices1
INTEGER(I4B) :: ii, m
INTEGER(I4B), ALLOCATABLE :: indx(:, :), acol(:), indx2(:, :)

SELECT CASE (d)
CASE (1_I4B)

  ALLOCATE (ans(2, n + 1))
  DO ii = 0, n
    ans(1:2, ii + 1) = [ii, n - ii]
  END DO

CASE DEFAULT

  ALLOCATE (ans(d + 1, 1))
  ans = 0; ans(1, 1) = n

  DO ii = n - 1, 0_I4B, -1_I4B

    indx = GetMultiIndices(n=n - ii, d=d - 1)
    m = SIZE(indx, 2)
    acol = ii * ones(m, 1_I4B)
    indx2 = acol.ROWCONCAT.indx
    ans = indx2.COLCONCAT.ans

  END DO

END SELECT

IF (ALLOCATED(indx)) DEALLOCATE (indx)
IF (ALLOCATED(acol)) DEALLOCATE (acol)
IF (ALLOCATED(indx2)) DEALLOCATE (indx2)

END PROCEDURE obj_GetMultiIndices1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMultiIndices2
INTEGER(I4B) :: ii, m, r1, r2

m = SIZE(n, d, .TRUE.)
ALLOCATE (ans(d + 1, m))

r1 = 0; r2 = 0
DO ii = 0, n
  m = SIZE(n=ii, d=d)
  r1 = r2 + 1_I4B
  r2 = r1 + m - 1
  ans(:, r1:r2) = GetMultiIndices(n=ii, d=d)
END DO

END PROCEDURE obj_GetMultiIndices2

!----------------------------------------------------------------------------
!                                                                        IN
!----------------------------------------------------------------------------

MODULE PROCEDURE in_1a
#include "./In/In_1.inc"
END PROCEDURE in_1a

MODULE PROCEDURE in_1b
#include "./In/In_1.inc"
END PROCEDURE in_1b

MODULE PROCEDURE in_1c
#include "./In/In_1.inc"
END PROCEDURE in_1c

MODULE PROCEDURE in_1d
#include "./In/In_1.inc"
END PROCEDURE in_1d

!----------------------------------------------------------------------------
!                                                                        isIN
!----------------------------------------------------------------------------

MODULE PROCEDURE IsIn_1a
#include "./In/IsIn_1.inc"
END PROCEDURE IsIn_1a

MODULE PROCEDURE IsIn_1b
#include "./In/IsIn_1.inc"
END PROCEDURE IsIn_1b

MODULE PROCEDURE IsIn_1c
#include "./In/IsIn_1.inc"
END PROCEDURE IsIn_1c

MODULE PROCEDURE IsIn_1d
#include "./In/IsIn_1.inc"
END PROCEDURE IsIn_1d

!----------------------------------------------------------------------------
!                                                                        IN
!----------------------------------------------------------------------------

MODULE PROCEDURE in_2a
ans = ANY(a .EQ. b)
END PROCEDURE in_2a

MODULE PROCEDURE in_2b
ans = ANY(a .EQ. b)
END PROCEDURE in_2b

MODULE PROCEDURE in_2c
ans = ANY(a .EQ. b)
END PROCEDURE in_2c

MODULE PROCEDURE in_2d
ans = ANY(a .EQ. b)
END PROCEDURE in_2d

!----------------------------------------------------------------------------
!                                                             RemoveDuplicate
!----------------------------------------------------------------------------

MODULE PROCEDURE RemoveDuplicates_1a
INTEGER(INT8) :: temp(SIZE(obj))
#include "./RemoveDuplicates/RemoveDuplicates_1.inc"
END PROCEDURE RemoveDuplicates_1a

MODULE PROCEDURE RemoveDuplicates_1b
INTEGER(INT16) :: temp(SIZE(obj))
#include "./RemoveDuplicates/RemoveDuplicates_1.inc"
END PROCEDURE RemoveDuplicates_1b

MODULE PROCEDURE RemoveDuplicates_1c
INTEGER(INT32) :: temp(SIZE(obj))
#include "./RemoveDuplicates/RemoveDuplicates_1.inc"
END PROCEDURE RemoveDuplicates_1c

MODULE PROCEDURE RemoveDuplicates_1d
INTEGER(INT64) :: temp(SIZE(obj))
#include "./RemoveDuplicates/RemoveDuplicates_1.inc"
END PROCEDURE RemoveDuplicates_1d

!----------------------------------------------------------------------------
!                                                             RemoveDuplicate
!----------------------------------------------------------------------------

MODULE PROCEDURE RemoveDuplicates_1a_
INTEGER(INT8) :: temp(SIZE(obj))
#include "./RemoveDuplicates/RemoveDuplicates_2.inc"
END PROCEDURE RemoveDuplicates_1a_

MODULE PROCEDURE RemoveDuplicates_1b_
INTEGER(INT16) :: temp(SIZE(obj))
#include "./RemoveDuplicates/RemoveDuplicates_2.inc"
END PROCEDURE RemoveDuplicates_1b_

MODULE PROCEDURE RemoveDuplicates_1c_
INTEGER(INT32) :: temp(SIZE(obj))
#include "./RemoveDuplicates/RemoveDuplicates_2.inc"
END PROCEDURE RemoveDuplicates_1c_

MODULE PROCEDURE RemoveDuplicates_1d_
INTEGER(INT64) :: temp(SIZE(obj))
#include "./RemoveDuplicates/RemoveDuplicates_2.inc"
END PROCEDURE RemoveDuplicates_1d_

!----------------------------------------------------------------------------
!                                                                 Repeat
!----------------------------------------------------------------------------

MODULE PROCEDURE Repeat_1a
#include "./Repeat/Repeat_1.inc"
END PROCEDURE Repeat_1a

MODULE PROCEDURE Repeat_1b
#include "./Repeat/Repeat_1.inc"
END PROCEDURE Repeat_1b

MODULE PROCEDURE Repeat_1c
#include "./Repeat/Repeat_1.inc"
END PROCEDURE Repeat_1c

MODULE PROCEDURE Repeat_1d
#include "./Repeat/Repeat_1.inc"
END PROCEDURE Repeat_1d

MODULE PROCEDURE Repeat_1e
#include "./Repeat/Repeat_1.inc"
END PROCEDURE Repeat_1e

MODULE PROCEDURE Repeat_1f
#include "./Repeat/Repeat_1.inc"
END PROCEDURE Repeat_1f

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE GetIndex1
Ans = MINLOC(ABS(obj - val), 1)
END PROCEDURE GetIndex1

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE GetIndex2
INTEGER(I4B) :: i, j, m
LOGICAL(LGT), ALLOCATABLE :: Search(:)

m = SIZE(val)
ALLOCATE (Search(m), Ans(m))
Search = .TRUE.
Ans = 0

DO i = 1, SIZE(obj)
  DO j = 1, m
    IF (Search(j)) THEN
      IF (val(j) .EQ. obj(i)) THEN
        Search(j) = .FALSE.
        Ans(j) = i
      END IF
    END IF
  END DO
END DO
END PROCEDURE GetIndex2

!----------------------------------------------------------------------------
!                                                                    Get
!----------------------------------------------------------------------------

MODULE PROCEDURE Get1_Int8
ans = 0
IF (indx .LE. SIZE(val)) ans = val(indx)
END PROCEDURE Get1_Int8

MODULE PROCEDURE Get1_Int16
ans = 0
IF (indx .LE. SIZE(val)) ans = val(indx)
END PROCEDURE Get1_Int16

MODULE PROCEDURE Get1_Int32
ans = 0
IF (indx .LE. SIZE(val)) ans = val(indx)
END PROCEDURE Get1_Int32

MODULE PROCEDURE Get1_Int64
ans = 0
IF (indx .LE. SIZE(val)) ans = val(indx)
END PROCEDURE Get1_Int64

!----------------------------------------------------------------------------
!                                                                    Get
!----------------------------------------------------------------------------

MODULE PROCEDURE Get2_Int8
ans = val(indx)
END PROCEDURE Get2_Int8

MODULE PROCEDURE Get2_Int16
ans = val(indx)
END PROCEDURE Get2_Int16

MODULE PROCEDURE Get2_Int32
ans = val(indx)
END PROCEDURE Get2_Int32

MODULE PROCEDURE Get2_Int64
ans = val(indx)
END PROCEDURE Get2_Int64

!----------------------------------------------------------------------------
!                                                                    Get
!----------------------------------------------------------------------------

MODULE PROCEDURE Get3_Int8
ans = val(istart:iend:stride)
END PROCEDURE Get3_Int8

MODULE PROCEDURE Get3_Int16
ans = val(istart:iend:stride)
END PROCEDURE Get3_Int16

MODULE PROCEDURE Get3_Int32
ans = val(istart:iend:stride)
END PROCEDURE Get3_Int32

MODULE PROCEDURE Get3_Int64
ans = val(istart:iend:stride)
END PROCEDURE Get3_Int64

!----------------------------------------------------------------------------
!                                                            GetIntersection
!----------------------------------------------------------------------------

MODULE PROCEDURE GetIntersection1
#include "./Intersection/Intersection.inc"
END PROCEDURE GetIntersection1

MODULE PROCEDURE GetIntersection2
#include "./Intersection/Intersection.inc"
END PROCEDURE GetIntersection2

MODULE PROCEDURE GetIntersection3
#include "./Intersection/Intersection.inc"
END PROCEDURE GetIntersection3

MODULE PROCEDURE GetIntersection4
#include "./Intersection/Intersection.inc"
END PROCEDURE GetIntersection4

!----------------------------------------------------------------------------
!                                               Get1DIndexFrom2DFortranIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE Get1DIndexFrom2DFortranIndex
ans = (j - 1) * dim1 + i
END PROCEDURE Get1DIndexFrom2DFortranIndex

!----------------------------------------------------------------------------
!                                               Get1DIndexFrom2DFortranIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE Get1DIndexFrom3DFortranIndex
ans = (k - 1) * dim1 * dim2 + (j - 1) * dim1 + i
END PROCEDURE Get1DIndexFrom3DFortranIndex

!----------------------------------------------------------------------------
!                                               Get1DIndexFrom2DFortranIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE Get1DIndexFrom4DFortranIndex
ans = (l - 1) * dim1 * dim2 * dim3 + (k - 1) * dim1 * dim2 &
      + (j - 1) * dim1 + i
END PROCEDURE Get1DIndexFrom4DFortranIndex

END SUBMODULE Methods
