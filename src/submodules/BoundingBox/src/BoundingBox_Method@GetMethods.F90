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
! date:         23 Feb 2021
! summary:         This submodule contains implementation of get method for [[BoundingBox_]] data type which are defined in [[BoundingBox_Method]] module.

SUBMODULE(BoundingBox_Method) GetMethods
USE GlobalData, ONLY: zero
IMPLICIT NONE
CONTAINS

!-----------------------------------------------------------------------------
!                                                                     getXmin
!-----------------------------------------------------------------------------

MODULE PROCEDURE getXmin
Ans = obj%Box(1, 1)
END PROCEDURE getXmin

!-----------------------------------------------------------------------------
!                                                                     getXmax
!-----------------------------------------------------------------------------

MODULE PROCEDURE getXmax
Ans = obj%Box(2, 1)
END PROCEDURE getXmax

!-----------------------------------------------------------------------------
!                                                                     getYmin
!-----------------------------------------------------------------------------

MODULE PROCEDURE getYmin
Ans = obj%Box(1, 2)
END PROCEDURE getYmin

!-----------------------------------------------------------------------------
!                                                                     getYmax
!-----------------------------------------------------------------------------

MODULE PROCEDURE getYmax
Ans = obj%Box(2, 2)
END PROCEDURE getYmax

!-----------------------------------------------------------------------------
!                                                                     getZmin
!-----------------------------------------------------------------------------

MODULE PROCEDURE getZmin
Ans = obj%Box(1, 3)
END PROCEDURE getZmin

!-----------------------------------------------------------------------------
!                                                                     getZmax
!-----------------------------------------------------------------------------

MODULE PROCEDURE getZmax
Ans = obj%Box(2, 3)
END PROCEDURE getZmax

!----------------------------------------------------------------------------
!                                                          is_intersect_in_X
!----------------------------------------------------------------------------

MODULE PROCEDURE is_intersect_in_X
! Define internal variables
REAL(DFP) :: min1, max1, min2, max2
LOGICAL(LGT) :: Left, Right

min1 = .Xmin.obj; max1 = .Xmax.obj
min2 = .Xmin.obj2; max2 = .Xmax.obj2

Right = (min2 .GE. min1) .AND. (min2 .LE. max1)
Left = (max2 .GE. min1) .AND. (max2 .LE. max1)

IF (Left .OR. Right) THEN
  Ans = .TRUE.
ELSE
  Ans = .FALSE.
END IF
END PROCEDURE is_intersect_in_X

!----------------------------------------------------------------------------
!                                                          is_intersect_in_Y
!----------------------------------------------------------------------------

MODULE PROCEDURE is_intersect_in_Y
! Define internal variables
REAL(DFP) :: min1, max1, min2, max2
LOGICAL(LGT) :: Left, Right

min1 = .Ymin.obj; max1 = .Ymax.obj
min2 = .Ymin.obj2; max2 = .Ymax.obj2

Right = (min2 .GE. min1) .AND. (min2 .LE. max1)
Left = (max2 .GE. min1) .AND. (max2 .LE. max1)

IF (Left .OR. Right) THEN
  Ans = .TRUE.
ELSE
  Ans = .FALSE.
END IF
END PROCEDURE is_intersect_in_Y

!----------------------------------------------------------------------------
!                                                          is_intersect_in_Z
!----------------------------------------------------------------------------

MODULE PROCEDURE is_intersect_in_Z
! Define internal variables
REAL(DFP) :: min1, max1, min2, max2
LOGICAL(LGT) :: Left, Right

min1 = .Zmin.obj; max1 = .Zmax.obj
min2 = .Zmin.obj2; max2 = .Zmax.obj2

Right = (min2 .GE. min1) .AND. (min2 .LE. max1)
Left = (max2 .GE. min1) .AND. (max2 .LE. max1)

IF (Left .OR. Right) THEN
  Ans = .TRUE.
ELSE
  Ans = .FALSE.
END IF
END PROCEDURE is_intersect_in_Z

!----------------------------------------------------------------------------
!                                                               is_intersect
!----------------------------------------------------------------------------

MODULE PROCEDURE is_intersect
Ans = isIntersectInX(obj, obj2) &
  & .AND. isIntersectInY(obj, obj2) &
  & .AND. isIntersectInZ(obj, obj2)
END PROCEDURE is_intersect

!----------------------------------------------------------------------------
!                                                                 isEmpty
!----------------------------------------------------------------------------

MODULE PROCEDURE bbox_isEmpty
REAL(DFP) :: min1, max1

min1 = .Xmin.obj
max1 = .Xmax.obj
ans = .TRUE.
IF (ABS(max1 - min1) .GE. zero) ans = .FALSE.
min1 = .Ymin.obj
max1 = .Ymax.obj
IF (ABS(max1 - min1) .GE. zero) ans = .FALSE.
min1 = .Zmin.obj
max1 = .Zmax.obj
IF (ABS(max1 - min1) .GE. zero) ans = .FALSE.
END PROCEDURE bbox_isEmpty

!----------------------------------------------------------------------------
!                                                           get_intersection
!----------------------------------------------------------------------------

MODULE PROCEDURE get_intersection
!> main
Ans%NSD = MAX(obj%NSD, obj2%NSD)
Ans%Box = 0.0_DFP
IF (obj.isIntersect.obj2) THEN
  CALL setXmin(Ans, MAX(.Xmin.obj, .Xmin.obj2))
  CALL setXmax(Ans, MIN(.Xmax.obj, .Xmax.obj2))
  CALL setYmin(Ans, MAX(.Ymin.obj, .Ymin.obj2))
  CALL setYmax(Ans, MIN(.Ymax.obj, .Ymax.obj2))
  CALL setZmin(Ans, MAX(.Zmin.obj, .Zmin.obj2))
  CALL setZmax(Ans, MIN(.Zmax.obj, .Zmax.obj2))
END IF
END PROCEDURE get_intersection

!----------------------------------------------------------------------------
!                                                                      Union
!----------------------------------------------------------------------------

MODULE PROCEDURE get_union
! Define Internal variables
REAL(DFP) :: Val, Val1, Val2

Ans%NSD = MAX(obj%NSD, obj2%NSD)

Val1 = .Xmin.obj; Val2 = .Xmin.obj2
Val = MIN(Val1, Val2)
CALL SetXMin(Ans, Val)
Val1 = .Xmax.obj; Val2 = .Xmax.obj2
Val = MAX(Val1, Val2)
CALL SetXMax(Ans, Val)

Val1 = .Ymin.obj; Val2 = .Ymin.obj2
Val = MIN(Val1, Val2)
CALL SetYMin(Ans, Val)
Val1 = .Ymax.obj; Val2 = .Ymax.obj2
Val = MAX(Val1, Val2)
CALL SetYMax(Ans, Val)

Val1 = .Zmin.obj; Val2 = .Zmin.obj2
Val = MIN(Val1, Val2)
CALL SetZMin(Ans, Val)
Val1 = .Zmax.obj; Val2 = .Zmax.obj2
Val = MAX(Val1, Val2)
CALL SetZMax(Ans, Val)
END PROCEDURE get_union

!----------------------------------------------------------------------------
!                                                                    Center
!----------------------------------------------------------------------------

MODULE PROCEDURE get_Center
Ans(1) = SUM(obj%Box(:, 1)) / 2.0_DFP
Ans(2) = SUM(obj%Box(:, 2)) / 2.0_DFP
Ans(3) = SUM(obj%Box(:, 3)) / 2.0_DFP
END PROCEDURE get_Center

!----------------------------------------------------------------------------
!                                                                 IsInside
!----------------------------------------------------------------------------

MODULE PROCEDURE is_Inside

! internal variables
INTEGER(I4B) :: NSD
REAL(DFP) :: min1, max1
LOGICAL(LGT) :: Ans1, Ans2, Ans3

NSD = SIZE(Val)

SELECT CASE (NSD)

CASE (1)

  min1 = .Xmin.obj; max1 = .Xmax.obj
  IF (Val(1) .GE. min1 .AND. Val(1) .LE. max1) THEN
    Ans = .TRUE.
  ELSE
    Ans = .FALSE.
  END IF

CASE (2)

  min1 = .Xmin.obj; max1 = .Xmax.obj
  IF (Val(1) .GE. min1 .AND. Val(1) .LE. max1) THEN
    Ans1 = .TRUE.
  ELSE
    Ans2 = .FALSE.
  END IF

  min1 = .Ymin.obj; max1 = .Ymax.obj
  IF (Val(2) .GE. min1 .AND. Val(2) .LE. max1) THEN
    Ans2 = .TRUE.
  ELSE
    Ans2 = .FALSE.
  END IF

  IF (Ans1 .AND. Ans2) THEN
    Ans = .TRUE.
  ELSE
    Ans = .FALSE.
  END IF

CASE DEFAULT

  min1 = .Xmin.obj; max1 = .Xmax.obj
  IF (Val(1) .GE. min1 .AND. Val(1) .LE. max1) THEN
    Ans1 = .TRUE.
  ELSE
    Ans1 = .FALSE.
  END IF

  min1 = .Ymin.obj; max1 = .Ymax.obj
  IF (Val(2) .GE. min1 .AND. Val(2) .LE. max1) THEN
    Ans2 = .TRUE.
  ELSE
    Ans2 = .FALSE.
  END IF

  min1 = .Zmin.obj; max1 = .Zmax.obj
  IF (Val(3) .GE. min1 .AND. Val(3) .LE. max1) THEN
    Ans3 = .TRUE.
  ELSE
    Ans3 = .FALSE.
  END IF

  IF (Ans1 .AND. Ans2 .AND. Ans3) THEN
    Ans = .TRUE.
  ELSE
    Ans = .FALSE.
  END IF

END SELECT
END PROCEDURE is_Inside

!----------------------------------------------------------------------------
!                                                                   getNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE get_nptrs
INTEGER(I4B) :: n, i
LOGICAL(LGT), ALLOCATABLE :: msk(:)
INTEGER(I4B), ALLOCATABLE :: Indx(:)

n = SIZE(xij, 2)
ALLOCATE (msk(n), Indx(n))
DO i = 1, n
  msk(i) = isInside(obj, xij(:, i))
  Indx(i) = i
END DO
ans = PACK(Indx, msk)
DEALLOCATE (msk, Indx)
END PROCEDURE get_nptrs

!----------------------------------------------------------------------------
!                                                               GetDiameter
!----------------------------------------------------------------------------

MODULE PROCEDURE bbox_GetDiameter
REAL(DFP) :: a(3)
a(1) = ABS((.xmax.obj) - (.xmin.obj))
a(2) = ABS((.ymax.obj) - (.ymin.obj))
a(3) = ABS((.zmax.obj) - (.zmin.obj))
ans = MAXVAL(a)
END PROCEDURE bbox_GetDiameter

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
