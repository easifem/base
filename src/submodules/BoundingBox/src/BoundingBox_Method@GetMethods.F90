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

SUBMODULE(BoundingBox_Method) GetMethods
USE GlobalData, ONLY: zero
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     getXmin
!----------------------------------------------------------------------------

MODULE PROCEDURE getXmin
ans = obj%box(1, 1)
END PROCEDURE getXmin

!----------------------------------------------------------------------------
!                                                                     getXmax
!----------------------------------------------------------------------------

MODULE PROCEDURE getXmax
ans = obj%box(2, 1)
END PROCEDURE getXmax

!----------------------------------------------------------------------------
!                                                                     getYmin
!----------------------------------------------------------------------------

MODULE PROCEDURE getYmin
ans = obj%box(1, 2)
END PROCEDURE getYmin

!----------------------------------------------------------------------------
!                                                                     getYmax
!----------------------------------------------------------------------------

MODULE PROCEDURE getYmax
ans = obj%box(2, 2)
END PROCEDURE getYmax

!----------------------------------------------------------------------------
!                                                                     getZmin
!----------------------------------------------------------------------------

MODULE PROCEDURE getZmin
ans = obj%box(1, 3)
END PROCEDURE getZmin

!----------------------------------------------------------------------------
!                                                                     getZmax
!----------------------------------------------------------------------------

MODULE PROCEDURE getZmax
ans = obj%box(2, 3)
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
  ans = .TRUE.
ELSE
  ans = .FALSE.
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
  ans = .TRUE.
ELSE
  ans = .FALSE.
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
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE is_intersect_in_Z

!----------------------------------------------------------------------------
!                                                               is_intersect
!----------------------------------------------------------------------------

MODULE PROCEDURE is_intersect
ans = isIntersectInX(obj, obj2) &
  & .AND. isIntersectInY(obj, obj2) &
  & .AND. isIntersectInZ(obj, obj2)
END PROCEDURE is_intersect

!----------------------------------------------------------------------------
!                                                                 isEmpty
!----------------------------------------------------------------------------

MODULE PROCEDURE bbox_isEmpty
ans = .TRUE.
IF (ANY(obj%l .GT. zero)) ans = .FALSE.
END PROCEDURE bbox_isEmpty

!----------------------------------------------------------------------------
!                                                           get_intersection
!----------------------------------------------------------------------------

MODULE PROCEDURE get_intersection
!> main
ans%NSD = MAX(obj%NSD, obj2%NSD)
ans%box = 0.0_DFP
IF (obj.isIntersect.obj2) THEN
  CALL setXmin(ans, MAX(.Xmin.obj, .Xmin.obj2))
  CALL setXmax(ans, MIN(.Xmax.obj, .Xmax.obj2))
  CALL setYmin(ans, MAX(.Ymin.obj, .Ymin.obj2))
  CALL setYmax(ans, MIN(.Ymax.obj, .Ymax.obj2))
  CALL setZmin(ans, MAX(.Zmin.obj, .Zmin.obj2))
  CALL setZmax(ans, MIN(.Zmax.obj, .Zmax.obj2))
END IF
END PROCEDURE get_intersection

!----------------------------------------------------------------------------
!                                                                      Union
!----------------------------------------------------------------------------

MODULE PROCEDURE get_union
! Define Internal variables
INTEGER(I4B) :: nsd
REAL(DFP) :: val(6), val1, val2

nsd = MAX(obj%nsd, obj2%nsd)

val1 = .Xmin.obj; val2 = .Xmin.obj2
val(1) = MIN(val1, val2)

val1 = .Xmax.obj; val2 = .Xmax.obj2
val(2) = MAX(val1, val2)

val1 = .Ymin.obj; val2 = .Ymin.obj2
val(3) = MIN(val1, val2)

val1 = .Ymax.obj; val2 = .Ymax.obj2
val(4) = MAX(val1, val2)

val1 = .Zmin.obj; val2 = .Zmin.obj2
val(5) = MIN(val1, val2)

val1 = .Zmax.obj; val2 = .Zmax.obj2
val(6) = MAX(val1, val2)

CALL Initiate(obj=ans, nsd=nsd, lim=val)
END PROCEDURE get_union

!----------------------------------------------------------------------------
!                                                                    Center
!----------------------------------------------------------------------------

MODULE PROCEDURE get_Center
ans(1) = SUM(obj%box(:, 1)) / 2.0_DFP
ans(2) = SUM(obj%box(:, 2)) / 2.0_DFP
ans(3) = SUM(obj%box(:, 3)) / 2.0_DFP
END PROCEDURE get_Center

!----------------------------------------------------------------------------
!                                                                 IsInside
!----------------------------------------------------------------------------

MODULE PROCEDURE is_Inside

! internal variables
INTEGER(I4B) :: NSD
REAL(DFP) :: min1, max1
LOGICAL(LGT) :: ans1, ans2, ans3

ans = .FALSE.
ans1 = .FALSE.
ans2 = .FALSE.
ans3 = .FALSE.
NSD = SIZE(val)

SELECT CASE (NSD)

CASE (1)

  min1 = .Xmin.obj; max1 = .Xmax.obj
  IF (val(1) .GE. min1 .AND. val(1) .LE. max1) THEN
    ans = .TRUE.
  END IF

CASE (2)

  min1 = .Xmin.obj; max1 = .Xmax.obj
  IF (val(1) .GE. min1 .AND. val(1) .LE. max1) THEN
    ans1 = .TRUE.
  END IF

  min1 = .Ymin.obj; max1 = .Ymax.obj
  IF (val(2) .GE. min1 .AND. val(2) .LE. max1) THEN
    ans2 = .TRUE.
  END IF

  ans = ans1 .AND. ans2

CASE DEFAULT

  min1 = .Xmin.obj; max1 = .Xmax.obj
  IF (val(1) .GE. min1 .AND. val(1) .LE. max1) THEN
    ans1 = .TRUE.
  END IF

  min1 = .Ymin.obj; max1 = .Ymax.obj
  IF (val(2) .GE. min1 .AND. val(2) .LE. max1) THEN
    ans2 = .TRUE.
  END IF

  min1 = .Zmin.obj; max1 = .Zmax.obj
  IF (val(3) .GE. min1 .AND. val(3) .LE. max1) THEN
    ans3 = .TRUE.
  END IF

  ans = ans1 .AND. ans2 .AND. ans3

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

MODULE PROCEDURE bbox_GetDiameterSqr
ans = obj%l(1)**2 + obj%l(2)**2 + obj%l(3)**2
END PROCEDURE bbox_GetDiameterSqr

!----------------------------------------------------------------------------
!                                                               GetDiameter
!----------------------------------------------------------------------------

MODULE PROCEDURE bbox_GetDiameter
ans = SQRT(bbox_GetDiameterSqr(obj))
END PROCEDURE bbox_GetDiameter

!----------------------------------------------------------------------------
!                                                               GetRadius
!----------------------------------------------------------------------------

MODULE PROCEDURE bbox_GetRadius
ans = bbox_GetDiameter(obj) * 0.5_DFP
END PROCEDURE bbox_GetRadius

!----------------------------------------------------------------------------
!                                                               GetRadius
!----------------------------------------------------------------------------

MODULE PROCEDURE bbox_GetRadiusSqr
ans = 0.25_DFP * bbox_GetDiameterSqr(obj)
END PROCEDURE bbox_GetRadiusSqr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
