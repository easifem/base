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

SUBMODULE(BoundingBox_Method) ConstructorMethods
USE Display_Method, ONLY: BlankLines
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_1
obj%nsd = nsd
obj%box(1, 1) = lim(1) !xmin
obj%box(1, 2) = lim(3) !ymin
obj%box(1, 3) = lim(5) !zmin
obj%box(2, 1) = lim(2) !xmax
obj%box(2, 2) = lim(4) !ymax
obj%box(2, 3) = lim(6) !zmax
obj%l(1) = lim(2) - lim(1)
obj%l(2) = lim(4) - lim(3)
obj%l(3) = lim(6) - lim(5)
END PROCEDURE Initiate_1

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_2
obj%box = anotherobj%box
obj%nsd = anotherobj%nsd
obj%l = anotherobj%l
END PROCEDURE Initiate_2

!---------------------------------------------------------------------------
!                                                               Initiate
!---------------------------------------------------------------------------

MODULE PROCEDURE Initiate_3
INTEGER(I4B) :: ii, tsize
tsize = SIZE(anotherobj)
DO ii = 1, tsize
  obj(ii) = anotherobj(ii)
END DO
END PROCEDURE Initiate_3

!----------------------------------------------------------------------------
!                                                               Append
!----------------------------------------------------------------------------

MODULE PROCEDURE Append_1
INTEGER(I4B) :: tsize1, tsize2
TYPE(BoundingBox_), ALLOCATABLE :: tempbox(:)

tsize2 = SIZE(VALUE)
IF (ALLOCATED(obj)) THEN
  tsize1 = SIZE(obj)
  ALLOCATE (tempbox(tsize1))
  CALL Initiate(obj=tempbox, anotherobj=obj)
  CALL DEALLOCATE (obj)
  ALLOCATE (obj(tsize1 + tsize2))
  CALL Initiate(obj(1:tsize1), tempbox)
  CALL Initiate(obj(tsize1 + 1:), VALUE)
  CALL DEALLOCATE (tempbox)
  RETURN

END IF

tsize1 = 0
ALLOCATE (obj(tsize1 + tsize2))
CALL Initiate(obj(tsize1 + 1:), VALUE)
END PROCEDURE Append_1

!----------------------------------------------------------------------------
!                                                                BoundingBox
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
CALL Initiate(ans, nsd, lim)
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                               Bounding box
!----------------------------------------------------------------------------
MODULE PROCEDURE Constructor2
CALL Initiate(ans, anotherobj)
END PROCEDURE Constructor2

!----------------------------------------------------------------------------
!                                                               Bounding box
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor3
REAL(DFP) :: lim(6)
INTEGER(I4B) :: nsd

lim = 0.0_DFP
nsd = SIZE(xij, 1)

SELECT CASE (nsd)
CASE (1)
  lim(1) = MINVAL(xij(1, :))
  lim(2) = MAXVAL(xij(1, :))
CASE (2)
  lim(1) = MINVAL(xij(1, :))
  lim(2) = MAXVAL(xij(1, :))
  lim(3) = MINVAL(xij(2, :))
  lim(4) = MAXVAL(xij(2, :))
CASE (3)
  lim(1) = MINVAL(xij(1, :))
  lim(2) = MAXVAL(xij(1, :))
  lim(3) = MINVAL(xij(2, :))
  lim(4) = MAXVAL(xij(2, :))
  lim(5) = MINVAL(xij(3, :))
  lim(6) = MAXVAL(xij(3, :))
END SELECT

CALL Initiate(obj=ans, nsd=nsd, lim=lim)
END PROCEDURE Constructor3

!----------------------------------------------------------------------------
!                                                         BoundingBox_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_1
ALLOCATE (ans)
CALL Initiate(ans, nsd, lim)
END PROCEDURE Constructor_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_2
ALLOCATE (ans)
CALL Initiate(ans, anotherobj)
END PROCEDURE Constructor_2

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE BB_Deallocate
obj%nsd = 0
obj%box = 0.0_DFP
obj%l = 0.0_DFP
END PROCEDURE BB_Deallocate

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE bb_deallocate2
INTEGER(I4B) :: ii
IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    CALL DEALLOCATE (obj(ii))
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE bb_deallocate2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE ConstructorMethods
