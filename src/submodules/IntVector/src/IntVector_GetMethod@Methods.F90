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
! date:         25 Feb 2021
! summary: This submodule implements get methods of [[IntVector_]]

SUBMODULE(IntVector_GetMethod) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_1
IF (ALLOCATED(obj%val)) THEN
  val = IntVector(obj%val)
END IF
END PROCEDURE intVec_get_1

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_2
IF (ALLOCATED(obj%val)) THEN
  val = IntVector(obj%val(Indx))
END IF
END PROCEDURE intVec_get_2

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_3
IF (ALLOCATED(obj%val)) THEN
  val = IntVector(obj%val( &
    & istart:&
    & Input(default=SIZE(obj), option=iend):&
    & Input(option=stride, default=1)))
END IF
END PROCEDURE intVec_get_3

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_4
val = IntVector(get(obj, TypeInt))
END PROCEDURE intVec_get_4

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_5
val = IntVector(get(obj, Indx, TypeInt))
END PROCEDURE intVec_get_5

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_6
val = IntVector(get(obj, iStart, iEnd, Stride, &
  & TypeInt))
END PROCEDURE intVec_get_6

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_7a
IF (ALLOCATED(obj%val)) THEN
  val = obj%val
END IF
END PROCEDURE intVec_get_7a
MODULE PROCEDURE intVec_get_7b
IF (ALLOCATED(obj%val)) THEN
  val = obj%val
END IF
END PROCEDURE intVec_get_7b
MODULE PROCEDURE intVec_get_7c
IF (ALLOCATED(obj%val)) THEN
  val = obj%val
END IF
END PROCEDURE intVec_get_7c
MODULE PROCEDURE intVec_get_7d
IF (ALLOCATED(obj%val)) THEN
  val = obj%val
END IF
END PROCEDURE intVec_get_7d

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_8a
IF (ALLOCATED(obj%val)) THEN
  val = obj%val(Indx)
END IF
END PROCEDURE intVec_get_8a

MODULE PROCEDURE intVec_get_8b
IF (ALLOCATED(obj%val)) THEN
  val = obj%val(Indx)
END IF
END PROCEDURE intVec_get_8b

MODULE PROCEDURE intVec_get_8c
IF (ALLOCATED(obj%val)) THEN
  val = obj%val(Indx)
END IF
END PROCEDURE intVec_get_8c

MODULE PROCEDURE intVec_get_8d
IF (ALLOCATED(obj%val)) THEN
  val = obj%val(Indx)
END IF
END PROCEDURE intVec_get_8d

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_9a
IF (ALLOCATED(obj%val)) THEN
  val = obj%val(iStart:iEnd:Stride)
END IF
END PROCEDURE intVec_get_9a

MODULE PROCEDURE intVec_get_9b
IF (ALLOCATED(obj%val)) THEN
  val = obj%val(iStart:iEnd:Stride)
END IF
END PROCEDURE intVec_get_9b

MODULE PROCEDURE intVec_get_9c
IF (ALLOCATED(obj%val)) THEN
  val = obj%val(iStart:iEnd:Stride)
END IF
END PROCEDURE intVec_get_9c

MODULE PROCEDURE intVec_get_9d
IF (ALLOCATED(obj%val)) THEN
  val = obj%val(iStart:iEnd:Stride)
END IF
END PROCEDURE intVec_get_9d

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_10a
#include "./include/intvec_get_10.inc"
END PROCEDURE intVec_get_10a

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_10b
#include "./include/intvec_get_10.inc"
END PROCEDURE intVec_get_10b
MODULE PROCEDURE intVec_get_10c
#include "./include/intvec_get_10.inc"
END PROCEDURE intVec_get_10c
MODULE PROCEDURE intVec_get_10d
#include "./include/intvec_get_10.inc"
END PROCEDURE intVec_get_10d

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_11a
#include "./include/intvec_get_11.inc"
END PROCEDURE intVec_get_11a
MODULE PROCEDURE intVec_get_11b
#include "./include/intvec_get_11.inc"
END PROCEDURE intVec_get_11b
MODULE PROCEDURE intVec_get_11c
#include "./include/intvec_get_11.inc"
END PROCEDURE intVec_get_11c
MODULE PROCEDURE intVec_get_11d
#include "./include/intvec_get_11.inc"
END PROCEDURE intVec_get_11d

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_12a
#include "./include/intvec_get_12.inc"
END PROCEDURE intVec_get_12a
MODULE PROCEDURE intVec_get_12b
#include "./include/intvec_get_12.inc"
END PROCEDURE intVec_get_12b
MODULE PROCEDURE intVec_get_12c
#include "./include/intvec_get_12.inc"
END PROCEDURE intVec_get_12c
MODULE PROCEDURE intVec_get_12d
#include "./include/intvec_get_12.inc"
END PROCEDURE intVec_get_12d

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_13a
#include "./include/intvec_get_13.inc"
END PROCEDURE intVec_get_13a


!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------
MODULE PROCEDURE intVec_get_13b
#include "./include/intvec_get_13.inc"
END PROCEDURE intVec_get_13b

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------
MODULE PROCEDURE intVec_get_13c
#include "./include/intvec_get_13.inc"
END PROCEDURE intVec_get_13c

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------
!
MODULE PROCEDURE intVec_get_13d
#include "./include/intvec_get_13.inc"
END PROCEDURE intVec_get_13d

!----------------------------------------------------------------------------
!                                                                 getPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_getPointer_1
val => obj
END PROCEDURE intVec_getPointer_1

!----------------------------------------------------------------------------
!                                                                 getPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_getPointer_2
val => obj%val
END PROCEDURE intVec_getPointer_2

!----------------------------------------------------------------------------
!                                                                 getPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_getPointer_3
val => obj%val
END PROCEDURE intVec_getPointer_3

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_getIndex1
ans = MINLOC(ABS(obj%val - val), 1)
END PROCEDURE intVec_getIndex1

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_getIndex2
INTEGER(I4B) :: i, j, m
LOGICAL(LGT), ALLOCATABLE :: search(:)
!
m = SIZE(val)
ALLOCATE (search(m), ans(m))
search = .TRUE.
ans = 0

DO i = 1, SIZE(obj%val)
  DO j = 1, m
    IF (search(j)) THEN
      IF (val(j) .EQ. obj%val(i)) THEN
        search(j) = .FALSE.
        ans(j) = i
      END IF
    END IF
  END DO
END DO
END PROCEDURE intVec_getIndex2

END SUBMODULE Methods
