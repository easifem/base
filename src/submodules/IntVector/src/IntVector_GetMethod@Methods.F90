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

SUBMODULE(IntVector_GetMethod) Methods
USE BaseType, ONLY: math => TypeMathOpt
USE InputUtility, ONLY: Input
USE IntegerUtility, ONLY: Util_GetIndex => GetIndex
USE IntVector_ConstructorMethod, ONLY: IntVectorInitiate => Initiate
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
CALL IntVectorInitiate(obj=val, obj2=obj)
END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif
INTEGER(I4B) :: tsize, ii

tsize = SIZE(indx)
CALL IntVectorInitiate(obj=val, tsize=tsize)

IF (ALLOCATED(obj%val)) THEN
  loop1: DO ii = 1, tsize
#ifdef DEBUG_VER
    isok = indx(ii) .LE. obj%tsize
    IF (.NOT. isok) CYCLE loop1
#endif
    val%val(ii) = obj%val(indx(ii))
  END DO loop1
END IF
END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif
INTEGER(I4B) :: i1, i2, i3, tsize, ii, jj

i1 = istart
i2 = Input(default=obj%tsize, option=iend)
i3 = Input(default=math%one_i, option=stride)
tsize = MAX((i2 - i1 + i3) / i3, 0)

CALL IntVectorInitiate(obj=val, tsize=tsize)

IF (ALLOCATED(obj%val)) THEN
  jj = 0
  loop1: DO ii = i1, i2, i3
    jj = jj + 1
#ifdef DEBUG_VER
    isok = ii .LE. obj%tsize
    IF (.NOT. isok) CYCLE loop1
#endif
    val%val(jj) = obj%val(ii)
  END DO loop1
END IF
END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
INTEGER(I4B) :: tsize, ii, jj
tsize = 0
DO ii = 1, SIZE(obj)
  tsize = tsize + obj(ii)%tsize
END DO
CALL IntVectorInitiate(obj=val, tsize=tsize)

tsize = 0
DO ii = 1, SIZE(obj)
  DO jj = 1, obj(ii)%tsize
    tsize = tsize + 1
    val%val(tsize) = obj(ii)%val(jj)
  END DO
END DO
END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif
INTEGER(I4B) :: tsize, ii, jj

tsize = SIZE(indx) * SIZE(obj)
CALL IntVectorInitiate(val, tsize)

tsize = 0
loop1: DO ii = 1, SIZE(obj)
  loop2: DO jj = 1, SIZE(indx)
    tsize = tsize + 1

#ifdef DEBUG_VER
    isok = indx(jj) .LE. obj(ii)%tsize
    IF (.NOT. isok) CYCLE loop2
#endif

    val%val(tsize) = obj(ii)%val(indx(jj))
  END DO loop2
END DO loop1
END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif
INTEGER(I4B) :: tsize, ii, jj

tsize = MAX((iend - istart + stride) / stride, 0)
tsize = tsize * SIZE(obj)
CALL IntVectorInitiate(val, tsize)

tsize = 0
loop1: DO ii = 1, SIZE(obj)
  loop2: DO jj = istart, iend, stride
    tsize = tsize + 1

#ifdef DEBUG_VER
    isok = jj .LE. obj(ii)%tsize
    IF (.NOT. isok) CYCLE loop2
#endif

    val%val(tsize) = obj(ii)%val(jj)
  END DO loop2
END DO loop1
END PROCEDURE obj_Get6

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7a
#define _KIND_ INT8
#include "./include/Get7.F90"
#undef _KIND_
END PROCEDURE obj_Get7a

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7b
#define _KIND_ INT16
#include "./include/Get7.F90"
#undef _KIND_
END PROCEDURE obj_Get7b

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7c
#define _KIND_ INT32
#include "./include/Get7.F90"
#undef _KIND_
END PROCEDURE obj_Get7c

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7d
#define _KIND_ INT64
#include "./include/Get7.F90"
#undef _KIND_
END PROCEDURE obj_Get7d

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get8a
#define _KIND_ INT8
#include "./include/Get8.F90"
#undef _KIND_
END PROCEDURE obj_Get8a

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get8b
#define _KIND_ INT16
#include "./include/Get8.F90"
#undef _KIND_
END PROCEDURE obj_Get8b

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get8c
#define _KIND_ INT32
#include "./include/Get8.F90"
#undef _KIND_
END PROCEDURE obj_Get8c

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get8d
#define _KIND_ INT64
#include "./include/Get8.F90"
#undef _KIND_
END PROCEDURE obj_Get8d

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get9a
#define _KIND_ INT8
#include "./include/Get9.F90"
#undef _KIND_
END PROCEDURE obj_Get9a

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get9b
#define _KIND_ INT16
#include "./include/Get9.F90"
#undef _KIND_
END PROCEDURE obj_Get9b

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get9c
#define _KIND_ INT32
#include "./include/Get9.F90"
#undef _KIND_
END PROCEDURE obj_Get9c

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get9d
#define _KIND_ INT64
#include "./include/Get9.F90"
#undef _KIND_
END PROCEDURE obj_Get9d

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get10a
#define _KIND_ INT8
#include "./include/Get10.F90"
#undef _KIND_
END PROCEDURE obj_Get10a

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get10b
#define _KIND_ INT16
#include "./include/Get10.F90"
#undef _KIND_
END PROCEDURE obj_Get10b

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get10c
#define _KIND_ INT32
#include "./include/Get10.F90"
#undef _KIND_
END PROCEDURE obj_Get10c

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get10d
#define _KIND_ INT64
#include "./include/Get10.F90"
#undef _KIND_
END PROCEDURE obj_Get10d

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get11a
#define _KIND_ INT8
#include "./include/Get11.F90"
#undef _KIND_
END PROCEDURE obj_Get11a

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get11b
#define _KIND_ INT16
#include "./include/Get11.F90"
#undef _KIND_
END PROCEDURE obj_Get11b

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get11c
#define _KIND_ INT32
#include "./include/Get11.F90"
#undef _KIND_
END PROCEDURE obj_Get11c

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get11d
#define _KIND_ INT64
#include "./include/Get11.F90"
#undef _KIND_
END PROCEDURE obj_Get11d

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get12a
#define _KIND_ INT8
#include "./include/Get12.F90"
#undef _KIND_
END PROCEDURE obj_Get12a

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get12b
#define _KIND_ INT16
#include "./include/Get12.F90"
#undef _KIND_
END PROCEDURE obj_Get12b

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get12c
#define _KIND_ INT32
#include "./include/Get12.F90"
#undef _KIND_
END PROCEDURE obj_Get12c

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get12d
#define _KIND_ INT64
#include "./include/Get12.F90"
#undef _KIND_
END PROCEDURE obj_Get12d

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get13a
IF (indx .LE. obj%tsize) THEN
  val = INT(obj%val(indx), kind=INT8)
ELSE
  val = 0
END IF
END PROCEDURE obj_Get13a

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------
MODULE PROCEDURE obj_Get13b
IF (indx .LE. obj%tsize) THEN
  val = INT(obj%val(indx), kind=INT16)
ELSE
  val = 0
END IF
END PROCEDURE obj_Get13b

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------
MODULE PROCEDURE obj_Get13c
IF (indx .LE. obj%tsize) THEN
  val = INT(obj%val(indx), kind=INT32)
ELSE
  val = 0
END IF
END PROCEDURE obj_Get13c

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------
!
MODULE PROCEDURE obj_Get13d
IF (indx .LE. obj%tsize) THEN
  val = INT(obj%val(indx), kind=INT64)
ELSE
  val = 0
END IF
END PROCEDURE obj_Get13d

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIndex1
ans = Util_GetIndex(obj%val(1:obj%tsize), val)
END PROCEDURE obj_GetIndex1

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIndex2
INTEGER(I4B) :: i, j, m
LOGICAL(LGT), ALLOCATABLE :: search(:)
!
m = SIZE(val)
ALLOCATE (search(m), ans(m))
search = .TRUE.
ans = 0

DO i = 1, obj%tsize
  DO j = 1, m
    IF (search(j)) THEN
      IF (val(j) .EQ. obj%val(i)) THEN
        search(j) = .FALSE.
        ans(j) = i
      END IF
    END IF
  END DO
END DO

IF (ALLOCATED(search)) DEALLOCATE (search)
END PROCEDURE obj_GetIndex2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
