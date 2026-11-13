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

SUBMODULE(IntVector_SetMethod) Methods
USE BaseType, ONLY: math => TypeMathOpt
USE IntegerUtility, ONLY: IntRepeat => Repeat
USE IntegerUtility, ONLY: IntRemoveDuplicates_ => RemoveDuplicates_
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                        setTotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTotalDimension
obj%tDimension = tDimension
END PROCEDURE obj_SetTotalDimension

!----------------------------------------------------------------------------
!                                                                 setMethod
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
isok = ALLOCATED(obj%val) .AND. ALL(indx .LE. obj%tsize)
IF (.NOT. isok) RETURN
#endif

! check the size of indx and value in debug mode

IF (SIZE(VALUE) .EQ. 1) THEN
  obj%val(indx) = VALUE(1)
ELSE
#ifdef DEBUG_VER
  isok = SIZE(indx) .EQ. SIZE(VALUE)
  IF (.NOT. isok) RETURN
#endif
  obj%val(indx) = VALUE
END IF

END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                 setMethod
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
isok = ALLOCATED(obj%val) .AND. (indx .LE. obj%tsize)
IF (.NOT. isok) RETURN
#endif

obj%val(indx) = VALUE
END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                 setMethod
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
isok = ALLOCATED(obj%val)
IF (.NOT. isok) RETURN
#endif

IF (obj%tsize .GT. 0) THEN
  obj%val(1:obj%tsize) = VALUE
END IF
END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                 setMethod
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif
INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
isok = ALLOCATED(obj%val)
IF (.NOT. isok) RETURN
#endif

tsize = MIN(SIZE(VALUE), obj%tsize)
DO ii = 1, tsize
  obj%val(ii) = VALUE(ii)
END DO
END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                             RemoveDuplicate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RemoveDuplicates1
INTEGER(I4B) :: tsize
IF (ALLOCATED(obj%val)) THEN
  CALL IntRemoveDuplicates_(obj%val(1:obj%tsize), tsize=tsize, &
                            isSorted=math%no)
  obj%tsize = tsize
END IF
END PROCEDURE obj_RemoveDuplicates1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
