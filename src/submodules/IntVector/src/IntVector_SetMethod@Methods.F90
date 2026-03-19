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
! summary:         This submodule implements set methods of [[IntVector_]]

SUBMODULE(IntVector_SetMethod) Methods
USE IntegerUtility, ONLY: IntRepeat => Repeat
USE IntegerUtility, ONLY: IntRemoveDuplicates => RemoveDuplicates
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
LOGICAL(LGT) :: isok

isok = ALLOCATED(obj%val)

IF (isok) THEN
  isok = SIZE(VALUE) .EQ. 1

  IF (isok) THEN
    obj%val(indx) = VALUE(1)
  ELSE
    obj%val(indx) = VALUE
  END IF

END IF
END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                 setMethod
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
LOGICAL(LGT) :: isok

isok = ALLOCATED(obj%val)

IF (isok) THEN
  obj%val(indx) = VALUE
END IF

END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                 setMethod
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
LOGICAL(LGT) :: isok

isok = ALLOCATED(obj%val)

IF (isok) THEN
  obj%val = VALUE
END IF
END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                 setMethod
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, tsize

isok = ALLOCATED(obj%val)

tsize = SIZE(VALUE)

IF (isok) THEN
  DO ii = 1, tsize
    obj%val(ii) = VALUE(ii)
  END DO
END IF
END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                             RemoveDuplicate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RemoveDuplicates1
CALL IntRemoveDuplicates(obj%val)
END PROCEDURE obj_RemoveDuplicates1

!----------------------------------------------------------------------------
!                                                                     Repeat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Repeat1
ans = IntRepeat(val=obj%val, rtimes=rtimes)
END PROCEDURE obj_Repeat1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
