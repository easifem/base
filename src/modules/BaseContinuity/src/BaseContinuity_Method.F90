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
!

MODULE BaseContinuity_Method
USE ErrorHandling, ONLY: Errormsg

USE GlobalData, ONLY: I4B, LGT, stderr

USE String_Class, ONLY: String

USE BaseType, ONLY: BaseContinuity_, &
                    H1_, &
                    HCURL_, &
                    HDIV_, &
                    DG_

USE StringUtility, ONLY: UpperCase

IMPLICIT NONE

PRIVATE

PUBLIC :: ASSIGNMENT(=)
PUBLIC :: BaseContinuity_ToString
PUBLIC :: BaseContinuity_FromString
PUBLIC :: BaseContinuityPointer_FromString

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE BaseContinuity_Copy
END INTERFACE

CONTAINS

!----------------------------------------------------------------------------
!                                       BaseContinuityPointer_FromString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 30 Aug 2021
! summary: This routine returns a pointer to a child of BaseContinuity_

FUNCTION BaseContinuityPointer_FromString(name) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: name
  CLASS(BaseContinuity_), POINTER :: ans
  !!
  CHARACTER(len=2) :: astr

  astr = UpperCase(name(1:2))

  SELECT CASE (astr)
  CASE ("H1")
    ALLOCATE (H1_ :: ans)

  CASE ("HD")
    ALLOCATE (HDiv_ :: ans)

  CASE ("HC")
    ALLOCATE (HCurl_ :: ans)

  CASE ("DG")
    ALLOCATE (DG_ :: ans)

  CASE DEFAULT
    CALL ErrorMsg(msg="NO CASE FOUND for given name="//astr, &
                  routine="BaseContinuityPointer_FromString()", &
                  line=__LINE__, unitno=stderr, file=__FILE__)
    STOP
  END SELECT
END FUNCTION BaseContinuityPointer_FromString

!----------------------------------------------------------------------------
!                                                 BaseContinuity_Copy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Copy BaseContinuity

SUBROUTINE BaseContinuity_Copy(obj1, obj2)
  CLASS(BaseContinuity_), ALLOCATABLE, INTENT(INOUT) :: obj1
  CLASS(BaseContinuity_), INTENT(IN) :: obj2

  IF (ALLOCATED(obj1)) THEN
    DEALLOCATE (obj1)
  END IF

  SELECT TYPE (obj2)
  CLASS IS (H1_)
    ALLOCATE (H1_ :: obj1)

  CLASS IS (HDiv_)
    ALLOCATE (HDiv_ :: obj1)

  CLASS IS (HCurl_)
    ALLOCATE (HCurl_ :: obj1)

  CLASS IS (DG_)
    ALLOCATE (DG_ :: obj1)

  CLASS DEFAULT
    CALL ErrorMsg(msg="NO CASE FOUND for type of obj2", &
                  routine="BaseContinuity_Copy()", line=__LINE__, &
                  unitno=stderr, file=__FILE__)
    STOP

  END SELECT
END SUBROUTINE BaseContinuity_Copy

!----------------------------------------------------------------------------
!                                                 BaseContinuity_toString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Returns a string name of base interpolation type

FUNCTION BaseContinuity_ToString(obj) RESULT(ans)
  CLASS(BaseContinuity_), INTENT(IN) :: obj
  TYPE(String) :: ans

  SELECT TYPE (obj)
  CLASS IS (H1_)
    ans = "H1"
  CLASS IS (HCurl_)
    ans = "HCurl"
  CLASS IS (HDiv_)
    ans = "HDiv"
  CLASS IS (DG_)
    ans = "DG"
  CLASS DEFAULT
    CALL ErrorMsg(msg="NO CASE FOUND for type of obj", &
                  routine="BaseContinuity_toString()", &
                  line=__LINE__, unitno=stderr, file=__FILE__)
    STOP
  END SELECT
END FUNCTION BaseContinuity_ToString

!----------------------------------------------------------------------------
!                                                 BaseContinuity_fromString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Returns a string name of base interpolation type

SUBROUTINE BaseContinuity_FromString(obj, name)
  CLASS(BaseContinuity_), ALLOCATABLE, INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: name

  CHARACTER(len=2) :: ans

  ans = UpperCase(name(1:2))

  IF (ALLOCATED(obj)) DEALLOCATE (obj)

  SELECT CASE (ans)

  CASE ("H1")
    ALLOCATE (H1_ :: obj)

  CASE ("HD")
    ALLOCATE (HDiv_ :: obj)

  CASE ("HC")
    ALLOCATE (HCurl_ :: obj)

  CASE ("DG")
    ALLOCATE (DG_ :: obj)

  CASE DEFAULT
    CALL ErrorMsg(msg="NO CASE FOUND for given name="//TRIM(name), &
                  routine="BaseContinuity_fromString()", &
                  line=__LINE__, unitno=stderr, file=__FILE__)
    STOP
  END SELECT
END SUBROUTINE BaseContinuity_FromString

END MODULE BaseContinuity_Method
