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
USE GlobalData
USE String_Class, ONLY: String
USE BaseType
USE Utility, ONLY: UpperCase
IMPLICIT NONE
PRIVATE
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: BaseContinuity_toString
PUBLIC :: BaseContinuity_fromString

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE BaseContinuity_Copy
END INTERFACE

CONTAINS

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
    CALL ErrorMsg(&
    & msg="NO CASE FOUND for type of obj2", &
    & line=__LINE__,  &
    & unitno=stdout, &
    & routine="BaseContinuity_Copy()",  &
    & file=__FILE__ &
    & )

  END SELECT
END SUBROUTINE BaseContinuity_Copy

!----------------------------------------------------------------------------
!                                                 BaseContinuity_toString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Returns a string name of base interpolation type

FUNCTION BaseContinuity_toString(obj) RESULT(ans)
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
    CALL ErrorMsg(&
    & msg="NO CASE FOUND for type of obj", &
    & line=__LINE__,  &
    & unitno=stdout, &
    & routine="BaseContinuity_toString()",  &
    & file=__FILE__ &
    & )
  END SELECT
END FUNCTION BaseContinuity_toString

!----------------------------------------------------------------------------
!                                                 BaseContinuity_fromString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Returns a string name of base interpolation type

SUBROUTINE BaseContinuity_fromString(obj, name)
  CLASS(BaseContinuity_), ALLOCATABLE, INTENT(OUT) :: obj
  CHARACTER(*), INTENT(IN) :: name
  TYPE(String) :: ans

  ans = UpperCase(name)
  IF (ALLOCATED(obj)) DEALLOCATE (obj)

  SELECT CASE (ans%chars())
  CASE ("H1")
    ALLOCATE (H1_ :: obj)
  CASE ("HDIV")
    ALLOCATE (HDiv_ :: obj)
  CASE ("HCURL")
    ALLOCATE (HCurl_ :: obj)
  CASE ("DG")
    ALLOCATE (DG_ :: obj)
  CASE DEFAULT
    CALL ErrorMsg(&
    & msg="NO CASE FOUND for given name="//TRIM(name), &
    & line=__LINE__,  &
    & unitno=stdout, &
    & routine="BaseContinuity_fromString()",  &
    & file=__FILE__ &
    & )
  END SELECT
END SUBROUTINE BaseContinuity_fromString

END MODULE BaseContinuity_Method
