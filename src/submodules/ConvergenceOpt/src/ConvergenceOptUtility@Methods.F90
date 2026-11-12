! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(ConvergenceOptUtility) Methods
USE Display_Method, ONLY: ToString
USE StringUtility, ONLY: UpperCase
USE BaseType, ONLY: TypeConvergenceOpt
USE BaseType, ONLY: math => TypeMathOpt
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "ConvergenceOptUtility@Methods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                      ConvergenceType_ToInt
!----------------------------------------------------------------------------

MODULE PROCEDURE ConvergenceType_ToInt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ConvergenceType_ToInt()"
#endif
CHARACTER(:), ALLOCATABLE :: acase

acase = UpperCase(TRIM(name))

SELECT CASE (acase)
CASE ("RELATIVE")
  ans = TypeConvergenceOpt%relative
CASE ("ABSOLUTE")
  ans = TypeConvergenceOpt%absolute
CASE ("BOTH")
  ans = TypeConvergenceOpt%both
CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, modName, __LINE__, &
                    "no case found for name="//acase)
#endif
END SELECT

acase = ""
END PROCEDURE ConvergenceType_ToInt

!----------------------------------------------------------------------------
!                                                      ConvergenceType_ToChar
!----------------------------------------------------------------------------

MODULE PROCEDURE ConvergenceType_ToChar
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ConvergenceType_ToChar()"
#endif

! internal variables
LOGICAL(LGT) :: isUpper0

isUpper0 = math%no
IF (PRESENT(isUpper)) isUpper0 = isUpper

SELECT CASE (name)

CASE (TypeConvergenceOpt%relative)
  IF (isUpper0) THEN
    ans = "RELATIVE"
  ELSE
    ans = "Relative"
  END IF

CASE (TypeConvergenceOpt%absolute)
  IF (isUpper0) THEN
    ans = "ABSOLUTE"
  ELSE
    ans = "Absolute"
  END IF

CASE (TypeConvergenceOpt%both)
  IF (isUpper0) THEN
    ans = "BOTH"
  ELSE
    ans = "Both"
  END IF

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, modName, __LINE__, &
                    "no case found for name="//ToString(name))
#endif
END SELECT

END PROCEDURE ConvergenceType_ToChar

!----------------------------------------------------------------------------
!                                                   ConvergenceType_ToString
!----------------------------------------------------------------------------

MODULE PROCEDURE ConvergenceType_ToString
ans = ConvergenceType_ToChar(name=name, isUpper=isUpper)
END PROCEDURE ConvergenceType_ToString

!----------------------------------------------------------------------------
!                                                       ConvergenceIn_ToInt
!----------------------------------------------------------------------------

MODULE PROCEDURE ConvergenceIn_ToInt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ConvergenceIn_ToInt()"
#endif
CHARACTER(:), ALLOCATABLE :: acase

acase = UpperCase(TRIM(name))

SELECT CASE (acase)
CASE ("RESIDUAL")
  ans = TypeConvergenceOpt%relative
CASE ("SOLUTION")
  ans = TypeConvergenceOpt%absolute
CASE ("BOTH")
  ans = TypeConvergenceOpt%both
CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, modName, __LINE__, &
                    "no case found for name="//acase)
#endif
END SELECT

acase = ""
END PROCEDURE ConvergenceIn_ToInt

!----------------------------------------------------------------------------
!                                                        ConvergenceIn_ToChar
!----------------------------------------------------------------------------

MODULE PROCEDURE ConvergenceIn_ToChar
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ConvergenceIn_ToChar()"
#endif

! internal variables
LOGICAL(LGT) :: isUpper0

isUpper0 = math%no
IF (PRESENT(isUpper)) isUpper0 = isUpper

SELECT CASE (name)

CASE (TypeConvergenceOpt%res)
  IF (isUpper0) THEN
    ans = "RESIDUAL"
  ELSE
    ans = "Residual"
  END IF

CASE (TypeConvergenceOpt%sol)
  IF (isUpper0) THEN
    ans = "SOLUTION"
  ELSE
    ans = "Solution"
  END IF

CASE (TypeConvergenceOpt%both)
  IF (isUpper0) THEN
    ans = "BOTH"
  ELSE
    ans = "Both"
  END IF

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, modName, __LINE__, &
                    "no case found for name="//ToString(name))
#endif
END SELECT

END PROCEDURE ConvergenceIn_ToChar

!----------------------------------------------------------------------------
!                                                     ConvergenceIn_ToString
!----------------------------------------------------------------------------

MODULE PROCEDURE ConvergenceIn_ToString
ans = ConvergenceIn_ToChar(name=name, isUpper=isUpper)
END PROCEDURE ConvergenceIn_ToString

!----------------------------------------------------------------------------
!                                                             NormType_ToInt
!----------------------------------------------------------------------------

MODULE PROCEDURE NormType_ToInt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "NormType_ToInt()"
#endif
CHARACTER(:), ALLOCATABLE :: acase

acase = UpperCase(TRIM(name))

SELECT CASE (acase)
CASE ("L1")
  ans = TypeConvergenceOpt%normL1
CASE ("L2")
  ans = TypeConvergenceOpt%normL2
CASE ("INFINITY")
  ans = TypeConvergenceOpt%normInfinity

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, modName, __LINE__, &
                    "no case found for name="//acase)
#endif
END SELECT

acase = ""
END PROCEDURE NormType_ToInt

!----------------------------------------------------------------------------
!                                                             NormType_ToChar
!----------------------------------------------------------------------------

MODULE PROCEDURE NormType_ToChar
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "NormType_ToChar()"
#endif

! internal variables
LOGICAL(LGT) :: isUpper0

isUpper0 = math%no
IF (PRESENT(isUpper)) isUpper0 = isUpper

SELECT CASE (name)

CASE (TypeConvergenceOpt%normL1)
  ans = "L1"

CASE (TypeConvergenceOpt%normL2)
  ans = "L2"

CASE (TypeConvergenceOpt%normInfinity)
  IF (isUpper0) THEN
    ans = "INFINITY"
  ELSE
    ans = "Infinity"
  END IF

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, modName, __LINE__, &
                    "no case found for name="//ToString(name))
#endif
END SELECT

END PROCEDURE NormType_ToChar

!----------------------------------------------------------------------------
!                                                          NormType_ToString
!----------------------------------------------------------------------------

MODULE PROCEDURE NormType_ToString
ans = NormType_ToChar(name=name, isUpper=isUpper)
END PROCEDURE NormType_ToString

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods

