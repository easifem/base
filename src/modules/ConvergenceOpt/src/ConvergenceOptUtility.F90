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

MODULE ConvergenceOptUtility
USE GlobalData, ONLY: DFP, I4B, LGT
USE String_Class, ONLY: String
IMPLICIT NONE
PRIVATE

PUBLIC :: ConvergenceType_ToInt
PUBLIC :: ConvergenceType_ToChar
PUBLIC :: ConvergenceType_ToString
PUBLIC :: ConvergenceIn_ToInt
PUBLIC :: ConvergenceIn_ToChar
PUBLIC :: ConvergenceIn_ToString
PUBLIC :: NormType_ToInt
PUBLIC :: NormType_ToChar
PUBLIC :: NormType_ToString

!----------------------------------------------------------------------------
!                                                       ConvergenceType_ToInt
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: convert convergence from character to integer

INTERFACE
  MODULE FUNCTION ConvergenceType_ToInt(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION ConvergenceType_ToInt
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ConvergenceType_ToChar
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: convert convergence from integer to character

INTERFACE
  MODULE FUNCTION ConvergenceType_ToChar(name, isUpper) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: name
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUpper
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION ConvergenceType_ToChar
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ConvergenceType_ToString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: convert convergence from integer to character

INTERFACE
  MODULE FUNCTION ConvergenceType_ToString(name, isUpper) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: name
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUpper
    TYPE(String) :: ans
  END FUNCTION ConvergenceType_ToString
END INTERFACE

!----------------------------------------------------------------------------
!                                                        ConvergenceIn_ToInt
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: convert convergence from character to integer

INTERFACE
  MODULE FUNCTION ConvergenceIn_ToInt(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION ConvergenceIn_ToInt
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ConvergenceIn_ToChar
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: convert convergence from integer to character

INTERFACE
  MODULE FUNCTION ConvergenceIn_ToChar(name, isUpper) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: name
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUpper
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION ConvergenceIn_ToChar
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ConvergenceIn_ToString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: convert convergence from integer to character

INTERFACE
  MODULE FUNCTION ConvergenceIn_ToString(name, isUpper) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: name
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUpper
    TYPE(String) :: ans
  END FUNCTION ConvergenceIn_ToString
END INTERFACE

!----------------------------------------------------------------------------
!                                                             NormType_ToInt
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: convert convergence from character to integer

INTERFACE
  MODULE FUNCTION NormType_ToInt(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION NormType_ToInt
END INTERFACE

!----------------------------------------------------------------------------
!                                                            NormType_ToChar
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: convert convergence from integer to character

INTERFACE
  MODULE FUNCTION NormType_ToChar(name, isUpper) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: name
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUpper
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION NormType_ToChar
END INTERFACE

!----------------------------------------------------------------------------
!                                                           NormType_ToString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: convert convergence from integer to character

INTERFACE
  MODULE FUNCTION NormType_ToString(name, isUpper) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: name
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUpper
    TYPE(String) :: ans
  END FUNCTION NormType_ToString
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ConvergenceOptUtility
