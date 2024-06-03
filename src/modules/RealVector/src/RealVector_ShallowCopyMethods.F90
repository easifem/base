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

MODULE RealVector_ShallowCopyMethods
USE GlobalData, ONLY: DFP, I4B, REAL32, REAL64
USE BaseType, ONLY: RealVector_, DOF_

IMPLICIT NONE

PRIVATE
PUBLIC :: ShallowCopy

!----------------------------------------------------------------------------
!                                            ShallowCopy@ShallowCopyMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-28
! summary: Copy only the structure for Y = X

INTERFACE ShallowCopy
  MODULE PURE SUBROUTINE obj_ShallowCopy1a(Y, X)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Y(:)
    REAL(REAL32), INTENT(IN) :: X(:)
  END SUBROUTINE obj_ShallowCopy1a

  MODULE PURE SUBROUTINE obj_ShallowCopy1b(Y, X)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Y(:)
    REAL(REAL64), INTENT(IN) :: X(:)
  END SUBROUTINE obj_ShallowCopy1b

  MODULE PURE SUBROUTINE obj_ShallowCopy1c(Y, X)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Y(:)
    REAL(REAL32), INTENT(IN) :: X(:)
  END SUBROUTINE obj_ShallowCopy1c

  MODULE PURE SUBROUTINE obj_ShallowCopy1d(Y, X)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Y(:)
    REAL(REAL64), INTENT(IN) :: X(:)
  END SUBROUTINE obj_ShallowCopy1d
END INTERFACE ShallowCopy

!----------------------------------------------------------------------------
!                                            ShallowCopy@ShallowCopyMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE ShallowCopy
  MODULE PURE SUBROUTINE obj_ShallowCopy2(Y, X)
    TYPE(RealVector_), INTENT(INOUT) :: Y
    CLASS(RealVector_), INTENT(IN) :: X
  END SUBROUTINE obj_ShallowCopy2
END INTERFACE ShallowCopy

!----------------------------------------------------------------------------
!                                            ShallowCopy@ShallowCopyMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE ShallowCopy
  MODULE PURE SUBROUTINE obj_ShallowCopy3(Y, X)
    TYPE(RealVector_), INTENT(INOUT), ALLOCATABLE :: Y(:)
    CLASS(RealVector_), INTENT(IN) :: X(:)
  END SUBROUTINE obj_ShallowCopy3
END INTERFACE ShallowCopy

!----------------------------------------------------------------------------
!                                            ShallowCopy@ShallowCopyMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE ShallowCopy
  MODULE PURE SUBROUTINE obj_ShallowCopy4(Y, X)
    TYPE(RealVector_), INTENT(INOUT) :: Y
    CLASS(RealVector_), INTENT(IN) :: X(:)
  END SUBROUTINE obj_ShallowCopy4
END INTERFACE ShallowCopy

!----------------------------------------------------------------------------
!                                            ShallowCopy@ShallowCopyMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE ShallowCopy
  MODULE PURE SUBROUTINE obj_ShallowCopy5a(Y, X)
    CLASS(RealVector_), INTENT(INOUT) :: Y
    REAL(REAL32), INTENT(IN) :: X(:)
  END SUBROUTINE obj_ShallowCopy5a
  MODULE PURE SUBROUTINE obj_ShallowCopy5b(Y, X)
    CLASS(RealVector_), INTENT(INOUT) :: Y
    REAL(REAL64), INTENT(IN) :: X(:)
  END SUBROUTINE obj_ShallowCopy5b
END INTERFACE ShallowCopy

!----------------------------------------------------------------------------
!                                            ShallowCopy@ShallowCopyMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE ShallowCopy
  MODULE PURE SUBROUTINE obj_ShallowCopy6a(Y, X)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Y(:)
    CLASS(RealVector_), INTENT(IN) :: X
  END SUBROUTINE obj_ShallowCopy6a
  MODULE PURE SUBROUTINE obj_ShallowCopy6b(Y, X)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Y(:)
    CLASS(RealVector_), INTENT(IN) :: X
  END SUBROUTINE obj_ShallowCopy6b
END INTERFACE ShallowCopy

!----------------------------------------------------------------------------
!                                            ShallowCopy@ShallowCopyMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE ShallowCopy
  MODULE PURE SUBROUTINE obj_ShallowCopy7a(Y, X)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Y(:)
    CLASS(RealVector_), INTENT(IN) :: X(:)
  END SUBROUTINE obj_ShallowCopy7a
  MODULE PURE SUBROUTINE obj_ShallowCopy7b(Y, X)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Y(:)
    CLASS(RealVector_), INTENT(IN) :: X(:)
  END SUBROUTINE obj_ShallowCopy7b
END INTERFACE ShallowCopy

END MODULE RealVector_ShallowCopyMethods
