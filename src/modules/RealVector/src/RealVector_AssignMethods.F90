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

MODULE RealVector_AssignMethods
USE GlobalData, ONLY: DFP, I4B, REAL32, REAL64
USE BaseType, ONLY: RealVector_

IMPLICIT NONE

PRIVATE

PUBLIC :: ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE obj_assign1(lhs, rhs)
    CLASS(RealVector_), INTENT(INOUT) :: lhs
    CLASS(RealVector_), INTENT(IN) :: rhs
  END SUBROUTINE obj_assign1
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE obj_assign2(lhs, rhs)
    CLASS(RealVector_), INTENT(INOUT) :: lhs
    CLASS(RealVector_), INTENT(IN) :: rhs(:)
  END SUBROUTINE obj_assign2
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE obj_assign3a(lhs, rhs)
    CLASS(RealVector_), INTENT(INOUT) :: lhs
    REAL(REAL32), INTENT(IN) :: rhs(:)
  END SUBROUTINE obj_assign3a

  MODULE PURE SUBROUTINE obj_assign3b(lhs, rhs)
    CLASS(RealVector_), INTENT(INOUT) :: lhs
    REAL(REAL64), INTENT(IN) :: rhs(:)
  END SUBROUTINE obj_assign3b
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE obj_assign4a(lhs, rhs)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: lhs(:)
    CLASS(RealVector_), INTENT(IN) :: rhs
  END SUBROUTINE obj_assign4a
  MODULE PURE SUBROUTINE obj_assign4b(lhs, rhs)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: lhs(:)
    CLASS(RealVector_), INTENT(IN) :: rhs
  END SUBROUTINE obj_assign4b
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE obj_assign5a(lhs, rhs)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: lhs(:)
    CLASS(RealVector_), INTENT(IN) :: rhs(:)
  END SUBROUTINE obj_assign5a
  MODULE PURE SUBROUTINE obj_assign5b(lhs, rhs)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: lhs(:)
    CLASS(RealVector_), INTENT(IN) :: rhs(:)
  END SUBROUTINE obj_assign5b
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE obj_assign6(lhs, rhs)
    CLASS(RealVector_), INTENT(INOUT) :: lhs
    INTEGER(I4B), INTENT(IN) :: rhs(:)
  END SUBROUTINE obj_assign6
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE obj_assign7(lhs, rhs)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: lhs(:)
    CLASS(RealVector_), INTENT(IN) :: rhs
  END SUBROUTINE obj_assign7
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE obj_assign8(lhs, rhs)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: lhs(:)
    CLASS(RealVector_), INTENT(IN) :: rhs(:)
  END SUBROUTINE obj_assign8
END INTERFACE ASSIGNMENT(=)

END MODULE RealVector_AssignMethods
