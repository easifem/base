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

#define _OP_ -

MODULE FEVariable_SubtractionMethod
USE BaseType, ONLY: FEVariable_
USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE
! PUBLIC :: OPERATOR(_OP_)
PUBLIC :: Subtraction_

!----------------------------------------------------------------------------
!                                      Subtraction_@SubtractionMethods
!----------------------------------------------------------------------------
!> author: Vikas Sharma, Ph. D.
! date: 2025-12-28
! summary: Subtraction_ is without extra allocation, user should know
!          what they are doing

INTERFACE
  MODULE PURE SUBROUTINE fevar_Subtraction_1(obj1, obj2, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    CLASS(FEVariable_), INTENT(INOUT) :: ans
  END SUBROUTINE fevar_Subtraction_1
END INTERFACE

INTERFACE Subtraction_
  MODULE PROCEDURE fevar_Subtraction_1
END INTERFACE Subtraction_

!----------------------------------------------------------------------------
!                                      Subtraction_@SubtractionMethods
!----------------------------------------------------------------------------
!> author: Vikas Sharma, Ph. D.
! date: 2025-12-28
! summary: Subtraction_ is without extra allocation, user should know
!          what they are doing

INTERFACE
  MODULE PURE SUBROUTINE fevar_Subtraction_2(obj, val, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val
    CLASS(FEVariable_), INTENT(INOUT) :: ans
  END SUBROUTINE fevar_Subtraction_2
END INTERFACE

INTERFACE Subtraction_
  MODULE PROCEDURE fevar_Subtraction_2
END INTERFACE Subtraction_

!----------------------------------------------------------------------------
!                                      Subtraction@SubtractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-1
! summary: FEVariable = FEVariable * FEVariable

INTERFACE
  MODULE PURE FUNCTION fevar_Subtraction1(obj1, obj2) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Subtraction1
END INTERFACE

INTERFACE OPERATOR(_OP_)
  MODULE PROCEDURE fevar_Subtraction1
END INTERFACE OPERATOR(_OP_)

!----------------------------------------------------------------------------
!                                      Subtraction@SubtractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: FEVariable = FEVariable * Real

INTERFACE
  MODULE PURE FUNCTION fevar_Subtraction2(obj1, val) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    REAL(DFP), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Subtraction2
END INTERFACE

INTERFACE OPERATOR(_OP_)
  MODULE PROCEDURE fevar_Subtraction2
END INTERFACE OPERATOR(_OP_)

!----------------------------------------------------------------------------
!                                      Subtraction@SubtractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: FEVariable = Real * FEVariable

INTERFACE
  MODULE PURE FUNCTION fevar_Subtraction3(val, obj1) RESULT(ans)
    REAL(DFP), INTENT(IN) :: val
    CLASS(FEVariable_), INTENT(IN) :: obj1
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_Subtraction3
END INTERFACE

INTERFACE OPERATOR(_OP_)
  MODULE PROCEDURE fevar_Subtraction3
END INTERFACE OPERATOR(_OP_)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_SubtractionMethod

#undef _OP_
