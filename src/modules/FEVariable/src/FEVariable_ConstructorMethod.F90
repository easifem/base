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

MODULE FEVariable_ConstructorMethod
USE BaseType, ONLY: FEVariable_, &
                    FEVariableScalar_, &
                    FEVariableVector_, &
                    FEVariableMatrix_, &
                    FEVariableConstant_, &
                    FEVariableSpace_, &
                    FEVariableTime_, &
                    FEVariableSpaceTime_, &
                    TypeFEVariableOpt

USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: DEALLOCATE
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: Copy
PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-02
! summary: Initiate FEVariable

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate1(obj, s, defineon, vartype, rank, &
                                       len, val)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: s(:)
    !! shape of data
    INTEGER(I4B), INTENT(IN) :: defineon
    !! where is the data defined nodal or quadrature
    INTEGER(I4B), INTENT(IN) :: vartype
    !! variable type
    INTEGER(I4B), INTENT(IN) :: rank
    !! rank of the variable
    INTEGER(I4B), INTENT(IN) :: len
    !! length of data to be extractd from val
    REAL(DFP), INTENT(IN) :: val(:)
    !! The size of val should be atleast len
  END SUBROUTINE obj_Initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-02
! summary: Initiate FEVariable

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate2(obj, s, defineon, vartype, rank, &
                                       len)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: s(:)
    !! shape of data
    INTEGER(I4B), INTENT(IN) :: defineon
    !! where is the data defined nodal or quadrature
    INTEGER(I4B), INTENT(IN) :: vartype
    !! variable type
    INTEGER(I4B), INTENT(IN) :: rank
    !! rank of the variable
    INTEGER(I4B), INTENT(IN) :: len
    !! length of data to be extractd from val
  END SUBROUTINE obj_Initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Deallocates the content of FEVariable

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE obj_Deallocate(obj)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                              Assignment@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-13
! summary: obj1 = obj2

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE obj_Copy(obj1, obj2)
    TYPE(FEVariable_), INTENT(INOUT) :: obj1
    TYPE(FEVariable_), INTENT(IN) :: obj2
  END SUBROUTINE obj_Copy
END INTERFACE

INTERFACE Copy
  MODULE PROCEDURE obj_Copy
END INTERFACE Copy

END MODULE FEVariable_ConstructorMethod
