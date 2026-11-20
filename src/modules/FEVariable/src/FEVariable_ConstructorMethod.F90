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

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Constructor methods of FEVariable_
!
! Methods to initiate, set the length of, deallocate and copy
! `FEVariable_`, a finite element variable.

MODULE FEVariable_ConstructorMethod
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: TypeFEVariableOpt
USE GlobalData, ONLY: I4B, DFP, LGT
IMPLICIT NONE

PRIVATE

PUBLIC :: DEALLOCATE
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: Copy
PUBLIC :: Initiate
PUBLIC :: SetLen

!----------------------------------------------------------------------------
!                                                   SetLen@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-11
! summary: Set the length of FEVariable_
!
! Sets `obj%len` to `tsize`.
! It does nothing when `obj` is not initiated.
!
! If `obj%val` has no allocation, it gets size `tsize`.
! If its size is at least `tsize`, the allocation and capacity stay.
! Otherwise `obj%val` gets a new allocation of size `tsize`.
!
! @warning
! This call sets `obj%val` to zero in every case. You lose the old data.
! @endwarning
!
! Examples
!
! - [test 1](|base|FEVariable/examples/SetLen_test_1.F90)
!
! See also: [[Initiate]]

INTERFACE SetLen
  MODULE PURE SUBROUTINE obj_SetLen1(obj, tsize)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    !! initiated finite element variable
    INTEGER(I4B), INTENT(IN) :: tsize
    !! new length of data
  END SUBROUTINE obj_SetLen1
END INTERFACE SetLen

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-02
! summary: Initiate FEVariable_ with shape, type and values
!
! Initiates `obj` as in [[obj_Initiate2]].
! Then it copies `val(1:len)` into `obj%val(1:len)`.
!
! @note
! `val` must have at least `len` entries.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Initiate_test_1.F90)
! - [test 2](|base|FEVariable/examples/Initiate_test_2.F90) with long `val`
!
! See also: [[SetLen]], [[obj_Initiate2]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate1( &
    obj, s, defineon, vartype, rank, len, val)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    !! finite element variable
    INTEGER(I4B), INTENT(IN) :: s(:)
    !! shape of data, `SIZE(s) <= TypeFEVariableOpt%maxRank`
    INTEGER(I4B), INTENT(IN) :: defineon
    !! where is the data defined nodal or quadrature
    INTEGER(I4B), INTENT(IN) :: vartype
    !! variable type: constant, space, time, `spaceTime`
    INTEGER(I4B), INTENT(IN) :: rank
    !! rank of the variable: scalar, vector, matrix
    INTEGER(I4B), INTENT(IN) :: len
    !! number of entries to copy from `val`
    REAL(DFP), INTENT(IN) :: val(:)
    !! data in column major order, SIZE(val) >= len
  END SUBROUTINE obj_Initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-02
! summary: Initiate FEVariable_ with shape and type, zero values
!
! Initiates `obj` with zero values.
! It sets `isInit`, `s`, `tshape = SIZE(s)`, `defineOn`, `varType`,
! `rank` and `len`.
!
! If `obj%val` has an allocation of at least `len` entries, `obj` keeps
! it. Then `capacity` is the size of `obj%val`, and all values become
! zero.
!
! Otherwise `obj%val` gets a new allocation with
! `capacity = TypeFEVariableOpt%capacityExpandFactor * len`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Initiate_test_1.F90) without values
! - [test 2](|base|FEVariable/examples/Initiate_test_2.F90) with `len = 0`
!
! See also: [[obj_Initiate1]], [[SetLen]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate2( &
    obj, s, defineon, vartype, rank, len)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    !! finite element variable
    INTEGER(I4B), INTENT(IN) :: s(:)
    !! shape of data, `SIZE(s) <= TypeFEVariableOpt%maxRank`
    INTEGER(I4B), INTENT(IN) :: defineon
    !! where is the data defined nodal or quadrature
    INTEGER(I4B), INTENT(IN) :: vartype
    !! variable type: constant, space, time, `spaceTime`
    INTEGER(I4B), INTENT(IN) :: rank
    !! rank of the variable: scalar, vector, matrix
    INTEGER(I4B), INTENT(IN) :: len
    !! length of data
  END SUBROUTINE obj_Initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Deallocates the content of FEVariable_
!
! Resets every component of `obj` to its default value.
! It also deallocates `obj%val`.
! It is safe to call this on an object that is not initiated.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Deallocate_test_1.F90)

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE obj_Deallocate(obj)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    !! finite element variable
  END SUBROUTINE obj_Deallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                              Assignment@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-13
! summary: Copy obj2 into obj1, obj1 = obj2
!
! Copies `obj2` into `obj1`.
! This copies `s`, `tshape`, `defineOn`, `rank`, `varType`, `len` and
! `isInit`. It also copies `val(1:len)`.
!
! If the capacity of `obj1` is at least `obj2%len`, `obj1` keeps its
! allocation and capacity.
! Otherwise `obj1%val` gets a new allocation with
! `capacity = TypeFEVariableOpt%capacityExpandFactor * len`.
!
! You can call it as `Copy(obj1, obj2)` or as `obj1 = obj2`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Copy_test_1.F90)
! - [test 2](|base|FEVariable/examples/Copy_test_2.F90) with growth

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE obj_Copy(obj1, obj2)
    TYPE(FEVariable_), INTENT(INOUT) :: obj1
    !! destination
    TYPE(FEVariable_), INTENT(IN) :: obj2
    !! source
  END SUBROUTINE obj_Copy
END INTERFACE

INTERFACE Copy
  MODULE PROCEDURE obj_Copy
END INTERFACE Copy

END MODULE FEVariable_ConstructorMethod
