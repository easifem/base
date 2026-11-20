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
! Initiate, set length, deallocate and copy a finite element variable.

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
! Set `obj%len` to `tsize`. Nothing is done when `obj` is not initiated.
!
! If `obj%val` is not allocated, it is allocated with size `tsize`. If its
! size is at least `tsize`, the allocation and capacity are kept. Otherwise
! `obj%val` is reallocated to size `tsize`. In all cases the whole
! `obj%val` is set to zero, so the old data is lost.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE FEVariable_Method, ONLY: Initiate
! USE FEVariable_Method, ONLY: SetLen
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: SetLen to a smaller length keeps capacity, zeroes data
! BLOCK
!   TYPE(FEVariable_) :: obj
!
!   CALL Initiate(obj=obj, s=[6], defineon=fevaropt%nodal, &
!                 vartype=fevaropt%space, rank=fevaropt%scalar, len=6, &
!                 val=SPREAD(1.0_DFP, 1, 6))
!   CALL SetLen(obj=obj, tsize=3)
!
!   CALL OK(obj%len .EQ. 3, "len = 3")
!   CALL OK(obj%capacity .EQ. 6, "capacity = 6 is kept")
!   CALL OK(ALL(obj%val .EQ. 0.0_DFP), "val is zero")
! END BLOCK
!
! ! Test 2: SetLen to a larger length grows the allocation
! BLOCK
!   TYPE(FEVariable_) :: obj
!
!   CALL Initiate(obj=obj, s=[2], defineon=fevaropt%nodal, &
!                 vartype=fevaropt%space, rank=fevaropt%scalar, len=2)
!   CALL SetLen(obj=obj, tsize=10)
!
!   CALL OK(obj%len .EQ. 10, "len = 10")
!   CALL OK(obj%capacity .EQ. 10, "capacity = 10")
!   CALL OK(SIZE(obj%val) .EQ. 10, "SIZE(val) = 10")
! END BLOCK
!
! ! Test 3: SetLen does nothing on an uninitiated object
! BLOCK
!   TYPE(FEVariable_) :: obj
!
!   CALL SetLen(obj=obj, tsize=5)
!
!   CALL OK(obj%len .EQ. 0, "len = 0 when not initiated")
!   CALL OK(.NOT. ALLOCATED(obj%val), "val not allocated")
! END BLOCK
!
! END PROGRAM main
!```
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
! Initiate `obj` as in [[obj_Initiate2]], then copy `val(1:len)` into
! `obj%val(1:len)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE FEVariable_Method, ONLY: Initiate
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: Initiate with values, vector space nodal variable
! BLOCK
!   TYPE(FEVariable_) :: obj
!   REAL(DFP) :: val(6)
!
!   val = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
!   CALL Initiate(obj=obj, s=[3, 2], defineon=fevaropt%nodal, &
!                 vartype=fevaropt%space, rank=fevaropt%vector, len=6, &
!                 val=val)
!
!   CALL OK(obj%isInit, "isInit is true")
!   CALL OK(obj%tshape .EQ. 2, "tshape = 2")
!   CALL OK(ALL(obj%s(1:2) .EQ. [3, 2]), "s = [3, 2]")
!   CALL OK(obj%defineOn .EQ. fevaropt%nodal, "defineOn = nodal")
!   CALL OK(obj%varType .EQ. fevaropt%space, "varType = space")
!   CALL OK(obj%rank .EQ. fevaropt%vector, "rank = vector")
!   CALL OK(obj%len .EQ. 6, "len = 6")
!   CALL OK(obj%capacity .GE. 6, "capacity >= len")
!   CALL OK(ALL(obj%val(1:6) .EQ. val), "val is copied")
! END BLOCK
!
! ! Test 2: Initiate without values gives zero data
! BLOCK
!   TYPE(FEVariable_) :: obj
!
!   CALL Initiate(obj=obj, s=[4], defineon=fevaropt%quadrature, &
!                 vartype=fevaropt%space, rank=fevaropt%scalar, len=4)
!
!   CALL OK(obj%len .EQ. 4, "len = 4 without val")
!   CALL OK(obj%defineOn .EQ. fevaropt%quadrature, "defineOn = quadrature")
!   CALL OK(ALL(obj%val(1:4) .EQ. 0.0_DFP), "val is zero without val")
! END BLOCK
!
! ! Test 3: re-Initiate with a smaller len keeps the allocation
! BLOCK
!   TYPE(FEVariable_) :: obj
!
!   CALL Initiate(obj=obj, s=[8], defineon=fevaropt%nodal, &
!                 vartype=fevaropt%space, rank=fevaropt%scalar, len=8, &
!                 val=SPREAD(1.0_DFP, 1, 8))
!   CALL Initiate(obj=obj, s=[2], defineon=fevaropt%nodal, &
!                 vartype=fevaropt%space, rank=fevaropt%scalar, len=2)
!
!   CALL OK(obj%len .EQ. 2, "len = 2 after re-Initiate")
!   CALL OK(obj%capacity .EQ. 8, "capacity = 8 is kept")
!   CALL OK(SIZE(obj%val) .EQ. 8, "SIZE(val) = 8 is kept")
!   CALL OK(ALL(obj%val .EQ. 0.0_DFP), "val is reset to zero")
! END BLOCK
!
! ! Test 4: re-Initiate with a larger len grows the allocation
! BLOCK
!   TYPE(FEVariable_) :: obj
!
!   CALL Initiate(obj=obj, s=[2], defineon=fevaropt%nodal, &
!                 vartype=fevaropt%space, rank=fevaropt%scalar, len=2)
!   CALL Initiate(obj=obj, s=[3, 3], defineon=fevaropt%nodal, &
!                 vartype=fevaropt%constant, rank=fevaropt%matrix, len=9, &
!                 val=SPREAD(2.0_DFP, 1, 9))
!
!   CALL OK(obj%len .EQ. 9, "len = 9 after growing")
!   CALL OK(SIZE(obj%val) .GE. 9, "SIZE(val) >= 9 after growing")
!   CALL OK(ALL(obj%val(1:9) .EQ. 2.0_DFP), "val after growing")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[SetLen]], [[NodalVariable]], [[QuadratureVariable]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate1( &
    obj, s, defineon, vartype, rank, len, val)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    !! finite element variable
    INTEGER(I4B), INTENT(IN) :: s(:)
    !! shape of data, SIZE(s) <= TypeFEVariableOpt%maxRank
    INTEGER(I4B), INTENT(IN) :: defineon
    !! where is the data defined nodal or quadrature
    INTEGER(I4B), INTENT(IN) :: vartype
    !! variable type: constant, space, time, spaceTime
    INTEGER(I4B), INTENT(IN) :: rank
    !! rank of the variable: scalar, vector, matrix
    INTEGER(I4B), INTENT(IN) :: len
    !! length of data to be extracted from val
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
! Set `isInit`, `s`, `tshape = SIZE(s)`, `defineOn`, `varType`, `rank`
! and `len` of `obj`.
!
! If `obj%val` is allocated and its size is at least `len`, the
! allocation is kept, `capacity` is set to its size and all values are
! set to zero. Otherwise `obj%val` is reallocated with
! `capacity = TypeFEVariableOpt%capacityExpandFactor * len` and set to
! zero.
!
! Examples
!
! See [[obj_Initiate1]] (Test 2 and Test 3).
!
! See also: [[obj_Initiate1]], [[SetLen]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate2( &
    obj, s, defineon, vartype, rank, len)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    !! finite element variable
    INTEGER(I4B), INTENT(IN) :: s(:)
    !! shape of data, SIZE(s) <= TypeFEVariableOpt%maxRank
    INTEGER(I4B), INTENT(IN) :: defineon
    !! where is the data defined nodal or quadrature
    INTEGER(I4B), INTENT(IN) :: vartype
    !! variable type: constant, space, time, spaceTime
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
! Reset all components of `obj` to their default values and deallocate
! `obj%val`. Safe to call on an object that is not initiated.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE FEVariable_Method, ONLY: DEALLOCATE
! USE FEVariable_Method, ONLY: Initiate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: Deallocate resets all components
! BLOCK
!   TYPE(FEVariable_) :: obj
!
!   CALL Initiate(obj=obj, s=[3, 2], defineon=fevaropt%nodal, &
!                 vartype=fevaropt%space, rank=fevaropt%vector, len=6)
!   CALL DEALLOCATE(obj)
!
!   CALL OK(.NOT. obj%isInit, "isInit is false")
!   CALL OK(ALL(obj%s .EQ. 0), "s = 0")
!   CALL OK(obj%tshape .EQ. 0, "tshape = 0")
!   CALL OK(obj%defineOn .EQ. 0, "defineOn = 0")
!   CALL OK(obj%varType .EQ. 0, "varType = 0")
!   CALL OK(obj%rank .EQ. 0, "rank = 0")
!   CALL OK(obj%len .EQ. 0, "len = 0")
!   CALL OK(obj%capacity .EQ. 0, "capacity = 0")
!   CALL OK(.NOT. ALLOCATED(obj%val), "val is deallocated")
! END BLOCK
!
! ! Test 2: Deallocate an uninitiated object is safe
! BLOCK
!   TYPE(FEVariable_) :: obj
!
!   CALL DEALLOCATE(obj)
!   CALL OK(.NOT. obj%isInit, "uninitiated object stays empty")
! END BLOCK
!
! END PROGRAM main
!```

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
! Copy `s`, `tshape`, `defineOn`, `rank`, `varType`, `len`, `isInit`
! and `val(1:len)` of `obj2` into `obj1`.
!
! If the capacity of `obj1` is at least `obj2%len`, the allocation and
! capacity of `obj1` are kept. Otherwise `obj1%val` is reallocated with
! `capacity = TypeFEVariableOpt%capacityExpandFactor * len`.
!
! Also available as `Copy(obj1, obj2)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE FEVariable_Method, ONLY: ASSIGNMENT(=)
! USE FEVariable_Method, ONLY: Copy
! USE FEVariable_Method, ONLY: Initiate
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(FEVariable_) :: obj2
! REAL(DFP) :: val(6)
!
! val = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL Initiate(obj=obj2, s=[3, 2], defineon=fevaropt%quadrature, &
!               vartype=fevaropt%space, rank=fevaropt%vector, len=6, val=val)
!
! ! Test 1: Copy into an empty object
! BLOCK
!   TYPE(FEVariable_) :: obj1
!
!   CALL Copy(obj1, obj2)
!
!   CALL OK(obj1%isInit, "isInit is copied")
!   CALL OK(obj1%tshape .EQ. 2, "tshape is copied")
!   CALL OK(ALL(obj1%s .EQ. obj2%s), "s is copied")
!   CALL OK(obj1%defineOn .EQ. fevaropt%quadrature, "defineOn is copied")
!   CALL OK(obj1%varType .EQ. fevaropt%space, "varType is copied")
!   CALL OK(obj1%rank .EQ. fevaropt%vector, "rank is copied")
!   CALL OK(obj1%len .EQ. 6, "len is copied")
!   CALL OK(ALL(obj1%val(1:6) .EQ. val), "val is copied")
! END BLOCK
!
! ! Test 2: assignment into an object with larger capacity
! BLOCK
!   TYPE(FEVariable_) :: obj1
!
!   CALL Initiate(obj=obj1, s=[10], defineon=fevaropt%nodal, &
!                 vartype=fevaropt%space, rank=fevaropt%scalar, len=10)
!   obj1 = obj2
!
!   CALL OK(obj1%len .EQ. 6, "len = 6 after assignment")
!   CALL OK(obj1%capacity .EQ. 10, "capacity = 10 is kept")
!   CALL OK(obj1%rank .EQ. fevaropt%vector, "rank after assignment")
!   CALL OK(ALL(obj1%val(1:6) .EQ. val), "val after assignment")
! END BLOCK
!
! ! Test 3: copy of an uninitiated object
! BLOCK
!   TYPE(FEVariable_) :: obj1, obj3
!
!   obj1 = obj3
!   CALL OK(.NOT. obj1%isInit, "uninitiated copy has isInit false")
!   CALL OK(obj1%len .EQ. 0, "uninitiated copy has len 0")
! END BLOCK
!
! END PROGRAM main
!```

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
