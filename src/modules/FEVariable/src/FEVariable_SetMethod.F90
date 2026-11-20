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

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the values of FEVariable_
!
! `Set` copies `scale * val` into an initiated FEVariable_, or adds it to
! the current values when `addContribution` is true. The rank and the
! variable type are passed as type arguments and select the specific
! procedure.

MODULE FEVariable_SetMethod
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: FEVariableScalar_
USE BaseType, ONLY: FEVariableVector_
USE BaseType, ONLY: FEVariableMatrix_
USE BaseType, ONLY: FEVariableConstant_
USE BaseType, ONLY: FEVariableSpace_
USE BaseType, ONLY: FEVariableTime_
USE BaseType, ONLY: FEVariableSpaceTime_
USE GlobalData, ONLY: I4B, DFP, LGT
IMPLICIT NONE

PRIVATE
PUBLIC :: Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the constant value of scalar variable
!
! Sets `obj%val(1)` from the scalar `val`, and sets `obj%s(1) = 1` and
! `obj%len = 1`.
! `obj%varType` is set to constant.
!
! If `addContribution` is false, `obj%val = scale * val`; otherwise
! `obj%val = obj%val + scale * val`.
!
! `obj%tshape` is set to 1.
!
! `obj` must be initiated as a scalar variable with `capacity >= 1`;
! its variable type may differ and is replaced. `Set` does not change
! `rank`, `defineOn` or `capacity`, and does not reallocate `obj%val`.
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val
! val = 1.0_DFP
! ! obj initiated by NodalVariable or QuadratureVariable
! CALL Set(obj=obj, val=val, rank=TypeFEVariableScalar, &
!          vartype=TypeFEVariableConstant, scale=1.0_DFP, &
!          addContribution=.FALSE.)
!```
!
! Full example: src/modules/FEVariable/examples/Set_test_1.F90

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set1( &
    obj, val, rank, vartype, scale, addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    !! initiated finite element variable
    REAL(DFP), INTENT(IN) :: val
    !! constant value
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
    REAL(DFP), INTENT(IN) :: scale
    !! scale applied to val
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add scale * val to the current values
  END SUBROUTINE obj_Set1
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the space value of scalar variable
!
! Sets `obj%val(1:len)` from `val` in column major order, and sets
! `obj%s(1:1) = SHAPE(val)` and `obj%len = SIZE(val)`.
! `obj%varType` is set to space.
!
! If `addContribution` is false, `obj%val = scale * val`; otherwise
! `obj%val = obj%val + scale * val`.
!
! `obj%tshape` is set to 1.
!
! `obj` must be initiated as a scalar variable with `capacity >= SIZE(val)`;
! its variable type may differ and is replaced. `Set` does not change
! `rank`, `defineOn` or `capacity`, and does not reallocate `obj%val`.
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(3)
! val = 1.0_DFP
! ! obj initiated by NodalVariable or QuadratureVariable
! CALL Set(obj=obj, val=val, rank=TypeFEVariableScalar, &
!          vartype=TypeFEVariableSpace, scale=1.0_DFP, &
!          addContribution=.FALSE.)
!```
!
! Full example: src/modules/FEVariable/examples/Set_test_1.F90

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set2( &
    obj, val, rank, vartype, scale, addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    !! initiated finite element variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values, SHAPE(val) = [nns]
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    REAL(DFP), INTENT(IN) :: scale
    !! scale applied to val
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add scale * val to the current values
  END SUBROUTINE obj_Set2
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the spaceTime value of scalar variable
!
! Sets `obj%val(1:len)` from `val` in column major order, and sets
! `obj%s(1:2) = SHAPE(val)` and `obj%len = SIZE(val)`.
! `obj%varType` is set to spaceTime.
!
! If `addContribution` is false, `obj%val = scale * val`; otherwise
! `obj%val = obj%val + scale * val`.
!
! `obj%tshape` is set to 2.
!
! `obj` must be initiated as a scalar variable with `capacity >= SIZE(val)`;
! its variable type may differ and is replaced. `Set` does not change
! `rank`, `defineOn` or `capacity`, and does not reallocate `obj%val`.
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2)
! val = 1.0_DFP
! ! obj initiated by NodalVariable or QuadratureVariable
! CALL Set(obj=obj, val=val, rank=TypeFEVariableScalar, &
!          vartype=TypeFEVariableSpaceTime, scale=1.0_DFP, &
!          addContribution=.FALSE.)
!```
!
! Full example: src/modules/FEVariable/examples/Set_test_1.F90

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set3( &
    obj, val, rank, vartype, scale, addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    !! initiated finite element variable
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! values, SHAPE(val) = [nns, nnt]
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    REAL(DFP), INTENT(IN) :: scale
    !! scale applied to val
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add scale * val to the current values
  END SUBROUTINE obj_Set3
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the time value of scalar variable
!
! Sets `obj%val(1:len)` from `val` in column major order, and sets
! `obj%s(1:1) = SHAPE(val)` and `obj%len = SIZE(val)`.
! `obj%varType` is set to time.
!
! If `addContribution` is false, `obj%val = scale * val`; otherwise
! `obj%val = obj%val + scale * val`.
!
! `obj%tshape` is set to 1.
!
! `obj` must be initiated as a scalar variable with `capacity >= SIZE(val)`;
! its variable type may differ and is replaced. `Set` does not change
! `rank`, `defineOn` or `capacity`, and does not reallocate `obj%val`.
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(3)
! val = 1.0_DFP
! ! obj initiated by NodalVariable or QuadratureVariable
! CALL Set(obj=obj, val=val, rank=TypeFEVariableScalar, &
!          vartype=TypeFEVariableTime, scale=1.0_DFP, &
!          addContribution=.FALSE.)
!```
!
! Full example: src/modules/FEVariable/examples/Set_test_1.F90

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set10( &
    obj, val, rank, vartype, scale, addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    !! initiated finite element variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values, SHAPE(val) = [nnt]
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
    REAL(DFP), INTENT(IN) :: scale
    !! scale applied to val
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add scale * val to the current values
  END SUBROUTINE obj_Set10
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the constant value of vector variable
!
! Sets `obj%val(1:len)` from `val` in column major order, and sets
! `obj%s(1:1) = SHAPE(val)` and `obj%len = SIZE(val)`.
! `obj%varType` is set to constant.
!
! If `addContribution` is false, `obj%val = scale * val`; otherwise
! `obj%val = obj%val + scale * val`.
!
! `obj%tshape` is set to 1.
!
! `obj` must be initiated as a vector variable with `capacity >= SIZE(val)`;
! its variable type may differ and is replaced. `Set` does not change
! `rank`, `defineOn` or `capacity`, and does not reallocate `obj%val`.
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(3)
! val = 1.0_DFP
! ! obj initiated by NodalVariable or QuadratureVariable
! CALL Set(obj=obj, val=val, rank=TypeFEVariableVector, &
!          vartype=TypeFEVariableConstant, scale=1.0_DFP, &
!          addContribution=.FALSE.)
!```
!
! Full example: src/modules/FEVariable/examples/Set_test_1.F90

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set4( &
    obj, val, rank, vartype, scale, addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    !! initiated finite element variable
    REAL(DFP), INTENT(IN) :: val(:)
    !! values, SHAPE(val) = [ncomp]
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
    REAL(DFP), INTENT(IN) :: scale
    !! scale applied to val
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add scale * val to the current values
  END SUBROUTINE obj_Set4
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the space value of vector variable
!
! Sets `obj%val(1:len)` from `val` in column major order, and sets
! `obj%s(1:2) = SHAPE(val)` and `obj%len = SIZE(val)`.
! `obj%varType` is set to space.
!
! If `addContribution` is false, `obj%val = scale * val`; otherwise
! `obj%val = obj%val + scale * val`.
!
! `obj%tshape` is set to 2.
!
! `obj` must be initiated as a vector variable with `capacity >= SIZE(val)`;
! its variable type may differ and is replaced. `Set` does not change
! `rank`, `defineOn` or `capacity`, and does not reallocate `obj%val`.
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2)
! val = 1.0_DFP
! ! obj initiated by NodalVariable or QuadratureVariable
! CALL Set(obj=obj, val=val, rank=TypeFEVariableVector, &
!          vartype=TypeFEVariableSpace, scale=1.0_DFP, &
!          addContribution=.FALSE.)
!```
!
! Full example: src/modules/FEVariable/examples/Set_test_1.F90

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set5( &
    obj, val, rank, vartype, scale, addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    !! initiated finite element variable
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! values, SHAPE(val) = [ncomp, nns]
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    REAL(DFP), INTENT(IN) :: scale
    !! scale applied to val
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add scale * val to the current values
  END SUBROUTINE obj_Set5
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the spaceTime value of vector variable
!
! Sets `obj%val(1:len)` from `val` in column major order, and sets
! `obj%s(1:3) = SHAPE(val)` and `obj%len = SIZE(val)`.
! `obj%varType` is set to spaceTime.
!
! If `addContribution` is false, `obj%val = scale * val`; otherwise
! `obj%val = obj%val + scale * val`.
!
! `obj%tshape` is set to 3.
!
! `obj` must be initiated as a vector variable with `capacity >= SIZE(val)`;
! its variable type may differ and is replaced. `Set` does not change
! `rank`, `defineOn` or `capacity`, and does not reallocate `obj%val`.
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2, 2)
! val = 1.0_DFP
! ! obj initiated by NodalVariable or QuadratureVariable
! CALL Set(obj=obj, val=val, rank=TypeFEVariableVector, &
!          vartype=TypeFEVariableSpaceTime, scale=1.0_DFP, &
!          addContribution=.FALSE.)
!```
!
! Full example: src/modules/FEVariable/examples/Set_test_1.F90

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set6( &
    obj, val, rank, vartype, scale, addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    !! initiated finite element variable
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! values, SHAPE(val) = [ncomp, nns, nnt]
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    REAL(DFP), INTENT(IN) :: scale
    !! scale applied to val
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add scale * val to the current values
  END SUBROUTINE obj_Set6
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the time value of vector variable
!
! Sets `obj%val(1:len)` from `val` in column major order, and sets
! `obj%s(1:2) = SHAPE(val)` and `obj%len = SIZE(val)`.
! `obj%varType` is set to time.
!
! If `addContribution` is false, `obj%val = scale * val`; otherwise
! `obj%val = obj%val + scale * val`.
!
! `obj%tshape` is set to 2.
!
! `obj` must be initiated as a vector variable with `capacity >= SIZE(val)`;
! its variable type may differ and is replaced. `Set` does not change
! `rank`, `defineOn` or `capacity`, and does not reallocate `obj%val`.
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2)
! val = 1.0_DFP
! ! obj initiated by NodalVariable or QuadratureVariable
! CALL Set(obj=obj, val=val, rank=TypeFEVariableVector, &
!          vartype=TypeFEVariableTime, scale=1.0_DFP, &
!          addContribution=.FALSE.)
!```
!
! Full example: src/modules/FEVariable/examples/Set_test_1.F90

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set11( &
    obj, val, rank, vartype, scale, addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    !! initiated finite element variable
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! values, SHAPE(val) = [ncomp, nnt]
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
    REAL(DFP), INTENT(IN) :: scale
    !! scale applied to val
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add scale * val to the current values
  END SUBROUTINE obj_Set11
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the constant value of matrix variable
!
! Sets `obj%val(1:len)` from `val` in column major order, and sets
! `obj%s(1:2) = SHAPE(val)` and `obj%len = SIZE(val)`.
! `obj%varType` is set to constant.
!
! If `addContribution` is false, `obj%val = scale * val`; otherwise
! `obj%val = obj%val + scale * val`.
!
! `obj%tshape` is set to 2.
!
! `obj` must be initiated as a matrix variable with `capacity >= SIZE(val)`;
! its variable type may differ and is replaced. `Set` does not change
! `rank`, `defineOn` or `capacity`, and does not reallocate `obj%val`.
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2)
! val = 1.0_DFP
! ! obj initiated by NodalVariable or QuadratureVariable
! CALL Set(obj=obj, val=val, rank=TypeFEVariableMatrix, &
!          vartype=TypeFEVariableConstant, scale=1.0_DFP, &
!          addContribution=.FALSE.)
!```
!
! Full example: src/modules/FEVariable/examples/Set_test_1.F90

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set7( &
    obj, val, rank, vartype, scale, addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    !! initiated finite element variable
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! values, SHAPE(val) = [nrow, ncol]
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
    REAL(DFP), INTENT(IN) :: scale
    !! scale applied to val
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add scale * val to the current values
  END SUBROUTINE obj_Set7
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the space value of matrix variable
!
! Sets `obj%val(1:len)` from `val` in column major order, and sets
! `obj%s(1:3) = SHAPE(val)` and `obj%len = SIZE(val)`.
! `obj%varType` is set to space.
!
! If `addContribution` is false, `obj%val = scale * val`; otherwise
! `obj%val = obj%val + scale * val`.
!
! `obj%tshape` is set to 3.
!
! `obj` must be initiated as a matrix variable with `capacity >= SIZE(val)`;
! its variable type may differ and is replaced. `Set` does not change
! `rank`, `defineOn` or `capacity`, and does not reallocate `obj%val`.
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2, 2)
! val = 1.0_DFP
! ! obj initiated by NodalVariable or QuadratureVariable
! CALL Set(obj=obj, val=val, rank=TypeFEVariableMatrix, &
!          vartype=TypeFEVariableSpace, scale=1.0_DFP, &
!          addContribution=.FALSE.)
!```
!
! Full example: src/modules/FEVariable/examples/Set_test_1.F90

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set8( &
    obj, val, rank, vartype, scale, addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    !! initiated finite element variable
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! values, SHAPE(val) = [nrow, ncol, nns]
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    REAL(DFP), INTENT(IN) :: scale
    !! scale applied to val
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add scale * val to the current values
  END SUBROUTINE obj_Set8
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the spaceTime value of matrix variable
!
! Sets `obj%val(1:len)` from `val` in column major order, and sets
! `obj%s(1:4) = SHAPE(val)` and `obj%len = SIZE(val)`.
! `obj%varType` is set to spaceTime.
!
! If `addContribution` is false, `obj%val = scale * val`; otherwise
! `obj%val = obj%val + scale * val`.
!
! `obj%tshape` is set to 4.
!
! `obj` must be initiated as a matrix variable with `capacity >= SIZE(val)`;
! its variable type may differ and is replaced. `Set` does not change
! `rank`, `defineOn` or `capacity`, and does not reallocate `obj%val`.
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2, 2, 2)
! val = 1.0_DFP
! ! obj initiated by NodalVariable or QuadratureVariable
! CALL Set(obj=obj, val=val, rank=TypeFEVariableMatrix, &
!          vartype=TypeFEVariableSpaceTime, scale=1.0_DFP, &
!          addContribution=.FALSE.)
!```
!
! Full example: src/modules/FEVariable/examples/Set_test_1.F90

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set9( &
    obj, val, rank, vartype, scale, addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    !! initiated finite element variable
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! values, SHAPE(val) = [nrow, ncol, nns, nnt]
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    REAL(DFP), INTENT(IN) :: scale
    !! scale applied to val
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add scale * val to the current values
  END SUBROUTINE obj_Set9
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the time value of matrix variable
!
! Sets `obj%val(1:len)` from `val` in column major order, and sets
! `obj%s(1:3) = SHAPE(val)` and `obj%len = SIZE(val)`.
! `obj%varType` is set to time.
!
! If `addContribution` is false, `obj%val = scale * val`; otherwise
! `obj%val = obj%val + scale * val`.
!
! `obj%tshape` is set to 3.
!
! `obj` must be initiated as a matrix variable with `capacity >= SIZE(val)`;
! its variable type may differ and is replaced. `Set` does not change
! `rank`, `defineOn` or `capacity`, and does not reallocate `obj%val`.
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj
! REAL(DFP) :: val(2, 2, 2)
! val = 1.0_DFP
! ! obj initiated by NodalVariable or QuadratureVariable
! CALL Set(obj=obj, val=val, rank=TypeFEVariableMatrix, &
!          vartype=TypeFEVariableTime, scale=1.0_DFP, &
!          addContribution=.FALSE.)
!```
!
! Full example: src/modules/FEVariable/examples/Set_test_1.F90

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set12( &
    obj, val, rank, vartype, scale, addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    !! initiated finite element variable
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! values, SHAPE(val) = [nrow, ncol, nnt]
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
    REAL(DFP), INTENT(IN) :: scale
    !! scale applied to val
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add scale * val to the current values
  END SUBROUTINE obj_Set12
END INTERFACE Set

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_SetMethod
