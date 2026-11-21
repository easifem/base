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
! `Set` copies `scale * val` into an initiated `FEVariable_`.
! If `addContribution` is true, it adds `scale * val` to the current
! values instead.
!
! The arguments `rank` and `vartype` select the specific procedure.
! Pass the type constants of `BaseType`, for example `TypeFEVariableMatrix`
! and `TypeFEVariableSpaceTime`.

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
! Stores a scalar that is constant in space and time in `obj`.
!
! The routine sets `obj%val(1)` to `scale * val`.
! If `addContribution` is true, it adds `scale * val` to `obj%val(1)`
! instead.
!
! It also sets `obj%len = 1`, `obj%s(1) = 1` and `obj%tshape = 1`.
! The variable type of `obj` becomes constant.
!
! @note
! `obj` must be initiated as a scalar variable with `capacity >= 1`.
! `Set` does not change `rank`, `defineOn` or `capacity`.
! It does not reallocate `obj%val`.
! The old variable type of `obj` may differ, and `Set` replaces it.
! @endnote
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Set_test_1.F90)
! - [test 2](|base|FEVariable/examples/Set_test_2.F90) with a change of variable type

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
! Stores a scalar that varies in space in `obj`.
! The array `val` holds one value per space node or quadrature point.
!
! The routine copies `scale * val` into `obj%val` in column major order.
! If `addContribution` is true, it adds `scale * val` instead.
!
! It also sets `obj%len = SIZE(val)`, `obj%s(1) = SIZE(val)` and
! `obj%tshape = 1`.
! The variable type of `obj` becomes space.
!
! @note
! `obj` must be initiated as a scalar variable with
! `capacity >= SIZE(val)`.
! `Set` does not change `rank`, `defineOn` or `capacity`.
! It does not reallocate `obj%val`.
! The old variable type of `obj` may differ, and `Set` replaces it.
! @endnote
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Set_test_1.F90)

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
! Stores a scalar that varies in space and time in `obj`.
! The array `val` has one row per space node and one column per time node.
!
! The routine copies `scale * val` into `obj%val` in column major order.
! If `addContribution` is true, it adds `scale * val` instead.
!
! It also sets `obj%s(1:2) = SHAPE(val)`, `obj%len = SIZE(val)` and
! `obj%tshape = 2`.
! The variable type of `obj` becomes space-time.
!
! @note
! `obj` must be initiated as a scalar variable with
! `capacity >= SIZE(val)`.
! `Set` does not change `rank`, `defineOn` or `capacity`.
! It does not reallocate `obj%val`.
! The old variable type of `obj` may differ, and `Set` replaces it.
! @endnote
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Set_test_1.F90)

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
! Stores a scalar that varies in time in `obj`.
! The array `val` holds one value per time node.
!
! The routine copies `scale * val` into `obj%val`.
! If `addContribution` is true, it adds `scale * val` instead.
!
! It also sets `obj%len = SIZE(val)`, `obj%s(1) = SIZE(val)` and
! `obj%tshape = 1`.
! The variable type of `obj` becomes time.
!
! @note
! `obj` must be initiated as a scalar variable with
! `capacity >= SIZE(val)`.
! `Set` does not change `rank`, `defineOn` or `capacity`.
! It does not reallocate `obj%val`.
! The old variable type of `obj` may differ, and `Set` replaces it.
! @endnote
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Set_test_1.F90)
! - [test 2](|base|FEVariable/examples/Set_test_2.F90) with zero and negative scale

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
! Stores a vector that is constant in space and time in `obj`.
! The array `val` holds the components of the vector.
!
! The routine copies `scale * val` into `obj%val`.
! If `addContribution` is true, it adds `scale * val` instead.
!
! It also sets `obj%len = SIZE(val)`, `obj%s(1) = SIZE(val)` and
! `obj%tshape = 1`.
! The variable type of `obj` becomes constant.
!
! @note
! `obj` must be initiated as a vector variable with
! `capacity >= SIZE(val)`.
! `Set` does not change `rank`, `defineOn` or `capacity`.
! It does not reallocate `obj%val`.
! The old variable type of `obj` may differ, and `Set` replaces it.
! @endnote
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Set_test_1.F90)
! - [test 2](|base|FEVariable/examples/Set_test_2.F90) with a change of variable type

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
! Stores a vector that varies in space in `obj`.
! The array `val` has one row per component and one column per space node.
!
! The routine copies `scale * val` into `obj%val` in column major order.
! If `addContribution` is true, it adds `scale * val` instead.
!
! It also sets `obj%s(1:2) = SHAPE(val)`, `obj%len = SIZE(val)` and
! `obj%tshape = 2`.
! The variable type of `obj` becomes space.
!
! @note
! `obj` must be initiated as a vector variable with
! `capacity >= SIZE(val)`.
! `Set` does not change `rank`, `defineOn` or `capacity`.
! It does not reallocate `obj%val`.
! The old variable type of `obj` may differ, and `Set` replaces it.
! @endnote
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Set_test_1.F90)
! - [test 2](|base|FEVariable/examples/Set_test_2.F90) with a quadrature variable

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
! Stores a vector that varies in space and time in `obj`.
! The array `val` is indexed by component, space node and time node.
!
! The routine copies `scale * val` into `obj%val` in column major order.
! If `addContribution` is true, it adds `scale * val` instead.
!
! It also sets `obj%s(1:3) = SHAPE(val)`, `obj%len = SIZE(val)` and
! `obj%tshape = 3`.
! The variable type of `obj` becomes space-time.
!
! @note
! `obj` must be initiated as a vector variable with
! `capacity >= SIZE(val)`.
! `Set` does not change `rank`, `defineOn` or `capacity`.
! It does not reallocate `obj%val`.
! The old variable type of `obj` may differ, and `Set` replaces it.
! @endnote
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Set_test_1.F90)
! - [test 2](|base|FEVariable/examples/Set_test_2.F90) with a quadrature variable

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
! Stores a vector that varies in time in `obj`.
! The array `val` has one row per component and one column per time node.
!
! The routine copies `scale * val` into `obj%val` in column major order.
! If `addContribution` is true, it adds `scale * val` instead.
!
! It also sets `obj%s(1:2) = SHAPE(val)`, `obj%len = SIZE(val)` and
! `obj%tshape = 2`.
! The variable type of `obj` becomes time.
!
! @note
! `obj` must be initiated as a vector variable with
! `capacity >= SIZE(val)`.
! `Set` does not change `rank`, `defineOn` or `capacity`.
! It does not reallocate `obj%val`.
! The old variable type of `obj` may differ, and `Set` replaces it.
! @endnote
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Set_test_1.F90)
! - [test 2](|base|FEVariable/examples/Set_test_2.F90) with a quadrature variable

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
! Stores a matrix that is constant in space and time in `obj`.
! The array `val` holds the matrix, indexed by row and column.
!
! The routine copies `scale * val` into `obj%val` in column major order.
! If `addContribution` is true, it adds `scale * val` instead.
!
! It also sets `obj%s(1:2) = SHAPE(val)`, `obj%len = SIZE(val)` and
! `obj%tshape = 2`.
! The variable type of `obj` becomes constant.
!
! @note
! `obj` must be initiated as a matrix variable with
! `capacity >= SIZE(val)`.
! `Set` does not change `rank`, `defineOn` or `capacity`.
! It does not reallocate `obj%val`.
! The old variable type of `obj` may differ, and `Set` replaces it.
! @endnote
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Set_test_1.F90)
! - [test 2](|base|FEVariable/examples/Set_test_2.F90) with a change of variable type

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
! Stores a matrix that varies in space in `obj`.
! The array `val` is indexed by matrix row, matrix column and space node.
!
! The routine copies `scale * val` into `obj%val` in column major order.
! If `addContribution` is true, it adds `scale * val` instead.
!
! It also sets `obj%s(1:3) = SHAPE(val)`, `obj%len = SIZE(val)` and
! `obj%tshape = 3`.
! The variable type of `obj` becomes space.
!
! @note
! `obj` must be initiated as a matrix variable with
! `capacity >= SIZE(val)`.
! `Set` does not change `rank`, `defineOn` or `capacity`.
! It does not reallocate `obj%val`.
! The old variable type of `obj` may differ, and `Set` replaces it.
! @endnote
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Set_test_1.F90)
! - [test 2](|base|FEVariable/examples/Set_test_2.F90) with a change of variable type

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
! summary: Set the space-time value of matrix variable
!
! Stores a matrix that varies in space and time in `obj`.
! The array `val` holds one matrix per space node and time node.
!
! The routine copies `scale * val` into `obj%val` in column major order.
! If `addContribution` is true, it adds `scale * val` instead.
!
! It also sets `obj%len = SIZE(val)`, `obj%s(1:4) = SHAPE(val)` and
! `obj%tshape = 4`.
! The variable type of `obj` becomes space-time.
!
! @note
! `obj` must be initiated as a matrix variable with
! `capacity >= SIZE(val)`.
! `Set` does not change `rank`, `defineOn` or `capacity`.
! It does not reallocate `obj%val`.
! The old variable type of `obj` may differ, and `Set` replaces it.
! @endnote
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Set_test_1.F90)
! - [test 2](|base|FEVariable/examples/Set_test_2.F90) with a change of variable type

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
! Stores a matrix that varies in time in `obj`.
! The array `val` holds one matrix per time node.
!
! The routine copies `scale * val` into `obj%val` in column major order.
! If `addContribution` is true, it adds `scale * val` instead.
!
! It also sets `obj%len = SIZE(val)`, `obj%s(1:3) = SHAPE(val)` and
! `obj%tshape = 3`.
! The variable type of `obj` becomes time.
!
! @note
! `obj` must be initiated as a matrix variable with
! `capacity >= SIZE(val)`.
! `Set` does not change `rank`, `defineOn` or `capacity`.
! It does not reallocate `obj%val`.
! The old variable type of `obj` may differ, and `Set` replaces it.
! @endnote
!
! Changes:
!   - 2026-11-14: `tshape` is set, so the variable type can be changed
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Set_test_1.F90)
! - [test 2](|base|FEVariable/examples/Set_test_2.F90) with a change of variable type

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
