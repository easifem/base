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

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Gradient with respect to the reference coordinates
!
! Compute the gradient of a quantity with respect to the reference
! coordinates `xi` at the quadrature points.
! The methods use the nodal values and `dNdXi`.
! The local gradient of a scalar is a vector of size `xidim`.
! For a vector it is a matrix. For a matrix it is a rank-3 array.

MODULE ElemshapeData_LocalGradientMethods
USE BaseType, ONLY: ElemshapeData_
USE BaseType, ONLY: STElemshapeData_
USE BaseType, ONLY: FEVariable_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: getLocalGradient
PUBLIC :: LocalGradient

!----------------------------------------------------------------------------
!                                                    getLocalGradient@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Local gradient of scalar nodal values
!
! Returns the gradient of a scalar with respect to the reference
! coordinates `xi`. `val(nns)` holds the nodal values.
! It reallocates `lg(xidim, nips)`.
!
! Formula:
!   $$ lg(k, ips) = \sum_I val(I) \, dNdXi(I, k, ips) $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetLocalGradient_test_1.F90)
! first block
!
! See also: [[elemsd_getLocalGradient_5]], [[elemsd_LocalGradient_1]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_1(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :)
    !! local gradient, lg(xidim, nips)
    REAL(DFP), INTENT(IN) :: val(:)
    !! nodal values, val(nns)
  END SUBROUTINE elemsd_getLocalGradient_1
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_1
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                                    getLocalGradient@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Local gradient of vector nodal values
!
! Returns the local gradient of a vector.
! `val(ncomp, nns)` holds the nodal values.
! It reallocates `lg(ncomp, xidim, nips)`.
!
! Formula:
!   $$ lg(:, k, ips) = \sum_I val(:, I) \, dNdXi(I, k, ips) $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetLocalGradient_test_1.F90)
! second block
!
! See also: [[elemsd_getLocalGradient_6]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_2(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :, :)
    !! local gradient, lg(ncomp, xidim, nips)
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal values, val(ncomp, nns)
  END SUBROUTINE elemsd_getLocalGradient_2
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_2
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                                    getLocalGradient@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Local gradient of scalar space-time nodal values
!
! Returns the local gradient of scalar space-time nodal values.
! It interpolates the nodal values in time with `obj%T`.
! Then it sets `lg(xidim, nips)` like [[elemsd_getLocalGradient_1]].
!
! @note
! `obj` must have dynamic type exactly `STElemShapeData_`.
! Debug builds reject other types, including extensions, with `ERROR STOP`.
! Release builds leave `lg` untouched for those types.
! @endnote
!
! Changes:
!   - 2026-11-21: Reject unsupported object types in debug builds.
!
! Examples
!
! - [test 4](|base|ElemshapeData/examples/GetLocalGradient_test_4.F90)
! - [test 1](|base|ElemshapeData/examples/GetLocalGradient_test_1.F90)
! fourth block
!
! See also: [[elemsd_getLocalGradient_1]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_3(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :)
    !! local gradient, lg(xidim, nips)
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values, val(nns, nnt)
  END SUBROUTINE elemsd_getLocalGradient_3
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_3
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                                    getLocalGradient@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Local gradient of vector space-time nodal values
!
! Returns the local gradient of vector space-time nodal values.
! It interpolates the nodal values in time with `obj%T`.
! Then it sets `lg(ncomp, xidim, nips)` like [[elemsd_getLocalGradient_2]].
!
! @note
! `obj` must have dynamic type exactly `STElemShapeData_`.
! Debug builds reject other types, including extensions, with `ERROR STOP`.
! Release builds leave `lg` untouched for those types.
! @endnote
!
! Changes:
!   - 2026-11-21: Reject unsupported object types in debug builds.
!
! Examples
!
! - [test 4](|base|ElemshapeData/examples/GetLocalGradient_test_4.F90)
! - [test 2](|base|ElemshapeData/examples/GetLocalGradient_test_2.F90)
! first block
!
! See also: [[elemsd_getLocalGradient_2]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_4(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :, :)
    !! local gradient, lg(ncomp, xidim, nips)
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal values, val(ncomp, nns, nnt)
  END SUBROUTINE elemsd_getLocalGradient_4
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_4
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                                    getLocalGradient@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Local gradient of a scalar FEVariable
!
! Returns the local gradient of the scalar `FEVariable_` `val`.
! It reallocates `lg(xidim, nips)`.
!
! - A constant `val` gives zero.
! - A space `val` works like [[elemsd_getLocalGradient_1]].
! - A space-time `val` works like [[elemsd_getLocalGradient_3]].
!   Then `obj` must be a `STElemShapeData_`.
!
! Debug builds reject unsupported variable types with `ERROR STOP`.
! They also reject quadrature data and incompatible space-time objects.
! A space-time input needs dynamic type exactly `STElemShapeData_`.
! Release builds retain the unchecked behavior for invalid inputs.
!
! @note
! `val` must hold nodal values. A quadrature variable is not supported.
! @endnote
!
! Changes:
!   - 2026-11-21: Validate FEVariable inputs in debug builds.
!
! Examples
!
! - [test 5](|base|ElemshapeData/examples/GetLocalGradient_test_5.F90)
! - [test 1](|base|ElemshapeData/examples/GetLocalGradient_test_1.F90)
! third block
!
! See also: [[elemsd_getLocalGradient_1]], [[elemsd_getLocalGradient_3]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_5(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :)
    !! local gradient, lg(xidim, nips)
    TYPE(FEVariable_), INTENT(IN) :: val
    !! scalar nodal FEVariable
  END SUBROUTINE elemsd_getLocalGradient_5
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_5
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                                    getLocalGradient@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Local gradient of a vector FEVariable
!
! Returns the local gradient of the vector `FEVariable_` `val`.
! It works like [[elemsd_getLocalGradient_5]].
! It sets `lg(ncomp, xidim, nips)`.
!
! @note
! `val` must hold nodal values. A quadrature variable is not supported.
! Debug builds apply the input checks of [[elemsd_getLocalGradient_5]].
! @endnote
!
! Changes:
!   - 2026-11-21: Validate FEVariable inputs in debug builds.
!
! Examples
!
! - [test 5](|base|ElemshapeData/examples/GetLocalGradient_test_5.F90)
! - [test 2](|base|ElemshapeData/examples/GetLocalGradient_test_2.F90)
! second block
!
! See also: [[elemsd_getLocalGradient_5]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_6(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :, :)
    !! local gradient, lg(ncomp, xidim, nips)
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector nodal FEVariable
  END SUBROUTINE elemsd_getLocalGradient_6
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_6
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                                    getLocalGradient@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Local gradient of matrix nodal values
!
! Returns the local gradient of a matrix.
! `val(nrow, ncol, nns)` holds the nodal values.
! It reallocates `lg(nrow, ncol, xidim, nips)`.
!
! Formula:
!   $$ lg(:, :, k, ips) = \sum_I val(:, :, I) \, dNdXi(I, k, ips) $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetLocalGradient_test_1.F90)
! second block
!
! See also: [[elemsd_getLocalGradient_9]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_7(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :, :, :)
    !! local gradient, lg(nrow, ncol, xidim, nips)
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! nodal values, val(nrow, ncol, nns)
  END SUBROUTINE elemsd_getLocalGradient_7
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_7
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                                    getLocalGradient@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Local gradient of matrix space-time nodal values
!
! Returns the local gradient of matrix space-time nodal values.
! It interpolates the nodal values in time with `obj%T`.
! Then it sets `lg(nrow, ncol, xidim, nips)` like
! [[elemsd_getLocalGradient_7]].
!
! @note
! `obj` must have dynamic type exactly `STElemShapeData_`.
! Debug builds reject other types, including extensions, with `ERROR STOP`.
! Release builds leave `lg` untouched for those types.
! @endnote
!
! Changes:
!   - 2026-11-21: Reject unsupported object types in debug builds.
!
! Examples
!
! - [test 4](|base|ElemshapeData/examples/GetLocalGradient_test_4.F90)
! - [test 2](|base|ElemshapeData/examples/GetLocalGradient_test_2.F90)
! third block
!
! See also: [[elemsd_getLocalGradient_7]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_8(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :, :, :)
    !! local gradient, lg(nrow, ncol, xidim, nips)
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal values, val(nrow, ncol, nns, nnt)
  END SUBROUTINE elemsd_getLocalGradient_8
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_8
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                                    getLocalGradient@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Local gradient of a matrix FEVariable
!
! Returns the local gradient of the matrix `FEVariable_` `val`.
! It works like [[elemsd_getLocalGradient_5]].
! It sets `lg(nrow, ncol, xidim, nips)`.
!
! @note
! `val` must hold nodal values. A quadrature variable is not supported.
! Debug builds apply the input checks of [[elemsd_getLocalGradient_5]].
! @endnote
!
! Changes:
!   - 2026-11-21: Validate FEVariable inputs in debug builds.
!
! Examples
!
! - [test 5](|base|ElemshapeData/examples/GetLocalGradient_test_5.F90)
! - [test 2](|base|ElemshapeData/examples/GetLocalGradient_test_2.F90)
! fourth block
!
! See also: [[elemsd_getLocalGradient_5]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_9(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :, :, :)
    !! local gradient, lg(nrow, ncol, xidim, nips)
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix nodal FEVariable
  END SUBROUTINE elemsd_getLocalGradient_9
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_9
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                                    getLocalGradient@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Local gradient of a FEVariable as a FEVariable
!
! Returns the local gradient of `val` as a quadrature, space
! `FEVariable_`.
!
! - A scalar `val` gives a vector with shape `[xidim, nips]`.
! - A vector `val` gives a matrix with shape `[ncomp, xidim, nips]`.
!
! @note
! `val` must hold nodal values. A quadrature variable is not supported.
! A matrix `val` is unsupported as a FEVariable result.
! Debug builds reject it with `ERROR STOP`.
! Release builds leave `lg` untouched for matrix inputs.
! Use the raw-array overload to obtain a matrix field's gradient.
! @endnote
!
! Changes:
!   - 2026-11-21: Reject matrix FEVariable results in debug builds.
!
! Examples
!
! - [test 6](|base|ElemshapeData/examples/GetLocalGradient_test_6.F90)
!
! - [test 3](|base|ElemshapeData/examples/GetLocalGradient_test_3.F90)
! first block
!
! See also: [[elemsd_getLocalGradient_5]], [[elemsd_LocalGradient_1]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_10(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(INOUT) :: lg
    !! local gradient, quadrature space variable
    TYPE(FEVariable_), INTENT(IN) :: val
    !! nodal FEVariable
  END SUBROUTINE elemsd_getLocalGradient_10
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_10
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                                    getLocalGradient@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Local gradient of a FEVariable at all time points
!
! Returns the local gradient of `val` at all time points.
! Here `obj(ipt)` is the shape data at time quadrature point `ipt`.
! The result is a quadrature, space-time `FEVariable_`.
!
! - A scalar `val` gives a vector with shape `[xidim, nips, nipt]`.
! - A vector `val` gives a matrix with shape
!   `[ncomp, xidim, nips, nipt]`.
!
! `val` can hold nodal values in space or in space-time.
!
! @note
! A matrix `val` is unsupported as a FEVariable result.
! Debug builds reject it with `ERROR STOP`.
! Release builds leave `lg` untouched for matrix inputs.
! Use the raw-array overload to obtain a matrix field's gradient.
! @endnote
!
! Changes:
!   - 2026-11-21: Reject matrix FEVariable results in debug builds.
!
! Examples
!
! - [test 6](|base|ElemshapeData/examples/GetLocalGradient_test_6.F90)
!
! - [test 1](|base|ElemshapeData/examples/GetLocalGradient_test_1.F90)
! fourth block
! - [test 3](|base|ElemshapeData/examples/GetLocalGradient_test_3.F90)
! scalar and vector `FEVariable_`
!
! See also: [[elemsd_getLocalGradient_10]], [[elemsd_LocalGradient_2]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_11(obj, lg, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    TYPE(FEVariable_), INTENT(INOUT) :: lg
    !! local gradient, quadrature space-time variable
    TYPE(FEVariable_), INTENT(IN) :: val
    !! nodal FEVariable
  END SUBROUTINE elemsd_getLocalGradient_11
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_11
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                                       LocalGradient@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Function form of the local gradient of a FEVariable
!
! Returns the local gradient of `val` as a quadrature `FEVariable_`.
! It gives the same result as [[elemsd_getLocalGradient_10]].
!
! @warning
! A matrix `val` triggers `ERROR STOP` in debug builds.
! In release builds, it returns a default, uncomputed FEVariable.
! @endwarning
!
! Changes:
!   - 2026-11-21: Reject matrix FEVariable results in debug builds.
!
! Returns:
!   ans : `FEVariable_` - local gradient, quadrature space variable
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/LocalGradient_test_1.F90)
! nodal scalar `FEVariable_`
!
! - [test 6](|base|ElemshapeData/examples/GetLocalGradient_test_6.F90)
!
! See also: [[elemsd_getLocalGradient_10]]

INTERFACE
  MODULE PURE FUNCTION elemsd_LocalGradient_1(obj, val) RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(IN) :: val
    !! nodal FEVariable
    TYPE(FEVariable_) :: ans
    !! local gradient, quadrature space variable
  END FUNCTION elemsd_LocalGradient_1
END INTERFACE

INTERFACE localGradient
  MODULE PROCEDURE elemsd_LocalGradient_1
END INTERFACE localGradient

!----------------------------------------------------------------------------
!                                                       LocalGradient@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Function form of the local gradient at all time points
!
! Returns the local gradient of `val` at all time points.
! It gives the same result as [[elemsd_getLocalGradient_11]].
! Here `obj(ipt)` is the shape data at time quadrature point `ipt`.
!
! @warning
! A matrix `val` triggers `ERROR STOP` in debug builds.
! In release builds, it returns a default, uncomputed FEVariable.
! @endwarning
!
! Changes:
!   - 2026-11-21: Reject matrix FEVariable results in debug builds.
!
! Returns:
!   ans : `FEVariable_` - local gradient, quadrature space-time variable
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/LocalGradient_test_2.F90)
! scalar and vector space-time `FEVariable_`
!
! - [test 6](|base|ElemshapeData/examples/GetLocalGradient_test_6.F90)
!
! See also: [[elemsd_getLocalGradient_11]], [[elemsd_LocalGradient_1]]

INTERFACE
  MODULE PURE FUNCTION elemsd_LocalGradient_2(obj, val) RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    TYPE(FEVariable_), INTENT(IN) :: val
    !! nodal FEVariable
    TYPE(FEVariable_) :: ans
    !! local gradient, quadrature space-time variable
  END FUNCTION elemsd_LocalGradient_2
END INTERFACE

INTERFACE LocalGradient
  MODULE PROCEDURE elemsd_LocalGradient_2
END INTERFACE LocalGradient

END MODULE ElemshapeData_LocalGradientMethods
