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
!

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Interpolation of a `FEVariable_` at quadrature points
!
! Interpolates a `FEVariable_` `val` of any rank at the quadrature
! points of the shape data `obj`.
! The result `ans` is a quadrature `FEVariable_` with the rank of `val`.
!
! - For a nodal `val`, it uses the shape functions.
! - For a quadrature `val`, it copies the values.
! - For a constant `val`, it repeats the value at each point.
!
! It does not support a `val` of variable type time. Then it does not
! set the values of `ans`.

MODULE ElemshapeData_InterpolMethods
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: FEVariable_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: GetInterpolation_
PUBLIC :: GetInterpolation
PUBLIC :: Interpolation

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Interpolation of a FEVariable in space
!
! Interpolates `val` at the quadrature points of `obj`.
! The result `ans` is a quadrature, space `FEVariable_` with the rank
! of `val`.
!
! If `ans` is not initiated, it initiates `ans` with this shape:
!
! - `[nips]` for a scalar
! - `[ncomp, nips]` for a vector
! - `[nrow, ncol, nips]` for a matrix
!
! If `ans` is already initiated, it only sets the values.
!
! If `ans` is not initiated and the rank of `val` is not scalar, vector
! or matrix (for example, `val` is not initiated), a debug build stops
! with an error. A release build gives `ans` an empty shape and zero
! length.
!
! @note
! For a space-time `val`, the result depends on the type of `obj`:
!
! - `ElemShapeData_`: it uses only the first time slice of `val`.
! - `STElemShapeData_`: it also interpolates `val` in time, with the
!   time shape functions `T` of `obj`.
!
! For all time points at once, use [[GetInterpolation2]].
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetInterpolation_test_1.F90)
! first block, scalar
! - [test 1](|base|ElemshapeData/examples/GetInterpolation_test_1.F90)
! second block, vector
! - [test 9](|base|ElemshapeData/examples/GetInterpolation_test_9.F90)
! constant and quadrature `val`
! - [test 10](|base|ElemshapeData/examples/GetInterpolation_test_10.F90)
! second block, space-time `val`
! - [test 20](|base|ElemshapeData/examples/GetInterpolation_test_20.F90)
! second block, space-time `val` and shape data
!
! See also: [[GetInterpolation_1]], [[Interpolation1]]

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation1(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! quadrature variable, initiated here if needed
    TYPE(FEVariable_), INTENT(IN) :: val
    !! variable to interpolate
  END SUBROUTINE GetInterpolation1
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Interpolation of a FEVariable in space, without initiation
!
! Interpolates `val` like [[GetInterpolation1]], but does not initiate
! `ans`. It only sets the values of `ans`.
! It gives the same result as [[GetInterpolation_1a]] with
! `scale = 1` and `addContribution = .FALSE.`.
!
! @note
! `ans` must already be a quadrature `FEVariable_` with the rank and
! shape of the result.
! @endnote
!
! Examples
!
! - [test 5](|base|ElemshapeData/examples/GetInterpolation_test_5.F90)
! first block
! - [test 20](|base|ElemshapeData/examples/GetInterpolation_test_20.F90)
! first block, space-time `val` and shape data
!
! See also: [[GetInterpolation1]], [[GetInterpolation_1a]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_1(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! initiated quadrature variable
    TYPE(FEVariable_), INTENT(IN) :: val
    !! variable to interpolate
  END SUBROUTINE GetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Interpolation of a FEVariable in space with scale
!
! Adds `scale` times the interpolated `val` to `ans`.
! Otherwise it works like [[GetInterpolation_1]].
! If `addContribution` is false, it clears `ans` first.
!
! If `obj` is a `STElemShapeData_`, it also interpolates a space-time
! `val` in time. It writes the result at time index 1.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetInterpolation_test_1.F90)
! first block, with `scale = 0.5`
! - [test 10](|base|ElemshapeData/examples/GetInterpolation_test_10.F90)
! first block, space-time shape data
!
! See also: [[GetInterpolation_1]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_1a(obj, ans, val, scale, &
                                             addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! initiated quadrature variable
    TYPE(FEVariable_), INTENT(IN) :: val
    !! variable to interpolate
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL, INTENT(IN) :: addContribution
    !! if true, adds the scaled interpolated value to `ans`;
    !! if false, clears `ans` first
  END SUBROUTINE GetInterpolation_1a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Interpolation of a FEVariable in space and time
!
! Interpolates `val` at all space and time quadrature points.
! Here `obj(ipt)` is the shape data at time quadrature point `ipt`.
! The result `ans` is a quadrature, space-time `FEVariable_`.
!
! If `ans` is not initiated, it initiates `ans` with this shape:
!
! - `[nips, nipt]` for a scalar
! - `[ncomp, nips, nipt]` for a vector
! - `[nrow, ncol, nips, nipt]` for a matrix
!
! Then it fills each time point `ipt` from `obj(ipt)`.
! If `ans` is already initiated, it only sets the values.
!
! If `ans` is not initiated and the rank of `val` is not scalar, vector
! or matrix (for example, `val` is not initiated), a debug build stops
! with an error. A release build gives `ans` an empty shape and zero
! length.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetInterpolation_test_1.F90)
! third block
! - [test 9](|base|ElemshapeData/examples/GetInterpolation_test_9.F90)
! fourth block, space `val`
!
! See also: [[GetInterpolation_2]]

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation2(obj, ans, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! quadrature space-time variable, initiated here if needed
    TYPE(FEVariable_), INTENT(IN) :: val
    !! variable to interpolate
  END SUBROUTINE GetInterpolation2
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Interpolation of a FEVariable in space and time, no initiation
!
! Interpolates `val` like [[GetInterpolation2]], but does not initiate
! `ans`. It only sets the values of `ans`.
!
! @note
! `ans` must already be a quadrature, space-time `FEVariable_` with
! the shape of the result.
! @endnote
!
! Examples
!
! - [test 5](|base|ElemshapeData/examples/GetInterpolation_test_5.F90)
! second block
!
! See also: [[GetInterpolation2]], [[GetInterpolation_2a]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_2(obj, ans, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! initiated quadrature space-time variable
    TYPE(FEVariable_), INTENT(IN) :: val
    !! variable to interpolate
  END SUBROUTINE GetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-01
! summary: Interpolation of a FEVariable in space and time with scale
!
! Adds `scale` times the interpolated `val` to `ans` at all time
! points. Otherwise it works like [[GetInterpolation_2]].
! If `addContribution` is false, it clears each time point of `ans`
! first.
!
! Examples
!
! - [test 5](|base|ElemshapeData/examples/GetInterpolation_test_5.F90)
! third block
!
! See also: [[GetInterpolation_2]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_2a(obj, ans, val, scale, &
                                             addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! initiated quadrature space-time variable
    TYPE(FEVariable_), INTENT(IN) :: val
    !! variable to interpolate
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL, INTENT(IN) :: addContribution
    !! if true, adds the scaled interpolated value to `ans`;
    !! if false, clears `ans` at each time point first
  END SUBROUTINE GetInterpolation_2a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                       Interpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-13
! summary: Interpolation of FEVariable, function form
!
! Returns the interpolation of `val` at the quadrature points of `obj`.
! It gives the same result as [[GetInterpolation1]] with a new `ans`.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetInterpolation_test_1.F90)
! first block
! - [test 9](|base|ElemshapeData/examples/GetInterpolation_test_9.F90)
! second block, constant vector
! - [test 20](|base|ElemshapeData/examples/GetInterpolation_test_20.F90)
! third block, space-time `val` and shape data
!
! See also: [[GetInterpolation1]]

INTERFACE Interpolation
  MODULE PURE FUNCTION Interpolation1(obj, val) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(IN) :: val
    !! variable to interpolate
    TYPE(FEVariable_) :: ans
    !! quadrature space variable
  END FUNCTION Interpolation1
END INTERFACE Interpolation

END MODULE ElemshapeData_InterpolMethods
