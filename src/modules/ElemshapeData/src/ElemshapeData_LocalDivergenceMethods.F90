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
! summary: Divergence with respect to the reference coordinates
!
! Compute the divergence of a vector or matrix with respect to the
! reference coordinates `xi` at the quadrature points, from its nodal
! values and `dNdXi`. The divergence of a matrix is the vector of row
! divergences, `SUM_k dM_ik/dxi_k`.

module ElemshapeData_LocalDivergenceMethods
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: getLocalDivergence
PUBLIC :: LocalDivergence

!----------------------------------------------------------------------------
!                                       getLocalDivergence@DivergenceMethods

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Local divergence of vector nodal values
!
! Returns the divergence of a vector with respect to the reference
! coordinates `xi`. `val(xidim, nns)` holds the nodal values.
! It reallocates `lg(nips)`.
!
! Formula:
!   $$ lg(ips) = \sum_I \sum_k val(k, I) \, dNdXi(I, k, ips) $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetLocalDivergence_test_1.F90) first block
!
! See also: [[elemsd_getLocalDivergence_3]], [[elemsd_LocalDivergence_1]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalDivergence_1(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:)
    !! local divergence, lg(nips)
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal values, val(xidim, nns)
  END SUBROUTINE elemsd_getLocalDivergence_1
END INTERFACE

INTERFACE getLocalDivergence
  MODULE PROCEDURE elemsd_getLocalDivergence_1
END INTERFACE getLocalDivergence

!----------------------------------------------------------------------------
!                                        getLocalDivergence@DivergenceMethods

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Local divergence of vector space-time nodal values
!
! Returns the local divergence of a space-time vector.
! It interpolates the nodal values in time with `obj%T`.
! Then it sets `lg(nips)` like [[elemsd_getLocalDivergence_1]].
!
! @note
! `obj` must be a `STElemShapeData_`. Otherwise, it does nothing.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetLocalDivergence_test_1.F90) third block
!
! See also: [[elemsd_getLocalDivergence_1]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalDivergence_2(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:)
    !! local divergence, lg(nips)
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal values, val(xidim, nns, nnt)
  END SUBROUTINE elemsd_getLocalDivergence_2
END INTERFACE

INTERFACE getLocalDivergence
  MODULE PROCEDURE elemsd_getLocalDivergence_2
END INTERFACE getLocalDivergence

!----------------------------------------------------------------------------
!                                      getLocalDivergence@DivergenceMethods

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Local divergence of a vector FEVariable
!
! Returns the local divergence of the vector `FEVariable_` `val`.
! It reallocates `lg(nips)`.
!
! - A constant `val` gives zero.
! - A space `val` works like [[elemsd_getLocalDivergence_1]].
! - A space-time `val` works like [[elemsd_getLocalDivergence_2]].
!   Then `obj` must be a `STElemShapeData_`.
!
! For other variable types, it does not set `lg`.
!
! @note
! `val` must hold nodal values. A quadrature variable is not supported.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetLocalDivergence_test_1.F90) second block
!
! See also: [[elemsd_getLocalDivergence_1]], [[elemsd_getLocalDivergence_2]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalDivergence_3(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:)
    !! local divergence, lg(nips)
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector nodal FEVariable
  END SUBROUTINE elemsd_getLocalDivergence_3
END INTERFACE

INTERFACE getLocalDivergence
  MODULE PROCEDURE elemsd_getLocalDivergence_3
END INTERFACE getLocalDivergence

!----------------------------------------------------------------------------
!                                      getLocalDivergence@DivergenceMethods

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Local row divergence of matrix nodal values
!
! Returns the local divergence of each row of a matrix.
! `val(nrow, xidim, nns)` holds the nodal values.
! It reallocates `lg(nrow, nips)`.
!
! Formula:
!   $$ lg(i, ips) = \sum_I \sum_k val(i, k, I) \, dNdXi(I, k, ips) $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetLocalDivergence_test_1.F90) first block
!
! See also: [[elemsd_getLocalDivergence_6]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalDivergence_4(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :)
    !! local row divergence, lg(nrow, nips)
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! nodal values, val(nrow, xidim, nns)
  END SUBROUTINE elemsd_getLocalDivergence_4
END INTERFACE

INTERFACE getLocalDivergence
  MODULE PROCEDURE elemsd_getLocalDivergence_4
END INTERFACE getLocalDivergence

!----------------------------------------------------------------------------
!                                       getLocalDivergence@DivergenceMethods

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Local row divergence of matrix space-time nodal values
!
! Returns the local row divergence of a space-time matrix.
! It interpolates the nodal values in time with `obj%T`.
! Then it sets `lg(nrow, nips)` like [[elemsd_getLocalDivergence_4]].
!
! @note
! `obj` must be a `STElemShapeData_`. Otherwise, it does nothing.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetLocalDivergence_test_1.F90) third block
!
! See also: [[elemsd_getLocalDivergence_4]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalDivergence_5(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :)
    !! local row divergence, lg(nrow, nips)
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal values, val(nrow, xidim, nns, nnt)
  END SUBROUTINE elemsd_getLocalDivergence_5
END INTERFACE

INTERFACE getLocalDivergence
  MODULE PROCEDURE elemsd_getLocalDivergence_5
END INTERFACE getLocalDivergence

!----------------------------------------------------------------------------
!                                       getLocalDivergence@DivergenceMethods

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Local row divergence of a matrix FEVariable
!
! Returns the local row divergence of the matrix `FEVariable_` `val`.
! It works like [[elemsd_getLocalDivergence_3]] and sets
! `lg(nrow, nips)`.
!
! @note
! `val` must hold nodal values. A quadrature variable is not supported.
! @endnote
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetLocalDivergence_test_2.F90) space and space-time
!
! See also: [[elemsd_getLocalDivergence_3]], [[elemsd_getLocalDivergence_4]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalDivergence_6(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :)
    !! local row divergence, lg(nrow, nips)
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix nodal FEVariable
  END SUBROUTINE elemsd_getLocalDivergence_6
END INTERFACE

INTERFACE getLocalDivergence
  MODULE PROCEDURE elemsd_getLocalDivergence_6
END INTERFACE getLocalDivergence

!----------------------------------------------------------------------------
!                                      getLocalDivergence@DivergenceMethods

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Local divergence of a FEVariable as a FEVariable
!
! Returns the local divergence of `val` as a quadrature, space
! `FEVariable_`. A vector `val` gives a scalar.
! A matrix `val` gives a vector of row divergences.
!
! @note
! `val` must hold nodal values. A quadrature variable is not supported.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetLocalDivergence_test_1.F90) second block
!
! See also: [[elemsd_getLocalDivergence_3]], [[elemsd_getLocalDivergence_6]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalDivergence_7(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(INOUT) :: lg
    !! local divergence, quadrature space variable
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector or matrix nodal FEVariable
  END SUBROUTINE elemsd_getLocalDivergence_7
END INTERFACE

INTERFACE getLocalDivergence
  MODULE PROCEDURE elemsd_getLocalDivergence_7
END INTERFACE getLocalDivergence

!----------------------------------------------------------------------------
!                                      getLocalDivergence@DivergenceMethods

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Local divergence of a FEVariable at all time points
!
! Returns the local divergence of `val` at all time points.
! Here `obj(ipt)` is the shape data at time quadrature point `ipt`.
! The result is a quadrature, space-time `FEVariable_`.
!
! - A vector `val` gives a scalar with shape `[nips, nipt]`.
! - A matrix `val` gives a vector with shape `[nrow, nips, nipt]`.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetLocalDivergence_test_1.F90) third block
!
! See also: [[elemsd_getLocalDivergence_7]], [[elemsd_LocalDivergence_2]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalDivergence_8(obj, lg, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    TYPE(FEVariable_), INTENT(INOUT) :: lg
    !! local divergence, quadrature space-time variable
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector or matrix nodal FEVariable
  END SUBROUTINE elemsd_getLocalDivergence_8
END INTERFACE

INTERFACE getLocalDivergence
  MODULE PROCEDURE elemsd_getLocalDivergence_8
END INTERFACE getLocalDivergence

!----------------------------------------------------------------------------
!

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Function form of the local divergence of a FEVariable
!
! Returns the local divergence of `val` as a quadrature `FEVariable_`.
! It gives the same result as [[elemsd_getLocalDivergence_7]].
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/LocalDivergence_test_1.F90) first block
!
! See also: [[elemsd_getLocalDivergence_7]]

INTERFACE
  MODULE PURE FUNCTION elemsd_LocalDivergence_1(obj, val) RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector or matrix nodal FEVariable
    TYPE(FEVariable_) :: ans
    !! local divergence, quadrature space variable
  END FUNCTION elemsd_LocalDivergence_1
END INTERFACE

INTERFACE localDivergence
  MODULE PROCEDURE elemsd_LocalDivergence_1
END INTERFACE localDivergence

!----------------------------------------------------------------------------
!

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Function form of the local divergence at all time points
!
! Returns the local divergence of `val` at all time points.
! It gives the same result as [[elemsd_getLocalDivergence_8]].
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/LocalDivergence_test_1.F90) second block, space-time
!
! See also: [[elemsd_getLocalDivergence_8]]

INTERFACE
  MODULE PURE FUNCTION elemsd_LocalDivergence_2(obj, val) RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector or matrix nodal FEVariable
    TYPE(FEVariable_) :: ans
    !! local divergence, quadrature space-time variable
  END FUNCTION elemsd_LocalDivergence_2
END INTERFACE

INTERFACE localDivergence
  MODULE PROCEDURE elemsd_LocalDivergence_2
END INTERFACE localDivergence

end module ElemshapeData_LocalDivergenceMethods
