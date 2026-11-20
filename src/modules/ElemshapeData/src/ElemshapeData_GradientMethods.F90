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
! date: 2021-03-04
! summary: Gradient with respect to the physical coordinates
!
! Compute the spatial gradient of a quantity at the quadrature points
! from its nodal values and `dNdXt` (or `dNTdXt` for space-time). The
! gradient of a scalar is a vector of size `nsd`, of a vector a matrix,
! and of a matrix a rank-3 array. `dNdXt` (and `dNTdXt`) must be set
! first, for example by [[Set]].

MODULE ElemshapeData_GradientMethods
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: FEVariableScalar_
USE BaseType, ONLY: FEVariableVector_
USE BaseType, ONLY: FEVariableMatrix_
USE GlobalData, ONLY: DFP, I4B, LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: GetSpatialGradient
PUBLIC :: GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of scalar nodal values
!
! Returns the spatial gradient of scalar nodal values `val(nns)`.
! It reallocates `ans(nsd, nips)`.
! Then it works like [[obj_GetSpatialGradient_1]].
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetSpatialGradient_test_1.F90) first block
!
! See also: [[obj_GetSpatialGradient_1]]

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient1(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
    !! spatial gradient, ans(nsd, nips)
    REAL(DFP), INTENT(IN) :: val(:)
    !! nodal values, val(nns)
  END SUBROUTINE obj_GetSpatialGradient1
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of scalar nodal values, no allocation
!
! Returns the spatial gradient of scalar nodal values `val(nns)`.
! It writes into the existing `ans` and returns the used size in
! `nrow` and `ncol`.
! If `nsd /= xidim`, `obj` is a facet and it sets `ans` to zero.
!
! Formula:
!   $$ ans(:, ips) = \sum_I val(I) \, dNdXt(I, :, ips) $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetSpatialGradient__test_1.F90)
! first block
!
! See also: [[obj_GetSpatialGradient1]]

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_1(obj, val, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(IN) :: val(:)
    !! nodal values, val(nns)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! spatial gradient, ans(nsd, nips)
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
  END SUBROUTINE obj_GetSpatialGradient_1
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of vector nodal values
!
! Returns the spatial gradient of vector nodal values `val(ncomp, nns)`.
! It reallocates `ans(ncomp, nsd, nips)`.
! Then it works like [[obj_GetSpatialGradient_2]].
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetSpatialGradient_test_1.F90)
! first block
!
! See also: [[obj_GetSpatialGradient_2]]

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient2(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :)
    !! spatial gradient, ans(ncomp, nsd, nips)
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal values, val(ncomp, nns)
  END SUBROUTINE obj_GetSpatialGradient2
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of vector nodal values, no allocation
!
! Returns the spatial gradient of vector nodal values `val(ncomp, nns)`.
! It writes into the existing `ans` and returns the used size in
! `dim1`, `dim2` and `dim3`.
! If `nsd /= xidim`, `obj` is a facet and it sets `ans` to zero.
!
! Formula:
!   $$ ans(:, :, ips) = \sum_I val(:, I) \, dNdXt(I, :, ips) $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetSpatialGradient__test_1.F90)
! first block
!
! See also: [[obj_GetSpatialGradient2]]

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_2( &
    obj, val, ans, dim1, dim2, dim3)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal values, val(ncomp, nns)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! spatial gradient, ans(ncomp, nsd, nips)
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
  END SUBROUTINE obj_GetSpatialGradient_2
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                          GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of scalar space-time nodal values
!
! Returns the spatial gradient of scalar space-time nodal values
! `val(nns, nnt)`.
! It reallocates `ans(nsd, nips)`.
! Then it works like [[obj_GetSpatialGradient_3]].
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetSpatialGradient_test_1.F90)
! third block
!
! See also: [[obj_GetSpatialGradient_3]]

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient3(obj, ans, val)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
    !! spatial gradient, ans(nsd, nips)
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values, val(nns, nnt)
  END SUBROUTINE obj_GetSpatialGradient3
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of scalar space-time nodal values, no allocation
!
! Returns the spatial gradient of scalar space-time nodal values
! `val(nns, nnt)`. It writes into the existing `ans` and returns the
! used size in `nrow` and `ncol`.
! If `nsd /= xidim`, `obj` is a facet and it sets `ans` to zero.
!
! Formula:
!   $$ ans(:, ips) = \sum_I \sum_a val(I, a) \, dNTdXt(I, a, :, ips) $$
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetSpatialGradient__test_2.F90)
! second block
!
! See also: [[obj_GetSpatialGradient3]]

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_3( &
    obj, val, ans, nrow, ncol)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values, val(nns, nnt)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! spatial gradient, ans(nsd, nips)
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
  END SUBROUTINE obj_GetSpatialGradient_3
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                          GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of vector space-time nodal values
!
! Returns the spatial gradient of vector space-time nodal values
! `val(ncomp, nns, nnt)`.
! It reallocates `ans(ncomp, nsd, nips)`.
! Then it works like [[obj_GetSpatialGradient_4]].
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetSpatialGradient_test_2.F90)
! first block
!
! See also: [[obj_GetSpatialGradient_4]]

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient4(obj, ans, val)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :)
    !! spatial gradient, ans(ncomp, nsd, nips)
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal values, val(ncomp, nns, nnt)
  END SUBROUTINE obj_GetSpatialGradient4
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of vector space-time nodal values, no allocation
!
! Returns the spatial gradient of vector space-time nodal values
! `val(ncomp, nns, nnt)`. It writes into the existing `ans` and returns
! the used size in `dim1`, `dim2` and `dim3`.
! If `nsd /= xidim`, `obj` is a facet and it sets `ans` to zero.
!
! Formula:
!   $$ ans(i, :, ips) = \sum_I \sum_a val(i, I, a) \,
!   dNTdXt(I, a, :, ips) $$
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetSpatialGradient__test_2.F90)
! second block
!
! See also: [[obj_GetSpatialGradient4]]

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_4( &
    obj, val, ans, dim1, dim2, dim3)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal values, val(ncomp, nns, nnt)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! spatial gradient, ans(ncomp, nsd, nips)
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
  END SUBROUTINE obj_GetSpatialGradient_4
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                          GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of a scalar FEVariable
!
! Returns the spatial gradient of the scalar `FEVariable_` `val`.
! It reallocates `ans(nsd, nips)`.
! Then it works like [[obj_GetSpatialGradient_5]].
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetSpatialGradient_test_1.F90)
! second block
!
! See also: [[obj_GetSpatialGradient_5]]

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient5(obj, ans, val, valRank)
    TYPE(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
    !! spatial gradient, ans(nsd, nips)
    TYPE(FEVariable_), INTENT(IN) :: val
    !! scalar nodal FEVariable
    TYPE(FEVariableScalar_), INTENT(IN) :: valRank
    !! TypeFEVariableScalar
  END SUBROUTINE obj_GetSpatialGradient5
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of a scalar FEVariable, no allocation
!
! Returns the spatial gradient of the scalar `FEVariable_` `val`.
! It writes into the existing `ans` and returns the used size in
! `nrow` and `ncol`.
!
! A space `val` works like [[obj_GetSpatialGradient_1]].
! For any other variable type, it sets `ans` to zero.
! `tempVec(nns)` is a work array.
!
! @note
! `val` must hold nodal values. A quadrature variable is not supported.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetSpatialGradient__test_1.F90)
! second block
!
! See also: [[obj_GetSpatialGradient5]]

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_5( &
    obj, val, valRank, ans, nrow, ncol, tempVec)
    TYPE(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(IN) :: val
    !! scalar nodal FEVariable
    TYPE(FEVariableScalar_), INTENT(IN) :: valRank
    !! TypeFEVariableScalar
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! spatial gradient, ans(nsd, nips)
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
    REAL(DFP), INTENT(INOUT) :: tempVec(:)
    !! temporary vector for internal use, its size should be
    !! atleast obj%nns
  END SUBROUTINE obj_GetSpatialGradient_5
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of a vector FEVariable
!
! Returns the spatial gradient of the vector `FEVariable_` `val`.
! It reallocates `ans(ncomp, nsd, nips)`.
! Then it works like [[obj_GetSpatialGradient_6]].
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetSpatialGradient_test_1.F90)
! second block
!
! See also: [[obj_GetSpatialGradient_6]]

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient6(obj, ans, val, valRank)
    TYPE(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :)
    !! spatial gradient, ans(ncomp, nsd, nips)
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector nodal FEVariable
    TYPE(FEVariableVector_), INTENT(IN) :: valRank
    !! TypeFEVariableVector
  END SUBROUTINE obj_GetSpatialGradient6
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of a vector FEVariable, no allocation
!
! Returns the spatial gradient of the vector `FEVariable_` `val`.
! It writes into the existing `ans` and returns the used size in
! `dim1`, `dim2` and `dim3`.
!
! A space `val` works like [[obj_GetSpatialGradient_2]].
! For any other variable type, it sets `ans` to zero.
! `tempMat2(ncomp, nns)` is a work array.
!
! @note
! `val` must hold nodal values. A quadrature variable is not supported.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetSpatialGradient__test_1.F90)
! second block
!
! See also: [[obj_GetSpatialGradient6]]

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_6( &
    obj, val, valRank, ans, dim1, dim2, dim3, tempMat2)
    TYPE(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector nodal FEVariable
    TYPE(FEVariableVector_), INTENT(IN) :: valRank
    !! TypeFEVariableVector
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! spatial gradient, ans(ncomp, nsd, nips)
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
    REAL(DFP), INTENT(INOUT) :: tempMat2(:, :)
    !! work array, SHAPE >= [ncomp, nns]
  END SUBROUTINE obj_GetSpatialGradient_6
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of matrix nodal values
!
! Returns the spatial gradient of matrix nodal values `val(nrow, ncol, nns)`.
! It reallocates `ans(nrow, ncol, nsd, nips)`.
! Then it works like [[obj_GetSpatialGradient_7]].
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetSpatialGradient_test_1.F90)
! first block
!
! See also: [[obj_GetSpatialGradient_7]]

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient7(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
    !! spatial gradient, ans(nrow, ncol, nsd, nips)
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! nodal values, val(nrow, ncol, nns)
  END SUBROUTINE obj_GetSpatialGradient7
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of matrix nodal values, no allocation
!
! Returns the spatial gradient of matrix nodal values
! `val(nrow, ncol, nns)`. It writes into the existing `ans` and returns
! the used size in `dim1` to `dim4`.
! If `nsd /= xidim`, `obj` is a facet and it sets `ans` to zero.
!
! Formula:
!   $$ ans(:, :, :, ips) = \sum_I val(:, :, I) \, dNdXt(I, :, ips) $$
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetSpatialGradient__test_2.F90)
! first block
!
! See also: [[obj_GetSpatialGradient7]]

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_7( &
    obj, val, ans, dim1, dim2, dim3, dim4)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! nodal values, val(nrow, ncol, nns)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    !! spatial gradient, ans(nrow, ncol, nsd, nips)
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! size of dim4 written in ans
  END SUBROUTINE obj_GetSpatialGradient_7
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of matrix space-time nodal values
!
! Returns the spatial gradient of matrix space-time nodal values
! `val(nrow, ncol, nns, nnt)`.
! It reallocates `ans(nrow, ncol, nsd, nips)`.
! Then it works like [[obj_GetSpatialGradient_8]].
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetSpatialGradient_test_2.F90)
! second block
!
! See also: [[obj_GetSpatialGradient_8]]

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient8(obj, ans, val)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
    !! spatial gradient, ans(nrow, ncol, nsd, nips)
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal values, val(nrow, ncol, nns, nnt)
  END SUBROUTINE obj_GetSpatialGradient8
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of matrix space-time nodal values, no allocation
!
! Returns the spatial gradient of matrix space-time nodal values
! `val(nrow, ncol, nns, nnt)`. It writes into the existing `ans` and
! returns the used size in `dim1` to `dim4`.
! If `nsd /= xidim`, `obj` is a facet and it sets `ans` to zero.
!
! Formula:
!   $$ ans(i, j, :, ips) = \sum_I \sum_a val(i, j, I, a) \,
!   dNTdXt(I, a, :, ips) $$
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetSpatialGradient__test_2.F90)
! second block
!
! See also: [[obj_GetSpatialGradient8]]

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_8( &
    obj, val, ans, dim1, dim2, dim3, dim4)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal values, val(nrow, ncol, nns, nnt)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    !! spatial gradient, ans(nrow, ncol, nsd, nips)
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! size of dim4 written in ans
  END SUBROUTINE obj_GetSpatialGradient_8
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of a matrix FEVariable
!
! Returns the spatial gradient of the matrix `FEVariable_` `val`.
! It reallocates `ans(nrow, ncol, nsd, nips)`.
!
! - A constant `val` gives zero.
! - A space `val` works like [[obj_GetSpatialGradient_7]].
! - A space-time `val` works like [[obj_GetSpatialGradient_8]].
!   Then `obj` must be a `STElemShapeData_`.
!
! For other variable types, it does not set `ans`.
!
! @note
! `val` must hold nodal values. A quadrature variable is not supported.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetSpatialGradient_test_1.F90)
! second block
!
! See also: [[obj_GetSpatialGradient_9]]

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient9(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
    !! spatial gradient, ans(nrow, ncol, nsd, nips)
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix nodal FEVariable
  END SUBROUTINE obj_GetSpatialGradient9
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of a matrix FEVariable, no allocation
!
! Returns the spatial gradient of the matrix `FEVariable_` `val`.
! It writes into the existing `ans` and returns the used size in
! `dim1` to `dim4`.
!
! A space `val` works like [[obj_GetSpatialGradient_7]].
! For any other variable type, it sets `ans` to zero.
! `tempMat3(nrow, ncol, nns)` is a work array. It can be larger than
! needed.
!
! @note
! `val` must hold nodal values. A quadrature variable is not supported.
! @endnote
!
! Changes:
!   - 2026-11-14: ans is set to zero for variable types other than
!     space, and only the used part of tempMat3 is passed on
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetSpatialGradient__test_1.F90)
! second block, space and space-time `val`
!
! See also: [[obj_GetSpatialGradient9]]

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_9( &
    obj, val, valRank, ans, dim1, dim2, dim3, dim4, tempMat3)
    TYPE(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix nodal FEVariable
    TYPE(FEVariableMatrix_), INTENT(IN) :: valRank
    !! TypeFEVariableMatrix
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    !! spatial gradient, ans(nrow, ncol, nsd, nips)
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! size of dim4 written in ans
    REAL(DFP), INTENT(INOUT) :: tempMat3(:, :, :)
    !! work array, SHAPE >= [nrow, ncol, nns]
  END SUBROUTINE obj_GetSpatialGradient_9
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of a scalar FEVariable, space-time
!
! Returns the spatial gradient of the scalar `FEVariable_` `val` for
! space-time shape data.
! It reallocates `ans(nsd, nips)`.
! Then it works like [[obj_GetSpatialGradient_10]].
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetSpatialGradient_test_1.F90)
! third block
!
! See also: [[obj_GetSpatialGradient_10]]

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient10( &
    obj, val, valRank, ans)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    TYPE(FEVariable_), INTENT(IN) :: val
    !! scalar nodal FEVariable
    TYPE(FEVariableScalar_), INTENT(IN) :: valRank
    !! TypeFEVariableScalar
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
    !! spatial gradient, ans(nsd, nips)
  END SUBROUTINE obj_GetSpatialGradient10
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of a scalar FEVariable, space-time, no allocation
!
! Returns the spatial gradient of the scalar `FEVariable_` `val` for
! space-time shape data. It writes into the existing `ans` and returns
! the used size in `nrow` and `ncol`.
!
! - A space `val` works like [[obj_GetSpatialGradient_1]].
! - A space-time `val` works like [[obj_GetSpatialGradient_3]].
!
! For any other variable type, it sets `ans` to zero.
! `tempMat2(nns, nnt)` is a work array.
!
! @note
! `val` must hold nodal values. A quadrature variable is not supported.
! @endnote
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetSpatialGradient__test_2.F90)
! third block
!
! See also: [[obj_GetSpatialGradient10]]

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_10( &
    obj, val, valRank, ans, nrow, ncol, tempMat2)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    TYPE(FEVariable_), INTENT(IN) :: val
    !! scalar nodal FEVariable
    TYPE(FEVariableScalar_), INTENT(IN) :: valRank
    !! TypeFEVariableScalar
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! spatial gradient, ans(nsd, nips)
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
    REAL(DFP), INTENT(INOUT) :: tempMat2(:, :)
    !! work array, SHAPE >= [nns, nnt]
  END SUBROUTINE obj_GetSpatialGradient_10
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of a vector FEVariable, space-time
!
! Returns the spatial gradient of the vector `FEVariable_` `val` for
! space-time shape data.
! It reallocates `ans(ncomp, nsd, nips)`.
! Then it works like [[obj_GetSpatialGradient_11]].
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetSpatialGradient_test_2.F90)
! third block
!
! See also: [[obj_GetSpatialGradient_11]]

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient11( &
    obj, val, valRank, ans)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector nodal FEVariable
    TYPE(FEVariableVector_), INTENT(IN) :: valRank
    !! TypeFEVariableVector
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :)
    !! spatial gradient, ans(ncomp, nsd, nips)
  END SUBROUTINE obj_GetSpatialGradient11
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of a vector FEVariable, space-time, no allocation
!
! Returns the spatial gradient of the vector `FEVariable_` `val` for
! space-time shape data. It writes into the existing `ans` and returns
! the used size in `dim1`, `dim2` and `dim3`.
!
! - A space `val` works like [[obj_GetSpatialGradient_2]].
! - A space-time `val` works like [[obj_GetSpatialGradient_4]].
!
! For any other variable type, it sets `ans` to zero.
! `tempMat3(ncomp, nns, nnt)` is a work array.
!
! @note
! `val` must hold nodal values. A quadrature variable is not supported.
! @endnote
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetSpatialGradient__test_2.F90)
! fourth block
!
! See also: [[obj_GetSpatialGradient11]]

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_11( &
    obj, val, valRank, ans, dim1, dim2, dim3, tempMat3)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector nodal FEVariable
    TYPE(FEVariableVector_), INTENT(IN) :: valRank
    !! TypeFEVariableVector
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! spatial gradient, ans(ncomp, nsd, nips)
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
    REAL(DFP), INTENT(INOUT) :: tempMat3(:, :, :)
    !! work array, SHAPE >= [ncomp, nns, nnt]
  END SUBROUTINE obj_GetSpatialGradient_11
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of a matrix FEVariable, space-time
!
! Returns the spatial gradient of the matrix `FEVariable_` `val` for
! space-time shape data.
! It reallocates `ans(nrow, ncol, nsd, nips)`.
! Then it works like [[obj_GetSpatialGradient_12]].
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetSpatialGradient_test_2.F90)
! fourth block
!
! See also: [[obj_GetSpatialGradient_12]]

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient12( &
    obj, val, valRank, ans)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix nodal FEVariable
    TYPE(FEVariableMatrix_), INTENT(IN) :: valRank
    !! TypeFEVariableMatrix
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
    !! spatial gradient, ans(nrow, ncol, nsd, nips)
  END SUBROUTINE obj_GetSpatialGradient12
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Spatial gradient of a matrix FEVariable, space-time, no allocation
!
! Returns the spatial gradient of the matrix `FEVariable_` `val` for
! space-time shape data. It writes into the existing `ans` and returns
! the used size in `dim1` to `dim4`.
!
! - A space `val` works like [[obj_GetSpatialGradient_7]].
! - A space-time `val` works like [[obj_GetSpatialGradient_8]].
!
! For any other variable type, it sets `ans` to zero.
! `tempMat4(nrow, ncol, nns, nnt)` is a work array.
!
! @note
! `val` must hold nodal values. A quadrature variable is not supported.
! @endnote
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetSpatialGradient__test_2.F90)
! fourth block
!
! See also: [[obj_GetSpatialGradient12]]

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_12( &
    obj, val, valRank, ans, dim1, dim2, dim3, dim4, tempMat4)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix nodal FEVariable
    TYPE(FEVariableMatrix_), INTENT(IN) :: valRank
    !! TypeFEVariableMatrix
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    !! spatial gradient, ans(nrow, ncol, nsd, nips)
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! size of dim4 written in ans
    REAL(DFP), INTENT(INOUT) :: tempMat4(:, :, :, :)
    !! temporary matrix of rank 4, its size should be enough
    !! to keep the matrix nodal value from val
    !! index 1 and index 2 should be atleast shape of matrix.
    !! index 3 and index 4 of tempMat4 should be atleast
    !! obj%nns, obj%nnt
  END SUBROUTINE obj_GetSpatialGradient_12
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ElemshapeData_GradientMethods
