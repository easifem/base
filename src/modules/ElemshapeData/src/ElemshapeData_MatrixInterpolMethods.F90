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
! This file contains the interfaces of the matrix interpolation methods

MODULE ElemshapeData_MatrixInterpolMethods
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: FEVariable_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: GetInterpolation
PUBLIC :: GetInterpolation_
PUBLIC :: Interpolation
PUBLIC :: STInterpolation

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Interpolation of matrix nodal values, with allocation
!
! Interpolates the matrix nodal values `val(nrow, ncol, nns)` at the
! quadrature points of `obj`. It reallocates `ans(nrow, ncol, nips)`.
!
! Formula:
!   $$ ans(:, :, ips) = \sum_I N(I, ips) \, val(:, :, I) $$
!
! Examples
!
! - [test 4](|base|ElemshapeData/examples/GetInterpolation_test_4.F90),
!   Test 1
!
! See also: [[GetInterpolation_1]], [[Interpolation1]]

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation1(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :)
    !! interpolation of matrix
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! nodal value of matrix
  END SUBROUTINE GetInterpolation1
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-03-04
! summary: Interpolation of matrix nodal values, without allocation
!
! Interpolates `val` like [[GetInterpolation1]].
! It writes into the existing `ans` and returns the used size in
! `dim1`, `dim2` and `dim3`.
!
! Examples
!
! - [test 8](|base|ElemshapeData/examples/GetInterpolation_test_8.F90),
!   Test 1
!
! See also: [[GetInterpolation1]], [[GetInterpolation_1a]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_1(obj, ans, val, dim1, dim2, dim3)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! interpolated value
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! nodal values, val(nrow, ncol, nns)
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
  END SUBROUTINE GetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-03-04
! summary: Interpolation of matrix nodal values with scale
!
! Interpolates `val` like [[GetInterpolation_1]].
! It adds `scale` times the interpolated value to `ans`.
! If `addContribution` is false, it clears `ans` first.
!
! It uses only the first `MIN(SIZE(val, 3), obj%nns)` nodes.
!
! Examples
!
! - [test 8](|base|ElemshapeData/examples/GetInterpolation_test_8.F90),
!   Test 1
!
! See also: [[GetInterpolation_1]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_1a(obj, ans, val, dim1, dim2, &
                                             dim3, scale, addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! interpolated value
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! nodal values, val(nrow, ncol, nns)
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add the interpolated value to ans
  END SUBROUTINE GetInterpolation_1a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-01
! summary: Interpolation of matrix space-time nodal values at one time point
!
! Interpolates the space-time nodal values `val(nrow, ncol, nns, nnt)`
! at one time point. `obj` is the shape data at that time quadrature
! point. It reallocates `ans(nrow, ncol, nips)`.
!
! Formula:
!   $$ ans(:, :, ips) = \sum_a T(a) \sum_I N(I, ips) \,
!   val(:, :, I, a) $$
!
! Examples
!
! - [test 8](|base|ElemshapeData/examples/GetInterpolation_test_8.F90),
!   Test 2
!
! See also: [[GetInterpolation_2]], [[STInterpolation1]]

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation2(obj, ans, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: ans(:, :, :)
    !! interpolated value
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal value of matrix
  END SUBROUTINE GetInterpolation2
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-03-04
! summary: Interpolation of matrix space-time nodal values, without allocation
!
! Interpolates `val` like [[GetInterpolation2]].
! It writes into the existing `ans` and returns the used size in
! `dim1`, `dim2` and `dim3`.
!
! Examples
!
! - [test 8](|base|ElemshapeData/examples/GetInterpolation_test_8.F90),
!   Test 2
!
! See also: [[GetInterpolation2]], [[GetInterpolation_2a]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_2(obj, ans, val, dim1, dim2, dim3)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! interpolated value
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal values, val(nrow, ncol, nns, nnt)
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
  END SUBROUTINE GetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-03-04
! summary: Interpolation of matrix space-time nodal values with scale
!
! Interpolates `val` like [[GetInterpolation_2]].
! It adds `scale` times the interpolated value to `ans`.
! If `addContribution` is false, it clears `ans` first.
!
! It uses only the first `MIN(SIZE(val, 4), obj%nnt)` time nodes.
!
! Examples
!
! - [test 8](|base|ElemshapeData/examples/GetInterpolation_test_8.F90),
!   Test 2
!
! See also: [[GetInterpolation_2]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_2a(obj, ans, val, dim1, dim2, &
                                             dim3, scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! interpolated value
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal values, val(nrow, ncol, nns, nnt)
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add the interpolated value to ans
  END SUBROUTINE GetInterpolation_2a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-01
! summary: Interpolation of matrix space-time nodal values at all time points
!
! Interpolates the space-time nodal values `val(nrow, ncol, nns, nnt)`
! at all time points. Here `obj(ipt)` is the shape data at time
! quadrature point `ipt`. It reallocates
! `ans(nrow, ncol, nips, nipt)`.
!
! It fills `ans(:, :, :, ipt)` like [[GetInterpolation2]] with
! `obj(ipt)`.
! With empty `obj`, it allocates shape `(nrow, ncol, 0, 0)`.
! The first two extents come from `val`.
!
! Changes:
!   - 2026-11-21: Handle empty time-point arrays.
!
! Examples
!
! - [test 18](|base|ElemshapeData/examples/GetInterpolation_test_18.F90)
!
! - [test 4](|base|ElemshapeData/examples/GetInterpolation_test_4.F90),
!   Test 2
!
! See also: [[GetInterpolation_3]]

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation3(obj, ans, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: ans(:, :, :, :)
    !! space-time interpolation
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal value of matrix
  END SUBROUTINE GetInterpolation3
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-01
! summary: Interpolation of matrix space-time nodal values at all time
!   points, without allocation
!
! Interpolates `val` like [[GetInterpolation3]].
! It writes into the existing `ans` and returns the used size in
! `dim1` to `dim4`.
! With empty `obj`, all four sizes are zero. It leaves `ans` untouched.
!
! Changes:
!   - 2026-11-21: Handle empty time-point arrays.
!
! Examples
!
! - [test 18](|base|ElemshapeData/examples/GetInterpolation_test_18.F90)
!
! - [test 8](|base|ElemshapeData/examples/GetInterpolation_test_8.F90),
!   Test 3
!
! See also: [[GetInterpolation3]], [[GetInterpolation_3a]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_3(obj, ans, val, dim1, dim2, dim3, &
                                            dim4)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    !! space-time interpolation
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal value of matrix
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! size of dim4 written in ans
  END SUBROUTINE GetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-01
! summary: Interpolation of matrix space-time nodal values at all time points
!   with scale
!
! Interpolates `val` like [[GetInterpolation_3]].
! It adds `scale` times the interpolated value to `ans`.
! If `addContribution` is false, it clears `ans` first.
! With empty `obj`, all four sizes are zero. It leaves `ans` untouched.
!
! Examples
!
! - [test 8](|base|ElemshapeData/examples/GetInterpolation_test_8.F90),
!   Test 3
!
! See also: [[GetInterpolation_3]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_3a(obj, ans, val, dim1, dim2, &
                                             dim3, dim4, scale, &
                                             addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    !! space-time interpolation
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal value of matrix
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! size of dim4 written in ans
    REAL(DFP), INTENT(IN) :: scale
    !! scaling factor
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add the interpolated value to ans
  END SUBROUTINE GetInterpolation_3a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Interpolation of matrix FEVariable, with allocation
!
! Interpolates the matrix `FEVariable_` `val` at the quadrature points
! of `obj`. It reallocates `ans(nrow, ncol, nips)`.
!
! - For a nodal `val`, it interpolates with `N`.
! - For a quadrature `val`, it copies the values.
! - For a constant `val`, it repeats the value at every point.
!
! A time-only `val` returns shape `(0, 0, 0)`.
! A space-time `val` with plain spatial shape data does the same.
!
! Changes:
!   - 2026-11-21: Return empty outputs for unsupported combinations.
!
! Examples
!
! - [test 19](|base|ElemshapeData/examples/GetInterpolation_test_19.F90)
!
! - [test 4](|base|ElemshapeData/examples/GetInterpolation_test_4.F90),
!   Test 1
! - [test 11](|base|ElemshapeData/examples/GetInterpolation_test_11.F90),
!   Test 1
!
! See also: [[GetInterpolation_4]]

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation4(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :)
    !! interpolation of matrix
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix fe variable
  END SUBROUTINE GetInterpolation4
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-03-04
! summary: Interpolation of matrix FEVariable, without allocation
!
! Interpolates `val` like [[GetInterpolation4]].
! It writes into the existing `ans` and returns the used size in
! `dim1`, `dim2` and `dim3`.
! Unsupported combinations return zero sizes and leave `ans` untouched.
! See [[GetInterpolation_4a]] for these combinations.
!
! Examples
!
! - [test 8](|base|ElemshapeData/examples/GetInterpolation_test_8.F90),
!   Test 4
! - [test 11](|base|ElemshapeData/examples/GetInterpolation_test_11.F90),
!   Test 2
!
! See also: [[GetInterpolation4]], [[GetInterpolation_4a]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_4(obj, ans, val, dim1, dim2, dim3)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! interpolated value
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix FEVariable
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
  END SUBROUTINE GetInterpolation_4
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-04
! summary: Interpolation of matrix FEVariable with scale
!
! Interpolates `val` like [[GetInterpolation_4]].
! It adds `scale` times the interpolated value to `ans`.
! If `addContribution` is false, it clears `ans` first.
!
! A space-time `val` needs `obj` to be a `STElemShapeData_`.
! For a quadrature `val`, `timeIndx` selects the time point.
! The default `timeIndx` is 1.
!
! @note
! A time `val` is not supported. A space-time `val` with a plain
! `ElemShapeData_` is not supported either. Both cases return zero sizes.
! They leave `ans` untouched, regardless of `addContribution`.
! @endnote
!
! Changes:
!   - 2026-11-21: Define sizes for unsupported combinations.
!
! Examples
!
! - [test 19](|base|ElemshapeData/examples/GetInterpolation_test_19.F90)
!
! - [test 8](|base|ElemshapeData/examples/GetInterpolation_test_8.F90),
!   Test 4
! - [test 11](|base|ElemshapeData/examples/GetInterpolation_test_11.F90),
!   Test 1 and Test 4
!
! See also: [[GetInterpolation_4]], [[GetInterpolation_4b]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_4a(obj, ans, val, dim1, dim2, &
                                             dim3, scale, addContribution, &
                                             timeIndx)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! interpolated value
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix FEVariable
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add the interpolated value to ans
    INTEGER(I4B), INTENT(IN), OPTIONAL :: timeIndx
    !! time quadrature point index
  END SUBROUTINE GetInterpolation_4a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-04
! summary: Interpolation of matrix FEVariable at one quadrature point
!
! Interpolates `val` at the quadrature point `spaceIndx`.
! It writes the matrix in `ans(1:nrow, 1:ncol)`.
! For a space-time `val`, `timeIndx` selects the time point.
! It adds `scale` times the interpolated value to `ans`.
! If `addContribution` is false, it clears `ans` first.
!
! A space-time `val` needs `obj` to be a `STElemShapeData_`.
! A time-only `val` returns zero in `nrow` and `ncol`.
! A space-time `val` with plain spatial shape data does the same.
! Both leave `ans` untouched, regardless of `addContribution`.
!
! Changes:
!   - 2026-11-21: Define sizes for unsupported combinations.
!
! Examples
!
! - [test 19](|base|ElemshapeData/examples/GetInterpolation_test_19.F90)
!
! - [test 8](|base|ElemshapeData/examples/GetInterpolation_test_8.F90),
!   Test 4
! - [test 11](|base|ElemshapeData/examples/GetInterpolation_test_11.F90),
!   Test 5
!
! See also: [[GetInterpolation_4a]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_4b(obj, ans, val, nrow, ncol, &
                                             scale, addContribution, &
                                             spaceIndx, timeIndx)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interpolated value
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix FEVariable
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add the interpolated value to ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index
    INTEGER(I4B), INTENT(IN) :: spaceIndx
    !! space quadrature point index
  END SUBROUTINE GetInterpolation_4b
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Interpolation of matrix FEVariable at all time points
!
! Interpolates the matrix `FEVariable_` `val` at all time points.
! Here `obj(ipt)` is the shape data at time quadrature point `ipt`.
! It reallocates `ans(nrow, ncol, nips, nipt)`.
!
! It fills `ans(:, :, :, ipt)` like [[GetInterpolation_4a]] with
! `timeIndx = ipt`.
! With empty `obj`, it allocates shape `(nrow, ncol, 0, 0)`.
! The first two extents come from the matrix dimensions of `val`.
! A time-only `val` with nonempty `obj` returns `(0, 0, 0, SIZE(obj))`.
!
! Changes:
!   - 2026-11-21: Handle empty time-point arrays.
!   - 2026-11-21: Return empty outputs for unsupported combinations.
!
! Examples
!
! - [test 18](|base|ElemshapeData/examples/GetInterpolation_test_18.F90)
!
! - [test 8](|base|ElemshapeData/examples/GetInterpolation_test_8.F90),
!   Test 5
! - [test 11](|base|ElemshapeData/examples/GetInterpolation_test_11.F90),
!   Test 3
!
! - [test 19](|base|ElemshapeData/examples/GetInterpolation_test_19.F90)
!
! See also: [[GetInterpolation_5]]

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation5(obj, ans, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
    !! space-time interpolation of matrix
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix fe variable
  END SUBROUTINE GetInterpolation5
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-03-04
! summary: Interpolation of matrix FEVariable at all time points, without
!   allocation
!
! Interpolates `val` like [[GetInterpolation5]].
! It writes into the existing `ans` and returns the used size in
! `dim1` to `dim4`.
! A time-only `val` leaves `ans` untouched.
! It returns zero in `dim1` to `dim3`, and `SIZE(obj)` in `dim4`.
! With empty `obj`, all four sizes are zero. It leaves `ans` untouched.
!
! Examples
!
! - [test 8](|base|ElemshapeData/examples/GetInterpolation_test_8.F90),
!   Test 5
!
! See also: [[GetInterpolation5]], [[GetInterpolation_5a]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_5(obj, ans, val, dim1, dim2, dim3, &
                                            dim4)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    !! interpolated value
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix FEVariable
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! size of dim4 written in ans
  END SUBROUTINE GetInterpolation_5
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-03-04
! summary: Interpolation of matrix FEVariable at all time points with scale
!
! Interpolates `val` like [[GetInterpolation_5]].
! It adds `scale` times the interpolated value to `ans`.
! If `addContribution` is false, it clears `ans` first.
! A time-only `val` leaves `ans` untouched.
! It returns zero in `dim1` to `dim3`, and `SIZE(obj)` in `dim4`.
! With empty `obj`, all four sizes are zero. It leaves `ans` untouched.
!
! Examples
!
! - [test 8](|base|ElemshapeData/examples/GetInterpolation_test_8.F90),
!   Test 5
!
! See also: [[GetInterpolation_5]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_5a(obj, ans, val, dim1, dim2, &
                                             dim3, dim4, scale, &
                                             addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    !! interpolated value
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix FEVariable
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! size of dim4 written in ans
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add the interpolated value to ans
  END SUBROUTINE GetInterpolation_5a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                       Interpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Function form of the interpolation of matrix nodal values
!
! Returns the interpolation of the matrix nodal values `val` at the
! quadrature points of `obj`. The result is `ans(nrow, ncol, nips)`.
!
! Formula:
!   $$ ans(:, :, ips) = \sum_I N(I, ips) \, val(:, :, I) $$
!
! Examples
!
! - [test 4](|base|ElemshapeData/examples/GetInterpolation_test_4.F90),
!   Test 1
!
! See also: [[GetInterpolation1]]

INTERFACE
  MODULE PURE FUNCTION Interpolation1(obj, val) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! nodal values, val(nrow, ncol, nns)
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
    !! interpolated value
  END FUNCTION Interpolation1
END INTERFACE

INTERFACE Interpolation
  MODULE PROCEDURE Interpolation1
END INTERFACE Interpolation

!----------------------------------------------------------------------------
!                                                     STInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Function form of the interpolation of matrix space-time nodal
!   values
!
! Returns the interpolation of the space-time nodal values `val` at one
! time point. `obj` is the shape data at that time quadrature point.
! The result is `ans(nrow, ncol, nips)`.
!
! Formula:
!   $$ ans(:, :, ips) = \sum_a T(a) \sum_I N(I, ips) \,
!   val(:, :, I, a) $$
!
! Examples
!
! - [test 8](|base|ElemshapeData/examples/GetInterpolation_test_8.F90),
!   Test 2
!
! See also: [[GetInterpolation2]]

INTERFACE

  MODULE PURE FUNCTION STInterpolation1(obj, val) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal values, val(nrow, ncol, nns, nnt)
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
    !! interpolated value
  END FUNCTION STInterpolation1
END INTERFACE

INTERFACE STInterpolation
  MODULE PROCEDURE STInterpolation1
END INTERFACE STInterpolation

END MODULE ElemshapeData_MatrixInterpolMethods
