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
! date: 2025-08-29
! summary: Interpolation of matrix FEVariable_ to quadrature points
!
! `GetInterpolation_` evaluates a matrix `FEVariable_` at quadrature
! points. It does not allocate memory.
!
! For a nodal variable, it uses the space shape functions `N`.
! For a space-time variable, it also uses the time shape functions `T`.
! For a quadrature variable, it copies the values.
!
! Each variable type has three forms. The types are constant, space and
! space-time.
!
! - `_1`: values at all `nips` points in an array
! - `_2`: values at all points in a `FEVariable_`, at `timeIndx`
! - `_3`: value at the single point `spaceIndx`
!
! `MatrixGetInterpolation_3` picks the variable type from `obj`.
! Time variables are not supported.

MODULE FEVariable_MatrixInterpolationMethod
USE BaseType, ONLY: FEVariable_, &
                    FEVariableMatrix_, &
                    FEVariableConstant_, &
                    FEVariableSpace_, &
                    FEVariableTime_, &
                    FEVariableSpaceTime_, &
                    TypeFEVariableOpt

USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE
PUBLIC :: GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of matrix, constant, at all quadrature points
!
! Interpolates a constant matrix at every quadrature point.
! The result goes to `ans(1:dim1, 1:dim2, 1:nips)`.
! Each quadrature point gets `scale * obj`.
! The routine returns `dim1`, `dim2` and `dim3 = nips`.
!
! If `addContribution` is false, it clears that part of `ans` first.
! Otherwise it adds to the values already in `ans`.
!
! @note
! `N` and `nns` are not used for a constant variable.
! @endnote
!
! Examples
!
! - [test 3](|base|FEVariable/examples/GetInterpolation__test_3.F90)
! - [test 7](|base|FEVariable/examples/GetInterpolation__test_7.F90)
!
! See also: [[MatrixConstantGetInterpolation_2]],
! [[MatrixConstantGetInterpolation_3]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE MatrixConstantGetInterpolation_1( &
    obj, rank, vartype, N, nns, nips, scale, addContribution, ans, dim1, &
    dim2, dim3)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! space shape functions, N(I, ips): I is node, ips is quadrature point
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of quadrature points, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, adds the interpolated value to ans
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! interpolated values, SHAPE(ans) >= [nrow, ncol, nips]
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
  END SUBROUTINE MatrixConstantGetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of matrix, constant, into a FEVariable
!
! Interpolates a constant matrix at every quadrature point.
! The result goes into the time slice `timeIndx` of `ans%val`.
! Each quadrature point gets `scale * obj`.
!
! `ans` must be a quadrature `FEVariable_` that already has its shape.
! The routine changes only the values of the slice `timeIndx`.
! It leaves `ans%s`, `ans%len` and the other components as they are.
!
! If `addContribution` is false, it clears that slice first.
! Otherwise it adds to the values already in the slice.
!
! @note
! `N` and `nns` are not used for a constant variable.
! @endnote
!
! Changes:
!   - 2026-11-14: the routine repeats the constant value at every point.
!     Before, it read past the values of obj when nips > 1.
!
! Examples
!
! - [test 3](|base|FEVariable/examples/GetInterpolation__test_3.F90)
! - [test 7](|base|FEVariable/examples/GetInterpolation__test_7.F90)
!
! See also: [[MatrixConstantGetInterpolation_1]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE MatrixConstantGetInterpolation_2( &
    obj, rank, vartype, N, nns, nips, scale, addContribution, timeIndx, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! space shape functions, N(I, ips): I is node, ips is quadrature point
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of quadrature points, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, adds the interpolated value to ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! quadrature variable, the routine writes the slice `timeIndx`
  END SUBROUTINE MatrixConstantGetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of matrix, constant, at one quadrature point
!
! Interpolates a constant matrix at one quadrature point.
! The result `scale * obj` goes to `ans(1:nrow, 1:ncol)`.
! The routine returns `nrow` and `ncol`.
!
! If `addContribution` is false, it clears that part of `ans` first.
! Otherwise it adds to the values already in `ans`.
!
! @note
! `N`, `nns`, `spaceIndx` and `timeIndx` are not used for a constant
! variable.
! @endnote
!
! Examples
!
! - [test 3](|base|FEVariable/examples/GetInterpolation__test_3.F90)
! - [test 7](|base|FEVariable/examples/GetInterpolation__test_7.F90)
!
! See also: [[MatrixConstantGetInterpolation_1]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE MatrixConstantGetInterpolation_3( &
    obj, rank, vartype, N, nns, spaceIndx, timeIndx, scale, addContribution, &
    ans, nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! space shape functions, N(I, ips): I is node, ips is quadrature point
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: spaceIndx
    !! space quadrature point index, column of N
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index, not used
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, adds the interpolated value to ans
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interpolated value, SHAPE(ans) >= [nrow, ncol]
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
  END SUBROUTINE MatrixConstantGetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of matrix, space, at all quadrature points
!
! Interpolates a matrix that varies in space at every quadrature point.
! The result goes to `ans(1:dim1, 1:dim2, 1:nips)`.
! The routine returns `dim1`, `dim2` and `dim3 = nips`.
!
! The result depends on whether `obj` holds nodal or quadrature values.
!
! - If `obj` is nodal, the routine uses the shape functions `N`.
! - If `obj` holds quadrature values, the routine copies
!   the values and ignores `N` and `nns`.
!
! If `addContribution` is false, it clears that part of `ans` first.
! Otherwise it adds to the values already in `ans`.
!
! Formula:
!   For a nodal `obj`
!   $$ ans(:, :, ips) = scale \sum_{I=1}^{nns} N(I, ips)\, obj(:, :, I) $$
!   For a quadrature `obj`
!   $$ ans(:, :, ips) = scale\, obj(:, :, ips) $$
!
! Examples
!
! - [test 3](|base|FEVariable/examples/GetInterpolation__test_3.F90)
! - [test 7](|base|FEVariable/examples/GetInterpolation__test_7.F90)
!
! See also: [[MatrixSpaceGetInterpolation_2]],
! [[MatrixSpaceGetInterpolation_3]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE MatrixSpaceGetInterpolation_1( &
    obj, rank, vartype, N, nns, nips, scale, addContribution, ans, dim1, &
    dim2, dim3)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! space shape functions, N(I, ips): I is node, ips is quadrature point
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of quadrature points, bound for dim2 in N
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! interpolated values, SHAPE(ans) >= [nrow, ncol, nips]
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, adds the interpolated value to ans
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
  END SUBROUTINE MatrixSpaceGetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of matrix, space, into a FEVariable
!
! Interpolates a matrix that varies in space at every quadrature point.
! The result goes into the time slice `timeIndx` of `ans%val`.
!
! The result depends on whether `obj` holds nodal or quadrature values.
!
! - If `obj` is nodal, the routine uses the shape functions `N`.
! - If `obj` holds quadrature values, the routine copies
!   the values and ignores `N` and `nns`.
!
! `ans` must be a quadrature `FEVariable_` that already has its shape.
! The routine changes only the values of the slice `timeIndx`.
! It leaves `ans%s`, `ans%len` and the other components as they are.
!
! If `addContribution` is false, it clears that slice first.
! Otherwise it adds to the values already in the slice.
!
! Formula:
!   For a nodal `obj`
!   $$ ans(:, :, ips) = scale \sum_{I=1}^{nns} N(I, ips)\, obj(:, :, I) $$
!   For a quadrature `obj`
!   $$ ans(:, :, ips) = scale\, obj(:, :, ips) $$
!
! Examples
!
! - [test 3](|base|FEVariable/examples/GetInterpolation__test_3.F90)
! - [test 7](|base|FEVariable/examples/GetInterpolation__test_7.F90)
!
! See also: [[MatrixSpaceGetInterpolation_1]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE MatrixSpaceGetInterpolation_2( &
    obj, rank, vartype, N, nns, nips, scale, addContribution, timeIndx, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! space shape functions, N(I, ips): I is node, ips is quadrature point
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of quadrature points, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, adds the interpolated value to ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! quadrature variable, the routine writes the slice `timeIndx`
  END SUBROUTINE MatrixSpaceGetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of matrix, space, at one quadrature point
!
! Interpolates a matrix that varies in space at one quadrature point.
! The point is `spaceIndx`.
! The result goes to `ans(1:nrow, 1:ncol)`.
! The routine returns `nrow` and `ncol`.
!
! The result depends on whether `obj` holds nodal or quadrature values.
!
! - If `obj` is nodal, the routine uses the shape functions `N`.
! - If `obj` holds quadrature values, the routine copies
!   the values and ignores `N` and `nns`.
!
! If `addContribution` is false, it clears that part of `ans` first.
! Otherwise it adds to the values already in `ans`.
!
! @note
! `timeIndx` is not used for a space variable.
! @endnote
!
! Formula:
!   For a nodal `obj`
!   $$ ans = scale \sum_{I=1}^{nns} N(I, spaceIndx)\, obj(:, :, I) $$
!   For a quadrature `obj`
!   $$ ans = scale\, obj(:, :, spaceIndx) $$
!
! Examples
!
! - [test 3](|base|FEVariable/examples/GetInterpolation__test_3.F90)
! - [test 7](|base|FEVariable/examples/GetInterpolation__test_7.F90)
!
! See also: [[MatrixSpaceGetInterpolation_1]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE MatrixSpaceGetInterpolation_3( &
    obj, rank, vartype, N, nns, spaceIndx, timeIndx, scale, addContribution, &
    ans, nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! space shape functions, N(I, ips): I is node, ips is quadrature point
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: spaceIndx
    !! space quadrature point index, column of N
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index, not used
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interpolated value, SHAPE(ans) >= [nrow, ncol]
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, adds the interpolated value to ans
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
  END SUBROUTINE MatrixSpaceGetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of matrix, space-time, at all quadrature points
!
! Interpolates a matrix that varies in space and time at every space
! quadrature point.
! The result goes to `ans(1:dim1, 1:dim2, 1:nips)`.
! The routine returns `dim1`, `dim2` and `dim3 = nips`.
!
! The result depends on whether `obj` holds nodal or quadrature values.
!
! - If `obj` is nodal, the routine uses `N` and the time shape
!   functions `T`. It ignores `timeIndx`.
! - If `obj` holds quadrature values, the routine copies the
!   values of the time point `timeIndx`. It ignores `N`, `nns`, `T`
!   and `nnt`.
!
! If `addContribution` is false, it clears that part of `ans` first.
! Otherwise it adds to the values already in `ans`.
!
! Formula:
!   For a nodal `obj`
!   $$ ans(:, :, ips) = scale \sum_{a=1}^{nnt} T(a)
!      \sum_{I=1}^{nns} N(I, ips)\, obj(:, :, I, a) $$
!   For a quadrature `obj`
!   $$ ans(:, :, ips) = scale\, obj(:, :, ips, timeIndx) $$
!
! Examples
!
! - [test 3](|base|FEVariable/examples/GetInterpolation__test_3.F90)
! - [test 7](|base|FEVariable/examples/GetInterpolation__test_7.F90)
!
! See also: [[MatrixSpaceTimeGetInterpolation_2]],
! [[MatrixSpaceTimeGetInterpolation_3]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE MatrixSpaceTimeGetInterpolation_1( &
    obj, rank, vartype, N, nns, nips, T, nnt, scale, addContribution, ans, &
    dim1, dim2, dim3, timeIndx)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! space shape functions, N(I, ips): I is node, ips is quadrature point
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of quadrature points, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: T(:)
    !! time shape functions at the time quadrature point, T(a)
    INTEGER(I4B), INTENT(IN) :: nnt
    !! number of time nodes, bound for T
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! interpolated values, SHAPE(ans) >= [nrow, ncol, nips]
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, adds the interpolated value to ans
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index
  END SUBROUTINE MatrixSpaceTimeGetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of matrix, space-time, into a FEVariable
!
! Interpolates a matrix that varies in space and time at every space
! quadrature point.
! The result goes into the time slice `timeIndx` of `ans%val`.
!
! The result depends on whether `obj` holds nodal or quadrature values.
!
! - If `obj` is nodal, the routine uses `N` and the time shape
!   functions `T`.
! - If `obj` holds quadrature values, the routine copies the
!   values of the time point `timeIndx`. It ignores `N`, `nns`, `T`
!   and `nnt`.
!
! `ans` must be a quadrature `FEVariable_` that already has its shape.
! The routine changes only the values of the slice `timeIndx`.
! It leaves `ans%s`, `ans%len` and the other components as they are.
!
! If `addContribution` is false, it clears that slice first.
! Otherwise it adds to the values already in the slice.
!
! Formula:
!   For a nodal `obj`
!   $$ ans(:, :, ips) = scale \sum_{a=1}^{nnt} T(a)
!      \sum_{I=1}^{nns} N(I, ips)\, obj(:, :, I, a) $$
!   For a quadrature `obj`
!   $$ ans(:, :, ips) = scale\, obj(:, :, ips, timeIndx) $$
!
! Examples
!
! - [test 3](|base|FEVariable/examples/GetInterpolation__test_3.F90)
! - [test 7](|base|FEVariable/examples/GetInterpolation__test_7.F90)
!
! See also: [[MatrixSpaceTimeGetInterpolation_1]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE MatrixSpaceTimeGetInterpolation_2( &
    obj, rank, vartype, N, nns, nips, T, nnt, scale, addContribution, &
    timeIndx, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! space shape functions, N(I, ips): I is node, ips is quadrature point
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of quadrature points, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: T(:)
    !! time shape functions at the time quadrature point, T(a)
    INTEGER(I4B), INTENT(IN) :: nnt
    !! number of time nodes, bound for T
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, adds the interpolated value to ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! quadrature variable, the routine writes the slice `timeIndx`
  END SUBROUTINE MatrixSpaceTimeGetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of matrix, space-time, at one quadrature point
!
! Interpolates a matrix that varies in space and time at one space
! quadrature point.
! The point is `spaceIndx`.
! The result goes to `ans(1:nrow, 1:ncol)`.
! The routine returns `nrow` and `ncol`.
!
! The result depends on whether `obj` holds nodal or quadrature values.
!
! - If `obj` is nodal, the routine uses `N` and the time shape
!   functions `T`. It ignores `timeIndx`.
! - If `obj` holds quadrature values, the routine copies the
!   values of the point `spaceIndx` at the time point `timeIndx`.
!   It ignores `N`, `nns`, `T` and `nnt`.
!
! If `addContribution` is false, it clears that part of `ans` first.
! Otherwise it adds to the values already in `ans`.
!
! Formula:
!   For a nodal `obj`
!   $$ ans = scale \sum_{a=1}^{nnt} T(a)
!      \sum_{I=1}^{nns} N(I, spaceIndx)\, obj(:, :, I, a) $$
!   For a quadrature `obj`
!   $$ ans = scale\, obj(:, :, spaceIndx, timeIndx) $$
!
! Examples
!
! - [test 3](|base|FEVariable/examples/GetInterpolation__test_3.F90)
! - [test 7](|base|FEVariable/examples/GetInterpolation__test_7.F90)
!
! See also: [[MatrixSpaceTimeGetInterpolation_1]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE MatrixSpaceTimeGetInterpolation_3( &
    obj, rank, vartype, N, nns, spaceIndx, timeIndx, T, nnt, scale, &
    addContribution, ans, nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! space shape functions, N(I, ips): I is node, ips is quadrature point
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: spaceIndx
    !! space quadrature point index, column of N
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index
    REAL(DFP), INTENT(IN) :: T(:)
    !! time shape functions at the time quadrature point, T(a)
    INTEGER(I4B), INTENT(IN) :: nnt
    !! number of time nodes, bound for T
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interpolated value, SHAPE(ans) >= [nrow, ncol]
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, adds the interpolated value to ans
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
  END SUBROUTINE MatrixSpaceTimeGetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of matrix at one point, any variable type
!
! Interpolates a matrix at one quadrature point, for any variable type.
! The routine reads the variable type from `obj`.
! It then does the same as the routine for that type.
! The result goes to `ans(1:nrow, 1:ncol)`.
! The routine returns `nrow` and `ncol`.
!
! 1. A constant `obj` follows [[MatrixConstantGetInterpolation_3]].
! 2. A space `obj` follows [[MatrixSpaceGetInterpolation_3]].
! 3. A space-time `obj` follows [[MatrixSpaceTimeGetInterpolation_3]].
!
! @note
! A time variable does nothing.
! `ans`, `nrow` and `ncol` stay unchanged.
! @endnote
!
! Examples
!
! - [test 3](|base|FEVariable/examples/GetInterpolation__test_3.F90)
! - [test 7](|base|FEVariable/examples/GetInterpolation__test_7.F90)

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE MatrixGetInterpolation_3( &
    obj, rank, N, nns, spaceIndx, timeIndx, T, nnt, scale, addContribution, &
    ans, nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! space shape functions, N(I, ips): I is node, ips is quadrature point
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: spaceIndx
    !! space quadrature point index, column of N
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index
    REAL(DFP), INTENT(IN) :: T(:)
    !! time shape functions at the time quadrature point, T(a)
    INTEGER(I4B), INTENT(IN) :: nnt
    !! number of time nodes, bound for T
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interpolated value, SHAPE(ans) >= [nrow, ncol]
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, adds the interpolated value to ans
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
  END SUBROUTINE MatrixGetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_MatrixInterpolationMethod
