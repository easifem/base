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
! summary: Interpolation of scalar `FEVariable_` at quadrature points
!
! `GetInterpolation_` evaluates a scalar `FEVariable_` at quadrature
! points. It does not allocate memory.
!
! A variable defined on nodes uses the space shape functions `N`.
! A space-time variable also uses the time shape functions `T`.
! A variable defined on quadrature points needs no shape functions.
! The routine copies its values.
!
! Each variable type (constant, space, space-time) has three forms.
!
! - `_1` writes the values at all `nips` points into an array.
! - `_2` writes the values at all points into a `FEVariable_`, at the
!   time point `timeIndx`.
! - `_3` returns the value at the single point `spaceIndx`.
!
! The `ScalarGetInterpolation_3` form reads the variable type from `obj`.
! It does not support time variables.

MODULE FEVariable_ScalarInterpolationMethod
USE BaseType, ONLY: FEVariable_, &
                    FEVariableScalar_, &
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
! summary: Interpolation of scalar, constant, at all quadrature points
!
! Computes a constant scalar at the first `nips` quadrature points.
! It stores `scale * obj%val(1)` in each of `ans(1:nips)`.
! It sets `tsize` to `nips`.
! The arguments `N` and `nns` are not used, because the value is the
! same at every point.
!
! If `addContribution` is true, the routine adds the values to `ans`.
! Otherwise, the routine sets `ans(1:nips)` to zero first.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetInterpolation__test_1.F90)

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarConstantGetInterpolation_1( &
    obj, rank, vartype, N, nns, nips, scale, addContribution, ans, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
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
    !! if true, add the interpolated value to `ans`
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! interpolated values, SIZE(ans) >= nips
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in ans
  END SUBROUTINE ScalarConstantGetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of scalar, constant, into a FEVariable
!
! Computes a constant scalar at all quadrature points of one time point.
! It stores `scale * obj%val(1)` in the first `nips` values of the time
! slice `timeIndx` of `ans%val`.
! The arguments `N` and `nns` are not used.
!
! If `addContribution` is true, the routine adds the values to `ans`.
! Otherwise, the routine sets the `nips` values of that slice to zero
! first.
!
! @note
! `ans` must already be a quadrature variable of the right shape.
! The routine changes only `ans%val`.
! Time slice `timeIndx` starts after `(timeIndx - 1) * ans%s(1)` values.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetInterpolation__test_1.F90)

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarConstantGetInterpolation_2( &
    obj, rank, vartype, N, nns, nips, scale, addContribution, timeIndx, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
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
    !! if true, add the interpolated value to `ans`
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! quadrature variable, values written at `timeIndx`
  END SUBROUTINE ScalarConstantGetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of scalar, constant, at one quadrature point
!
! Computes a constant scalar at one quadrature point.
! It stores the result `scale * obj%val(1)` in `ans`.
! The arguments `N`, `nns`, `spaceIndx` and `timeIndx` are not used,
! because the value is the same at every point.
!
! If `addContribution` is true, the routine adds the result to `ans`.
! Otherwise, the routine sets `ans` to zero first.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetInterpolation__test_1.F90)

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarConstantGetInterpolation_3( &
    obj, rank, vartype, N, nns, spaceIndx, timeIndx, scale, addContribution, &
    ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
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
    !! if true, add the interpolated value to `ans`
    REAL(DFP), INTENT(INOUT) :: ans
    !! interpolated value
  END SUBROUTINE ScalarConstantGetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of scalar, space, at all quadrature points
!
! Computes a space-varying scalar at the first `nips` quadrature points.
! It stores the values in `ans(1:nips)` and sets `tsize` to `nips`.
!
! The result depends on where `obj` is defined.
!
! - On nodes, `ans(ips) = scale * SUM_I N(I, ips) * obj(I)`.
! - On quadrature points, `ans(ips) = scale * obj(ips)`.
!   Then `N` is not used.
!
! If `addContribution` is true, the routine adds the values to `ans`.
! Otherwise, the routine sets `ans(1:nips)` to zero first.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetInterpolation__test_1.F90)

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarSpaceGetInterpolation_1( &
    obj, rank, vartype, N, nns, nips, scale, addContribution, ans, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! space shape functions, N(I, ips): I is node, ips is quadrature point
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of quadrature points, bound for dim2 in N
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! interpolated values, SIZE(ans) >= nips
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add the interpolated value to `ans`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in ans
  END SUBROUTINE ScalarSpaceGetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of scalar, space, into a FEVariable
!
! Computes a space-varying scalar at the first `nips` quadrature
! points. It stores the values in one time slice of `ans%val`.
!
! The result depends on where `obj` is defined.
!
! - On nodes, `ans(ips) = scale * SUM_I N(I, ips) * obj(I)`.
! - On quadrature points, `ans(ips) = scale * obj(ips)`.
!   Then `N` is not used.
!
! The routine writes to the slice `timeIndx` of `ans`.
! This slice starts after `(timeIndx - 1) * ans%s(1)` values.
!
! If `addContribution` is true, the routine adds the values to the slice.
! Otherwise, it sets the slice to zero first.
!
! @note
! `ans` must be a quadrature variable of the right shape.
! The routine does not change `ans%s`, `ans%len` or other components.
! @endnote
!
! Changes:
!   - 2026-11-14: `ans%s` and `ans%len` are no longer changed
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetInterpolation__test_1.F90)
!
! See also: [[ScalarSpaceGetInterpolation_1]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarSpaceGetInterpolation_2( &
    obj, rank, vartype, N, nns, nips, scale, addContribution, timeIndx, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
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
    !! if true, adds the interpolated values to `ans`
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! quadrature variable, the routine writes to the slice `timeIndx`
  END SUBROUTINE ScalarSpaceGetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of scalar, space, at one quadrature point
!
! Computes a space-varying scalar at one space quadrature point.
! It stores the value in `ans`.
!
! The result depends on where `obj` is defined.
!
! - On nodes, `ans = scale * SUM_I N(I, spaceIndx) * obj(I)`.
! - On quadrature points, `ans = scale * obj(spaceIndx)`.
!   Then `N` is not used.
!
! If `addContribution` is true, the routine adds the value to `ans`.
! Otherwise, it sets `ans` to zero first.
!
! The routine ignores `timeIndx`.
! It exists so that all variable types share one argument list.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetInterpolation__test_1.F90)
!
! See also: [[ScalarSpaceGetInterpolation_1]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarSpaceGetInterpolation_3( &
    obj, rank, vartype, N, nns, spaceIndx, timeIndx, scale, addContribution, &
    ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
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
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, adds the interpolated value to `ans`
    REAL(DFP), INTENT(INOUT) :: ans
    !! interpolated value
  END SUBROUTINE ScalarSpaceGetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of scalar, space-time, at all quadrature points
!
! Computes a space-time scalar at the first `nips` space quadrature
! points. It stores the values in `ans(1:nips)` and sets `tsize` to
! `nips`.
!
! The result depends on where `obj` is defined.
!
! - On nodes, `ans(ips) = scale * SUM_a T(a) * SUM_I N(I, ips) * obj(I, a)`.
!   Here `a` runs over the `nnt` time nodes.
! - On quadrature points, `ans(ips) = scale * obj(ips, timeIndx)`.
!   Then `N` and `T` are not used.
!
! If `addContribution` is true, the routine adds the values to `ans`.
! Otherwise, it sets `ans(1:nips)` to zero first.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetInterpolation__test_1.F90)

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarSpaceTimeGetInterpolation_1( &
    obj, rank, vartype, N, nns, nips, T, nnt, scale, addContribution, ans, &
    tsize, timeIndx)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
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
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! interpolated values, SIZE(ans) >= nips
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, adds the interpolated values to `ans`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in `ans`
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index, used only if `obj` is on quadrature points
  END SUBROUTINE ScalarSpaceTimeGetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of scalar, space-time, into a FEVariable
!
! Computes a space-time scalar at the first `nips` space quadrature
! points. It stores the values in one time slice of `ans%val`.
!
! The result depends on where `obj` is defined.
!
! - On nodes, `ans(ips) = scale * SUM_a T(a) * SUM_I N(I, ips) * obj(I, a)`.
!   Here `a` runs over the `nnt` time nodes.
! - On quadrature points, `ans(ips) = scale * obj(ips, timeIndx)`.
!   Then `N` and `T` are not used.
!
! The routine writes to the slice `timeIndx` of `ans`.
! This slice starts after `(timeIndx - 1) * ans%s(1)` values.
!
! If `addContribution` is true, the routine adds the values to the slice.
! Otherwise, it sets the slice to zero first.
!
! @note
! `ans` must be a quadrature variable of the right shape.
! The routine does not change `ans%s`, `ans%len` or other components.
! @endnote
!
! Changes:
!   - 2026-11-14: `ans%s` and `ans%len` are no longer changed
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetInterpolation__test_1.F90)
!
! See also: [[ScalarSpaceTimeGetInterpolation_1]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarSpaceTimeGetInterpolation_2( &
    obj, rank, vartype, N, nns, nips, T, nnt, scale, addContribution, &
    timeIndx, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
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
    !! if true, adds the interpolated values to `ans`
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! quadrature variable, the routine writes to the slice `timeIndx`
  END SUBROUTINE ScalarSpaceTimeGetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of scalar, space-time, at one quadrature point
!
! Computes a space-time scalar at one space quadrature point.
! It stores the value in `ans`.
!
! The result depends on where `obj` is defined.
!
! - On nodes, `ans = scale * SUM_a T(a) * SUM_I N(I, spaceIndx) * obj(I, a)`.
!   Here `a` runs over the `nnt` time nodes.
! - On quadrature points, `ans = scale * obj(spaceIndx, timeIndx)`.
!   Then `N` and `T` are not used.
!
! If `addContribution` is true, the routine adds the value to `ans`.
! Otherwise, it sets `ans` to zero first.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetInterpolation__test_1.F90)
!
! See also: [[ScalarSpaceTimeGetInterpolation_1]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarSpaceTimeGetInterpolation_3( &
    obj, rank, vartype, N, nns, spaceIndx, timeIndx, T, nnt, scale, &
    addContribution, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
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
    REAL(DFP), INTENT(INOUT) :: ans
    !! interpolated value
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, adds the interpolated value to `ans`
  END SUBROUTINE ScalarSpaceTimeGetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of scalar at one point, any variable type
!
! Interpolates a scalar variable at one quadrature point.
! It works for any variable type of `obj`.
! It reads the type from `obj` and picks the matching routine.
!
! 1. A constant variable gives `ans = scale * obj%val(1)`.
! 2. A space variable uses `spaceIndx` and `N`.
! 3. A space-time variable uses `spaceIndx`, `timeIndx`, `N` and `T`.
!
! A time variable is not supported.
! For it, the routine does nothing and leaves `ans` unchanged.
!
! If `addContribution` is false, the routine sets `ans` to zero first.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetInterpolation__test_1.F90)
!
! See also: [[ScalarSpaceGetInterpolation_3]],
! [[ScalarSpaceTimeGetInterpolation_3]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarGetInterpolation_3( &
    obj, rank, N, nns, spaceIndx, timeIndx, T, nnt, scale, addContribution, &
    ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
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
    REAL(DFP), INTENT(INOUT) :: ans
    !! interpolated value
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, adds the interpolated value to `ans`
  END SUBROUTINE ScalarGetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_ScalarInterpolationMethod
