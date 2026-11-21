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
! summary: Interpolation of any FEVariable_ into a quadrature FEVariable_
!
! `GetInterpolation_` without `rank` and `vartype` arguments reads the
! rank and the variable type of `obj` and calls the matching routine of
! [[FEVariable_ScalarInterpolationMethod]],
! [[FEVariable_VectorInterpolationMethod]] or
! [[FEVariable_MatrixInterpolationMethod]]. Time variables are not
! supported.

MODULE FEVariable_InterpolationMethod
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: TypeFEVariableOpt
USE GlobalData, ONLY: I4B, DFP, LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of a FEVariable in space only
!
! Interpolates `obj` at `nips` quadrature points and stores the result
! in `ans`. The rank and the variable type of `obj` are read from `obj`
! itself, so the caller does not pass them.
!
! The result depends on how `obj` is defined.
!
! - A nodal `obj` gives `scale * N(I, ips) * obj(I)`, summed over the
!   first `nns` nodes `I`, at each point `ips`.
! - A quadrature `obj` is not interpolated. Its values are only
!   multiplied by `scale`.
! - A constant `obj` gives `scale` times its value at every point.
!
! Time is not used. A space-time `obj` is read at its first time slice
! only. The result goes to the first time slice of `ans`.
!
! `ans` must already exist as a quadrature `FEVariable_` of the same
! rank as `obj`, with the shape of the result.
! If `addContribution` is false, the old values of `ans` are replaced.
! Otherwise the new values are added to them.
!
! @note
! A time variable is not supported. Then `ans` is left unchanged.
! @endnote
!
! Examples
!
! - [test 4](|base|FEVariable/examples/GetInterpolation__test_4.F90)
! - [test 5](|base|FEVariable/examples/GetInterpolation__test_5.F90)
!
! See also: [[FEVariableGetInterpolation_2]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE FEVariableGetInterpolation_1( &
    obj, N, nns, nips, scale, addContribution, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of integration points in N, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! interpolated value, a quadrature `FEVariable_` with the rank of
    !! `obj`. It must be allocated before the call.
  END SUBROUTINE FEVariableGetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of a FEVariable in space and time
!
! Interpolates `obj` at `nips` quadrature points and stores the result
! in the time slice `timeIndx` of `ans`. The rank and the variable type
! of `obj` are read from `obj` itself.
!
! The result depends on the variable type of `obj`.
!
! - A nodal space-time `obj` is interpolated in space with `N` and in
!   time with `T`. The first `nnt` time nodes are used.
! - A quadrature space-time `obj` is not interpolated. Its time slice
!   `timeIndx` is multiplied by `scale`.
! - A space or constant `obj` has no time. Its result is written to the
!   time slice `timeIndx` of `ans` as in
!   [[FEVariableGetInterpolation_1]].
!
! `ans` must already exist as a space-time quadrature `FEVariable_` of
! the same rank as `obj`. Other time slices of `ans` are not changed.
! If `addContribution` is false, the old values in the time slice
! `timeIndx` are replaced. Otherwise the new values are added to them.
!
! @note
! A time variable is not supported. Then `ans` is left unchanged.
! @endnote
!
! Examples
!
! - [test 4](|base|FEVariable/examples/GetInterpolation__test_4.F90)
! - [test 6](|base|FEVariable/examples/GetInterpolation__test_6.F90)
!
! See also: [[FEVariableGetInterpolation_1]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE FEVariableGetInterpolation_2( &
    obj, N, nns, nips, T, nnt, scale, addContribution, timeIndx, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of integration points in N, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: T(:)
    !! time shape functions at the time quadrature point, T(a) with
    !! a = 1, ..., nnt. Used only for a space-time nodal `obj`.
    INTEGER(I4B), INTENT(IN) :: nnt
    !! number of time nodes, bound for `T`
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time slice of `ans` that receives the result
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! interpolated value, a space-time quadrature `FEVariable_` with
    !! the rank of `obj`. It must be allocated before the call.
  END SUBROUTINE FEVariableGetInterpolation_2
END INTERFACE GetInterpolation_

END MODULE FEVariable_InterpolationMethod
