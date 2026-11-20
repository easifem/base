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
! Interpolates `obj` with the space shape functions `N` at `nips`
! quadrature points and writes the result in `ans` as the `_2` form of
! its rank with `timeIndx = 1`. A constant `obj` gives the same value at
! every point. A space-time `obj` is treated as a space variable: only
! its first time slice is used.
!
! `ans` must already be a quadrature FEVariable_ with the rank of `obj`
! and the shape of the result. If `addContribution` is false, the values
! of `ans` are cleared first.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: Scalar => TypeFEVariableScalar
! USE BaseType, ONLY: Space => TypeFEVariableSpace
! USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
! USE BaseType, ONLY: Vector => TypeFEVariableVector
! USE FEVariable_Method, ONLY: GetInterpolation_
! USE FEVariable_Method, ONLY: NodalVariable
! USE FEVariable_Method, ONLY: QuadratureVariable
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: N(2, 3) = RESHAPE([1.0_DFP, 0.0_DFP, 0.5_DFP, &
!                                            0.5_DFP, 0.0_DFP, 1.0_DFP], &
!                                           [2, 3])
! REAL(DFP), PARAMETER :: T(2) = [0.25_DFP, 0.75_DFP]
!
! ! Test 1: rank and variable type are taken from obj
! BLOCK
!   TYPE(FEVariable_) :: obj, ans
!
!   obj = NodalVariable([2.0_DFP, 4.0_DFP], Scalar, Space)
!   ans = QuadratureVariable(tsize=3, rank=Scalar, vartype=Space)
!   CALL GetInterpolation_(obj=obj, N=N, nns=2, nips=3, scale=1.0_DFP, &
!                          addContribution=.FALSE., ans=ans)
!   CALL OK(ALL(ABS(ans%val(1:3) - [2.0_DFP, 3.0_DFP, 4.0_DFP]) .LT. tol), &
!           "scalar nodal space")
!
!   obj = NodalVariable(RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], &
!                               [2, 2]), Vector, Space)
!   ans = QuadratureVariable(nrow=2, ncol=3, rank=Vector, vartype=Space)
!   CALL GetInterpolation_(obj=obj, N=N, nns=2, nips=3, scale=1.0_DFP, &
!                          addContribution=.FALSE., ans=ans)
!   CALL OK(ALL(ABS(ans%val(1:6) - [1.0_DFP, 2.0_DFP, 2.0_DFP, 3.0_DFP, &
!                                   3.0_DFP, 4.0_DFP]) .LT. tol), &
!           "vector nodal space")
! END BLOCK
!
! ! Test 2: space-time variable with time shape functions at timeIndx
! BLOCK
!   TYPE(FEVariable_) :: obj, ans
!
!   obj = NodalVariable(RESHAPE([2.0_DFP, 4.0_DFP, 6.0_DFP, 8.0_DFP], &
!                               [2, 2]), Scalar, SpaceTime)
!   ans = QuadratureVariable(nrow=3, ncol=2, rank=Scalar, vartype=SpaceTime)
!   CALL GetInterpolation_(obj=obj, N=N, nns=2, nips=3, T=T, nnt=2, &
!                          scale=1.0_DFP, addContribution=.FALSE., &
!                          timeIndx=2, ans=ans)
!   CALL OK(ALL(ABS(ans%val(4:6) - [5.0_DFP, 6.0_DFP, 7.0_DFP]) .LT. tol), &
!           "scalar nodal spaceTime at timeIndx 2")
!   CALL OK(ans%len .EQ. 6, "len of ans is kept")
! END BLOCK
!
! END PROGRAM main
!```
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
    !! Interpolated value in FEVariable_ format
    !! Scalar, or Vector, or Matrix, Quadrature, Space
  END SUBROUTINE FEVariableGetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of a FEVariable in space and time
!
! Same as [[FEVariableGetInterpolation_1]], but a space-time `obj` is
! interpolated with the time shape functions `T` as well, and the result
! is written in the time slice `timeIndx` of `ans`.
!
! Examples
!
! See [[FEVariableGetInterpolation_1]] (Test 2).

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
    !! time shape functions at the time quadrature point, T(a)
    INTEGER(I4B), INTENT(IN) :: nnt
    !! number of time nodes, bound for T
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! Interpolated value in FEVariable_ format
  END SUBROUTINE FEVariableGetInterpolation_2
END INTERFACE GetInterpolation_

END MODULE FEVariable_InterpolationMethod
