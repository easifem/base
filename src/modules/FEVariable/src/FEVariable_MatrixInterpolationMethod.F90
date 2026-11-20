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
! `GetInterpolation_` evaluates a matrix FEVariable_ at quadrature points
! without allocation. A nodal variable is interpolated with the space
! shape functions `N` (and the time shape functions `T` for space-time);
! a quadrature variable is copied. Three forms exist for each variable
! type (constant, space, space-time):
!
! - `_1`: values at all `nips` points in an array
! - `_2`: values at all points written in a FEVariable_ at `timeIndx`
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
! Writes the interpolated value in `ans(1:dim1, 1:dim2, 1:nips)`.
! The constant value is used at every quadrature point; `N` is not
! used.
!
! If `addContribution` is false, `ans` is cleared first.
!
! Examples
!
!```fortran
! CALL GetInterpolation_(obj=obj, rank=TypeFEVariableMatrix, &
!                        vartype=TypeFEVariableConstant, N=N, nns=nns, &
!                        nips=nips, scale=1.0_DFP, &
!                        addContribution=.FALSE., ans=ans, dim1=dim1, &
!                        dim2=dim2, dim3=dim3)
!```
!
! Full example:
! src/modules/FEVariable/examples/GetInterpolation__test_3.F90

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
    !! if true, the interpolated value is added to ans
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
! Writes the interpolated value in `ans%val` of the time slice `timeIndx`.
! The constant value is used at every quadrature point; `N` is not
! used.
!
! `ans` must already be a quadrature FEVariable_ with the shape of the
! result; the values of time point `timeIndx` start after
! `(timeIndx - 1)` time slices. `ans%s`, `ans%len` and the other
! components of `ans` are not changed.
! If `addContribution` is false, that time slice is cleared first.
!
! Changes:
!   - 2026-11-14: the constant value is repeated at every point; before
!     it read past the values of obj when nips > 1
!
! Examples
!
!```fortran
! CALL GetInterpolation_(obj=obj, rank=TypeFEVariableMatrix, &
!                        vartype=TypeFEVariableConstant, N=N, nns=nns, &
!                        nips=nips, scale=1.0_DFP, &
!                        addContribution=.FALSE., timeIndx=timeIndx, &
!                        ans=ans)
!```
!
! Full example:
! src/modules/FEVariable/examples/GetInterpolation__test_3.F90

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
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! quadrature variable, values written at timeIndx
  END SUBROUTINE MatrixConstantGetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of matrix, constant, at one quadrature point
!
! Writes the interpolated value in `ans(1:nrow, 1:ncol)`.
! The constant value is used at every quadrature point; `N` is not
! used.
!
! If `addContribution` is false, `ans` is cleared first.
!
! Examples
!
!```fortran
! CALL GetInterpolation_(obj=obj, rank=TypeFEVariableMatrix, &
!                        vartype=TypeFEVariableConstant, N=N, nns=nns, &
!                        spaceIndx=ips, timeIndx=1, scale=1.0_DFP, &
!                        addContribution=.FALSE., ans=ans, nrow=nrow, &
!                        ncol=ncol)
!```
!
! Full example:
! src/modules/FEVariable/examples/GetInterpolation__test_3.F90

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
    !! if true, the interpolated value is added to ans
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
! Writes the interpolated value in `ans(1:dim1, 1:dim2, 1:nips)`:
!
! - nodal:
!   `ans(:, :, ips) = scale * SUM_I N(I, ips) * obj(:, :, I)`
! - quadrature (`N` is not used):
!   `ans(:, :, ips) = scale * obj(:, :, ips)`
!
! If `addContribution` is false, `ans` is cleared first.
!
! Examples
!
!```fortran
! CALL GetInterpolation_(obj=obj, rank=TypeFEVariableMatrix, &
!                        vartype=TypeFEVariableSpace, N=N, nns=nns, &
!                        nips=nips, scale=1.0_DFP, &
!                        addContribution=.FALSE., ans=ans, dim1=dim1, &
!                        dim2=dim2, dim3=dim3)
!```
!
! Full example:
! src/modules/FEVariable/examples/GetInterpolation__test_3.F90

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
    !! if true, the interpolated value is added to ans
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
! Writes the interpolated value in `ans%val` of the time slice `timeIndx`:
!
! - nodal:
!   `ans(:, :, ips) = scale * SUM_I N(I, ips) * obj(:, :, I)`
! - quadrature (`N` is not used):
!   `ans(:, :, ips) = scale * obj(:, :, ips)`
!
! `ans` must already be a quadrature FEVariable_ with the shape of the
! result; the values of time point `timeIndx` start after
! `(timeIndx - 1)` time slices. `ans%s`, `ans%len` and the other
! components of `ans` are not changed.
! If `addContribution` is false, that time slice is cleared first.
!
! Examples
!
!```fortran
! CALL GetInterpolation_(obj=obj, rank=TypeFEVariableMatrix, &
!                        vartype=TypeFEVariableSpace, N=N, nns=nns, &
!                        nips=nips, scale=1.0_DFP, &
!                        addContribution=.FALSE., timeIndx=timeIndx, &
!                        ans=ans)
!```
!
! Full example:
! src/modules/FEVariable/examples/GetInterpolation__test_3.F90

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
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! quadrature variable, values written at timeIndx
  END SUBROUTINE MatrixSpaceGetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of matrix, space, at one quadrature point
!
! Writes the interpolated value in `ans(1:nrow, 1:ncol)`:
!
! - nodal:
!   `ans(:, :) = scale * SUM_I N(I, spaceIndx) * obj(:, :, I)`
! - quadrature (`N` is not used):
!   `ans(:, :) = scale * obj(:, :, spaceIndx)`
!
! If `addContribution` is false, `ans` is cleared first.
!
! Examples
!
!```fortran
! CALL GetInterpolation_(obj=obj, rank=TypeFEVariableMatrix, &
!                        vartype=TypeFEVariableSpace, N=N, nns=nns, &
!                        spaceIndx=ips, timeIndx=1, scale=1.0_DFP, &
!                        addContribution=.FALSE., ans=ans, nrow=nrow, &
!                        ncol=ncol)
!```
!
! Full example:
! src/modules/FEVariable/examples/GetInterpolation__test_3.F90

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
    !! if true, the interpolated value is added to ans
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
! Writes the interpolated value in `ans(1:dim1, 1:dim2, 1:nips)`:
!
! - nodal:
!   `ans(:, :, ips) = scale * SUM_a T(a) SUM_I N(I, ips) * obj(:, :, I, a)`
! - quadrature (`N` and `T` are not used):
!   `ans(:, :, ips) = scale * obj(:, :, ips, timeIndx)`
!
! If `addContribution` is false, `ans` is cleared first.
!
! Examples
!
!```fortran
! CALL GetInterpolation_(obj=obj, rank=TypeFEVariableMatrix, &
!                        vartype=TypeFEVariableSpaceTime, N=N, nns=nns, &
!                        nips=nips, T=T, nnt=nnt, scale=1.0_DFP, &
!                        addContribution=.FALSE., ans=ans, dim1=dim1, &
!                        dim2=dim2, dim3=dim3, timeIndx=1)
!```
!
! Full example:
! src/modules/FEVariable/examples/GetInterpolation__test_3.F90

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
    !! if true, the interpolated value is added to ans
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
! Writes the interpolated value in `ans%val` of the time slice `timeIndx`:
!
! - nodal:
!   `ans(:, :, ips) = scale * SUM_a T(a) SUM_I N(I, ips) * obj(:, :, I, a)`
! - quadrature (`N` and `T` are not used):
!   `ans(:, :, ips) = scale * obj(:, :, ips, timeIndx)`
!
! `ans` must already be a quadrature FEVariable_ with the shape of the
! result; the values of time point `timeIndx` start after
! `(timeIndx - 1)` time slices. `ans%s`, `ans%len` and the other
! components of `ans` are not changed.
! If `addContribution` is false, that time slice is cleared first.
!
! Examples
!
!```fortran
! CALL GetInterpolation_(obj=obj, rank=TypeFEVariableMatrix, &
!                        vartype=TypeFEVariableSpaceTime, N=N, nns=nns, &
!                        nips=nips, T=T, nnt=nnt, scale=1.0_DFP, &
!                        addContribution=.FALSE., timeIndx=timeIndx, &
!                        ans=ans)
!```
!
! Full example:
! src/modules/FEVariable/examples/GetInterpolation__test_3.F90

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
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! quadrature variable, values written at timeIndx
  END SUBROUTINE MatrixSpaceTimeGetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of matrix, space-time, at one quadrature point
!
! Writes the interpolated value in `ans(1:nrow, 1:ncol)`:
!
! - nodal:
!   `ans(:, :) = scale * SUM_a T(a) SUM_I N(I, spaceIndx) * obj(:, :, I, a)`
! - quadrature (`N` and `T` are not used):
!   `ans(:, :) = scale * obj(:, :, spaceIndx, timeIndx)`
!
! If `addContribution` is false, `ans` is cleared first.
!
! Examples
!
!```fortran
! CALL GetInterpolation_(obj=obj, rank=TypeFEVariableMatrix, &
!                        vartype=TypeFEVariableSpaceTime, N=N, nns=nns, &
!                        spaceIndx=ips, timeIndx=1, T=T, nnt=nnt, &
!                        scale=1.0_DFP, addContribution=.FALSE., ans=ans, &
!                        nrow=nrow, ncol=ncol)
!```
!
! Full example:
! src/modules/FEVariable/examples/GetInterpolation__test_3.F90

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
    !! if true, the interpolated value is added to ans
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
! Calls the `_3` form of the variable type of `obj`: constant, space or
! space-time. For a time variable nothing is done and `ans` is not
! changed.

!
! Examples
!
!```fortran
! CALL GetInterpolation_(obj=obj, rank=TypeFEVariableMatrix, N=N, nns=nns, &
!                        spaceIndx=ips, timeIndx=1, T=T, nnt=nnt, &
!                        scale=1.0_DFP, addContribution=.FALSE., ans=ans, &
!                        nrow=nrow, ncol=ncol)
!```
!
! Full example:
! src/modules/FEVariable/examples/GetInterpolation__test_3.F90

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
    !! if true, the interpolated value is added to ans
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
