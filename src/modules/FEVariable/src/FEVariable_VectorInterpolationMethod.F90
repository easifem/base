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

MODULE FEVariable_VectorInterpolationMethod
USE BaseType, ONLY: FEVariable_, &
                    FEVariableVector_, &
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
!                                     GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Vector, constant

INTERFACE GetInterpolation_
 MODULE PURE SUBROUTINE VectorConstantGetInterpolation_1(obj, rank, vartype, &
                                                          N, nns, nips, &
                                                          scale, &
                                                          addContribution, &
                                                          ans, nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Interpolated value
    !! Size of ans should be at least nips
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! Number of data written in ans
  END SUBROUTINE VectorConstantGetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                     GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Vector, constant
!
!# Introduction
!
! ans%s(1) and obj%s(1) should be same
! ans%s(2) and nips should be same

INTERFACE GetInterpolation_
 MODULE PURE SUBROUTINE VectorConstantGetInterpolation_2(obj, rank, vartype, &
                                                          N, nns, nips, &
                                                          scale, &
                                                          addContribution, &
                                                          timeIndx, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
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
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time index for ans
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! Interpolated value in FEVariable form
    !! Size of ans should be at least nips
  END SUBROUTINE VectorConstantGetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@VectorInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Vector, space

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorSpaceGetInterpolation_1(obj, rank, vartype, &
                                                       N, nns, nips, &
                                                       scale, &
                                                       addContribution, &
                                                       ans, nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of integration points in N, bound for dim2 in N
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Interpolated value
    !! Size of ans should be at least nips
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! Number of data written in ans
  END SUBROUTINE VectorSpaceGetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@VectorInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Vector, space

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorSpaceGetInterpolation_2(obj, rank, vartype, &
                                                       N, nns, nips, &
                                                       scale, &
                                                       addContribution, &
                                                       timeIndx, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
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
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time index for ans
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! Interpolated value
    !! Size of ans should be at least nips
  END SUBROUTINE VectorSpaceGetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@VectorInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Vector, space-time

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorSpaceTimeGetInterpolation_1(obj, rank, &
                                                           vartype, &
                                                           N, nns, nips, &
                                                           T, nnt, &
                                                           scale, &
                                                           addContribution, &
                                                           ans, nrow, ncol, &
                                                           timeIndx)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of integration points in N, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: T(:)
    !! time shape functions data, T(a) : a is time node or dof number
    INTEGER(I4B), INTENT(IN) :: nnt
    !! number of time nodes in T, bound for dim1 in T
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Interpolated value
    !! Size of ans should be at least nips
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! Number of data written in ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time index is used when varType is spaceTime and defined on Quad
  END SUBROUTINE VectorSpaceTimeGetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@VectorInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Vector, space-time
!
!# Introduction
!
! When obj%vartype is Nodal:
! - Convert nodal values to quadrature values by using N
! - make sure nns .LE. obj%len
! - obj%s(1) denotes the nsd in obj
! - obj%s(2) should be equal to nns
! - obj%s(3) should be atleast nnt
!
! No need for interpolation, just returnt the quadrature values
! make sure nips .LE. obj%len

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorSpaceTimeGetInterpolation_2(obj, rank, &
                                                           vartype, &
                                                           N, nns, nips, &
                                                           T, nnt, &
                                                           scale, &
                                                           addContribution, &
                                                           timeIndx, &
                                                           ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of integration points in N, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: T(:)
    !! time shape functions data, T(a) : a is time node or dof number
    INTEGER(I4B), INTENT(IN) :: nnt
    !! number of time nodes in T, bound for dim1 in T
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time index is used when varType is spaceTime and defined on Quad
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! Interpolated value
    !! Size of ans should be at least nips
  END SUBROUTINE VectorSpaceTimeGetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_VectorInterpolationMethod
