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
!                                     GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of scalar, constant

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarConstantGetInterpolation_1( &
    obj, rank, vartype, N, nns, nips, scale, addContribution, ans, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
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
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! Interpolated value
    !! Size of ans should be at least nips
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of data written in ans
  END SUBROUTINE ScalarConstantGetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                     GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of scalar, constant

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarConstantGetInterpolation_2( &
    obj, rank, vartype, N, nns, nips, scale, addContribution, timeIndx, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
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
    !! Interpolated value in FEVariable_ format
    !! Scalar, QuadratureVariable, Space
  END SUBROUTINE ScalarConstantGetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                     GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of scalar, constant

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarConstantGetInterpolation_3( &
    obj, rank, vartype, N, nns, spaceIndx, timeIndx, scale, addContribution, &
    ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: spaceIndx
    !! number of integration points in N, bound for dim2 in N
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time index
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    REAL(DFP), INTENT(INOUT) :: ans
    !! Interpolated value
    !! Size of ans should be at least nips
  END SUBROUTINE ScalarConstantGetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@ScalarInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of scalar, space

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarSpaceGetInterpolation_1( &
    obj, rank, vartype, N, nns, nips, scale, addContribution, ans, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of integration points in N, bound for dim2 in N
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! Interpolated value
    !! Size of ans should be at least nips
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of data written in ans
  END SUBROUTINE ScalarSpaceGetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@ScalarInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of scalar, space

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarSpaceGetInterpolation_2( &
    obj, rank, vartype, N, nns, nips, scale, addContribution, timeIndx, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
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
    !! time index
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! Interpolated value in FEVariable_ format
    !! Scalar, QuadratureVariable, Space
  END SUBROUTINE ScalarSpaceGetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@ScalarInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of scalar, space

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarSpaceGetInterpolation_3( &
    obj, rank, vartype, N, nns, spaceIndx, timeIndx, scale, addContribution, &
    ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: spaceIndx, timeIndx
    !! space and time integration point index
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    REAL(DFP), INTENT(INOUT) :: ans
    !! Interpolated value
    !! Size of ans should be at least nips
  END SUBROUTINE ScalarSpaceGetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@ScalarInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of scalar, space-time

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarSpaceTimeGetInterpolation_1( &
    obj, rank, vartype, N, nns, nips, T, nnt, scale, addContribution, &
    ans, tsize, timeIndx)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
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
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! Interpolated value
    !! Size of ans should be at least nips
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of data written in ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time index is used when varType is spaceTime and defined on Quad
  END SUBROUTINE ScalarSpaceTimeGetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@ScalarInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of scalar, space-time
!
!# Introduction
!
! If obj%varType is SpaceTime Then following thing happens
!   In this case ans will be Scalar, Space, QuadratureVariable
!   The values corresponding to timeIndx will be returned in ans as follows
!
! valStart = (timeIndx - 1) * obj%s(1)
! DO aa =  1,  tsize
!   ans%val(aa) = ans%val(aa) + scale * obj%val(aa+valStart)
! END DO

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarSpaceTimeGetInterpolation_2( &
    obj, rank, vartype, N, nns, nips, T, nnt, scale, addContribution, &
    timeIndx, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
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
    !! Interpolated value in FEVariable_ format
    !! Scalar, QuadratureVariable, SpaceTime
  END SUBROUTINE ScalarSpaceTimeGetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@ScalarInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of scalar, space-time

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarSpaceTimeGetInterpolation_3( &
    obj, rank, vartype, N, nns, spaceIndx, timeIndx, T, nnt, scale, &
    addContribution, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: spaceIndx, timeIndx
    !! number of integration points in N, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: T(:)
    !! time shape functions data, T(a) : a is time node or dof number
    INTEGER(I4B), INTENT(IN) :: nnt
    !! number of time nodes in T, bound for dim1 in T
    REAL(DFP), INTENT(INOUT) :: ans
    !! Interpolated value
    !! Size of ans should be at least nips
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
  END SUBROUTINE ScalarSpaceTimeGetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@ScalarInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of scalar, space-time

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarGetInterpolation_3( &
    obj, rank, N, nns, spaceIndx, timeIndx, T, nnt, scale, addContribution, &
    ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: spaceIndx, timeIndx
    !! number of integration points in N, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: T(:)
    !! time shape functions data, T(a) : a is time node or dof number
    INTEGER(I4B), INTENT(IN) :: nnt
    !! number of time nodes in T, bound for dim1 in T
    REAL(DFP), INTENT(INOUT) :: ans
    !! Interpolated value
    !! Size of ans should be at least nips
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
  END SUBROUTINE ScalarGetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_ScalarInterpolationMethod
