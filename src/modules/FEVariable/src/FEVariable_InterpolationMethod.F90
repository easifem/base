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

MODULE FEVariable_InterpolationMethod
USE BaseType, ONLY: FEVariable_, &
                    FEVariableScalar_, &
                    FEVariableVector_, &
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
!                                     GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of scalar, constant

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarConstantGetInterpolation_(obj, rank, vartype, &
                                                         N, nns, nips, &
                                                         scale, &
                                                         addContribution, &
                                                         ans, tsize)
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
  END SUBROUTINE ScalarConstantGetInterpolation_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@ScalarInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of scalar, space

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarSpaceGetInterpolation_(obj, rank, vartype, &
                                                      N, nns, nips, &
                                                      scale, &
                                                      addContribution, &
                                                      ans, tsize)
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
  END SUBROUTINE ScalarSpaceGetInterpolation_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@ScalarInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of scalar, space-time

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE ScalarSpaceTimeGetInterpolation_(obj, rank, &
                                                          vartype, &
                                                          N, nns, nips, &
                                                          T, nnt, &
                                                          scale, &
                                                          addContribution, &
                                                          ans, tsize, &
                                                          timeIndx)
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
  END SUBROUTINE ScalarSpaceTimeGetInterpolation_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                     GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Vector, constant

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorConstantGetInterpolation_(obj, rank, vartype, &
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
  END SUBROUTINE VectorConstantGetInterpolation_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@VectorInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Vector, space

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorSpaceGetInterpolation_(obj, rank, vartype, &
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
  END SUBROUTINE VectorSpaceGetInterpolation_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@VectorInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Vector, space-time

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorSpaceTimeGetInterpolation_(obj, rank, &
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
  END SUBROUTINE VectorSpaceTimeGetInterpolation_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                     GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Matrix, constant

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE MatrixConstantGetInterpolation_(obj, rank, vartype, &
                                                         N, nns, nips, &
                                                         scale, &
                                                         addContribution, &
                                                         ans, dim1, dim2, &
                                                         dim3)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! Interpolated value
    !! Size of ans should be at least nips
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! Number of data written in ans
  END SUBROUTINE MatrixConstantGetInterpolation_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@MatrixInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Matrix, space

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE MatrixSpaceGetInterpolation_(obj, rank, vartype, &
                                                      N, nns, nips, &
                                                      scale, &
                                                      addContribution, &
                                                      ans, dim1, dim2, dim3)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions data, N(I, ips) : I is node or dof number
    !! ips is integration point number
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in N, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of integration points in N, bound for dim2 in N
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! Interpolated value
    !! Size of ans should be at least nips
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! Number of data written in ans
  END SUBROUTINE MatrixSpaceGetInterpolation_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                               GetInterpolation_@MatrixInterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Matrix, space-time

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE MatrixSpaceTimeGetInterpolation_(obj, rank, &
                                                          vartype, &
                                                          N, nns, nips, &
                                                          T, nnt, &
                                                          scale, &
                                                          addContribution, &
                                                          ans, dim1, dim2, &
                                                          dim3, timeIndx)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! Interpolated value
    !! Size of ans should be at least nips
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor to be applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! Number of data written in ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time index is used when varType is spaceTime and defined on Quad
  END SUBROUTINE MatrixSpaceTimeGetInterpolation_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of Matrix, space-time

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE FEVariableGetInterpolation_1(obj, N, nns, nips, &
                                                     scale, addContribution, &
                                                      ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
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
  END SUBROUTINE FEVariableGetInterpolation_1
END INTERFACE GetInterpolation_

END MODULE FEVariable_InterpolationMethod
