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

module ElemshapeData_GradientMethods
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: getSpatialGradient
PUBLIC :: SpatialGradient

!----------------------------------------------------------------------------
!                                        getSpatialGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the spatial gradient of scalar

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getSpatialGradient_1(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :)
    !! Spatial gradient of scalar
    REAL(DFP), INTENT(IN) :: val(:)
    !! Nodal values of scalar
  END SUBROUTINE elemsd_getSpatialGradient_1
END INTERFACE

INTERFACE getSpatialGradient
  MODULE PROCEDURE elemsd_getSpatialGradient_1
END INTERFACE getSpatialGradient

!----------------------------------------------------------------------------
!                                        getSpatialGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the spatial gradient of vector

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getSpatialGradient_2(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :, :)
    !! spatial gradient of vector at integration points
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal values of vector in `xiJ` format
  END SUBROUTINE elemsd_getSpatialGradient_2
END INTERFACE

INTERFACE getSpatialGradient
  MODULE PROCEDURE elemsd_getSpatialGradient_2
END INTERFACE getSpatialGradient

!----------------------------------------------------------------------------
!                                        getSpatialGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the spatial gradient of scalar

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getSpatialGradient_3(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :)
    !! Spatial gradient of scalar
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time Nodal values of scalar
  END SUBROUTINE elemsd_getSpatialGradient_3
END INTERFACE

INTERFACE getSpatialGradient
  MODULE PROCEDURE elemsd_getSpatialGradient_3
END INTERFACE getSpatialGradient

!----------------------------------------------------------------------------
!                                        getSpatialGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the spatial gradient of scalar
!
INTERFACE
  MODULE PURE SUBROUTINE elemsd_getSpatialGradient_4(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :, :)
    !! spatial gradient of vector at integration points
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal values of vector in `xiJa` format
  END SUBROUTINE elemsd_getSpatialGradient_4
END INTERFACE

INTERFACE getSpatialGradient
  MODULE PROCEDURE elemsd_getSpatialGradient_4
END INTERFACE getSpatialGradient

!----------------------------------------------------------------------------
!                                        getSpatialGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the spatial gradient of scalar

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getSpatialGradient_5(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :)
    !! Spatial gradient of scalar
    TYPE(FEVariable_), INTENT(IN) :: val
    !! Nodal values of scalar
  END SUBROUTINE elemsd_getSpatialGradient_5
END INTERFACE

INTERFACE getSpatialGradient
  MODULE PROCEDURE elemsd_getSpatialGradient_5
END INTERFACE getSpatialGradient

!----------------------------------------------------------------------------
!                                        getSpatialGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the spatial gradient of scalar
!
INTERFACE
  MODULE PURE SUBROUTINE elemsd_getSpatialGradient_6(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :, :)
    !! spatial gradient of vector at integration points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! space-time nodal values of vector in `xiJa` format
  END SUBROUTINE elemsd_getSpatialGradient_6
END INTERFACE

INTERFACE getSpatialGradient
  MODULE PROCEDURE elemsd_getSpatialGradient_6
END INTERFACE getSpatialGradient

!----------------------------------------------------------------------------
!                                        getSpatialGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the spatial gradient of a matrix

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getSpatialGradient_7(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :, :, :)
    !! spatial gradient at integration points
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space nodal values of matrix in (i,j,I) format
  END SUBROUTINE elemsd_getSpatialGradient_7
END INTERFACE

INTERFACE getSpatialGradient
  MODULE PROCEDURE elemsd_getSpatialGradient_7
END INTERFACE getSpatialGradient

!----------------------------------------------------------------------------
!                                        getSpatialGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the spatial gradient of a matrix

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getSpatialGradient_8(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :, :, :)
    !! spatial gradient at integration points
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal values of matrix in (i,j,I,a) format
  END SUBROUTINE elemsd_getSpatialGradient_8
END INTERFACE

INTERFACE getSpatialGradient
  MODULE PROCEDURE elemsd_getSpatialGradient_8
END INTERFACE getSpatialGradient

!----------------------------------------------------------------------------
!                                         getSpatialGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the spatial gradient of a vector

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getSpatialGradient_9(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :, :, :)
    !! spatial gradient at integration points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! space nodal values of matrix in (i,j,I) format
  END SUBROUTINE elemsd_getSpatialGradient_9
END INTERFACE

INTERFACE getSpatialGradient
  MODULE PROCEDURE elemsd_getSpatialGradient_9
END INTERFACE getSpatialGradient

!----------------------------------------------------------------------------
!                                         getSpatialGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the spatial gradient
!
!# Introduction
!
! - This routine returns spatial gradient in [[FEVariable_]]
! the input is also a [[FEVariable_]].
! - This routine can be considered as a master routine

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getSpatialGradient_10(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: lg
    !! spatial gradient of scalar/vector/matrix at space integration points
    TYPE(FEVariable_), INTENT(IN) :: val
  END SUBROUTINE elemsd_getSpatialGradient_10
END INTERFACE

INTERFACE getSpatialGradient
  MODULE PROCEDURE elemsd_getSpatialGradient_10
END INTERFACE getSpatialGradient

!----------------------------------------------------------------------------
!                                         getSpatialGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the spatial gradient
!
!# Introduction
!
! - This routine returns spatial gradient in [[FEVariable_]]
! the input is also a [[FEVariable_]].
! - This routine can be considered as a master routine

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getSpatialGradient_11(obj, lg, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    TYPE(FEVariable_), INTENT(INOUT) :: lg
    !! spatial gradient of scalar/vector/matrix at space-time
    !! integration points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! space time nodal values of scalar/vector/matrix
  END SUBROUTINE elemsd_getSpatialGradient_11
END INTERFACE

INTERFACE getSpatialGradient
  MODULE PROCEDURE elemsd_getSpatialGradient_11
END INTERFACE getSpatialGradient

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION elemsd_SpatialGradient_1(obj, val) RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION elemsd_SpatialGradient_1
END INTERFACE

INTERFACE SpatialGradient
  MODULE PROCEDURE elemsd_SpatialGradient_1
END INTERFACE SpatialGradient

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION elemsd_SpatialGradient_2(obj, val) RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    TYPE(FEVariable_), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION elemsd_SpatialGradient_2
END INTERFACE

INTERFACE SpatialGradient
  MODULE PROCEDURE elemsd_SpatialGradient_2
END INTERFACE SpatialGradient

end module ElemshapeData_GradientMethods
