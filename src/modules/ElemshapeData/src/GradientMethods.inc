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

!----------------------------------------------------------------------------
!                                           getLocalGradient@GradientMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local gradient of a scalar
!
! $$\frac{\partial \phi }{\partial \xi_{i} } =\phi_{I} \frac{\partial N^{I}}
! {\partial \xi_{i} }$$
!
INTERFACE
  MODULE PURE SUBROUTINE getLocalGradient_1(obj, dPhidXi, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dPhidXi(:, :)
    REAL(DFP), INTENT(IN) :: val(:)
    !! Space nodal values of scalar
  END SUBROUTINE getLocalGradient_1
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE getLocalGradient_1
END INTERFACE getLocalGradient

PUBLIC :: getLocalGradient

!----------------------------------------------------------------------------
!                                           getLocalGradient@GradientMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local gradient of a vector

INTERFACE
  MODULE PURE SUBROUTINE getLocalGradient_2(obj, dVdXi, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dVdXi(:, :, :)
    !! local gradient at integration points
    !! first index: space component of V
    !! second index: space component of x
    !! third index: integration point
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal values of vector in `xiJ` format
    !! row index: space component
    !! col index: node number
  END SUBROUTINE getLocalGradient_2
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE getLocalGradient_2
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                          getLocalGradient@GradientMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local gradient of a scalar
!
! $$
! \frac{\partial \phi }{\partial \xi_{i} } =\phi^{a}_{I} T_{a}\frac
! {\partial N^{I}}{\partial \xi_{i} }
!$$
!
INTERFACE

  MODULE PURE SUBROUTINE getLocalGradient_3(obj, dPhidXi, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dPhidXi(:, :)
    !! local gradient of scalar
    !! first index = space component of xi
    !! second index= integration point in space
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values of scalar
    !! first index = space node
    !! second index = time node
  END SUBROUTINE getLocalGradient_3
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE getLocalGradient_3
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                          getLocalGradient@GradientMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local gradient of a vector
!
! $$
! \frac{\partial v_{i} }{\partial \xi_{j} } =v^{a}_{iI} T_{a}\frac
! {\partial N^{I}}{\partial \xi_{j} }
!$$
!
INTERFACE
  MODULE PURE SUBROUTINE getLocalGradient_4(obj, dVdXi, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dVdXi(:, :, :)
    !! local gradient at integration points
    !! first index : space compo of V
    !! second index: space compo of Xi
    !! third index: integration point in space
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal values of vector in `xiJa` format
    !! first index: space compo
    !! second index: space node
    !! third index: time node
  END SUBROUTINE getLocalGradient_4
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE getLocalGradient_4
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                          getLocalGradient@GradientMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local gradient of a scalar
!
! $$\frac{\partial \phi }{\partial \xi_{i} } =\phi_{I} \frac{\partial N^{I}}
! {\partial \xi_{i} }$$
!
INTERFACE
  MODULE PURE SUBROUTINE getLocalGradient_5(obj, dPhidXi, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dPhidXi(:, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    !! Space nodal values of scalar
  END SUBROUTINE getLocalGradient_5
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE getLocalGradient_5
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                           getLocalGradient@GradientMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local gradient of a vector
!
! $$
! \frac{\partial v_{i} }{\partial \xi_{j} } =v^{a}_{iI} T_{a}\frac
! {\partial N^{I}}{\partial \xi_{j} }
! $$
!
INTERFACE
  MODULE PURE SUBROUTINE getLocalGradient_6(obj, dVdXi, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dVdXi(:, :, :)
    !! local gradient at integration points
    !! first index : space compo of V
    !! second index: space compo of Xi
    !! third index: integration point in space
    TYPE(FEVariable_), INTENT(IN) :: val
    !! space-time nodal values of vector in `xiJa` format
    !! first index: space compo
    !! second index: space node
    !! third index: time node
  END SUBROUTINE getLocalGradient_6
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE getLocalGradient_6
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                        getSpatialGradient@GradientMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the spatial gradient of scalar

INTERFACE
  MODULE PURE SUBROUTINE getSpatialGradient_1(obj, dPhidXt, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dPhidXt(:, :)
    !! Spatial gradient of scalar
    REAL(DFP), INTENT(IN) :: val(:)
    !! Nodal values of scalar
  END SUBROUTINE getSpatialGradient_1
END INTERFACE

INTERFACE getSpatialGradient
  MODULE PROCEDURE getSpatialGradient_1
END INTERFACE getSpatialGradient

PUBLIC :: getSpatialGradient

!----------------------------------------------------------------------------
!                                        getSpatialGradient@GradientMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the spatial gradient of vector

INTERFACE
  MODULE PURE SUBROUTINE getSpatialGradient_2(obj, dVdXt, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dVdXt(:, :, :)
    !! spatial gradient of `val` at integration points
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal values of vector in `xiJ` format
  END SUBROUTINE getSpatialGradient_2
END INTERFACE

INTERFACE getSpatialGradient
  MODULE PROCEDURE getSpatialGradient_2
END INTERFACE getSpatialGradient

!----------------------------------------------------------------------------
!                                        getSpatialGradient@GradientMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the spatial gradient of scalar

INTERFACE
  MODULE PURE SUBROUTINE getSpatialGradient_3(obj, dPhidXt, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dPhidXt(:, :)
    !! Spatial gradient of scalar
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time Nodal values of scalar
  END SUBROUTINE getSpatialGradient_3
END INTERFACE

INTERFACE getSpatialGradient
  MODULE PROCEDURE getSpatialGradient_3
END INTERFACE getSpatialGradient

!----------------------------------------------------------------------------
!                                        getSpatialGradient@GradientMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the spatial gradient of scalar
!
INTERFACE
  MODULE PURE SUBROUTINE getSpatialGradient_4(obj, dVdXt, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dVdXt(:, :, :)
    !! spatial gradient of `val` at integration points
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal values of vector in `xiJa` format
  END SUBROUTINE getSpatialGradient_4
END INTERFACE

INTERFACE getSpatialGradient
  MODULE PROCEDURE getSpatialGradient_4
END INTERFACE getSpatialGradient

!----------------------------------------------------------------------------
!                                        getSpatialGradient@GradientMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the spatial gradient of scalar

INTERFACE
  MODULE PURE SUBROUTINE getSpatialGradient_5(obj, dPhidXt, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dPhidXt(:, :)
    !! Spatial gradient of scalar
    TYPE(FEVariable_), INTENT(IN) :: val
    !! Nodal values of scalar
  END SUBROUTINE getSpatialGradient_5
END INTERFACE

INTERFACE getSpatialGradient
  MODULE PROCEDURE getSpatialGradient_5
END INTERFACE getSpatialGradient

!----------------------------------------------------------------------------
!                                        getSpatialGradient@GradientMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the spatial gradient of scalar
!
INTERFACE
  MODULE PURE SUBROUTINE getSpatialGradient_6(obj, dVdXt, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dVdXt(:, :, :)
    !! spatial gradient of `val` at integration points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! space-time nodal values of vector in `xiJa` format
  END SUBROUTINE getSpatialGradient_6
END INTERFACE

INTERFACE getSpatialGradient
  MODULE PROCEDURE getSpatialGradient_6
END INTERFACE getSpatialGradient
