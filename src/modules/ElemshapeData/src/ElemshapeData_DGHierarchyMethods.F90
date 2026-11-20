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
module ElemshapeData_DGHierarchyMethods
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

public :: Initiate

!----------------------------------------------------------------------------
!                                                      Initiate@DGHierarchy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-23
! summary: Initiates the shape function data of an element for a
!   discontinuous Galerkin (DG) hierarchy basis.
!
! This routine should fill `obj` with the shape function data of
! the reference element `refElem` at the points of `quad`.
! The basis is a hierarchy basis for a DG continuity type.
!
! @warning
! This method is not implemented. No submodule has a body for it,
! and the build does not compile this file. Use `DG_Hierarchy1`
! in `ElemshapeData_DGMethods` instead.
! @endwarning

INTERFACE
  MODULE PURE SUBROUTINE DG_Hierarchy(obj, quad, refElem, &
    & continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! shape function data to initiate
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature points for evaluating the shape functions
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    !! reference element
    CLASS(DG_), INTENT(IN) :: continuityType
    !! continuity type, here the discontinuous Galerkin type
    CLASS(HierarchyInterpolation_), INTENT(IN) :: interpolType
    !! hierarchy interpolation type
  END SUBROUTINE DG_Hierarchy
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE DG_Hierarchy
END INTERFACE Initiate

end module ElemshapeData_DGHierarchyMethods
