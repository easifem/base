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

module ElemshapeData_DGSerendipityMethods
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

public :: Initiate

!----------------------------------------------------------------------------
!                                                    Initiate@DGSerendipity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-23
! summary: Initiates the shape function data of an element for the
!   discontinuous Galerkin (DG) Serendipity basis.
!
! This routine initiates the shape function data inside `obj`.
! The data belongs to a reference element `refElem`.
! The routine evaluates it at the points of `quad`.
!
! The type of `continuityType` selects the DG continuity.
! The type of `interpolType` selects the Serendipity interpolation.
!
! @warning
! This routine has no implementation yet. The module is not compiled.
! @endwarning

INTERFACE
  MODULE PURE SUBROUTINE DG_Serendipity(obj, quad, refElem, &
    & continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! shape function data to initiate
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature points where the routine evaluates the data
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    !! reference element
    CLASS(DG_), INTENT(IN) :: continuityType
    !! DG continuity type
    CLASS(SerendipityInterpolation_), INTENT(IN) :: interpolType
    !! Serendipity interpolation type
  END SUBROUTINE DG_Serendipity
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE DG_Serendipity
END INTERFACE Initiate

end module ElemshapeData_DGSerendipityMethods
