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
MODULE ElemshapeData_DGLagrangeMethods
USE BaseType, ONLY: ElemshapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ReferenceElement_
USE BaseType, ONLY: DG_
USE BaseType, ONLY: LagrangeInterpolation_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-23
! summary: Initiates the shape data for DG Lagrange interpolation
!
! This routine should fill `obj` with the shape function data of a
! discontinuous Galerkin (DG) Lagrange element.
! It should evaluate the data at the points of `quad` on `refElem`.
!
! @warning
! This method has no body. No submodule defines `DG_Lagrange`.
! The build (`CMakeLists.txt`) does not include this module.
! @endwarning

INTERFACE Initiate
  MODULE PURE SUBROUTINE DG_Lagrange(obj, quad, refElem, &
                                     continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! shape data to fill
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature points for the shape data
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    !! reference element
    CLASS(DG_), INTENT(IN) :: continuityType
    !! continuity type; `DG_` selects this overload
    CLASS(LagrangeInterpolation_), INTENT(IN) :: interpolType
    !! Lagrange interpolation type
  END SUBROUTINE DG_Lagrange
END INTERFACE Initiate

END MODULE ElemshapeData_DGLagrangeMethods
