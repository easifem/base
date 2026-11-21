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
MODULE ElemshapeData_DGHermitMethods
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ReferenceElement_
USE BaseType, ONLY: HermitInterpolation_
USE BaseType, ONLY: DG_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                         Initiate@DGHermit
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-23
! summary: Initiates the shape function data for Hermit interpolation
!  with discontinuous Galerkin (DG) continuity.
!
! This routine should initiate the shape function data of `obj`.
! It uses the quadrature points `quad` and the reference element
! `refElem`.
! The arguments `continuityType` and `interpolType` select the DG
! continuity and the Hermit interpolation.
!
! @warning
! This routine has no body. The module is not listed in the CMake file
! of `ElemshapeData`, so it is not compiled. No submodule implements
! `DG_Hermit`, so a call to it cannot link.
! @endwarning
!
! See also: [[ElemshapeData_DGMethods]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE DG_Hermit(obj, quad, refElem, &
                                   continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! shape function data to initiate
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature points
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    !! reference element
    CLASS(DG_), INTENT(IN) :: continuityType
    !! DG continuity type
    CLASS(HermitInterpolation_), INTENT(IN) :: interpolType
    !! Hermit interpolation type
  END SUBROUTINE DG_Hermit
END INTERFACE Initiate

END MODULE ElemshapeData_DGHermitMethods
