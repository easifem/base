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

!> author: Vikas Sharma, Ph. D.
! date:         5 March 2021
! summary: This module contains methods for [[ReferencePyramid_]]

MODULE ReferencePyramid_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE
PUBLIC :: Initiate
PUBLIC :: ReferencePyramid
PUBLIC :: ReferencePyramid_Pointer
PUBLIC :: highOrderElement_Pyramid
PUBLIC :: Measure_Simplex_Pyramid
PUBLIC :: Pyramid_Quality

!----------------------------------------------------------------------------
!                                                       Initiate@Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         1 March 2021
! summary: This subroutine for constructing the object

INTERFACE Initiate
  MODULE PURE SUBROUTINE initiate_ref_Pyramid(obj, NSD, XiJ)
    CLASS(ReferencePyramid_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: XiJ(:, :)
  END SUBROUTINE initiate_ref_Pyramid
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                            ReferencePyramid@Pyramid
!----------------------------------------------------------------------------

INTERFACE ReferencePyramid
  MODULE PURE FUNCTION reference_Pyramid(NSD, XiJ) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: XiJ(:, :)
    TYPE(ReferencePyramid_) :: obj
  END FUNCTION reference_Pyramid
END INTERFACE ReferencePyramid

!----------------------------------------------------------------------------
!                                   ReferencePyramid_Pointer@Pyramid
!----------------------------------------------------------------------------

INTERFACE ReferencePyramid_Pointer
  MODULE PURE FUNCTION reference_Pyramid_Pointer(NSD, XiJ) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: XiJ(:, :)
    CLASS(ReferencePyramid_), POINTER :: obj
  END FUNCTION reference_Pyramid_Pointer
END INTERFACE ReferencePyramid_Pointer

!----------------------------------------------------------------------------
!                                               LagrangeElement@Pyramid
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE highOrderElement_Pyramid(RefElem, Order, obj, ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: RefElem
    INTEGER(I4B), INTENT(IN) :: Order
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ipType
  END SUBROUTINE highOrderElement_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                  MeasureSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex_Pyramid(RefElem, XiJ) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: RefElem
    REAL(DFP), INTENT(IN) :: XiJ(:, :)
    REAL(DFP) :: Ans
  END FUNCTION Measure_Simplex_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Pyramid_Quality
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION Pyramid_Quality(refelem, xij, measure) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: Ans
  END FUNCTION Pyramid_Quality
END INTERFACE

END MODULE ReferencePyramid_Method
