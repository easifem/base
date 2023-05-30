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
! summary: This module contains methods for [[ReferencePrism_]]

MODULE ReferencePrism_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                       Initiate@Prism
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         1 March 2021
! summary: This subroutine for constructing the object
!
!### Usage
!
!```fortran
!
!```

INTERFACE
  MODULE PURE SUBROUTINE initiate_ref_Prism(obj, NSD, XiJ)
    CLASS(ReferencePrism_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: XiJ(:, :)
  END SUBROUTINE initiate_ref_Prism
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Prism
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                            ReferencePrism@Prism
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION reference_Prism(NSD, XiJ) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: XiJ(:, :)
    TYPE(ReferencePrism_) :: obj
  END FUNCTION reference_Prism
END INTERFACE

INTERFACE ReferencePrism
  MODULE PROCEDURE reference_Prism
END INTERFACE ReferencePrism

PUBLIC :: ReferencePrism

!----------------------------------------------------------------------------
!                                   ReferencePrism_Pointer@Prism
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION reference_Prism_Pointer(NSD, XiJ) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: XiJ(:, :)
    CLASS(ReferencePrism_), POINTER :: obj
  END FUNCTION reference_Prism_Pointer
END INTERFACE

INTERFACE ReferencePrism_Pointer
  MODULE PROCEDURE reference_Prism_Pointer
END INTERFACE ReferencePrism_Pointer

PUBLIC :: ReferencePrism_Pointer

!----------------------------------------------------------------------------
!                                               LagrangeElement@Prism
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE highOrderElement_Prism(RefElem, Order, obj, ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: RefElem
    INTEGER(I4B), INTENT(IN) :: Order
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ipType
  END SUBROUTINE highOrderElement_Prism
END INTERFACE

PUBLIC :: highOrderElement_Prism

!----------------------------------------------------------------------------
!                                                  MeasureSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex_Prism(RefElem, XiJ) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: RefElem
    REAL(DFP), INTENT(IN) :: XiJ(:, :)
    REAL(DFP) :: Ans
  END FUNCTION Measure_Simplex_Prism
END INTERFACE

PUBLIC :: Measure_Simplex_Prism

!----------------------------------------------------------------------------
!                                                             Prism_Quality
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION Prism_Quality(refelem, xij, measure) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: Ans
  END FUNCTION Prism_Quality
END INTERFACE

PUBLIC :: Prism_Quality

END MODULE ReferencePrism_Method
