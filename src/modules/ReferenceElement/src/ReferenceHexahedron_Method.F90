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

!> authors: Vikas Sharma, Ph. D.
! date: 	5 March 2021
! summary: This module contains methods for [[ReferenceHexahedron_]]

MODULE ReferenceHexahedron_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                       Initiate@Hexahedron
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: This subroutine for constructing the object
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE SUBROUTINE initiate_ref_Hexahedron( obj, NSD, XiJ )
  CLASS( ReferenceHexahedron_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
END SUBROUTINE initiate_ref_Hexahedron
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Hexahedron
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                            ReferenceHexahedron@Hexahedron
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Hexahedron( NSD, XiJ ) RESULT( obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferenceHexahedron_ ) :: obj
END FUNCTION reference_Hexahedron
END INTERFACE

INTERFACE ReferenceHexahedron
  MODULE PROCEDURE reference_Hexahedron
END INTERFACE ReferenceHexahedron

PUBLIC :: ReferenceHexahedron

!----------------------------------------------------------------------------
!                                   ReferenceHexahedron_Pointer@Hexahedron
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Hexahedron_Pointer( NSD, XiJ ) RESULT( obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferenceHexahedron_ ), POINTER :: obj
END FUNCTION reference_Hexahedron_Pointer
END INTERFACE

INTERFACE ReferenceHexahedron_Pointer
  MODULE PROCEDURE reference_Hexahedron_Pointer
END INTERFACE ReferenceHexahedron_Pointer

PUBLIC :: ReferenceHexahedron_Pointer

!----------------------------------------------------------------------------
!                                               LagrangeElement@Hexahedron
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE LagrangeElement_Hexahedron( RefElem, Order, obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  CLASS ( ReferenceElement_ ), INTENT( INOUT ) :: obj
END SUBROUTINE LagrangeElement_Hexahedron
END INTERFACE

PUBLIC :: LagrangeElement_Hexahedron

!----------------------------------------------------------------------------
!                                                 MeasureSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Measure_Simplex_Hexahedron( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferenceHexahedron_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Hexahedron
END INTERFACE

PUBLIC :: Measure_Simplex_Hexahedron

!----------------------------------------------------------------------------
!                                                        Hexahedron_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Hexahedron_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferenceHexahedron_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION Hexahedron_quality
END INTERFACE

PUBLIC :: Hexahedron_Quality

END MODULE ReferenceHexahedron_Method