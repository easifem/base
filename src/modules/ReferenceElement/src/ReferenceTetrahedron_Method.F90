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
! date: 	5 March 2021
! summary: This module contains methods for [[ReferenceTetrahedron_]]

MODULE ReferenceTetrahedron_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE


!----------------------------------------------------------------------------
!                                                       Initiate@Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: This subroutine for constructing the object
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE SUBROUTINE initiate_ref_Tetrahedron( obj, NSD, XiJ )
  CLASS( ReferenceTetrahedron_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
END SUBROUTINE initiate_ref_Tetrahedron
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Tetrahedron
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                            ReferenceTetrahedron@Tetrahedron
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Tetrahedron( NSD, XiJ ) RESULT( obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferenceTetrahedron_ ) :: obj
END FUNCTION reference_Tetrahedron
END INTERFACE

INTERFACE ReferenceTetrahedron
  MODULE PROCEDURE reference_Tetrahedron
END INTERFACE ReferenceTetrahedron

PUBLIC :: ReferenceTetrahedron

!----------------------------------------------------------------------------
!                                   ReferenceTetrahedron_Pointer@Tetrahedron
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Tetrahedron_Pointer( NSD, XiJ ) RESULT( obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferenceTetrahedron_ ), POINTER :: obj
END FUNCTION reference_Tetrahedron_Pointer
END INTERFACE

INTERFACE ReferenceTetrahedron_Pointer
  MODULE PROCEDURE reference_Tetrahedron_Pointer
END INTERFACE ReferenceTetrahedron_Pointer

PUBLIC :: ReferenceTetrahedron_Pointer

!----------------------------------------------------------------------------
!                                               LagrangeElement@Tetrahedron
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE highOrderElement_Tetrahedron( RefElem, Order, obj, &
  & ipType )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  CLASS ( ReferenceElement_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ipType
END SUBROUTINE highOrderElement_Tetrahedron
END INTERFACE

PUBLIC :: highOrderElement_Tetrahedron

!----------------------------------------------------------------------------
!                                                 MeasureSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Measure_Simplex_Tetrahedron( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferenceTetrahedron_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Tetrahedron
END INTERFACE

PUBLIC :: Measure_Simplex_Tetrahedron

!----------------------------------------------------------------------------
!                                                       Tetrahedron_Quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Tetrahedron_Quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( Referencetetrahedron_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION Tetrahedron_Quality
END INTERFACE

PUBLIC :: Tetrahedron_Quality

END MODULE ReferenceTetrahedron_Method