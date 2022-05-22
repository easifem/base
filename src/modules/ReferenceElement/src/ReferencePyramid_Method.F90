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
! summary: This module contains methods for [[ReferencePyramid_]]

MODULE ReferencePyramid_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                       Initiate@Pyramid
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
MODULE PURE SUBROUTINE initiate_ref_Pyramid( obj, NSD, XiJ )
  CLASS( ReferencePyramid_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
END SUBROUTINE initiate_ref_Pyramid
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Pyramid
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                            ReferencePyramid@Pyramid
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Pyramid( NSD, XiJ ) RESULT( obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferencePyramid_ ) :: obj
END FUNCTION reference_Pyramid
END INTERFACE

INTERFACE ReferencePyramid
  MODULE PROCEDURE reference_Pyramid
END INTERFACE ReferencePyramid

PUBLIC :: ReferencePyramid

!----------------------------------------------------------------------------
!                                   ReferencePyramid_Pointer@Pyramid
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Pyramid_Pointer( NSD, XiJ ) RESULT( obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferencePyramid_ ), POINTER :: obj
END FUNCTION reference_Pyramid_Pointer
END INTERFACE

INTERFACE ReferencePyramid_Pointer
  MODULE PROCEDURE reference_Pyramid_Pointer
END INTERFACE ReferencePyramid_Pointer

PUBLIC :: ReferencePyramid_Pointer

!----------------------------------------------------------------------------
!                                               LagrangeElement@Pyramid
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE LagrangeElement_Pyramid( RefElem, Order, obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  CLASS ( ReferenceElement_ ), INTENT( INOUT ) :: obj
END SUBROUTINE LagrangeElement_Pyramid
END INTERFACE

PUBLIC :: LagrangeElement_Pyramid

!----------------------------------------------------------------------------
!                                                  MeasureSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Measure_Simplex_Pyramid( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferencePyramid_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Pyramid
END INTERFACE

PUBLIC :: Measure_Simplex_Pyramid

!----------------------------------------------------------------------------
!                                                            Pyramid_Quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Pyramid_Quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferencePyramid_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION Pyramid_Quality
END INTERFACE

PUBLIC :: Pyramid_Quality

END MODULE ReferencePyramid_Method