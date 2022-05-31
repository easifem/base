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
! date: 18 June 2021
! summary: This submodule contains method for [[ReferencePoint_]]

MODULE ReferencePoint_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                            Initiate@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: This subroutine constructs an instance of point reference element
!
!# Introduction
! This routine constructs an instance of [[ReferencePoint_]] element
!
! - `XiJ` denotes the nodal coordinate, if it is not present than zero is
! used.
!
!@note
! 	Note that SIZE(XiJ,1) should be equal to 3, i.e., x,y,z coord.
! Also note that this routine creats a linear element.
!@endnote
!
!### Usage
!
!```fortran
! type( ReferencePoint_ ) :: obj1
! real( dfp ) :: xij( 3, 1 )
! call random_number( xij )
! call initiate( obj=obj1, NSD=3, xij )
! call display( obj1, "obj1 : " )
!```

INTERFACE
MODULE PURE SUBROUTINE refPoint_Initiate( obj, NSD, XiJ )
  CLASS( ReferencePoint_ ), INTENT( INOUT ) :: obj
    !! The instance
  INTEGER( I4B ), INTENT( IN ) :: NSD
    !! Spatial dimension of the problem
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
    !! Coords of element
END SUBROUTINE refPoint_Initiate
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE refPoint_Initiate
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                    ReferencePoint@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 March 2021
! summary: This routine constructs an instance of Point reference element
!
!# Introduction
! This routine constructs an instance of [[ReferencePoint_]] element
!
! - `XiJ` denotes the nodal coordinate, if it is not present than
! RESHAPE( [0.0, 0.0, 0.0], [3, 1] ) is used.
!
!@note
! 	Note that SIZE(XiJ,1) should be equal to 3, i.e., x,y,z coord.
!@endnote
!
!### Usage
!
!```fortran
! type( ReferencePoint_ ) :: obj
! obj = ReferencePoint(nsd=3)
! call display( obj, 'obj : ' )
!```

INTERFACE
MODULE PURE FUNCTION refPoint_Constructor1(NSD, XiJ) RESULT( obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferencePoint_ ) :: obj
END FUNCTION refPoint_Constructor1
END INTERFACE

INTERFACE ReferencePoint
  MODULE PROCEDURE refPoint_Constructor1
END INTERFACE ReferencePoint

PUBLIC :: ReferencePoint

!----------------------------------------------------------------------------
!                                             ReferencePoint_Pointer@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 March 2021
! summary: Constructs a pointer to an instance of point reference element
!
!# Introduction
! This routine constructs a pointer to an instance of [[ReferencePoint_]]
! element, and returns this pointer
!
! - `XiJ` denotes the nodal coordinate, if it is not present than RESHAPE( [
! 0.0_DFP, 0.0_DFP, 0.0_DFP], [3, 1] ) is used.
!
!@note
! 	Note that SIZE(XiJ,1) should be equal to 3, i.e., x,y,z coord.
!@endnote
!
!### Usage
!
!```fortran
! class( ReferenceElement_ ), pointer :: obj => NULL()
! obj => ReferencePoint_Pointer( nsd = 3 )
! call display( obj, "obj : ")
!```

INTERFACE
MODULE PURE FUNCTION refPoint_Constructor_1(NSD, XiJ) RESULT( obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferencePoint_ ), POINTER :: obj
END FUNCTION refPoint_Constructor_1
END INTERFACE

INTERFACE ReferencePoint_Pointer
  MODULE PROCEDURE refPoint_Constructor_1
END INTERFACE ReferencePoint_Pointer

PUBLIC :: ReferencePoint_Pointer

!----------------------------------------------------------------------------
!                                                   LagrangeElement@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: This function returns lagrange element on point
!
!# Introduction
! Returns lagrange point element.
!
!### Usage
!
!```fortran
! type( ReferencePoint_ ) :: obj1, obj3
! real( dfp ) :: xij( 3, 1 )
! call random_number( xij )
! call initiate( obj=obj1, NSD=3, XiJ=xij )
! call display( obj1, "obj1 : " )
! call obj1%LagrangeElement( Order=2, HighOrderobj=obj3 ) <---
! call display( obj3, "Second Order Lagrange Element : ")
!```

INTERFACE
MODULE PURE SUBROUTINE highOrderElement_Point( RefElem, Order, obj, ipType )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
    !! Linear line element
  INTEGER( I4B ), INTENT( IN ) :: Order
    !! Order or generated element
  CLASS( ReferenceElement_ ),  INTENT( INOUT ) ::  obj
    !! High order lagrange line element
  INTEGER( I4B ), INTENT( IN ) :: ipType
END SUBROUTINE highOrderElement_Point
END INTERFACE

PUBLIC :: highOrderElement_Point

!----------------------------------------------------------------------------
!                                                      MeasureSimplex@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 March 2021
! summary: 	This function returns the measure of linear point element
!
!# Introduction
!
! This function returns the measure of point element. Its generic form is
! given by [[ReferenceElement_Method:MeasureSimplex]]
!
!### Usage
!
!```fortran
! type( ReferencePoint_ ) :: obj
! real( dfp ) :: xij( 3, 1 )
! call random_number( xij )
! call initiate( obj=obj, NSD=3, XiJ=xij )
! call display( MeasureSimplex(obj, obj%xij), "Measure :: ")
!```

INTERFACE
MODULE PURE FUNCTION Measure_Simplex_Point( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferencePoint_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Point
END INTERFACE

PUBLIC :: Measure_Simplex_Point

!----------------------------------------------------------------------------
!                                                             Point_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Point_Quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferencePoint_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION Point_Quality
END INTERFACE

PUBLIC :: Point_Quality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ReferencePoint_Method