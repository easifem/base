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
! date: 5 March 2021
! summary: This submodule contains method for [[ReferenceLine_]]

MODULE ReferenceLine_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                            Initiate@Line
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: This subroutine constructs an instance of line reference element
!
!### Introduction
! This routine constructs an instance of [[ReferenceLine_]] element of order equal to 1.
!
! - `XiJ` denotes the nodal coordinate, if it is not present than RESHAPE( [-1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP], [3, 2] ) is used.
!
!@note
! 	Note that SIZE(XiJ,1) should be equal to 3, i.e., x,y,z coord. Also note that this routine creats a linear element.
!@endnote
!
!### Usage
!
!```fortran
! type( ReferenceLine_ ) :: obj1
! real( dfp ) :: xij( 3, 2 )
! call random_number( xij )
! call initiate( Obj=obj1, NSD=3, xij )
! call display( obj1, "obj1 : " )
!```

INTERFACE
MODULE PURE SUBROUTINE initiate_ref_Line( Obj, NSD, XiJ )
  CLASS( ReferenceLine_ ), INTENT( INOUT ) :: Obj
    !! The instance
  INTEGER( I4B ), INTENT( IN ) :: NSD
    !! Spatial dimension of the problem
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
    !! Coords of element
END SUBROUTINE initiate_ref_Line
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Line
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                       ReferenceLine@Line
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 March 2021
! summary: This routine constructs an instance of line reference element
!
!### Introduction
! This routine constructs an instance of [[ReferenceLine_]] element of order equal to 1.
!
! - `XiJ` denotes the nodal coordinate, if it is not present than RESHAPE( [-1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP], [3, 2] ) is used.
!
!@note
! 	Note that SIZE(XiJ,1) should be equal to 3, i.e., x,y,z coord. Also note that this routine creats a linear element.
!@endnote
!
!### Usage
!
!```fortran
! type( ReferenceLine_ ) :: obj
! obj = ReferenceLine(nsd=3)
! call display( obj, 'obj : ' )
!```

INTERFACE
MODULE PURE FUNCTION reference_line(NSD, XiJ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferenceLine_ ) :: Obj
END FUNCTION reference_line
END INTERFACE

INTERFACE ReferenceLine
  MODULE PROCEDURE reference_line
END INTERFACE ReferenceLine

PUBLIC :: ReferenceLine

!----------------------------------------------------------------------------
!                                                ReferenceLine_Pointer@Line
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 March 2021
! summary: This routine constructs an instance of line reference element
!
!### Introduction
! This routine constructs an instance of [[ReferenceLine_]] element of order equal to 1.
!
! - `XiJ` denotes the nodal coordinate, if it is not present than RESHAPE( [-1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP], [3, 2] ) is used.
!
!@note
! 	Note that SIZE(XiJ,1) should be equal to 3, i.e., x,y,z coord. Also note that this routine creats a linear element.
!@endnote
!
!### Usage
!
!```fortran
! class( ReferenceElement_ ), pointer :: obj => NULL()
! obj => ReferenceLine_Pointer( nsd = 3 )
! call display( obj, "obj : ")
!```

INTERFACE
MODULE PURE FUNCTION reference_line_pointer_1(NSD, XiJ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferenceLine_ ), POINTER :: Obj
END FUNCTION reference_line_pointer_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ReferenceLine_Pointer@Line
!----------------------------------------------------------------------------

INTERFACE ReferenceLine_Pointer
  MODULE PROCEDURE reference_line_Pointer_1
END INTERFACE ReferenceLine_Pointer

PUBLIC :: ReferenceLine_Pointer

!----------------------------------------------------------------------------
!                                                       LagrangeElement@Line
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: This function returns lagrange element on line
!
!### Introduction
! Returns lagrange line element of higher order. By lagrange element we means
! standard finite elements, with equi-distance lagrange interpolation points.
!
!### Usage
!
!```fortran
! type( ReferenceLine_ ) :: obj1, obj3
! real( dfp ) :: xij( 3, 2 )
! call random_number( xij )
! call initiate( Obj=obj1, NSD=3, XiJ=xij )
! call display( obj1, "obj1 : " )
! call obj1%LagrangeElement( Order=2, HighOrderObj=obj3 ) <---
! call display( obj3, "Second Order Lagrange Element : ")
!```

INTERFACE
MODULE PURE SUBROUTINE LagrangeElement_Line( RefElem, Order, Obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
    !! Linear line element
  INTEGER( I4B ), INTENT( IN ) :: Order
    !! Order or generated element
  CLASS( ReferenceElement_ ),  INTENT( INOUT ) ::  Obj
    !! High order lagrange line element
END SUBROUTINE LagrangeElement_Line
END INTERFACE

PUBLIC :: LagrangeElement_Line

!----------------------------------------------------------------------------
!                                                      MeasureSimplex@Line
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 March 2021
! summary: 	This function returns the measure of linear line element
!
!### Introduction
!
! This function returns the measure of linear line element. Its generic form is given by [[ReferenceElement_Method:MeasureSimplex]]
!
!
!### Usage
!
!```fortran
! type( ReferenceLine_ ) :: obj
! real( dfp ) :: xij( 3, 2 )
! call random_number( xij )
! call initiate( Obj=obj, NSD=3, XiJ=xij )
! call display( MeasureSimplex(obj, obj%xij), "Measure :: ")
!```

INTERFACE
MODULE PURE FUNCTION Measure_Simplex_Line( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferenceLine_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Line
END INTERFACE

PUBLIC :: Measure_Simplex_Line

!----------------------------------------------------------------------------
!                                                             line_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Line_Quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferenceLine_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION Line_Quality
END INTERFACE

PUBLIC :: Line_Quality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ReferenceLine_Method