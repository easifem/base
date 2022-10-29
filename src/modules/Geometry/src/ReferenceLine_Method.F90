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
! date: 5 March 2021
! summary: This submodule contains method for [[ReferenceLine_]]

MODULE ReferenceLine_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         1 March 2021
! summary: This subroutine constructs an instance of line reference element
!
!# Introduction
! This routine constructs an instance of [[ReferenceLine_]]
! element of order equal to 1.
!
! - `xij` denotes the nodal coordinate, if it is not present than RESHAPE(
! [-1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP], [3, 2] ) is used.
!
!@note
! Note that SIZE(xij,1) should be equal to 3, i.e., x,y,z coord. Also note
! that this routine creats a linear element.
!@endnote
!
!### Usage
!
!```fortran
! type( ReferenceLine_ ) :: obj1
! real( dfp ) :: xij( 3, 2 )
! call random_number( xij )
! call initiate( obj=obj1, nsd=3, xij )
! call display( obj1, "obj1 : " )
!```

INTERFACE
  MODULE SUBROUTINE initiate_ref_Line(obj, nsd, xij)
    CLASS(ReferenceLine_), INTENT(INOUT) :: obj
    !! The instance
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Spatial dimension of the problem
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    !! Coords of element
  END SUBROUTINE initiate_ref_Line
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Line
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                     ReferenceLine@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         3 March 2021
! summary: This routine constructs an instance of line reference element
!
!# Introduction
! This routine constructs an instance of [[ReferenceLine_]] element of order
! equal to 1.
!
! - `xij` denotes the nodal coordinate, if it is not present than RESHAPE(
! [-1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP], [3, 2] ) is used.
!
!@note
! Note that SIZE(xij,1) should be equal to 3, i.e., x,y,z coord. Also note
! that this routine creats a linear element.
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
  MODULE FUNCTION reference_line(nsd, xij) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    TYPE(ReferenceLine_) :: obj
  END FUNCTION reference_line
END INTERFACE

INTERFACE ReferenceLine
  MODULE PROCEDURE reference_line
END INTERFACE ReferenceLine

PUBLIC :: ReferenceLine

!----------------------------------------------------------------------------
!                                              ReferenceLine_Pointer@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         3 March 2021
! summary: This routine constructs an instance of line reference element
!
!# Introduction
! This routine constructs an instance of [[ReferenceLine_]] element of order
! equal to 1.
!
! - `xij` denotes the nodal coordinate, if it is not present than RESHAPE(
! [-1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP], [3, 2] ) is used.
!
!@note
! Note that SIZE(xij,1) should be equal to 3, i.e., x,y,z coord. Also note
! that this routine creats a linear element.
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
  MODULE FUNCTION reference_line_pointer_1(nsd, xij) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CLASS(ReferenceLine_), POINTER :: obj
  END FUNCTION reference_line_pointer_1
END INTERFACE

INTERFACE ReferenceLine_Pointer
  MODULE PROCEDURE reference_line_Pointer_1
END INTERFACE ReferenceLine_Pointer

PUBLIC :: ReferenceLine_Pointer

!----------------------------------------------------------------------------
!                                                   LagrangeElement@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: This function returns lagrange element on line
!
!# Introduction
! Returns lagrange line element of higher order. By lagrange element we means
! standard finite elements, with equi-distance lagrange interpolation points.
!
!### Usage
!
!```fortran
! type( ReferenceLine_ ) :: obj1, obj3
! real( dfp ) :: xij( 3, 2 )
! call random_number( xij )
! call initiate( obj=obj1, nsd=3, xij=xij )
! call display( obj1, "obj1 : " )
! call obj1%highorderElement( order=2, Highorderobj=obj3 ) <---
! call display( obj3, "Second order Lagrange Element : ")
!```

INTERFACE
  MODULE SUBROUTINE highorderElement_Line(refelem, order, obj, &
    & ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! Linear line element
    INTEGER(I4B), INTENT(IN) :: order
    !! order or generated element
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    !! High order lagrange line element
    INTEGER(I4B), INTENT(IN) :: ipType
  END SUBROUTINE highorderElement_Line
END INTERFACE

PUBLIC :: highorderElement_Line

!----------------------------------------------------------------------------
!                                                    MeasureSimplex@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 March 2021
! summary: This function returns the measure of linear line element
!
!# Introduction
!
! This function returns the measure of linear line element. Its generic form
! is given by [[ReferenceElement_Method:MeasureSimplex]]
!
!
!### Usage
!
!```fortran
! type( ReferenceLine_ ) :: obj
! real( dfp ) :: xij( 3, 2 )
! call random_number( xij )
! call initiate( obj=obj, nsd=3, xij=xij )
! call display( MeasureSimplex(obj, obj%xij), "Measure :: ")
!```

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex_Line(refelem, xij) RESULT(Ans)
    CLASS(ReferenceLine_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans
  END FUNCTION Measure_Simplex_Line
END INTERFACE

PUBLIC :: Measure_Simplex_Line

!----------------------------------------------------------------------------
!                                                      line_quality@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION Line_Quality(refelem, xij, measure) RESULT(Ans)
    CLASS(ReferenceLine_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: Ans
  END FUNCTION Line_Quality
END INTERFACE

PUBLIC :: Line_Quality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ReferenceLine_Method
