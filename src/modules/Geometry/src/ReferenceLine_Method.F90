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
PUBLIC :: Initiate
PUBLIC :: ReferenceLine
PUBLIC :: ReferenceLine_Pointer
PUBLIC :: HighOrderElement_Line
PUBLIC :: Measure_Simplex_Line
PUBLIC :: Line_Quality
PUBLIC :: Quality_Line
PUBLIC :: LineName
PUBLIC :: RefLineCoord
PUBLIC :: RefCoord_Line

!----------------------------------------------------------------------------
!                                                                 LineName
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-17
! summary:  Returns the integer name of reference line for given order

INTERFACE LineName
  MODULE PURE FUNCTION LineName1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LineName1
END INTERFACE LineName

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

INTERFACE Initiate
  MODULE PURE SUBROUTINE initiate_ref_Line(obj, nsd, xij, domainName)
    CLASS(ReferenceLine_), INTENT(INOUT) :: obj
    !! The instance
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Spatial dimension of the problem
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    !! Coords of element
    CHARACTER(*), INTENT(IN), OPTIONAL :: domainName
    !! Domain name
    !! UNIT
    !! BIUNIT
    !! GENERAL
  END SUBROUTINE initiate_ref_Line
END INTERFACE Initiate

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

INTERFACE ReferenceLine
  MODULE PURE FUNCTION reference_line(nsd, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    TYPE(ReferenceLine_) :: obj
    CHARACTER(*), INTENT(IN), OPTIONAL :: domainName
    !! Domain name
    !! UNIT
    !! BIUNIT
    !! GENERAL
  END FUNCTION reference_line
END INTERFACE ReferenceLine

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

INTERFACE ReferenceLine_Pointer
  MODULE FUNCTION reference_line_pointer_1(nsd, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CLASS(ReferenceLine_), POINTER :: obj
    CHARACTER(*), INTENT(IN), OPTIONAL :: domainName
    !! Domain name
    !! UNIT
    !! BIUNIT
    !! GENERAL
  END FUNCTION reference_line_pointer_1
END INTERFACE ReferenceLine_Pointer

!----------------------------------------------------------------------------
!                                                   LagrangeElement@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: This function returns lagrange element on line
!
!# Introduction
! Returns lagrange line element of Higher order. By lagrange element we means
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
! call obj1%HighOrderElement( order=2, HighOrderobj=obj3 ) <---
! call display( obj3, "Second order Lagrange Element : ")
!```

INTERFACE
  MODULE SUBROUTINE HighOrderElement_Line(refelem, order, obj, &
    & ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! Linear line element
    INTEGER(I4B), INTENT(IN) :: order
    !! order or generated element
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    !! High order lagrange line element
    INTEGER(I4B), INTENT(IN) :: ipType
  END SUBROUTINE HighOrderElement_Line
END INTERFACE

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
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans
  END FUNCTION Measure_Simplex_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                      line_quality@Methods
!----------------------------------------------------------------------------

INTERFACE Quality_Line
  MODULE FUNCTION Line_Quality(refelem, xij, measure) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: Ans
  END FUNCTION Line_Quality
END INTERFACE Quality_Line

!----------------------------------------------------------------------------
!                                                           RefLineCoord
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-03
! summary:  Returns the coordinate of reference triangle

INTERFACE RefCoord_Line
  MODULE PURE FUNCTION RefLineCoord(refLine) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refLine
    !! "unit"
    !! "biunit"
    REAL(DFP) :: ans(1, 2)
  END FUNCTION RefLineCoord
END INTERFACE RefCoord_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ReferenceLine_Method
