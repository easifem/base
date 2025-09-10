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
USE BaseType, ONLY: ReferenceTopology_, &
                    ReferenceElement_, &
                    ReferenceLine_

USE GlobalData, ONLY: I4B, DFP, LGT

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
PUBLIC :: DEFAULT_Ref_LINE_COORD
PUBLIC :: FacetElements_Line
PUBLIC :: ElementType_Line
PUBLIC :: ElementOrder_Line
PUBLIC :: TotalNodesInElement_Line
PUBLIC :: TotalEntities_Line
PUBLIC :: FacetTopology_Line
PUBLIC :: ElementName_Line
PUBLIC :: MaxOrder_Line
PUBLIC :: GetFaceElemType_Line
PUBLIC :: GetEdgeConnectivity_Line
PUBLIC :: GetFaceConnectivity_Line

#ifdef MAX_LINE_ORDER
INTEGER(I4B), PARAMETER :: MaxOrder_Line = MAX_LINE_ORDER
#else
INTEGER(I4B), PARAMETER :: MaxOrder_Line = 5_I4B
#endif

#ifdef REF_LINE_IS_UNIT
REAL(DFP), PARAMETER :: DEFAULT_Ref_LINE_COORD(3, 2) = &
                        RESHAPE([0, 0, 0, 1, 0, 0], [3, 2])
#else
REAL(DFP), PARAMETER :: DEFAULT_Ref_LINE_COORD(3, 2) = &
                        RESHAPE([-1, 0, 0, 1, 0, 0], [3, 2])
#endif

!----------------------------------------------------------------------------
!                                                              ElementName
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-25
! summary: Returns element name in character from element number/type

INTERFACE
  MODULE PURE FUNCTION ElementName_Line(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION ElementName_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                         FacetTopology_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-22
! summary: Returns the facet topology of the given element type

INTERFACE
  MODULE PURE SUBROUTINE FacetTopology_Line(elemType, nptrs, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nptrs(:)
    TYPE(ReferenceTopology_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetTopology_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                    TotalEntities_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns total entities

INTERFACE
  MODULE PURE FUNCTION TotalEntities_Line(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans(4)
  END FUNCTION TotalEntities_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                   TotalNodesInElement_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns total nodes in element

INTERFACE
  MODULE PURE FUNCTION TotalNodesInElement_Line(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION TotalNodesInElement_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ElementOrder_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns order of element

INTERFACE
  MODULE PURE FUNCTION ElementOrder_Line(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION ElementOrder_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ElementType_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns ElementType for line from char

INTERFACE
  MODULE PURE FUNCTION ElementType_Line(elemName) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: elemName
    INTEGER(I4B) :: ans
  END FUNCTION ElementType_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                       FacetElements_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-21
! summary:  Get FacetElements

INTERFACE FacetElements_Line
  MODULE SUBROUTINE FacetElements_Line1(refelem, ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Line1
END INTERFACE FacetElements_Line

!----------------------------------------------------------------------------
!                                                       FacetElements_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-21
! summary:  Get FacetElements

INTERFACE FacetElements_Line
  MODULE SUBROUTINE FacetElements_Line2(elemType, nsd, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nsd
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Line2
END INTERFACE FacetElements_Line

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
  MODULE PURE SUBROUTINE Initiate_Ref_Line(obj, nsd, xij, domainName)
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
  END SUBROUTINE Initiate_Ref_Line
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
  MODULE PURE FUNCTION Reference_Line(nsd, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    TYPE(ReferenceLine_) :: obj
    CHARACTER(*), INTENT(IN), OPTIONAL :: domainName
    !! Domain name
    !! UNIT
    !! BIUNIT
    !! GENERAL
  END FUNCTION Reference_Line
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
! class( ReferenceElement_ ), Pointer :: obj => NULL()
! obj => ReferenceLine_Pointer( nsd = 3 )
! call display( obj, "obj : ")
!```

INTERFACE ReferenceLine_Pointer
  MODULE FUNCTION Reference_Line_Pointer_1(nsd, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CLASS(ReferenceLine_), POINTER :: obj
    CHARACTER(*), INTENT(IN), OPTIONAL :: domainName
    !! Domain name
    !! UNIT
    !! BIUNIT
    !! GENERAL
  END FUNCTION Reference_Line_Pointer_1
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
  MODULE SUBROUTINE HighOrderElement_Line(refelem, order, obj, ipType)
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
!                                                        GetEdgeConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-19
! summary: Get the face connectivity of Line
!
!# Introduction
!
! This routine calls [[GetEdgeConnectivity_Line]] with opt=2

INTERFACE
  MODULE PURE SUBROUTINE GetFaceConnectivity_Line(con, opt, order, nrow, ncol)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! Connectivity
    !! The columns represents the Face number
    !! The row represents a Face
    !! con should be allocated by the user
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! This option is ignored now
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order of element
    !! Currently any order is valid
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! Number of rows written in con
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! Numbers of cols written in con
  END SUBROUTINE GetFaceConnectivity_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetEdgeElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-19
! summary:  Returns the element type of each face

INTERFACE
  MODULE PURE SUBROUTINE GetEdgeConnectivity_Line(con, opt, order, nrow, ncol)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! Connectivity
    !! The columns represents the edge number
    !! The row represents a edge
    !! con should be allocated by the user
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! [1,2], [1,3], [2,3]. This is DEFAULT
    !! If opt =2, then edge connectivity for Lagrangian approximation
    !! [1,2], [2,3], [3,1]
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order of element
    !! Currently order is used only when opt=2
    !! Currently any order is valid
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! Number of rows written in con
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! Numbers of cols written in con
  END SUBROUTINE GetEdgeConnectivity_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetFaceElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-19
! summary:  Returns the element type of each face

INTERFACE GetFaceElemType_Line
  MODULE PURE SUBROUTINE GetFaceElemType_Line1(elemType, faceElemType, opt, &
                                               tFaceNodes)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! name of element
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: faceElemType(:)
    !! Element names of faces
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: tFaceNodes(:)
    !! Total number of nodes in each face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt = 2, then edge connectivity for Lagrangian approximation
    !! opt = 1 is default
  END SUBROUTINE GetFaceElemType_Line1
END INTERFACE GetFaceElemType_Line

!----------------------------------------------------------------------------
!                                           GetFaceElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-19
! summary:  Returns the element type of each face

INTERFACE GetFaceElemType_Line
  MODULE PURE SUBROUTINE GetFaceElemType_Line2(elemType, localFaceNumber, &
                                               faceElemType, opt, tFaceNodes)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! name of element
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number
    INTEGER(I4B), INTENT(INOUT) :: faceElemType
    !! Element names of faces
    INTEGER(I4B), INTENT(INOUT) :: tFaceNodes
    !! Total number of nodes in each face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt = 2, then edge connectivity for Lagrangian approximation
    !! opt = 1 is default
  END SUBROUTINE GetFaceElemType_Line2
END INTERFACE GetFaceElemType_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ReferenceLine_Method
