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
! date: 2 March 2021
! summary:         This submodule contains methods for [[ReferenceLine_]]

SUBMODULE(ReferenceLine_Method) Methods

USE GlobalData, ONLY: Line, Line1, Line2, Line3, Line4, Line5, &
                      Line6, Point1, Equidistance

USE ReallocateUtility, ONLY: Reallocate

USE ReferenceElement_Method, ONLY: ReferenceTopology, &
                                   ElementType, DEALLOCATE

USE StringUtility, ONLY: UpperCase

USE ApproxUtility, ONLY: OPERATOR(.approxeq.)

USE String_Class, ONLY: String

USE LineInterpolationUtility, ONLY: InterpolationPoint_Line

USE Display_Method, ONLY: ToString

USE InputUtility, ONLY: Input

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                          ElementName_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementName_Line
SELECT CASE (elemType)
CASE (Point1)
  ans = "Point1"
CASE (Line2)
  ans = "Line2"
CASE (Line3)
  ans = "Line3"
CASE (Line4)
  ans = "Line4"
CASE (Line5)
  ans = "Line5"
CASE (Line6)
  ans = "Line6"
CASE DEFAULT
  ans = "NONE"
END SELECT
END PROCEDURE ElementName_Line

!----------------------------------------------------------------------------
!                                                     FacetTopology_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetTopology_Line
ans(1)%nptrs = nptrs([1])
ans(1)%xiDimension = 0
ans(1)%name = Point1

ans(2)%nptrs = nptrs([2])
ans(2)%xiDimension = 0
ans(2)%name = Point1
END PROCEDURE FacetTopology_Line

!----------------------------------------------------------------------------
!                                                         TotalEntities_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE TotalEntities_Line
ans(1) = TotalNodesInElement_Line(elemType)
ans(2) = 1
ans(3:4) = 0
END PROCEDURE TotalEntities_Line

!----------------------------------------------------------------------------
!                                                   TotalNodesInElement_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE TotalNodesInElement_Line
SELECT CASE (elemType)
CASE (Line1)
  ans = 1
CASE (Line2)
  ans = 2
CASE (Line3)
  ans = 3
CASE (Line4)
  ans = 4
CASE (Line5)
  ans = 5
CASE (Line6)
  ans = 6
CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE TotalNodesInElement_Line

!----------------------------------------------------------------------------
!                                                         ElementOrder_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementOrder_Line
SELECT CASE (elemType)
CASE (Line2)
  ans = 1
CASE (Line3)
  ans = 2
CASE (Line4)
  ans = 3
CASE (Line5)
  ans = 4
CASE (Line6)
  ans = 5
CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE ElementOrder_Line

!----------------------------------------------------------------------------
!                                                          ElementType_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementType_Line
SELECT CASE (elemName)
CASE ("Line1", "Point", "Point1")
  ans = Point1
CASE ("Line2", "Line")
  ans = Line2
CASE ("Line3")
  ans = Line3
CASE ("Line4")
  ans = Line4
CASE ("Line5")
  ans = Line5
CASE ("Line6")
  ans = Line6
CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE ElementType_Line

!----------------------------------------------------------------------------
!                                                        FacetElements_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetElements_Line1
INTEGER(I4B) :: ii
INTEGER(I4B) :: nptrs(1)

DO ii = 1, 2
  nptrs = refelem%topology(ii)%nptrs
  CALL Reallocate(ans(ii)%xij, 3_I4B, 1)
  ans(ii)%xij(1:3, 1) = DEFAULT_REF_LINE_COORD(1:3, ii)
  ans(ii)%entityCounts = [1, 0, 0, 0]
  ans(ii)%xiDimension = 0
  ans(ii)%name = Point1
  ans(ii)%interpolationPointType = refelem%interpolationPointType
  ans(ii)%order = 0
  ans(ii)%nsd = refelem%nsd
  ALLOCATE (ans(ii)%topology(1))
  ans(ii)%topology(1) = Referencetopology(nptrs=nptrs, name=Point1)
  ans(ii)%highOrderElement => NULL()
END DO
END PROCEDURE FacetElements_Line1

!----------------------------------------------------------------------------
!                                                        FacetElements_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetElements_Line2
INTEGER(I4B), PARAMETER :: nptrs(2) = [1, 2]
INTEGER(I4B) :: ii

DO ii = 1, 2
  ans(ii)%xij = RESHAPE(DEFAULT_REF_LINE_COORD(1:3, ii), [3, 1])
  ans(ii)%entityCounts = [1, 0, 0, 0]
  ans(ii)%xiDimension = 0
  ans(ii)%name = Point1
  ans(ii)%interpolationPointType = Equidistance
  ans(ii)%order = 0
  ans(ii)%nsd = nsd
  ALLOCATE (ans(ii)%topology(1))
  ans(ii)%topology(1) = Referencetopology(nptrs=nptrs(ii:ii), name=Point1)
  ans(ii)%highOrderElement => NULL()
END DO
END PROCEDURE FacetElements_Line2

!----------------------------------------------------------------------------
!                                                                  LineName
!----------------------------------------------------------------------------

MODULE PROCEDURE LineName1
SELECT CASE (order)
CASE (1)
  ans = Line2
CASE (2)
  ans = Line3
CASE (3)
  ans = Line4
CASE (4)
  ans = Line5
CASE (5)
  ans = Line6
CASE (6:)
  ans = Line6 * 100 + order - 5
END SELECT
END PROCEDURE LineName1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_ref_Line
REAL(DFP) :: unit_xij(1, 2), biunit_xij(1, 2)

CALL DEALLOCATE (obj)

unit_xij = RefCoord_Line("UNIT")
biunit_xij = RefCoord_Line("BIUNIT")

IF (PRESENT(xij)) THEN
  obj%xij = xij(1:1, 1:2)
  IF (ALL(obj%xij(1:1, 1:2) .approxeq.unit_xij)) THEN
    obj%domainName = "UNIT"
  ELSE IF (ALL(obj%xij(1:1, 1:2) .approxeq.biunit_xij)) THEN
    obj%domainName = "BIUNIT"
  ELSE
    obj%domainName = "GENERAL"
  END IF
ELSE
  IF (PRESENT(domainName)) THEN
    obj%domainName = UpperCase(domainName)
    IF (obj%domainName .EQ. "UNIT" .OR. obj%domainName .EQ. "BIUNIT") THEN
      obj%xij = RefCoord_Line(obj%domainName)
    END IF
  ELSE
    obj%domainName = "BIUNIT"
    obj%xij = RefCoord_Line(obj%domainName)
  END IF
END IF

obj%entityCounts = [2, 1, 0, 0]
obj%xiDimension = 1
obj%order = 1
obj%nsd = nsd
obj%name = Line2
ALLOCATE (obj%topology(3))
obj%topology(1) = ReferenceTopology([1], Point1)
obj%topology(2) = ReferenceTopology([2], Point1)
obj%topology(3) = ReferenceTopology([1, 2], Line2)
obj%highorderElement => highorderElement_Line
END PROCEDURE Initiate_ref_Line

!----------------------------------------------------------------------------
!                                                              ReferenceLine
!----------------------------------------------------------------------------

MODULE PROCEDURE Reference_Line
CALL Initiate_ref_line(obj=obj, nsd=nsd, xij=xij, domainName=domainName)
END PROCEDURE Reference_Line

!----------------------------------------------------------------------------
!                                                     ReferenceLine_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Reference_Line_Pointer_1
ALLOCATE (obj)
CALL Initiate_ref_line(obj=obj, nsd=nsd, xij=xij, domainName=domainName)
END PROCEDURE Reference_Line_Pointer_1

!----------------------------------------------------------------------------
!                                                           LagrangeElement
!----------------------------------------------------------------------------

MODULE PROCEDURE HighorderElement_Line
INTEGER(I4B) :: nns, i

obj%xij = InterpolationPoint_Line( &
          xij=refelem%xij, &
          order=order, &
          ipType=ipType, &
          layout="VEFC")

obj%domainName = refelem%domainName
obj%nsd = refelem%nsd
nns = SIZE(obj%xij, 2)
obj%entityCounts = [nns, 1, 0, 0]
obj%xiDimension = 1
obj%order = order
obj%name = ElementType("Line"//ToString(nns))
ALLOCATE (obj%topology(nns + 1))
DO CONCURRENT(i=1:nns)
  obj%topology(i) = ReferenceTopology([i], Point1)
END DO
obj%topology(nns + 1) = ReferenceTopology([(i, i=1, nns)], obj%name)
END PROCEDURE HighorderElement_Line

!----------------------------------------------------------------------------
!                                                              MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Line
SELECT CASE (SIZE(xij, 1))
CASE (1)
  Ans = ABS(xij(1, 1) - xij(1, 2))
CASE (2)
  Ans = SQRT((xij(1, 1) - xij(1, 2))**2 &
    & + (xij(2, 1) - xij(2, 2))**2)
CASE default
  Ans = SQRT((xij(1, 1) - xij(1, 2))**2 &
    & + (xij(2, 1) - xij(2, 2))**2 &
    & + (xij(3, 1) - xij(3, 2))**2)
END SELECT
END PROCEDURE Measure_Simplex_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Line_quality
ans = 0.0_DFP
END PROCEDURE Line_quality

!----------------------------------------------------------------------------
!                                                              RefLineCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE RefLineCoord
CHARACTER(1) :: astr

astr = refline(1:1)
SELECT CASE (astr)
CASE ("U", "u")
  ans(1, 1:2) = [0.0_DFP, 1.0_DFP]
CASE ("B", "b")
  ans(1, 1:2) = [-1.0_DFP, 1.0_DFP]
END SELECT
END PROCEDURE RefLineCoord

!----------------------------------------------------------------------------
!                                                 GetEdgeElemType_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE GetEdgeConnectivity_Line
IF (PRESENT(nrow)) nrow = 1
IF (PRESENT(ncol)) ncol = 2
con(1, 1) = 1
con(1, 2) = 2
END PROCEDURE GetEdgeConnectivity_Line

!----------------------------------------------------------------------------
!                                                 GetFaceElemType_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceElemType_Line1
INTEGER(I4B) :: elemType0
elemType0 = Input(default=Line, option=elemType)
IF (PRESENT(faceElemType)) faceElemType(1:2) = Point1
IF (PRESENT(tFaceNodes)) tFaceNodes(1:2) = 1_I4B
END PROCEDURE GetFaceElemType_Line1

!----------------------------------------------------------------------------
!                                                 GetFaceElemType_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceElemType_Line2
faceElemType = Point1
tFaceNodes = 1_I4B
END PROCEDURE GetFaceElemType_Line2

!----------------------------------------------------------------------------
!                                               GetFaceConnectivity_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceConnectivity_Line
IF (PRESENT(nrow)) nrow = 1
IF (PRESENT(ncol)) ncol = 2
con(1, 1) = 1
con(1, 2) = 2
END PROCEDURE GetFaceConnectivity_Line

END SUBMODULE Methods
