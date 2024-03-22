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
USE BaseMethod
IMPLICIT NONE
CONTAINS

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

MODULE PROCEDURE initiate_ref_Line
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
obj%topology(1) = ReferenceTopology([1], Point)
obj%topology(2) = ReferenceTopology([2], Point)
obj%topology(3) = ReferenceTopology([1, 2], Line2)
obj%highorderElement => highorderElement_Line
END PROCEDURE initiate_ref_Line

!----------------------------------------------------------------------------
!                                                              ReferenceLine
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Line
CALL Initiate(obj=obj, nsd=nsd, xij=xij, domainName=domainName)
END PROCEDURE reference_Line

!----------------------------------------------------------------------------
!                                                     ReferenceLine_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Line_Pointer_1
ALLOCATE (obj)
CALL Initiate(obj=obj, nsd=nsd, xij=xij, domainName=domainName)
END PROCEDURE reference_Line_Pointer_1

!----------------------------------------------------------------------------
!                                                           LagrangeElement
!----------------------------------------------------------------------------

MODULE PROCEDURE highorderElement_Line
INTEGER(I4B) :: nns, i
obj%xij = InterpolationPoint_Line( &
  & xij=refelem%xij, &
  & order=order, &
  & ipType=ipType, &
  & layout="VEFC")
obj%domainName = refelem%domainName
obj%nsd = refelem%nsd
nns = SIZE(obj%xij, 2)
obj%entityCounts = [nns, 1, 0, 0]
obj%xiDimension = 1
obj%order = order
obj%name = ElementType("Line"//TRIM(INT2STR(nns)))
ALLOCATE (obj%topology(nns + 1))
DO CONCURRENT(i=1:nns)
  obj%topology(i) = ReferenceTopology([i], Point)
END DO
obj%topology(nns + 1) = ReferenceTopology([(i, i=1, nns)], obj%name)
END PROCEDURE highorderElement_Line

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

END SUBMODULE Methods
