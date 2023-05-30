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
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_ref_Line
!
CALL Reallocate(obj%xij, 3, 2)
obj%xij = InterpolationPoint_Line(xij=xij, order=1, ipType=Equidistance, &
& layout="VEFC")
!
obj%EntityCounts = [2, 1, 0, 0]
obj%XiDimension = 1
obj%order = 1
obj%nsd = nsd
obj%Name = Line2
!
IF (ALLOCATED(obj%Topology)) DEALLOCATE (obj%Topology)
ALLOCATE (obj%Topology(3))
obj%Topology(1) = ReferenceTopology([1], Point)
obj%Topology(2) = ReferenceTopology([2], Point)
obj%Topology(3) = ReferenceTopology([1, 2], Line2)
!
obj%highorderElement => highorderElement_Line
!
END PROCEDURE initiate_ref_Line

!----------------------------------------------------------------------------
!                                                              ReferenceLine
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Line
!
IF (PRESENT(xij)) THEN
  CALL Initiate(obj, nsd, xij)
ELSE
  CALL Initiate(obj, nsd)
END IF
!
END PROCEDURE reference_Line

!----------------------------------------------------------------------------
!                                                     ReferenceLine_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Line_Pointer_1
!
ALLOCATE (obj)
!
IF (PRESENT(xij)) THEN
  CALL Initiate(obj, nsd, xij)
ELSE
  CALL Initiate(obj, nsd)
END IF
!
END PROCEDURE reference_Line_Pointer_1

!----------------------------------------------------------------------------
!                                                           LagrangeElement
!----------------------------------------------------------------------------

MODULE PROCEDURE highorderElement_Line
!
! Define internal variables
!
INTEGER(I4B) :: nns, i
!
obj%xij = InterpolationPoint_Line(xij=refelem%xij, order=order, &
  & ipType=ipType, layout="VEFC")
obj%nsd = refelem%nsd
nns = SIZE(obj%xij, 2)
obj%EntityCounts = [nns, 1, 0, 0]
obj%XiDimension = 1
obj%order = order
obj%Name = ElementType("Line"//TRIM(INT2STR(nns)))
!
ALLOCATE (obj%Topology(nns + 1))
DO CONCURRENT(i=1:nns)
  obj%Topology(i) = ReferenceTopology([i], Point)
END DO
!
obj%Topology(nns + 1) = ReferenceTopology([(i, i=1, nns)], obj%Name)
!
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
