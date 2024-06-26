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
! summary:         This submodule contains methods for [[ReferencePoint_]]

SUBMODULE(ReferencePoint_Method) Methods
USE ReallocateUtility
USE ReferenceElement_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE refPoint_Initiate
CALL Reallocate(obj%XiJ, 3, 1)
IF (PRESENT(XiJ)) THEN
  obj%XiJ = XiJ
END IF
obj%EntityCounts = [1, 0, 0, 0]
obj%XiDimension = 0
obj%Order = 0
obj%NSD = NSD
obj%Name = Point1
IF (ALLOCATED(obj%Topology)) DEALLOCATE (obj%Topology)
ALLOCATE (obj%Topology(1))
obj%Topology(1) = ReferenceTopology([1], Point)
obj%highOrderElement => HighOrderElement_Point
END PROCEDURE refPoint_Initiate

!----------------------------------------------------------------------------
!                                                              ReferencePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE refPoint_Constructor1
CALL refPoint_Initiate(obj, NSD, XiJ)
END PROCEDURE refPoint_Constructor1

!----------------------------------------------------------------------------
!                                                      ReferencePoint_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE refPoint_Constructor_1
ALLOCATE (obj)
CALL refpoint_Initiate(obj, NSD, XiJ)
END PROCEDURE refPoint_Constructor_1

!----------------------------------------------------------------------------
!                                                            LagrangeElement
!----------------------------------------------------------------------------

MODULE PROCEDURE HighOrderElement_Point
CALL Initiate(obj=obj, anotherobj=refelem)
END PROCEDURE HighOrderElement_Point

!----------------------------------------------------------------------------
!                                                              MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Point
ans = 0.0_DFP
END PROCEDURE Measure_Simplex_Point

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Point_Quality
ans = 0.0_DFP
END PROCEDURE Point_Quality

!----------------------------------------------------------------------------
!                                                         TotalNodesInElement
!----------------------------------------------------------------------------

MODULE PROCEDURE TotalNodesInElement_Point
SELECT CASE (elemType)
CASE (Point)
  ans = 1
CASE default
  ans = 0
END SELECT
END PROCEDURE TotalNodesInElement_Point

END SUBMODULE Methods
