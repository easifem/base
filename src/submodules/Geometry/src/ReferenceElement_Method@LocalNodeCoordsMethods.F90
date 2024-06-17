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
! date: 1 March 2021
! summary: This sumodule contains method for geometry

SUBMODULE(ReferenceElement_Method) LocalNodeCoordsMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            Localnodecoord
!----------------------------------------------------------------------------

MODULE PROCEDURE Local_nodecoord
IF (ALLOCATED(nodecoord)) DEALLOCATE (nodecoord)

SELECT CASE (ElemType)
CASE (Point1)
  ALLOCATE (nodecoord(3, 1))
  nodecoord = 0.0_DFP

CASE (Line2)
  ALLOCATE (nodecoord(3, 2))
  nodecoord = 0.0_DFP
  nodecoord(1, :) = [-1.0_DFP, 1.0_DFP]

CASE (Line3)
  ALLOCATE (nodecoord(3, 3))
  nodecoord = 0.0_DFP
  nodecoord(1, :) = [-1.0_DFP, 1.0_DFP, 0.0_DFP]

CASE (Line4)
  ALLOCATE (nodecoord(3, 4))
  nodecoord = 0.0_DFP
  nodecoord(1, :) = [ &
     -1.0_DFP, 1.0_DFP, &
     -0.333333333333333_DFP, &
      0.333333333333333_DFP]

CASE (Line5)
  ALLOCATE (nodecoord(3, 5))
  nodecoord = 0.0_DFP
  nodecoord(1, :) = [ &
     -1.0_DFP, 1.0_DFP, &
     -0.5_DFP, 0.0_DFP, &
     0.5_DFP]

CASE (Line6)
  ALLOCATE (nodecoord(3, 6))
  nodecoord = 0.0_DFP
  nodecoord(1, :) = [ &
     -1.0_DFP, 1.0_DFP, &
     -0.666666666666666_DFP, &
     -0.333333333333333_DFP, &
     0.666666666666666_DFP, &
     0.333333333333333_DFP]

CASE (Triangle3)
  ALLOCATE (nodecoord(3, 3))
  nodecoord = 0.0_DFP
  nodecoord(1, :) = [0.0_DFP, 1.0_DFP, 0.0_DFP]
  nodecoord(2, :) = [0.0_DFP, 0.0_DFP, 1.0_DFP]

CASE (Triangle6)
  ALLOCATE (nodecoord(3, 6))
  nodecoord = 0.0_DFP
  nodecoord(1, :) = [0.0_DFP, 1.0_DFP, 0.0_DFP, &
    & 0.5_DFP, 0.5_DFP, 0.0_DFP]
  nodecoord(2, :) = [0.0_DFP, 0.0_DFP, 1.0_DFP, &
    & 0.0_DFP, 0.5_DFP, 0.5_DFP]

CASE (Triangle9)
  ALLOCATE (nodecoord(3, 9))
  nodecoord = 0.0_DFP
  nodecoord(1, :) = [ &
     0.0_DFP, &
     1.0_DFP, &
     0.0_DFP, &
     0.33333333333333333333_DFP, &
     0.66666666666666666667_DFP, &
     0.66666666666666666667_DFP, &
     0.33333333333333333333_DFP, &
     0.0_DFP, &
     0.0_DFP]

  nodecoord(2, :) = [ &
     0.0_DFP, &
     0.0_DFP, &
     1.0_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.33333333333333333333_DFP, &
     0.66666666666666666667_DFP, &
     0.66666666666666666667_DFP, &
     0.33333333333333333333_DFP]

CASE (Triangle10)
  ALLOCATE (nodecoord(3, 10))
  nodecoord = 0.0_DFP
  nodecoord(1, :) = [ &
     0.0_DFP, &
     1.0_DFP, &
     0.0_DFP, &
     0.33333333333333333333_DFP, &
     0.66666666666666666667_DFP, &
     0.66666666666666666667_DFP, &
     0.33333333333333333333_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.33333333333333333333_DFP]

  nodecoord(2, :) = [ &
     0.0_DFP, &
     0.0_DFP, &
     1.0_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.33333333333333333333_DFP, &
     0.66666666666666666667_DFP, &
     0.66666666666666666667_DFP, &
     0.33333333333333333333_DFP, &
     0.33333333333333333333_DFP]

CASE (Triangle12)
  ! incomplete triangle; all nodes on boundary
  ALLOCATE (nodecoord(3, 12))
  nodecoord = 0.0_DFP
  nodecoord(1, :) = [ &
     0.0_DFP, &
     1.0_DFP, &
     0.0_DFP, &
     0.25_DFP, &
     0.5_DFP, &
     0.75_DFP, &
     0.75_DFP, &
     0.5_DFP, &
     0.25_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.0_DFP]

  nodecoord(2, :) = [ &
     0.0_DFP, &
     0.0_DFP, &
     1.0_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.25_DFP, &
     0.5_DFP, &
     0.75_DFP, &
     0.75_DFP, &
     0.5_DFP, &
     0.25_DFP]

CASE (Triangle15a)
  ! complete triangle; 12 nodes on boundary and
  ! 3 nodes are inside
  ALLOCATE (nodecoord(3, 15))
  nodecoord = 0.0_DFP
  nodecoord(1, :) = [ &
     0.0_DFP, &
     1.0_DFP, &
     0.0_DFP, &
     0.25_DFP, &
     0.5_DFP, &
     0.75_DFP, &
     0.75_DFP, &
     0.5_DFP, &
     0.25_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.25_DFP, &
     0.5_DFP, &
     0.25_DFP]

  nodecoord(2, :) = [ &
     0.0_DFP, &
     0.0_DFP, &
     1.0_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.25_DFP, &
     0.5_DFP, &
     0.75_DFP, &
     0.75_DFP, &
     0.5_DFP, &
     0.25_DFP, &
     0.25_DFP, &
     0.25_DFP, &
     0.5_DFP]

CASE (Triangle15b)
  ! Incomplete triangle
  ALLOCATE (nodecoord(3, 15))
  nodecoord = 0.0_DFP

  nodecoord(1, :) = [ &
     0.0_DFP, &
     1.0_DFP, &
     0.0_DFP, &
     0.2_DFP, &
     0.4_DFP, &
     0.6_DFP, &
     0.8_DFP, &
     0.8_DFP, &
     0.6_DFP, &
     0.4_DFP, &
     0.2_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.0_DFP]

  nodecoord(2, :) = [ &
     0.0_DFP, &
     0.0_DFP, &
     1.0_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.2_DFP, &
     0.4_DFP, &
     0.6_DFP, &
     0.8_DFP, &
     0.8_DFP, &
     0.6_DFP, &
     0.4_DFP, &
     0.2_DFP]

CASE (Triangle21)
  ALLOCATE (nodecoord(3, 21))
  nodecoord = 0.0_DFP

  nodecoord(1, :) = [ &
     0.0_DFP, &
     1.0_DFP, &
     0.0_DFP, &
     0.2_DFP, &
     0.4_DFP, &
     0.6_DFP, &
     0.8_DFP, &
     0.8_DFP, &
     0.6_DFP, &
     0.4_DFP, &
     0.2_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.2_DFP, &
     0.6_DFP, &
     0.2_DFP, &
     0.4_DFP, &
     0.4_DFP, &
     0.2_DFP]

  nodecoord(2, :) = [ &
     0.0_DFP, &
     0.0_DFP, &
     1.0_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.0_DFP, &
     0.2_DFP, &
     0.4_DFP, &
     0.6_DFP, &
     0.8_DFP, &
     0.8_DFP, &
     0.6_DFP, &
     0.4_DFP, &
     0.2_DFP, &
     0.2_DFP, &
     0.2_DFP, &
     0.6_DFP, &
     0.2_DFP, &
     0.4_DFP, &
     0.4_DFP]

CASE (Quadrangle4)
  ALLOCATE (nodecoord(3, 4))
  nodecoord = 0.0_DFP
  nodecoord(1, :) = [-1.0_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP]
  nodecoord(2, :) = [-1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP]

CASE (Quadrangle8)
  nodecoord = RESHAPE([ &
     -1.0_DFP, -1.0_DFP, 0.0_DFP, &
     1.0_DFP, -1.0_DFP, 0.0_DFP, &
     1.0_DFP, 1.0_DFP, 0.0_DFP, &
     -1.0_DFP, 1.0_DFP, 0.0_DFP, &
     0.0_DFP, -1.0_DFP, 0.0_DFP, &
     1.0_DFP, 0.0_DFP, 0.0_DFP, &
     0.0_DFP, 1.0_DFP, 0.0_DFP, &
     -1.0_DFP, 0.0_DFP, 0.0_DFP], [3, 8])

CASE (Quadrangle9)
  nodecoord = RESHAPE([ &
     -1.0_DFP, -1.0_DFP, 0.0_DFP, &
     1.0_DFP, -1.0_DFP, 0.0_DFP, &
     1.0_DFP, 1.0_DFP, 0.0_DFP, &
     -1.0_DFP, 1.0_DFP, 0.0_DFP, &
     0.0_DFP, -1.0_DFP, 0.0_DFP, &
     1.0_DFP, 0.0_DFP, 0.0_DFP, &
     0.0_DFP, 1.0_DFP, 0.0_DFP, &
     -1.0_DFP, 0.0_DFP, 0.0_DFP, &
     0.0_DFP, 0.0_DFP, 0.0_DFP], [3, 9])

CASE (Quadrangle16)
  nodecoord = RESHAPE([ &
     -1.0_DFP, -1.0_DFP, 0.0_DFP, &
     1.0_DFP, -1.0_DFP, 0.0_DFP, &
     1.0_DFP, 1.0_DFP, 0.0_DFP, &
     -1.0_DFP, 1.0_DFP, 0.0_DFP, &
     -1.0_dfp/3.0_dfp, -1.0_DFP, 0.0_DFP, &
     1.0_dfp/3.0_dfp, -1.0_DFP, 0.0_DFP, &
     1.0_dfp, -1.0_DFP/3.0_dfp, 0.0_DFP, &
     1.0_dfp, 1.0_DFP/3.0_dfp, 0.0_DFP, &
     1.0_dfp/3.0_dfp, 1.0_DFP, 0.0_DFP, &
     -1.0_dfp/3.0_dfp, 1.0_DFP, 0.0_DFP, &
     -1.0_dfp, 1.0_DFP/3.0_dfp, 0.0_DFP, &
     -1.0_dfp, -1.0_DFP/3.0_dfp, 0.0_DFP, &
     -1.0_dfp/3.0_Dfp, -1.0_DFP/3.0_dfp, 0.0_DFP, &
     1.0_dfp/3.0_Dfp, -1.0_DFP/3.0_dfp, 0.0_DFP, &
     1.0_dfp/3.0_Dfp, 1.0_DFP/3.0_dfp, 0.0_DFP, &
     -1.0_dfp/3.0_Dfp, 1.0_DFP/3.0_dfp, 0.0_DFP], &
    [3, 16])

CASE (Hexahedron8)
  nodecoord = RESHAPE([ &
   -1.0_DFP, -1.0_DFP, -1.0_DFP, &
   1.0_DFP, -1.0_DFP, -1.0_DFP, &
   1.0_DFP, 1.0_DFP, -1.0_DFP, &
   -1.0_DFP, 1.0_DFP, -1.0_DFP, &
   -1.0_DFP, -1.0_DFP, 1.0_DFP, &
   1.0_DFP, -1.0_DFP, 1.0_DFP, &
   1.0_DFP, 1.0_DFP, 1.0_DFP, &
   -1.0_DFP, 1.0_DFP, 1.0_DFP], [3, 8])

CASE (Hexahedron20)
  nodecoord = RESHAPE([ &
   -1.0_DFP, -1.0_DFP, -1.0_DFP, &
   1.0_DFP, -1.0_DFP, -1.0_DFP, &
   1.0_DFP, 1.0_DFP, -1.0_DFP, &
   -1.0_DFP, 1.0_DFP, -1.0_DFP, &
   -1.0_DFP, -1.0_DFP, 1.0_DFP, &
   1.0_DFP, -1.0_DFP, 1.0_DFP, &
   1.0_DFP, 1.0_DFP, 1.0_DFP, &
   -1.0_DFP, 1.0_DFP, 1.0_DFP, &
   0.0_DFP, -1.0_DFP, -1.0_DFP, &
   -1.0_DFP, 0.0_DFP, -1.0_DFP, &
   -1.0_DFP, -1.0_DFP, 0.0_DFP, &
   1.0_DFP, 0.0_DFP, -1.0_DFP, &
   1.0_DFP, -1.0_DFP, 0.0_DFP, &
   0.0_DFP, 1.0_DFP, -1.0_DFP, &
   1.0_DFP, 1.0_DFP, 0.0_DFP, &
   -1.0_DFP, 1.0_DFP, 0.0_DFP, &
   0.0_DFP, -1.0_DFP, 1.0_DFP, &
   -1.0_DFP, 0.0_DFP, 1.0_DFP, &
   1.0_DFP, 0.0_DFP, 1.0_DFP, &
   0.0_DFP, 1.0_DFP, 1.0_DFP], [3, 20])

CASE (Hexahedron27)
  nodecoord = RESHAPE([ &
   -1.0_DFP, -1.0_DFP, -1.0_DFP, &
   1.0_DFP, -1.0_DFP, -1.0_DFP, &
   1.0_DFP, 1.0_DFP, -1.0_DFP, &
   -1.0_DFP, 1.0_DFP, -1.0_DFP, &
   -1.0_DFP, -1.0_DFP, 1.0_DFP, &
   1.0_DFP, -1.0_DFP, 1.0_DFP, &
   1.0_DFP, 1.0_DFP, 1.0_DFP, &
   -1.0_DFP, 1.0_DFP, 1.0_DFP, &
   0.0_DFP, -1.0_DFP, -1.0_DFP, &
   -1.0_DFP, 0.0_DFP, -1.0_DFP, &
   -1.0_DFP, -1.0_DFP, 0.0_DFP, &
   1.0_DFP, 0.0_DFP, -1.0_DFP, &
   1.0_DFP, -1.0_DFP, 0.0_DFP, &
   0.0_DFP, 1.0_DFP, -1.0_DFP, &
   1.0_DFP, 1.0_DFP, 0.0_DFP, &
   -1.0_DFP, 1.0_DFP, 0.0_DFP, &
   0.0_DFP, -1.0_DFP, 1.0_DFP, &
   -1.0_DFP, 0.0_DFP, 1.0_DFP, &
   1.0_DFP, 0.0_DFP, 1.0_DFP, &
   0.0_DFP, 1.0_DFP, 1.0_DFP, &
   0.0_DFP, 0.0_DFP, -1.0_DFP, &
   0.0_DFP, -1.0_DFP, 0.0_DFP, &
   -1.0_DFP, 0.0_DFP, 0.0_DFP, &
   1.0_DFP, 0.0_DFP, 0.0_DFP, &
   0.0_DFP, 1.0_DFP, 0.0_DFP, &
   0.0_DFP, 0.0_DFP, 1.0_DFP, &
   0.0_DFP, 0.0_DFP, 0.0_DFP], [3, 27])
END SELECT

END PROCEDURE Local_nodecoord

!-----------------------------------------------------------------------------
!                                                                FacetMatrix
!-----------------------------------------------------------------------------

MODULE PROCEDURE Local_nodecoord_refelem
IF (ALLOCATED(refelem%xij)) THEN
  nodecoord = refelem%xij
ELSE
  ALLOCATE (nodecoord(0, 0))
END IF
END PROCEDURE Local_nodecoord_refelem

END SUBMODULE LocalNodeCoordsMethods
