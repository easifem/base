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
! date: 1 March 2021
! summary: This submodule contains constructor methods of [[ReferenceElement_]]

SUBMODULE( ReferenceElement_Method ) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         ReferenceTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_topology
  Obj % Nptrs = Nptrs
  Obj % Name = Name
  Obj % XiDimension = XiDimension( Name )
END PROCEDURE reference_topology

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocatedata_ref_topology
  IF( ALLOCATED( Obj % Nptrs ) ) DEALLOCATE( Obj % Nptrs )
  Obj % Name = -1
  Obj % XiDimension = -1
END PROCEDURE deallocatedata_ref_topology

!----------------------------------------------------------------------------
!                                                                        NNE
!----------------------------------------------------------------------------

MODULE PROCEDURE tNodes_RefTopo
  IF( ALLOCATED( Obj % Nptrs ) ) THEN
    Ans = SIZE( Obj % Nptrs )
  ELSE
    Ans = 0
  END IF
END PROCEDURE tNodes_RefTopo

!----------------------------------------------------------------------------
!                                                                        NNE
!----------------------------------------------------------------------------

MODULE PROCEDURE tNodes_RefElem
  IF( ALLOCATED( Obj % XiJ ) ) THEN
    Ans = SIZE( Obj % XiJ, 2 )
  ELSE
    Ans = 0
  END IF
END PROCEDURE tNodes_RefElem

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocatedata_ref_elem
  IF( ALLOCATED( Obj % XiJ ) ) DEALLOCATE( Obj % XiJ )
  Obj % EntityCounts = 0
  IF( ALLOCATED( Obj % Topology ) ) DEALLOCATE( Obj % Topology )
  Obj % XiDimension = -1
  Obj % Name = -1
  Obj % NSD = -1
END PROCEDURE deallocatedata_ref_elem

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE init_refelem
  IF( ALLOCATED( AnotherObj % XiJ ) ) Obj % XiJ = AnotherObj % XiJ
  Obj % EntityCounts = AnotherObj % EntityCounts
  Obj % XiDimension = AnotherObj % XiDimension
  Obj % NSD = AnotherObj % NSD
  Obj % Order = AnotherObj % Order
  Obj % Name = AnotherObj % Name
  IF( ALLOCATED( AnotherObj % Topology ) ) THEN
    Obj % Topology = AnotherObj % Topology
  END IF
END PROCEDURE init_refelem

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_ref_Line
  IF( PRESENT( XiJ ) ) THEN
    Obj % XiJ = XiJ
  ELSE
    Obj % XiJ = RESHAPE( [-1, 0, 0, 1, 0, 0], [3, 2] )
  END IF
  Obj % EntityCounts = [2, 1, 0, 0]
  Obj % XiDimension = 1
  Obj % Order = 1
  Obj % NSD = NSD
  Obj % Name = Line2
  ALLOCATE( Obj % Topology( 3 ) )
  Obj % Topology( 1 ) = ReferenceTopology( [1], Point )
  Obj % Topology( 2 ) = ReferenceTopology( [2], Point )
  Obj % Topology( 3 ) = ReferenceTopology( [1, 2], Line2 )
END PROCEDURE initiate_ref_Line

!----------------------------------------------------------------------------
!                                                              ReferenceLine
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Line
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END PROCEDURE reference_Line

!----------------------------------------------------------------------------
!                                                     ReferenceLine_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Line_Pointer
  ALLOCATE( Obj )
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END PROCEDURE reference_Line_Pointer

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_ref_Triangle
  IF( PRESENT( XiJ ) ) THEN
    Obj % XiJ = XiJ
  ELSE
    Obj % XiJ =  RESHAPE( [0, 0, 0, 1, 0, 0, 0, 1, 0], [3, 3] )
  END IF
  Obj % EntityCounts = [3, 3, 1, 0]
  Obj % XiDimension = 2
  Obj % Name = Triangle3
  Obj % Order = 1
  Obj % NSD = NSD
  ALLOCATE( Obj % Topology( 7 ) )
  Obj % Topology( 1 ) = ReferenceTopology( [1], Point )
  Obj % Topology( 2 ) = ReferenceTopology( [2], Point )
  Obj % Topology( 3 ) = ReferenceTopology( [3], Point )
  Obj % Topology( 4 ) = ReferenceTopology( [1, 2], Line2 )
  Obj % Topology( 5 ) = ReferenceTopology( [2, 3], Line2 )
  Obj % Topology( 6 ) = ReferenceTopology( [3, 1], Line2 )
  Obj % Topology( 7 ) = ReferenceTopology( [1, 2, 3], Triangle3 )
END PROCEDURE initiate_ref_Triangle

!----------------------------------------------------------------------------
!                                                          ReferenceTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Triangle
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END PROCEDURE reference_Triangle

!----------------------------------------------------------------------------
!                                                          ReferenceTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Triangle_Pointer
  ALLOCATE( Obj )
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END PROCEDURE reference_Triangle_Pointer

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_ref_Quadrangle
  IF( PRESENT( XiJ ) ) THEN
    Obj % XiJ = XiJ
  ELSE
    Obj % XiJ =  RESHAPE( [-1, -1, 0, 1, -1, 0, 1, 1, 0, -1, 1, 0], [3, 4] )
  END IF

  Obj % EntityCounts = [4, 4, 1, 0]
  Obj % XiDimension = 2
  Obj % Name = Quadrangle4
  Obj % Order = 1
  Obj % NSD = NSD
  ALLOCATE( Obj % Topology( 9 ) )

  Obj % Topology( 1 ) = ReferenceTopology( [1], Point )
  Obj % Topology( 2 ) = ReferenceTopology( [2], Point )
  Obj % Topology( 3 ) = ReferenceTopology( [3], Point )
  Obj % Topology( 4 ) = ReferenceTopology( [4], Point )

  Obj % Topology( 5 ) = ReferenceTopology( [1, 2], Line2 )
  Obj % Topology( 6 ) = ReferenceTopology( [2, 3], Line2 )
  Obj % Topology( 7 ) = ReferenceTopology( [3, 4], Line2 )
  Obj % Topology( 8 ) = ReferenceTopology( [4, 1], Line2 )

  Obj % Topology( 9 ) = ReferenceTopology( [1, 2, 3, 4], Quadrangle4 )
END PROCEDURE Initiate_ref_Quadrangle

!----------------------------------------------------------------------------
!                                                       ReferenceQuadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Quadrangle
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END PROCEDURE reference_Quadrangle

!----------------------------------------------------------------------------
!                                               ReferenceQuadrangle_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Quadrangle_Pointer
  ALLOCATE( Obj )
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END PROCEDURE reference_Quadrangle_Pointer


!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_ref_Tetrahedron
END PROCEDURE Initiate_ref_Tetrahedron

!----------------------------------------------------------------------------
!                                                      ReferenceTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Tetrahedron
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END PROCEDURE reference_Tetrahedron

!----------------------------------------------------------------------------
!                                              ReferenceTetrahedron_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Tetrahedron_Pointer
  ALLOCATE( Obj )
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END PROCEDURE reference_Tetrahedron_Pointer

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_ref_Hexahedron
END PROCEDURE Initiate_ref_Hexahedron

!----------------------------------------------------------------------------
!                                                      ReferenceHexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Hexahedron
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END PROCEDURE reference_Hexahedron

!----------------------------------------------------------------------------
!                                                      ReferenceHexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Hexahedron_Pointer
  ALLOCATE( Obj )
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END PROCEDURE reference_Hexahedron_Pointer

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_ref_Pyramid
END PROCEDURE Initiate_ref_Pyramid

!----------------------------------------------------------------------------
!                                                      ReferencePyramid
!----------------------------------------------------------------------------
MODULE PROCEDURE reference_Pyramid
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END PROCEDURE reference_Pyramid

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_ref_Prism
END PROCEDURE Initiate_ref_Prism

!----------------------------------------------------------------------------
!                                                      ReferencePrism
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Prism
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END PROCEDURE reference_Prism

!----------------------------------------------------------------------------
!                                                     ReferencePrism_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Prism_Pointer
  ALLOCATE( Obj )
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( Obj, NSD, XiJ )
  ELSE
    CALL Initiate( Obj, NSD )
  END IF
END PROCEDURE reference_Prism_Pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Constructor
