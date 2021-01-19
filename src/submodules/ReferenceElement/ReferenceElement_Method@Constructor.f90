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
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE display_ref_topo
  INTEGER( I4B ) :: I
  CHARACTER( LEN = 120 ) :: fmt

  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = Stdout
  END IF
  !
  IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
    WRITE( I, "(A)" ) TRIM( Msg )
  END IF
  !
  CALL Blanklines( NOL = 1, UnitNo = I )
  WRITE( I, "(A)" ) "ELEMENT TYPE :: " // TRIM( ElementName( Obj % Name ) )
  WRITE( I, "(A)" ) "XIDIM :: " // TRIM( INT2STR( Obj % XiDimension ) )
  CALL Display( Obj % Nptrs,  "NPTRS")
  CALL Blanklines( NOL = 1, UnitNo = I )
END PROCEDURE display_ref_topo

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE display_ref_elem
  ! Define internal variable
  INTEGER( I4B ) :: I, j
  CHARACTER( LEN = 120 ) :: fmt
  !
  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = Stdout
  END IF
  !
  IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
    WRITE( I, "(A)" ) TRIM( Msg )
  END IF
  !
  CALL Blanklines( NOL = 1, UnitNo = I )
  CALL Display( "Element Type :: "// trim( ElementName( Obj % Name ) ), I )
  CALL Display( Obj%XiDimension, "XiDimension :: ", UnitNo = I )
  CALL Display( Obj%NSD, "NSD :: ", UnitNo = I )
  CALL Display( Obj%Order, "Order :: ", UnitNo = I )

  CALL Blanklines( NOL = 1, UnitNo = I )
  WRITE( I, "(A)" ) "XiDim ---> Entities "
  SELECT CASE( COUNT( Obj % EntityCounts .NE. 0 ) )
  CASE( 0 )
    CALL Display( Obj % EntityCounts( 1 ), " XiDim(0) :: ", UnitNo = I  )
  CASE( 1 )
    CALL Display( Obj % EntityCounts( 1 ), " XiDim(0) :: ", UnitNo = I )
  CASE( 2 )
    CALL Display( Obj % EntityCounts( 1 ), " XiDim(0) :: ", UnitNo = I )
    CALL Display( Obj % EntityCounts( 2 ), " XiDim(1) :: ", UnitNo = I )
  CASE( 3 )
    CALL Display( Obj % EntityCounts( 1 ), " XiDim(0) :: ", UnitNo = I )
    CALL Display( Obj % EntityCounts( 2 ), " XiDim(1) :: ", UnitNo = I )
    CALL Display( Obj % EntityCounts( 3 ), " XiDim(2) :: ", UnitNo = I )
  CASE( 4 )
    CALL Display( Obj % EntityCounts( 1 ), " XiDim(0) :: ", UnitNo = I )
    CALL Display( Obj % EntityCounts( 2 ), " XiDim(1) :: ", UnitNo = I )
    CALL Display( Obj % EntityCounts( 3 ), " XiDim(2) :: ", UnitNo = I )
    CALL Display( Obj % EntityCounts( 4 ), " XiDim(3) :: ", UnitNo = I )
  END SELECT

  CALL Blanklines( NOL = 1, UnitNo = I )
  DO j = 1, SIZE( Obj % XiJ, 2 )
    CALL Display( Obj % XiJ( :, j), &
      & "Node(" // trim( str( j, .true. ) ) // " )" )
  END DO

  CALL Blanklines( NOL = 1, UnitNo = I )
  DO j = 1, SIZE( Obj % Topology )
    CALL Display( Obj % Topology( j ), &
      & "Obj % Topology( " // TRIM( INT2STR( j ) ) // " )", I )
  END DO
END PROCEDURE display_ref_elem

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

END SUBMODULE Constructor