SUBMODULE( ReferenceElement_Method ) Geometry
USE BaseMethod
IMPLICIT NONE

CONTAINS

!-----------------------------------------------------------------------------
!                                                                  ElementName
!-----------------------------------------------------------------------------

MODULE PROCEDURE Element_Name
  ! Define internal variables
  CHARACTER( LEN = 50 ) :: Str1

  SELECT CASE( ElemType )
  CASE( Point1 )
  Str1 = "Point1"

  CASE( Line2 )
  Str1 = "Line2"

  CASE( Triangle3 )
  Str1 = "Triangle3"

  CASE( Quadrangle4 )
  Str1 = "Quadrangle4"

  CASE( Tetrahedron4 )
  Str1 = "Tetrahedron4"

  CASE( Hexahedron8 )
  Str1 = "Hexahedron8"

  CASE( Prism6 )
  Str1 = "Prism6"

  CASE( Pyramid5 )
  Str1 = "Pyramid5"

  !! Order=2 elements
  CASE( Line3 )
  Str1 = "Line3"

  CASE( Triangle6 )
  Str1 = "Triangle6"

  CASE( Quadrangle9 )
  Str1 = "Quadrangle9"

  CASE( Quadrangle8 )
  Str1 = "Quadrangle8"

  CASE( Tetrahedron10 )
  Str1 = "Tetrahedron10"

  CASE( Hexahedron20 )
  Str1 = "Hexahedron20"

  CASE( Hexahedron27 )
  Str1 = "Hexahedron27"

  CASE( Prism15 )
  Str1 = "Prism15"

  CASE( Prism18 )
  Str1 = "Prism18"

  CASE( Pyramid13 )
  Str1 = "Pyramid13"

  CASE( Pyramid14 )
  Str1 = "Pyramid14"

  CASE( Triangle9 )
  Str1 = "Triangle9"

  CASE( Triangle10 )
  Str1 = "Triangle10"

  CASE( Triangle12 )
  Str1 = "Triangle12"

  CASE( Triangle15a )
  Str1 = "Triangle15a"

  CASE( Triangle15b )
  Str1 = "Triangle15b"

  CASE( Triangle21 )
  Str1 = "Triangle21"

  CASE( Line4 )
  Str1 = "Line4"
  CASE( Line5 )
  Str1 = "Line5"
  CASE( Line6 )
  Str1 = "Line6"
  CASE( Tetrahedron20 )
  Str1 = "Tetrahedron20"
  CASE( Tetrahedron35 )
  Str1 = "Tetrahedron35"
  CASE( Tetrahedron56 )
  Str1 = "Tetrahedron56"
  CASE( Hexahedron64 )
  Str1 = "Hexahedron64"
  CASE( Hexahedron125 )
  Str1 = "Hexahedron125"
  END SELECT

  Ans = TRIM( Str1 )

END PROCEDURE Element_Name

!-----------------------------------------------------------------------------
!                                                                   ElementType
!-----------------------------------------------------------------------------

MODULE PROCEDURE Element_Type

  SELECT CASE( TRIM( ElemName ) )

    CASE( "Line0" )
    Ans = 0
    CASE( "Line1" )
    Ans = Point
    CASE( "Line2" )
    Ans = Line2
    CASE( "Triangle3" )
    Ans = Triangle3
    CASE( "Quadrangle4" )
    Ans = Quadrangle4
    CASE( "Tetrahedron4" )
    Ans = Tetrahedron4
    CASE( "Hexahedron8" )
    Ans = Hexahedron8
    CASE( "Prism6" )
    Ans = Prism6
    CASE( "Pyramid5" )
    Ans = Pyramid5
    CASE( "Line3" )
    Ans = Line3
    CASE( "Triangle6" )
    Ans = Triangle6
    CASE( "Quadrangle9" )
    Ans = Quadrangle9
    CASE( "Tetrahedron10" )
    Ans = Tetrahedron10
    CASE( "Hexahedron27" )
    Ans = Hexahedron27
    CASE( "Prism18" )
    Ans = Prism18
    CASE( "Pyramid14" )
    Ans = Pyramid14
    CASE( "Point1" )
    Ans = Point1
    CASE( "Quadrangle8" )
    Ans = Quadrangle8
    CASE( "Hexahedron20" )
    Ans = Hexahedron20
    CASE( "Prism15" )
    Ans = Prism15
    CASE( "Pyramid13" )
    Ans = Pyramid13
    CASE( "Triangle9" )
    Ans = Triangle9
    CASE( "Triangle10" )
    Ans = Triangle10
    CASE( "Triangle12" )
    Ans = Triangle12
    CASE( "Triangle15a" )
    Ans = Triangle15a
    CASE( "Triangle15b" )
    Ans = Triangle15b
    CASE( "Triangle21" )
    Ans = Triangle21
    CASE( "Line4" )
    Ans = Line4
    CASE( "Line5" )
    Ans = Line5
    CASE( "Line6" )
    Ans = Line6
    CASE( "Tetrahedron20" )
    Ans = Tetrahedron20
    CASE( "Tetrahedron35" )
    Ans = Tetrahedron35
    CASE( "Tetrahedron56" )
    Ans = Tetrahedron56
    CASE( "Hexahedron64" )
    Ans = Hexahedron64
    CASE( "Hexahedron125" )
    Ans = Hexahedron125
  END SELECT

END PROCEDURE Element_Type

!-----------------------------------------------------------------------------
!                                                            TotalNodesInElement
!-----------------------------------------------------------------------------

MODULE PROCEDURE Total_Nodes_In_Element

  SELECT CASE( ElemType )

  CASE( Line2 )
  Ans = 2
  CASE( Triangle3 )
  Ans = 3
  CASE( Quadrangle4 )
  Ans = 4
  CASE( Tetrahedron4 )
  Ans = 4
  CASE( Hexahedron8 )
  Ans = 8
  CASE( Prism6 )
  Ans = 6
  CASE( Pyramid5 )
  Ans = 5
  CASE( Line3 )
  Ans = 3
  CASE( Triangle6 )
  Ans = 6
  CASE( Quadrangle9 )
  Ans = 9
  CASE( Tetrahedron10 )
  Ans = 10
  CASE( Hexahedron27 )
  Ans = 27
  CASE( Prism18 )
  Ans = 18
  CASE( Pyramid14 )
  Ans = 14
  CASE( Point1 )
  Ans = 1
  CASE( Quadrangle8 )
  Ans = 8
  CASE( Hexahedron20 )
  Ans = 20
  CASE( Prism15 )
  Ans = 15
  CASE( Pyramid13 )
  Ans = 13
  CASE( Triangle9 )
  Ans = 9
  CASE( Triangle10 )
  Ans = 10
  CASE( Triangle12 )
  Ans = 12
  CASE( Triangle15a )
  Ans = 15
  CASE( Triangle15b )
  Ans = 15
  CASE( Triangle21 )
  Ans = 21
  CASE( Line4 )
  Ans = 4
  CASE( Line5 )
  Ans = 5
  CASE( Line6 )
  Ans = 6
  CASE( Tetrahedron20 )
  Ans = 20
  CASE( Tetrahedron35 )
  Ans = 35
  CASE( Tetrahedron56 )
  Ans = 56
  CASE( Hexahedron64 )
  Ans = 64
  CASE( Hexahedron125 )
  Ans = 125
  END SELECT

END PROCEDURE Total_Nodes_In_Element

!-----------------------------------------------------------------------------
!                                                                  ElementOrder
!-----------------------------------------------------------------------------

MODULE PROCEDURE Element_Order

  SELECT CASE( ElemType )

  CASE( Line2 )
  Ans = 1
  CASE( Triangle3 )
  Ans = 1
  CASE( Quadrangle4 )
  Ans = 1
  CASE( Tetrahedron4 )
  Ans = 1
  CASE( Hexahedron8 )
  Ans = 1
  CASE( Prism6 )
  Ans = 1
  CASE( Pyramid5 )
  Ans = 1
  CASE( Line3 )
  Ans = 2
  CASE( Triangle6 )
  Ans = 2
  CASE( Quadrangle9 )
  Ans = 2
  CASE( Tetrahedron10 )
  Ans = 2
  CASE( Hexahedron27 )
  Ans = 2
  CASE( Prism18 )
  Ans = 2
  CASE( Pyramid14 )
  Ans = 2
  CASE( Point1 )
  Ans = 0
  CASE( Quadrangle8 )
  Ans = 2
  CASE( Hexahedron20 )
  Ans = 2
  CASE( Prism15 )
  Ans = 2
  CASE( Pyramid13 )
  Ans = 2
  CASE( Triangle9 )
  Ans = 3
  CASE( Triangle10 )
  Ans = 3
  CASE( Triangle12 )
  Ans = 4
  CASE( Triangle15a )
  Ans = 4
  CASE( Triangle15b )
  Ans = 5
  CASE( Triangle21 )
  Ans = 5
  CASE( Line4 )
  Ans = 3
  CASE( Line5 )
  Ans = 4
  CASE( Line6 )
  Ans = 5
  CASE( Tetrahedron20 )
  Ans = 3
  CASE( Tetrahedron35 )
  Ans = 4
  CASE( Tetrahedron56 )
  Ans = 5
  CASE( Hexahedron64 )
  Ans = 3
  CASE( Hexahedron125 )
  Ans = 4
  END SELECT

END PROCEDURE Element_Order

!-----------------------------------------------------------------------------
!                                                                 ElementOrder
!-----------------------------------------------------------------------------
MODULE PROCEDURE Element_Order_RefElem
  Ans = RefElem % Order
END PROCEDURE Element_Order_RefElem

!-----------------------------------------------------------------------------
!                                                                    XiDimension
!-----------------------------------------------------------------------------

MODULE PROCEDURE Elem_XiDimension

  SELECT CASE( ElemType )

  CASE( Tetrahedron4, &
  Hexahedron8, &
  Prism6, &
  Pyramid5, &
  Tetrahedron10, &
  Hexahedron27, &
  Prism18, &
  Pyramid14, &
  Hexahedron20, &
  Prism15, &
  Pyramid13, &
  Tetrahedron20, &
  Tetrahedron35, &
  Tetrahedron56, &
  Hexahedron64, &
  Hexahedron125 )

   Ans = 3

  CASE( Triangle3, &
  Triangle6, &
  Triangle9, &
  Triangle10, &
  Triangle12, &
  Triangle15a, &
  Triangle15b, &
  Triangle21, &
  Quadrangle4, &
  Quadrangle8, &
  Quadrangle9 )

  Ans = 2

  CASE( Line2, &
  Line3, &
  Line4, &
  Line5, &
  Line6 )

  Ans = 1

  CASE DEFAULT

  Ans = 0

  END SELECT

END PROCEDURE Elem_XiDimension

!-----------------------------------------------------------------------------
!                                                                      isVolume
!-----------------------------------------------------------------------------

MODULE PROCEDURE isVolume

  SELECT CASE( ElemType )

  CASE( Tetrahedron4, &
  Hexahedron8, &
  Prism6, &
  Pyramid5, &
  Tetrahedron10, &
  Hexahedron27, &
  Prism18, &
  Pyramid14, &
  Hexahedron20, &
  Prism15, &
  Pyramid13, &
  Tetrahedron20, &
  Tetrahedron35, &
  Tetrahedron56, &
  Hexahedron64, &
  Hexahedron125 )

  Ans = .TRUE.

  CASE DEFAULT

  Ans = .FALSE.

  END SELECT

END PROCEDURE isVolume

!-----------------------------------------------------------------------------
!                                                                     isSurface
!-----------------------------------------------------------------------------

MODULE PROCEDURE isSurface
  SELECT CASE( ElemType )
  CASE( Triangle3, &
  Triangle6, &
  Triangle9, &
  Triangle10, &
  Triangle12, &
  Triangle15a, &
  Triangle15b, &
  Triangle21, &
  Quadrangle4, &
  Quadrangle8, &
  Quadrangle9 )
  Ans = .TRUE.
  CASE DEFAULT
  Ans = .FALSE.
  END SELECT
END PROCEDURE isSurface

!-----------------------------------------------------------------------------
!                                                                        isLine
!-----------------------------------------------------------------------------

MODULE PROCEDURE isLine

  SELECT CASE( ElemType )
  CASE( Line2, &
    & Line3, &
    & Line4, &
    & Line5, &
    & Line6 )
    Ans = .TRUE.
  CASE DEFAULT
    Ans = .FALSE.
  END SELECT

END PROCEDURE isLine

!-----------------------------------------------------------------------------
!                                                                       isPoint
!-----------------------------------------------------------------------------

MODULE PROCEDURE isPoint
  SELECT CASE( ElemType )
  CASE( Point1 )
  Ans = .TRUE.
  CASE DEFAULT
  Ans = .FALSE.
  END SELECT
END PROCEDURE isPoint

!-----------------------------------------------------------------------------
!                                                                    isTriangle
!-----------------------------------------------------------------------------

MODULE PROCEDURE isTriangle
  SELECT CASE( ElemType )
  CASE( Triangle3, Triangle6, &
  & Triangle9, Triangle10, Triangle12, Triangle15a, &
  & Triangle15b, Triangle21 )
  Ans = .TRUE.
  CASE DEFAULT
  Ans = .FALSE.
  END SELECT
END PROCEDURE isTriangle

!-----------------------------------------------------------------------------
!                                                                  isQuadrangle
!-----------------------------------------------------------------------------

MODULE PROCEDURE isQuadrangle
  SELECT CASE( ElemType )
  CASE( Quadrangle4, Quadrangle8, &
  & Quadrangle9 )
  Ans = .TRUE.
  CASE DEFAULT
  Ans = .FALSE.
  END SELECT
END PROCEDURE isQuadrangle

!-----------------------------------------------------------------------------
!                                                                 isTetrahedron
!-----------------------------------------------------------------------------

MODULE PROCEDURE isTetrahedron
  SELECT CASE( ElemType )
  CASE( Tetrahedron4, Tetrahedron10, &
  & Tetrahedron20, Tetrahedron35, Tetrahedron56 )
  Ans = .TRUE.
  CASE DEFAULT
  Ans = .FALSE.
  END SELECT
END PROCEDURE isTetrahedron

!-----------------------------------------------------------------------------
!                                                                  isHexahedron
!-----------------------------------------------------------------------------

MODULE PROCEDURE isHexahedron
  SELECT CASE( ElemType )
  CASE( Hexahedron8, Hexahedron27, &
    & Hexahedron20, Hexahedron64, Hexahedron125 )
  Ans = .TRUE.
  CASE DEFAULT
  Ans = .FALSE.
  END SELECT
END PROCEDURE isHexahedron

!-----------------------------------------------------------------------------
!                                                                       isPrism
!-----------------------------------------------------------------------------

MODULE PROCEDURE isPrism
  SELECT CASE( ElemType )
  CASE( Prism6, Prism18, Prism15 )
  Ans = .TRUE.
  CASE DEFAULT
  Ans = .FALSE.
  END SELECT
END PROCEDURE isPrism

!-----------------------------------------------------------------------------
!                                                                     isPyramid
!-----------------------------------------------------------------------------

MODULE PROCEDURE isPyramid
  SELECT CASE( ElemType )
  CASE( Pyramid5, Pyramid13, Pyramid14 )
  Ans = .TRUE.
  CASE DEFAULT
  Ans = .FALSE.
  END SELECT
END PROCEDURE isPyramid

!-----------------------------------------------------------------------------
!                                                           isSerendipityElement
!-----------------------------------------------------------------------------

MODULE PROCEDURE isSerendipityElement
  SELECT CASE( ElemType )
  CASE( Triangle9, Triangle12, Triangle15b, Quadrangle8 )
    Ans = .TRUE.
  CASE DEFAULT
    Ans = .FALSE.
  END SELECT
END PROCEDURE isSerendipityElement

!-----------------------------------------------------------------------------
!                                                               ElementTopology
!-----------------------------------------------------------------------------

MODULE PROCEDURE Elem_Topology
  SELECT CASE( ElemType )
  CASE( Line2, &
    & Line3, &
    & Line4, &
    & Line5, &
    & Line6 )

    Ans = Line

  CASE( Triangle3, Triangle6, &
    & Triangle9, Triangle10, Triangle12, Triangle15a, &
    & Triangle15b, Triangle21 )
    Ans = Triangle

  CASE( Quadrangle4, Quadrangle8, &
    & Quadrangle9 )
    Ans = Quadrangle

  CASE( Tetrahedron4, Tetrahedron10, &
    & Tetrahedron20, Tetrahedron35, Tetrahedron56 )
    Ans = Tetrahedron

  CASE( Hexahedron8, Hexahedron27, &
    & Hexahedron20, Hexahedron64, Hexahedron125 )
    Ans = Hexahedron

  CASE( Prism6, Prism18, Prism15 )
    Ans = Prism

  CASE( Pyramid5, Pyramid13, Pyramid14 )
    Ans = Pyramid

  END SELECT

END PROCEDURE Elem_Topology

!-----------------------------------------------------------------------------
!                                                                 FacetMatrix
!-----------------------------------------------------------------------------

MODULE PROCEDURE Facet_Matrix_RefElem

  INTEGER( I4B ) :: XiCell, T( 4 ), i, istart, iend, max_nns, nns, tFacet

  T( 1 ) = 0
  DO i = 2, 4
    T( i ) = SUM( RefElem % EntityCounts( 1 : i - 1 ) )
  END DO

  XiCell = RefElem % XiDimension
  SELECT CASE( XiCell )
  CASE( 1 )
    tFacet = 2
    istart = 1
    iend = 2
    max_nns = 2

    ALLOCATE( FM( tFacet, max_nns + 3 ) )
    FM = 0

    DO i = 0, tFacet-1
      FM( i+1, 1 ) = RefElem % Topology( iStart + i ) % Name
      FM( i+1, 2 ) = RefElem % Topology( iStart + i ) % XiDimension
      nns = SIZE( RefElem % Topology( iStart + i ) % Nptrs )
      FM( i+1, 3 ) = nns
      FM( i+1, 4 : (3 + nns) ) = RefElem % Topology( iStart + i ) % Nptrs
    END DO
  CASE( 2, 3 )
    tFacet = RefElem % EntityCounts( XiCell )
    istart = T( XiCell ) + 1
    iend = T( XiCell ) + tFacet
    max_nns = 0
    DO i = istart, iend
      nns = SIZE( RefElem % Topology( i ) % Nptrs )
      IF( max_nns .LT. nns ) max_nns = nns
    END DO

    ALLOCATE( FM( tFacet, max_nns + 3 ) )
    FM = 0

    DO i = 0, tFacet-1
      FM( i+1, 1 ) = RefElem % Topology( iStart + i ) % Name
      FM( i+1, 2 ) = RefElem % Topology( iStart + i ) % XiDimension
      nns = SIZE( RefElem % Topology( iStart + i ) % Nptrs )
      FM( i+1, 3 ) = nns
      FM( i+1, 4 : (3 + nns) ) = RefElem % Topology( iStart + i ) % Nptrs
    END DO
  END SELECT
END PROCEDURE Facet_Matrix_RefElem

!-----------------------------------------------------------------------------
!                                                                   FacetMatrix
!-----------------------------------------------------------------------------

MODULE PROCEDURE Local_NodeCoord
  IF( ALLOCATED( NodeCoord ) ) DEALLOCATE( NodeCoord )

  SELECT CASE( ElemType )
    CASE( Line2 )
      ALLOCATE( NodeCoord( 3, 2 ) )
      NodeCoord = 0.0_DFP
      NodeCoord( 1, : ) = [-1.0_DFP, 1.0_DFP ]

    CASE( Line3 )
      ALLOCATE( NodeCoord( 3, 3 ) )
      NodeCoord = 0.0_DFP
      NodeCoord( 1, : ) = [-1.0_DFP, 1.0_DFP, 0.0_DFP ]

    CASE( Line4 )
      ALLOCATE( NodeCoord( 3, 4 ) )
      NodeCoord = 0.0_DFP
      NodeCoord( 1, : ) = [ &
        & -1.0_DFP, 1.0_DFP, &
        & -0.333333333333333_DFP, &
        &  0.333333333333333_DFP ]

    CASE( Line5 )
      ALLOCATE( NodeCoord( 3, 5 ) )
      NodeCoord = 0.0_DFP
      NodeCoord( 1, : ) = [&
        & -1.0_DFP, 1.0_DFP, &
        & -0.5_DFP, 0.0_DFP, &
        & 0.5_DFP ]

    CASE( Line6 )
      ALLOCATE( NodeCoord( 3, 6 ) )
      NodeCoord = 0.0_DFP
      NodeCoord( 1, : ) = [ &
        & -1.0_DFP, 1.0_DFP, &
        & -0.666666666666666_DFP, &
        & -0.333333333333333_DFP, &
        & 0.666666666666666_DFP, &
        & 0.333333333333333_DFP ]

    CASE( Triangle3 )
      ALLOCATE( NodeCoord( 3, 3 ) )
      NodeCoord = 0.0_DFP
      NodeCoord( 1, : ) = [ 0.0_DFP, 1.0_DFP, 0.0_DFP ]
      NodeCoord( 2, : ) = [ 0.0_DFP, 0.0_DFP, 1.0_DFP ]

    CASE( Triangle6 )
      ALLOCATE( NodeCoord( 3, 6 ) )
      NodeCoord = 0.0_DFP
      NodeCoord( 1, : ) = [ 0.0_DFP, 1.0_DFP, 0.0_DFP, &
        & 0.5_DFP, 0.5_DFP, 0.0_DFP ]
      NodeCoord( 2, : ) = [ 0.0_DFP, 0.0_DFP, 1.0_DFP, &
        & 0.0_DFP, 0.5_DFP, 0.5_DFP ]

    CASE( Triangle9 )
      ALLOCATE( NodeCoord( 3, 9 ) )
      NodeCoord = 0.0_DFP
      NodeCoord( 1, : ) = [ &
        & 0.0_DFP, &
        & 1.0_DFP, &
        & 0.0_DFP, &
        & 0.33333333333333333333_DFP, &
        & 0.66666666666666666667_DFP, &
        & 0.66666666666666666667_DFP, &
        & 0.33333333333333333333_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP ]

      NodeCoord( 2, : ) = [ &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 1.0_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.33333333333333333333_DFP, &
        & 0.66666666666666666667_DFP, &
        & 0.66666666666666666667_DFP, &
        & 0.33333333333333333333_DFP ]

    CASE( Triangle10 )
      ALLOCATE( NodeCoord( 3, 10 ) )
      NodeCoord = 0.0_DFP
      NodeCoord( 1, : ) = [ &
        & 0.0_DFP, &
        & 1.0_DFP, &
        & 0.0_DFP, &
        & 0.33333333333333333333_DFP, &
        & 0.66666666666666666667_DFP, &
        & 0.66666666666666666667_DFP, &
        & 0.33333333333333333333_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.33333333333333333333_DFP ]

      NodeCoord( 2, : ) = [ &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 1.0_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.33333333333333333333_DFP, &
        & 0.66666666666666666667_DFP, &
        & 0.66666666666666666667_DFP, &
        & 0.33333333333333333333_DFP, &
        & 0.33333333333333333333_DFP ]

    CASE( Triangle12 )
      ! incomplete triangle; all nodes on boundary
      ALLOCATE( NodeCoord( 3, 12 ) )
      NodeCoord = 0.0_DFP
      NodeCoord( 1, : ) = [ &
        & 0.0_DFP, &
        & 1.0_DFP, &
        & 0.0_DFP, &
        & 0.25_DFP, &
        & 0.5_DFP, &
        & 0.75_DFP, &
        & 0.75_DFP, &
        & 0.5_DFP, &
        & 0.25_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP ]

      NodeCoord( 2, : ) = [ &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 1.0_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.25_DFP, &
        & 0.5_DFP, &
        & 0.75_DFP, &
        & 0.75_DFP, &
        & 0.5_DFP, &
        & 0.25_DFP ]

    CASE( Triangle15a )
      ! complete triangle; 12 nodes on boundary and
      ! 3 nodes are inside
      ALLOCATE( NodeCoord( 3, 15 ) )
      NodeCoord = 0.0_DFP
      NodeCoord( 1, : ) = [ &
        & 0.0_DFP, &
        & 1.0_DFP, &
        & 0.0_DFP, &
        & 0.25_DFP, &
        & 0.5_DFP, &
        & 0.75_DFP, &
        & 0.75_DFP, &
        & 0.5_DFP, &
        & 0.25_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.25_DFP, &
        & 0.5_DFP, &
        & 0.25_DFP ]

      NodeCoord( 2, : ) = [ &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 1.0_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.25_DFP, &
        & 0.5_DFP, &
        & 0.75_DFP, &
        & 0.75_DFP, &
        & 0.5_DFP, &
        & 0.25_DFP, &
        & 0.25_DFP, &
        & 0.25_DFP, &
        & 0.5_DFP ]

    CASE( Triangle15b )
      ! Incomplete triangle
      ALLOCATE( NodeCoord( 3, 15 ) )
      NodeCoord = 0.0_DFP

      NodeCoord( 1, : ) = [ &
        & 0.0_DFP, &
        & 1.0_DFP, &
        & 0.0_DFP, &
        & 0.2_DFP, &
        & 0.4_DFP, &
        & 0.6_DFP, &
        & 0.8_DFP, &
        & 0.8_DFP, &
        & 0.6_DFP, &
        & 0.4_DFP, &
        & 0.2_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP ]

      NodeCoord( 2, : ) = [ &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 1.0_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.2_DFP, &
        & 0.4_DFP, &
        & 0.6_DFP, &
        & 0.8_DFP, &
        & 0.8_DFP, &
        & 0.6_DFP, &
        & 0.4_DFP, &
        & 0.2_DFP ]

    CASE( Triangle21 )
      ALLOCATE( NodeCoord( 3, 21 ) )
      NodeCoord = 0.0_DFP

      NodeCoord( 1, : ) = [ &
        & 0.0_DFP, &
        & 1.0_DFP, &
        & 0.0_DFP, &
        & 0.2_DFP, &
        & 0.4_DFP, &
        & 0.6_DFP, &
        & 0.8_DFP, &
        & 0.8_DFP, &
        & 0.6_DFP, &
        & 0.4_DFP, &
        & 0.2_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.2_DFP, &
        & 0.6_DFP, &
        & 0.2_DFP, &
        & 0.4_DFP, &
        & 0.4_DFP, &
        & 0.2_DFP ]

      NodeCoord( 2, : ) = [ &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 1.0_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.0_DFP, &
        & 0.2_DFP, &
        & 0.4_DFP, &
        & 0.6_DFP, &
        & 0.8_DFP, &
        & 0.8_DFP, &
        & 0.6_DFP, &
        & 0.4_DFP, &
        & 0.2_DFP, &
        & 0.2_DFP, &
        & 0.2_DFP, &
        & 0.6_DFP, &
        & 0.2_DFP, &
        & 0.4_DFP, &
        & 0.4_DFP ]

    CASE( Quadrangle4 )
      ALLOCATE( NodeCoord( 3, 4 ) )
      NodeCoord = 0.0_DFP
      NodeCoord( 1, : ) = [-1.0_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP ]
      NodeCoord( 2, : ) = [-1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP ]

    CASE( Quadrangle8 )
      NodeCoord = RESHAPE( [ &
        & -1.0_DFP, -1.0_DFP, 0.0_DFP, &
        & 1.0_DFP, -1.0_DFP, 0.0_DFP, &
        & 1.0_DFP, 1.0_DFP, 0.0_DFP, &
        & -1.0_DFP, 1.0_DFP, 0.0_DFP, &
        & 0.0_DFP, -1.0_DFP, 0.0_DFP, &
        & 1.0_DFP, 0.0_DFP, 0.0_DFP, &
        & 0.0_DFP, 1.0_DFP, 0.0_DFP, &
        & -1.0_DFP, 0.0_DFP, 0.0_DFP ], [3, 8])

    CASE( Quadrangle9 )
      NodeCoord = RESHAPE( [ &
        & -1.0_DFP, -1.0_DFP, 0.0_DFP, &
        & 1.0_DFP, -1.0_DFP, 0.0_DFP, &
        & 1.0_DFP, 1.0_DFP, 0.0_DFP, &
        & -1.0_DFP, 1.0_DFP, 0.0_DFP, &
        & 0.0_DFP, -1.0_DFP, 0.0_DFP, &
        & 1.0_DFP, 0.0_DFP, 0.0_DFP, &
        & 0.0_DFP, 1.0_DFP, 0.0_DFP, &
        & -1.0_DFP, 0.0_DFP, 0.0_DFP, &
        & 0.0_DFP, 0.0_DFP, 0.0_DFP ], [3, 9])

    CASE( Hexahedron8 )
      NodeCoord = RESHAPE( [ &
      & -1.0_DFP, -1.0_DFP, -1.0_DFP, &
      & 1.0_DFP, -1.0_DFP, -1.0_DFP, &
      & 1.0_DFP, 1.0_DFP, -1.0_DFP, &
      & -1.0_DFP, 1.0_DFP, -1.0_DFP, &
      & -1.0_DFP, -1.0_DFP, 1.0_DFP, &
      & 1.0_DFP, -1.0_DFP, 1.0_DFP, &
      & 1.0_DFP, 1.0_DFP, 1.0_DFP, &
      & -1.0_DFP, 1.0_DFP, 1.0_DFP ], [3, 8] )

    CASE( Hexahedron20 )
      NodeCoord = RESHAPE( [ &
      & -1.0_DFP, -1.0_DFP, -1.0_DFP, &
      & 1.0_DFP, -1.0_DFP, -1.0_DFP, &
      & 1.0_DFP, 1.0_DFP, -1.0_DFP, &
      & -1.0_DFP, 1.0_DFP, -1.0_DFP, &
      & -1.0_DFP, -1.0_DFP, 1.0_DFP, &
      & 1.0_DFP, -1.0_DFP, 1.0_DFP, &
      & 1.0_DFP, 1.0_DFP, 1.0_DFP, &
      & -1.0_DFP, 1.0_DFP, 1.0_DFP, &
      & 0.0_DFP, -1.0_DFP, -1.0_DFP, &
      & -1.0_DFP, 0.0_DFP, -1.0_DFP, &
      & -1.0_DFP, -1.0_DFP, 0.0_DFP, &
      & 1.0_DFP, 0.0_DFP, -1.0_DFP, &
      & 1.0_DFP, -1.0_DFP, 0.0_DFP, &
      & 0.0_DFP, 1.0_DFP, -1.0_DFP, &
      & 1.0_DFP, 1.0_DFP, 0.0_DFP, &
      & -1.0_DFP, 1.0_DFP, 0.0_DFP, &
      & 0.0_DFP, -1.0_DFP, 1.0_DFP, &
      & -1.0_DFP, 0.0_DFP, 1.0_DFP, &
      & 1.0_DFP, 0.0_DFP, 1.0_DFP, &
      & 0.0_DFP, 1.0_DFP, 1.0_DFP ], [3, 20])

    CASE( Hexahedron27 )
      NodeCoord = RESHAPE( [ &
      & -1.0_DFP, -1.0_DFP, -1.0_DFP, &
      & 1.0_DFP, -1.0_DFP, -1.0_DFP, &
      & 1.0_DFP, 1.0_DFP, -1.0_DFP, &
      & -1.0_DFP, 1.0_DFP, -1.0_DFP, &
      & -1.0_DFP, -1.0_DFP, 1.0_DFP, &
      & 1.0_DFP, -1.0_DFP, 1.0_DFP, &
      & 1.0_DFP, 1.0_DFP, 1.0_DFP, &
      & -1.0_DFP, 1.0_DFP, 1.0_DFP, &
      & 0.0_DFP, -1.0_DFP, -1.0_DFP, &
      & -1.0_DFP, 0.0_DFP, -1.0_DFP, &
      & -1.0_DFP, -1.0_DFP, 0.0_DFP, &
      & 1.0_DFP, 0.0_DFP, -1.0_DFP, &
      & 1.0_DFP, -1.0_DFP, 0.0_DFP, &
      & 0.0_DFP, 1.0_DFP, -1.0_DFP, &
      & 1.0_DFP, 1.0_DFP, 0.0_DFP, &
      & -1.0_DFP, 1.0_DFP, 0.0_DFP, &
      & 0.0_DFP, -1.0_DFP, 1.0_DFP, &
      & -1.0_DFP, 0.0_DFP, 1.0_DFP, &
      & 1.0_DFP, 0.0_DFP, 1.0_DFP, &
      & 0.0_DFP, 1.0_DFP, 1.0_DFP, &
      & 0.0_DFP, 0.0_DFP, -1.0_DFP, &
      & 0.0_DFP, -1.0_DFP, 0.0_DFP, &
      & -1.0_DFP, 0.0_DFP, 0.0_DFP, &
      & 1.0_DFP, 0.0_DFP, 0.0_DFP, &
      & 0.0_DFP, 1.0_DFP, 0.0_DFP, &
      & 0.0_DFP, 0.0_DFP, 1.0_DFP, &
      & 0.0_DFP, 0.0_DFP, 0.0_DFP ], [3, 27])
  END SELECT

END PROCEDURE Local_NodeCoord

!-----------------------------------------------------------------------------
!                                                                   FacetMatrix
!-----------------------------------------------------------------------------

MODULE PROCEDURE Local_NodeCoord_RefElem
  IF( ALLOCATED( RefElem % XiJ ) ) NodeCoord = RefElem % XiJ
END PROCEDURE Local_NodeCoord_RefElem

!----------------------------------------------------------------------------
!                                                            MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex
  Ans = 0.0_DFP
  SELECT TYPE ( RefElem )
  TYPE IS ( ReferenceLine_ )
    Ans = Measure_Simplex_Line( RefElem, XiJ )
  TYPE IS ( ReferenceTriangle_ )
    Ans = Measure_Simplex_Triangle( RefElem, XiJ )
  TYPE IS ( ReferenceQuadrangle_ )
    Ans = Measure_Simplex_Quadrangle( RefElem, XiJ )
  TYPE IS ( ReferenceTetrahedron_ )
    Ans = Measure_Simplex_Tetrahedron( RefElem, XiJ )
  TYPE IS ( ReferenceHexahedron_ )
    Ans = Measure_Simplex_Hexahedron( RefElem, XiJ )
  TYPE IS ( ReferencePrism_ )
    Ans = Measure_Simplex_Prism( RefElem, XiJ )
  TYPE IS ( ReferencePyramid_ )
    Ans = Measure_Simplex_Pyramid( RefElem, XiJ )
  END SELECT
END PROCEDURE Measure_Simplex

!----------------------------------------------------------------------------
!                                                             ElementQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE Element_Quality
  SELECT TYPE( refelem )
  CLASS IS (ReferenceLine_)
    Ans = Line_quality( refelem, xij, measure )
  CLASS IS (ReferenceTriangle_)
    Ans = Triangle_quality( refelem, xij, measure )
  CLASS IS (ReferenceQuadrangle_)
    Ans = Quadrangle_quality( refelem, xij, measure )
  CLASS IS (ReferenceTetrahedron_)
    Ans = Tetrahedron_quality( refelem, xij, measure )
  CLASS IS (ReferencePrism_)
    Ans = Prism_quality( refelem, xij, measure )
  CLASS IS (ReferenceHexahedron_)
    Ans = Hexahedron_quality( refelem, xij, measure )
  CLASS IS (ReferencePyramid_)
    Ans = Pyramid_quality( refelem, xij, measure )
  END SELECT
END PROCEDURE Element_Quality

!----------------------------------------------------------------------------
!                                                              ContainsPoint
!----------------------------------------------------------------------------

MODULE PROCEDURE contains_point
  SELECT TYPE( refelem )
  CLASS IS (ReferenceLine_)
    CALL Display( "ERROR:: ReferenceElement_Method@Geometry.f90")
    CALL Display( "          Contains_point()")
    CALL Display( "            No case found for ReferenceLine_")
    CALL Display( "            Program stopped!!!")
    STOP
  CLASS IS (ReferenceTriangle_)
    Ans = triangle_contains_point(refelem, xij, x)
  CLASS IS (ReferenceQuadrangle_)
    CALL Display( "ERROR:: ReferenceElement_Method@Geometry.f90")
    CALL Display( "          Contains_point()")
    CALL Display( "            No case found for Quadrangle_")
    CALL Display( "            Program stopped!!!")
    STOP
  END SELECT
END PROCEDURE contains_point

END SUBMODULE Geometry