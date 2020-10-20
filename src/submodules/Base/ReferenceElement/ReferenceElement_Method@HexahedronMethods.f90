SUBMODULE( ReferenceElement_Method ) HexahedronMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Hexahedron

  INTEGER( I4B ) :: Order0( 6 ), Node0( 6, 4 ), FM( 6, 7 ), iFace, b

  Order0 = [4, 4, 4, 4, 4, 4]
  FM = FacetMatrix( RefElem )
  DO iFace = 1, 6
    b = FM( iFace, 3 ) + 3
    Node0( iFace, 1:Order0( iFace ) ) = FM( iFace, 4 : b )
  END DO

  CALL POLYHEDRONVOLUME3D( coord = XiJ( 1:3, 1:8 ), &
    & order_max = 4, face_num = 6,  &
    & node = Node0, node_num = 8, &
    & order = Order0, &
    & volume = Ans )

END PROCEDURE Measure_Simplex_Hexahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Hexahedron_quality
END PROCEDURE Hexahedron_quality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "./modified_burkardt.inc"
END SUBMODULE HexahedronMethods