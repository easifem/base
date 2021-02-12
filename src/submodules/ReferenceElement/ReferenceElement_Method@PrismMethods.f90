SUBMODULE( ReferenceElement_Method ) PrismMethods
IMPLICIT NONE
CONTAINS

!-----------------------------------------------------------------------------
!                                                              MeasureSimplex
!-----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Prism
  INTEGER( I4B ) :: FM( 5, 7 ), Node0( 5, 4 ), Order0( 5 ), b, iFace

  FM = FacetMatrix(RefElem)

  DO iFace = 1, 5
    Order0( iFace ) = FM( iFace, 3 )
    b = Order0( iFace ) + 3
    Node0( iFace, 1:Order0( iFace ) ) = FM( iFace, 4 : b )
  END DO

  CALL POLYHEDRONVOLUME3D( coord = XiJ( 1:3, 1:6 ), &
    & order_max = 4, face_num = 5,  &
    & node = Node0, node_num = 6, &
    & order = Order0, &
    & volume = Ans )

END PROCEDURE Measure_Simplex_Prism

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Prism_quality
END PROCEDURE Prism_quality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "./modified_burkardt.inc"
END SUBMODULE PrismMethods