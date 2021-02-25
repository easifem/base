SUBMODULE( ReferenceElement_Method ) PyramidMethods
IMPLICIT NONE
CONTAINS

!-----------------------------------------------------------------------------
!                                                              MeasureSimplex
!-----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Pyramid

  INTEGER( I4B ) :: FM( 5, 7 ), Node0( 5, 4 ), Order0( 5 ), iFace, b

  FM = FacetMatrix(RefElem)

  DO iFace = 1, 5
    Order0( iFace ) = FM( iFace, 3 )
    b = Order0( iFace ) + 3
    Node0( iFace, 1:Order0( iFace ) ) = FM( iFace, 4 : b )
  END DO

  CALL POLYHEDRONVOLUME3D( coord = XiJ( 1:3, 1:5 ), &
    & order_max = 4, face_num = 5,  &
    & node = Node0, node_num = 5, &
    & order = Order0, &
    & volume = Ans )

END PROCEDURE Measure_Simplex_Pyramid

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Pyramid_quality
END PROCEDURE Pyramid_quality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "./modified_burkardt.inc"

END SUBMODULE PyramidMethods