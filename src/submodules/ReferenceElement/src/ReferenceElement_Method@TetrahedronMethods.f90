SUBMODULE( ReferenceElement_Method ) TetrahedronMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Tetrahedron
  CALL TETRAHEDRONVOLUME3D( XiJ( 1:3, 1:4 ), Ans )
END PROCEDURE Measure_Simplex_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Tetrahedron_quality
END PROCEDURE Tetrahedron_quality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "./modified_burkardt.inc"

END SUBMODULE TetrahedronMethods