SUBMODULE( ReferenceElement_Method ) QuadrangleMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Quadrangle
  IF( refelem%nsd .EQ. 2 ) THEN
    CALL QUADAREA2D( XiJ( 1:2, 1:4 ), Ans )
  ELSE
    CALL QUADAREA3D( XiJ( 1:3, 1:4 ), Ans )
  END IF
END PROCEDURE Measure_Simplex_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrangle_quality
END PROCEDURE Quadrangle_quality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "./modified_burkardt.inc"

END SUBMODULE QuadrangleMethods