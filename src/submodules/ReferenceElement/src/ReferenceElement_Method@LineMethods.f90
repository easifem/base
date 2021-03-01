SUBMODULE( ReferenceElement_Method ) LineMethods
IMPLICIT NONE
CONTAINS
!----------------------------------------------------------------------------
!                                                              MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Line
  Ans = ( XiJ( 1, 1 ) -  XiJ( 1, 2 ) ) ** 2 &
    & + ( XiJ( 2, 1 ) -  XiJ( 2, 2 ) ) ** 2 &
    & + ( XiJ( 3, 1 ) -  XiJ( 3, 2 ) ) ** 2
  Ans = SQRT( Ans )
END PROCEDURE Measure_Simplex_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Line_quality
END PROCEDURE Line_quality

END SUBMODULE LineMethods