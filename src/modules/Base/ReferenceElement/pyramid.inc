
!----------------------------------------------------------------------------
!                                                 MeasureOfSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
! Return measure of a linear Pyramid
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Measure_Simplex_Pyramid( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferencePyramid_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Pyramid_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Pyramid_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferencePyramid_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION Pyramid_quality
END INTERFACE