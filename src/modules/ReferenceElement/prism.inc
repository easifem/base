
!----------------------------------------------------------------------------
!                                                  MeasureOfSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
! Return measure of a straight Prism
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Measure_Simplex_Prism( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferencePrism_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Prism
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Prism_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Prism_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferencePrism_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION Prism_quality
END INTERFACE