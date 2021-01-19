
!----------------------------------------------------------------------------
!                                                 MeasureOfSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
! Return measure of a linear tetrahedron
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Measure_Simplex_Tetrahedron( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferenceTetrahedron_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                          tetrahedron_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION tetrahedron_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( Referencetetrahedron_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION tetrahedron_quality
END INTERFACE