
!----------------------------------------------------------------------------
!                                                 MeasureOfSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
! Return measure of a linear hexahedron
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Measure_Simplex_Hexahedron( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferenceHexahedron_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Hexahedron_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Hexahedron_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferenceHexahedron_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION Hexahedron_quality
END INTERFACE