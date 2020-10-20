
!----------------------------------------------------------------------------
!                                                 MeasureOfSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
! Return measure of a linear triangle
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Measure_Simplex_Triangle( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Triangle_Angle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_angles( refelem, xij ) RESULT( Ans )
  CLASS(ReferenceTriangle_), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(3)
END FUNCTION triangle_angles
END INTERFACE

INTERFACE Angles
  MODULE PROCEDURE triangle_angles
END INTERFACE Angles

PUBLIC :: Angles

!----------------------------------------------------------------------------
!                                                             Triangle_Area
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_area( refelem, xij ) RESULT( Ans )
  CLASS(ReferenceTriangle_), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans
END FUNCTION triangle_area
END INTERFACE

INTERFACE Area
  MODULE PROCEDURE triangle_area
END INTERFACE Area

PUBLIC :: Area

!----------------------------------------------------------------------------
!                                                        Triangle_AreaVector
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_areaVector( refelem, xij ) RESULT( Ans )
  CLASS(ReferenceTriangle_), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans( 3 )
END FUNCTION triangle_areaVector
END INTERFACE

INTERFACE AreaVector
  MODULE PROCEDURE triangle_areaVector
END INTERFACE AreaVector

PUBLIC :: AreaVector

!----------------------------------------------------------------------------
!                                                       Triangle_Barycentric
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_barycentric( refelem, xij, x ) RESULT( Ans )
  CLASS(ReferenceTriangle_), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ), INTENT( IN ) :: x( : )
  REAL( DFP ) :: Ans( 3 )
END FUNCTION triangle_barycentric
END INTERFACE

INTERFACE Barycentric
  MODULE PROCEDURE triangle_barycentric
END INTERFACE Barycentric

PUBLIC :: Barycentric

!----------------------------------------------------------------------------
!                                                          Triangle_Centroid
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_centroid( refelem, xij ) RESULT( Ans )
  CLASS(ReferenceTriangle_), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(3)
END FUNCTION triangle_centroid
END INTERFACE

INTERFACE Centroid
  MODULE PROCEDURE triangle_centroid
END INTERFACE Centroid

PUBLIC :: Centroid

!----------------------------------------------------------------------------
!                                                      Triangle_CircumCenter
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_circumcentre(  refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(3)
END FUNCTION triangle_circumcentre
END INTERFACE

INTERFACE CircumCenter
  MODULE PROCEDURE triangle_circumcentre
END INTERFACE CircumCenter

PUBLIC :: CircumCenter

!----------------------------------------------------------------------------
!                                                       Triangle_CircumCircle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_circumcircle( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(4)
    !! Ans(1) = radius and Ans(2:4) center
END FUNCTION triangle_circumcircle
END INTERFACE

INTERFACE CircumCircle
  MODULE PROCEDURE triangle_circumcircle
END INTERFACE CircumCircle

PUBLIC :: CircumCircle

!----------------------------------------------------------------------------
!                                                       Triangle_CircumRadius
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_circumradius( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans
END FUNCTION triangle_circumradius
END INTERFACE

INTERFACE CircumRadius
  MODULE PROCEDURE triangle_circumradius
END INTERFACE CircumRadius

PUBLIC :: CircumRadius

!----------------------------------------------------------------------------
!                                              triangle_contains_line
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE triangle_contains_line( refelem, xij, x1, x2, &
  & parametricLine, inside, xint )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:), x1(3), x2(3)
  LOGICAL(LGT), INTENT( IN ) :: parametricLine
  LOGICAL(LGT), INTENT (OUT) :: inside
  REAL( DFP ), INTENT( OUT ) :: xint(3)
END SUBROUTINE triangle_contains_line
END INTERFACE

INTERFACE ContainsLine
  MODULE PROCEDURE triangle_contains_line
END INTERFACE ContainsLine

PUBLIC :: ContainsLine

!----------------------------------------------------------------------------
!                                                   triangle_contains_point
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_contains_point( refelem, xij, x ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:), x(:)
  LOGICAL(LGT) :: Ans
END FUNCTION triangle_contains_point
END INTERFACE

!----------------------------------------------------------------------------
!                                                          triangle_diameter
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_diameter( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans
END FUNCTION triangle_diameter
END INTERFACE

INTERFACE diameter
  MODULE PROCEDURE triangle_diameter
END INTERFACE diameter

PUBLIC :: diameter

!----------------------------------------------------------------------------
!                                                       triangle_edge_length
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_edge_length( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(3)
END FUNCTION triangle_edge_length
END INTERFACE

INTERFACE EdgeLength
  MODULE PROCEDURE triangle_edge_length
END INTERFACE EdgeLength

PUBLIC :: EdgeLength

!----------------------------------------------------------------------------
!                                                         triangle_incenter
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_incenter( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(3)
END FUNCTION triangle_incenter
END INTERFACE

INTERFACE Incenter
  MODULE PROCEDURE triangle_incenter
END INTERFACE Incenter

PUBLIC :: Incenter

!----------------------------------------------------------------------------
!                                                         triangle_incircle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_incircle( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(4)
END FUNCTION triangle_incircle
END INTERFACE

INTERFACE Incircle
  MODULE PROCEDURE triangle_incircle
END INTERFACE Incircle

PUBLIC :: Incircle

!----------------------------------------------------------------------------
!                                                         triangle_inradius
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_inradius( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans
END FUNCTION triangle_inradius
END INTERFACE

INTERFACE Inradius
  MODULE PROCEDURE triangle_inradius
END INTERFACE Inradius

PUBLIC :: Inradius

!----------------------------------------------------------------------------
!                                                      triangle_orthocenter
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_orthocenter( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans( 3 )
END FUNCTION triangle_orthocenter
END INTERFACE

INTERFACE Orthocenter
  MODULE PROCEDURE triangle_orthocenter
END INTERFACE Orthocenter

PUBLIC :: Orthocenter

!----------------------------------------------------------------------------
!                                                        triangle_point_dist
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_point_dist( refelem, xij, x ) &
  & RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:), x(:)
  REAL( DFP ) :: Ans
END FUNCTION triangle_point_dist
END INTERFACE

INTERFACE DistanceFromPoint
  MODULE PROCEDURE triangle_point_dist
END INTERFACE DistanceFromPoint

PUBLIC :: DistanceFromPoint

!----------------------------------------------------------------------------
!                                                    triangle_nearest_point
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE triangle_get_nearest_point( refelem, xij, x, xn, dist )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:), x(:)
  REAL( DFP ) , INTENT (INOUT) :: xn(:)
  REAL( DFP ), INTENT (OUT) :: dist
END SUBROUTINE triangle_get_nearest_point
END INTERFACE

INTERFACE NearestPoint
  MODULE PROCEDURE triangle_get_nearest_point
END INTERFACE NearestPoint

PUBLIC :: NearestPoint

!----------------------------------------------------------------------------
!                                                     triangle_random_point
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_random_point( refelem, xij, n, seed ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ) :: n, seed
  REAL( DFP ) :: Ans(3, n)
END FUNCTION triangle_random_point
END INTERFACE

INTERFACE RandomPoint
  MODULE PROCEDURE triangle_random_point
END INTERFACE RandomPoint

PUBLIC :: RandomPoint

!----------------------------------------------------------------------------
!                                                          triangle_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION triangle_quality
END INTERFACE