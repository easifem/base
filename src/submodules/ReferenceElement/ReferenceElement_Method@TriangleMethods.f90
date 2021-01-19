SUBMODULE( ReferenceElement_Method ) TriangleMethods
IMPLICIT NONE
CONTAINS
!----------------------------------------------------------------------------
!                                                              MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Triangle
  IF( refelem%nsd .EQ. 2 ) THEN
    CALL TRIANGLEAREA2D( XiJ( 1:2, 1:3 ), Ans )
  ELSE
    CALL TRIANGLEAREA3D( XiJ( 1:3, 1:3), Ans )
  END IF
END PROCEDURE Measure_Simplex_Triangle

!----------------------------------------------------------------------------
!                                                             Triangle_Angles
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_angles
  SELECT CASE( refelem%nsd )
  CASE( 2 )
    CALL triangle_angles_2d( xij(1:2, 1:3), ans )
  CASE( 3 )
    CALL triangle_angles_3d( xij(1:3, 1:3), ans )
  END SELECT
END PROCEDURE triangle_angles

!----------------------------------------------------------------------------
!                                                              Triangle_Area
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_area
  SELECT CASE(refelem%nsd)
  CASE(2)
    CALL triangle_area_2d( xij(1:2, 1:3), ans )
  CASE(3)
    CALL TRIANGLE_AREA_3D_3( xij(1:3, 1:3), ans )
  END SELECT
END PROCEDURE triangle_area

!----------------------------------------------------------------------------
!                                                        Triangle_AreaVector
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_AreaVector
  SELECT CASE(refelem%nsd)
  CASE(2)
    ans(1:2) = 0.0_DFP
    CALL triangle_area_2d( xij(1:2, 1:3), ans(3))
  CASE(3)
    CALL triangle_area_vector_3d(xij(1:3, 1:3), ans)
  END SELECT
END PROCEDURE triangle_AreaVector

!----------------------------------------------------------------------------
!                                                       Triangle_Barycentric
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_barycentric
  CALL triangle_barycentric_2d ( xij(1:2, 1:3), x(1:2), ans )
END PROCEDURE triangle_barycentric

!----------------------------------------------------------------------------
!                                                         Triangle_Centroid
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_centroid
  IF( refelem%nsd .EQ. 2 ) THEN
    Ans(3)=0.0_DFP
    CALL triangle_centroid_2d( xij(1:2, 1:3), ans(1:2) )
  ELSE
    CALL triangle_centroid_3d( xij(1:3, 1:3), ans(1:3) )
  END IF
END PROCEDURE triangle_centroid

!----------------------------------------------------------------------------
!                                                      triangle_circumcentre
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_circumcentre
  IF( refelem%nsd .EQ. 2 ) THEN
    Ans(3)=0.0_DFP
    CALL triangle_circumcenter_2d( xij(1:2, 1:3), Ans(1:2))
  ELSE
    CALL triangle_circumcenter( 3, xij(1:3, 1:3), Ans(1:3))
  END IF
END PROCEDURE triangle_circumcentre

!----------------------------------------------------------------------------
!                                                      triangle_circumcircle
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_circumcircle
  Ans(4)=0.0_DFP
  CALL triangle_circumcircle_2d( xij(1:2, 1:3), Ans(1), Ans(2:3))
END PROCEDURE triangle_circumcircle

!----------------------------------------------------------------------------
!                                                     triangle_circumradius
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_circumradius
  CALL triangle_circumradius_2d( xij(1:2, 1:3), Ans )
END PROCEDURE triangle_circumradius

!----------------------------------------------------------------------------
!                                                   triangle_contains_line
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_contains_line
  IF( parametricLine ) THEN
    CALL triangle_contains_line_par_3d( xij(1:3, 1:3), x1, x2, &
      & inside, xint)
  ELSE
    CALL triangle_contains_line_exp_3d( xij(1:3, 1:3), x1, x2, &
      & inside, xint)
  END IF
END PROCEDURE triangle_contains_line

!----------------------------------------------------------------------------
!                                                    triangle_contains_point
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_contains_point
  CALL triangle_contains_point_2d_1(xij(1:2,1:3), x(1:2), ans)
END PROCEDURE triangle_contains_point

!----------------------------------------------------------------------------
!                                                         triangle_diameter
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_diameter
  CALL triangle_diameter_2d(xij(1:2, 1:3), Ans)
END PROCEDURE triangle_diameter

!----------------------------------------------------------------------------
!                                                       triangle_edge_length
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_edge_length
  CALL triangle_edge_length_2d(xij(1:2, 1:3), Ans)
END PROCEDURE triangle_edge_length

!----------------------------------------------------------------------------
!                                                        triangle_incenter
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_incenter
  Ans(3)=0.0_DFP
  CALL triangle_incenter_2d(xij(1:2,1:3), Ans(1:2))
END PROCEDURE triangle_incenter

!----------------------------------------------------------------------------
!                                                         triangle_incircle
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_incircle
  Ans(4) = 0.0_DFP
  CALL triangle_incircle_2d(xij(1:2,1:3), Ans(1), Ans(2:3))
END PROCEDURE triangle_incircle

!----------------------------------------------------------------------------
!                                                          triangle_inradius
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_inradius
  CALL triangle_inradius_2d(xij(1:2,1:3), Ans)
END PROCEDURE triangle_inradius

!----------------------------------------------------------------------------
!                                                      triangle_orthocenter
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_orthocenter
  Ans(3)=0.0_DFP
  CALL triangle_orthocenter_2d(xij(1:2,1:3), Ans(1:2))
END PROCEDURE triangle_orthocenter

!----------------------------------------------------------------------------
!                                                       triangle_point_dist
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_point_dist
  SELECT CASE( refelem%nsd)
  CASE(2)
    CALL triangle_point_dist_2d(xij(1:2,1:3), x(1:2), Ans)
  CASE(3)
      CALL triangle_point_dist_3d(xij(1:3,1:3), x(1:3), Ans)
  END SELECT
END PROCEDURE triangle_point_dist

!----------------------------------------------------------------------------
!                                                     triangle_nearest_point
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_get_nearest_point
  CALL triangle_point_near_2d(xij(1:2,1:3), x(1:2), xn(1:2), dist)
END PROCEDURE triangle_get_nearest_point

!----------------------------------------------------------------------------
!                                                      triangle_random_point
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_random_point
  Ans=0.0_DFP
  CALL triangle_sample(xij(1:2,1:3), n, seed, Ans(1:2, 1:n))
END PROCEDURE triangle_random_point

!----------------------------------------------------------------------------
!                                                          triangle_quality
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_quality
  REAL( DFP ) :: rvar(3)

  SELECT CASE(measure)

  CASE( QualityMeasure%area )
    Ans=Area(refelem=refelem, xij=xij)

  CASE( QualityMeasure%maxangle )
    Ans=MAXVAL(Angles(refelem=refelem, xij=xij))

  CASE( QualityMeasure%minangle )
    Ans=MINVAL(Angles(refelem=refelem, xij=xij))

  CASE( QualityMeasure%angleratio )
    Ans=3.0_DFP * MINVAL(Angles(refelem=refelem, xij=xij)) / Pi

  CASE( QualityMeasure%radiusRatio )
    Ans=2.0_DFP * InRadius(refelem=refelem, xij=xij) &
      & / CircumRadius(refelem=refelem, xij=xij)

  CASE( QualityMeasure%edgeRatio )
    rvar = EdgeLength(refelem=refelem, xij=xij)
    Ans = MINVAL(rvar)/MAXVAL(rvar)

  CASE( QualityMeasure%aspectRatio )
    rvar = EdgeLength(refelem=refelem, xij=xij)
    Ans = MAXVAL(rvar) * SUM(rvar) &
      & / (4.0_DFP*SQRT(3.0_DFP)*Area(refelem=refelem, xij=xij))

  END SELECT
END PROCEDURE triangle_quality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "./modified_burkardt.inc"
END SUBMODULE TriangleMethods