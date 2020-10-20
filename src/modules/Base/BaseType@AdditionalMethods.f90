SUBMODULE( BaseType ) AdditionalMethods
USE ReferenceElement_Method, ONLY: LagrangePoints, LagrangeElement, Initiate
IMPLICIT NONE

CONTAINS

!------------------------------------------------------------------------------
!                                                                   lp_refelem
!------------------------------------------------------------------------------

MODULE PROCEDURE lp_refelem
END PROCEDURE lp_refelem

!------------------------------------------------------------------------------
!                                                              lp_refelem_line
!------------------------------------------------------------------------------

MODULE PROCEDURE lp_refelem_line
  SELECT TYPE( Obj )
    TYPE IS (ReferenceLine_ )
      Ans = LagrangePoints( Obj, Order )
  END SELECT
END PROCEDURE lp_refelem_line

!------------------------------------------------------------------------------
!                                                          lp_refelem_Triangle
!------------------------------------------------------------------------------

MODULE PROCEDURE lp_refelem_Triangle
  SELECT TYPE( Obj )
    TYPE IS (ReferenceTriangle_ )
      Ans = LagrangePoints( Obj, Order )
  END SELECT
END PROCEDURE lp_refelem_Triangle

!------------------------------------------------------------------------------
!                                                       lp_refelem_Quadrangle
!------------------------------------------------------------------------------

MODULE PROCEDURE lp_refelem_Quadrangle
  SELECT TYPE( Obj )
    TYPE IS (ReferenceQuadrangle_ )
    Ans = LagrangePoints( Obj, Order )
  END SELECT
END PROCEDURE lp_refelem_Quadrangle

!------------------------------------------------------------------------------
!                                                       lp_refelem_Tetrahedron
!------------------------------------------------------------------------------

MODULE PROCEDURE lp_refelem_Tetrahedron
  SELECT TYPE( Obj )
    TYPE IS (ReferenceTetrahedron_ )
    Ans = LagrangePoints( Obj, Order )
  END SELECT
END PROCEDURE lp_refelem_Tetrahedron

!------------------------------------------------------------------------------
!                                                       lp_refelem_Hexahedron
!------------------------------------------------------------------------------

MODULE PROCEDURE lp_refelem_Hexahedron
  SELECT TYPE( Obj )
    TYPE IS (ReferenceHexahedron_ )
    Ans = LagrangePoints( Obj, Order )
  END SELECT
END PROCEDURE lp_refelem_Hexahedron

!------------------------------------------------------------------------------
!                                                           lp_refelem_Pyramid
!------------------------------------------------------------------------------

MODULE PROCEDURE lp_refelem_Pyramid
  SELECT TYPE( Obj )
  TYPE IS (ReferencePyramid_ )
  Ans = LagrangePoints( Obj, Order )
  END SELECT
END PROCEDURE lp_refelem_Pyramid

!------------------------------------------------------------------------------
!                                                             lp_refelem_Prism
!------------------------------------------------------------------------------

MODULE PROCEDURE lp_refelem_Prism
  SELECT TYPE( Obj )
  TYPE IS (ReferencePrism_ )
  Ans = LagrangePoints( Obj, Order )
  END SELECT
END PROCEDURE lp_refelem_Prism

!------------------------------------------------------------------------------
!                                                            lag_elem_refelem
!------------------------------------------------------------------------------

MODULE PROCEDURE lag_elem_refelem

END PROCEDURE lag_elem_refelem

!------------------------------------------------------------------------------
!                                                            lag_elem_refLine
!------------------------------------------------------------------------------

MODULE PROCEDURE lag_elem_refLine
  SELECT TYPE( Obj )
  TYPE IS ( ReferenceLine_ )
    ALLOCATE( Ans )
    CALL Initiate( Ans, LagrangeElement( Obj, Order ) )
  END SELECT
END PROCEDURE lag_elem_refLine

!------------------------------------------------------------------------------
!                                                         lag_elem_refTriangle
!------------------------------------------------------------------------------

MODULE PROCEDURE lag_elem_refTriangle
  SELECT TYPE( Obj )
  TYPE IS ( ReferenceTriangle_ )
    ALLOCATE( Ans )
    CALL Initiate( Ans, LagrangeElement( Obj, Order ) )
  END SELECT
END PROCEDURE lag_elem_refTriangle

!------------------------------------------------------------------------------
!                                                      lag_elem_refQuadrangle
!------------------------------------------------------------------------------

MODULE PROCEDURE lag_elem_refQuadrangle
  SELECT TYPE( Obj )
  TYPE IS ( ReferenceQuadrangle_ )
    ALLOCATE( Ans )
    CALL Initiate( Ans, LagrangeElement( Obj, Order ) )
  END SELECT
END PROCEDURE lag_elem_refQuadrangle

!------------------------------------------------------------------------------
!                                                      lag_elem_refTetrahedron
!------------------------------------------------------------------------------

MODULE PROCEDURE lag_elem_refTetrahedron
  SELECT TYPE( Obj )
  TYPE IS ( ReferenceTetrahedron_ )
    ALLOCATE( Ans )
    CALL Initiate( Ans, LagrangeElement( Obj, Order ) )
  END SELECT
END PROCEDURE lag_elem_refTetrahedron

!------------------------------------------------------------------------------
!                                                       lag_elem_refHexahedron
!------------------------------------------------------------------------------

MODULE PROCEDURE lag_elem_refHexahedron
  SELECT TYPE( Obj )
  TYPE IS ( ReferenceHexahedron_ )
    ALLOCATE( Ans )
    CALL Initiate( Ans, LagrangeElement( Obj, Order ) )
  END SELECT
END PROCEDURE lag_elem_refHexahedron

!------------------------------------------------------------------------------
!                                                           lag_elem_refPrism
!------------------------------------------------------------------------------

MODULE PROCEDURE lag_elem_refPrism
  SELECT TYPE( Obj )
  TYPE IS ( ReferencePrism_ )
    ALLOCATE( Ans )
    CALL Initiate( Ans, LagrangeElement( Obj, Order ) )
  END SELECT
END PROCEDURE lag_elem_refPrism

!------------------------------------------------------------------------------
!                                                         lag_elem_refPyramid
!------------------------------------------------------------------------------

MODULE PROCEDURE lag_elem_refPyramid
  SELECT TYPE( Obj )
  TYPE IS ( ReferencePyramid_ )
    ALLOCATE( Ans )
    CALL Initiate( Ans, LagrangeElement( Obj, Order ) )
  END SELECT
END PROCEDURE lag_elem_refPyramid

END SUBMODULE AdditionalMethods