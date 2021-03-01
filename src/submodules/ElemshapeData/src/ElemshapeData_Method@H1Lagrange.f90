SUBMODULE( ElemShapeData_Method ) H1Lagrange
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE H1_Lagrange
  SELECT TYPE( refelem )
  TYPE IS ( ReferenceLine_ )
    CALL Line_H1_Lagrange( Obj=Obj, Quad=Quad, RefElem=RefElem, &
      & ContinuityType=ContinuityType, InterpolType=InterpolType )
  TYPE IS ( ReferenceTriangle_ )
    CALL Triangle_H1_Lagrange( Obj=Obj, Quad=Quad, RefElem=RefElem, &
      & ContinuityType=ContinuityType, InterpolType=InterpolType )
  TYPE IS ( ReferenceQuadrangle_ )
    CALL Quadrangle_H1_Lagrange( Obj=Obj, Quad=Quad, RefElem=RefElem, &
      & ContinuityType=ContinuityType, InterpolType=InterpolType )
  TYPE IS ( ReferenceTetrahedron_ )
    CALL Tetrahedron_H1_Lagrange( Obj=Obj, Quad=Quad, RefElem=RefElem, &
      & ContinuityType=ContinuityType, InterpolType=InterpolType )
  TYPE IS ( ReferenceHexahedron_ )
    CALL Hexahedron_H1_Lagrange( Obj=Obj, Quad=Quad, RefElem=RefElem, &
      & ContinuityType=ContinuityType, InterpolType=InterpolType )
  TYPE IS ( ReferencePrism_ )
    CALL Prism_H1_Lagrange( Obj=Obj, Quad=Quad, RefElem=RefElem, &
      & ContinuityType=ContinuityType, InterpolType=InterpolType )
  TYPE IS ( ReferencePyramid_ )
    CALL Pyramid_H1_Lagrange( Obj=Obj, Quad=Quad, RefElem=RefElem, &
      & ContinuityType=ContinuityType, InterpolType=InterpolType )
  END SELECT
END PROCEDURE H1_Lagrange

END SUBMODULE H1Lagrange