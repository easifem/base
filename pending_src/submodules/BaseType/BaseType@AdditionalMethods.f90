SUBMODULE( BaseType ) AdditionalMethods
USE ReferenceElement_Method, ONLY: LagrangePoints, LagrangeElement, Initiate
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 lp_refelem
!----------------------------------------------------------------------------

MODULE PROCEDURE lp_refelem
END PROCEDURE lp_refelem

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lp_refelem_line
  SELECT TYPE( Obj )
    TYPE IS (ReferenceLine_ )
      Ans = LagrangePoints( Obj, Order )
  END SELECT
END PROCEDURE lp_refelem_line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lp_refelem_Triangle
  SELECT TYPE( Obj )
    TYPE IS (ReferenceTriangle_ )
      Ans = LagrangePoints( Obj, Order )
  END SELECT
END PROCEDURE lp_refelem_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lp_refelem_Quadrangle
  SELECT TYPE( Obj )
    TYPE IS (ReferenceQuadrangle_ )
    Ans = LagrangePoints( Obj, Order )
  END SELECT
END PROCEDURE lp_refelem_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lp_refelem_Tetrahedron
  SELECT TYPE( Obj )
    TYPE IS (ReferenceTetrahedron_ )
    Ans = LagrangePoints( Obj, Order )
  END SELECT
END PROCEDURE lp_refelem_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lp_refelem_Hexahedron
  SELECT TYPE( Obj )
    TYPE IS (ReferenceHexahedron_ )
    Ans = LagrangePoints( Obj, Order )
  END SELECT
END PROCEDURE lp_refelem_Hexahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lp_refelem_Pyramid
  SELECT TYPE( Obj )
  TYPE IS (ReferencePyramid_ )
  Ans = LagrangePoints( Obj, Order )
  END SELECT
END PROCEDURE lp_refelem_Pyramid

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lp_refelem_Prism
  SELECT TYPE( Obj )
  TYPE IS (ReferencePrism_ )
  Ans = LagrangePoints( Obj, Order )
  END SELECT
END PROCEDURE lp_refelem_Prism

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lag_elem_refelem
END PROCEDURE lag_elem_refelem

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lag_elem_refLine
  SELECT TYPE( Obj )
  TYPE IS ( ReferenceLine_ )
    ALLOCATE( Ans )
    CALL Initiate( Ans, LagrangeElement( Obj, Order ) )
  END SELECT
END PROCEDURE lag_elem_refLine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lag_elem_refTriangle
  SELECT TYPE( Obj )
  TYPE IS ( ReferenceTriangle_ )
    ALLOCATE( Ans )
    CALL Initiate( Ans, LagrangeElement( Obj, Order ) )
  END SELECT
END PROCEDURE lag_elem_refTriangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lag_elem_refQuadrangle
  SELECT TYPE( Obj )
  TYPE IS ( ReferenceQuadrangle_ )
    ALLOCATE( Ans )
    CALL Initiate( Ans, LagrangeElement( Obj, Order ) )
  END SELECT
END PROCEDURE lag_elem_refQuadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lag_elem_refTetrahedron
  SELECT TYPE( Obj )
  TYPE IS ( ReferenceTetrahedron_ )
    ALLOCATE( Ans )
    CALL Initiate( Ans, LagrangeElement( Obj, Order ) )
  END SELECT
END PROCEDURE lag_elem_refTetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lag_elem_refHexahedron
  SELECT TYPE( Obj )
  TYPE IS ( ReferenceHexahedron_ )
    ALLOCATE( Ans )
    CALL Initiate( Ans, LagrangeElement( Obj, Order ) )
  END SELECT
END PROCEDURE lag_elem_refHexahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lag_elem_refPrism
  SELECT TYPE( Obj )
  TYPE IS ( ReferencePrism_ )
    ALLOCATE( Ans )
    CALL Initiate( Ans, LagrangeElement( Obj, Order ) )
  END SELECT
END PROCEDURE lag_elem_refPrism

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lag_elem_refPyramid
  SELECT TYPE( Obj )
  TYPE IS ( ReferencePyramid_ )
    ALLOCATE( Ans )
    CALL Initiate( Ans, LagrangeElement( Obj, Order ) )
  END SELECT
END PROCEDURE lag_elem_refPyramid

END SUBMODULE AdditionalMethods


! INTERFACE
!   MODULE PURE FUNCTION lp_refelem_line( Obj, Order ) RESULT( Ans )
!     CLASS( ReferenceLine_ ), INTENT( IN ) :: Obj
!     INTEGER( I4B ), INTENT( IN ) :: Order
!     REAL( DFP ), ALLOCATABLE :: Ans( :, : )
!   END FUNCTION lp_refelem_line

!   MODULE FUNCTION lag_elem_refline(Obj, Order) RESULT( Ans )
!     CLASS( ReferenceLine_ ), INTENT( IN ) :: Obj
!     INTEGER( I4B ), INTENT( IN ) :: Order
!     CLASS( ReferenceElement_ ), POINTER :: Ans
!   END FUNCTION lag_elem_refline
! END INTERFACE

! INTERFACE
!   MODULE PURE FUNCTION lp_refelem_Triangle( Obj, Order ) RESULT( Ans )
!     CLASS( ReferenceTriangle_ ), INTENT( IN ) :: Obj
!     INTEGER( I4B ), INTENT( IN ) :: Order
!     REAL( DFP ), ALLOCATABLE :: Ans( :, : )
!   END FUNCTION lp_refelem_Triangle

!   MODULE FUNCTION lag_elem_refTriangle(Obj, Order) RESULT( Ans )
!     CLASS( ReferenceTriangle_ ), INTENT( IN ) :: Obj
!     INTEGER( I4B ), INTENT( IN ) :: Order
!     CLASS( ReferenceElement_ ), POINTER :: Ans
!   END FUNCTION lag_elem_refTriangle
! END INTERFACE

! INTERFACE
!   MODULE PURE FUNCTION lp_refelem_Quadrangle( Obj, Order ) RESULT( Ans )
!     CLASS( ReferenceQuadrangle_ ), INTENT( IN ) :: Obj
!     INTEGER( I4B ), INTENT( IN ) :: Order
!     REAL( DFP ), ALLOCATABLE :: Ans( :, : )
!   END FUNCTION lp_refelem_Quadrangle

!   MODULE FUNCTION lag_elem_refQuadrangle(Obj, Order) RESULT( Ans )
!     CLASS( ReferenceQuadrangle_ ), INTENT( IN ) :: Obj
!     INTEGER( I4B ), INTENT( IN ) :: Order
!     CLASS( ReferenceElement_ ), POINTER :: Ans
!   END FUNCTION lag_elem_refQuadrangle
! END INTERFACE

! INTERFACE
!   MODULE PURE FUNCTION lp_refelem_Tetrahedron( Obj, Order ) RESULT( Ans )
!     CLASS( ReferenceTetrahedron_ ), INTENT( IN ) :: Obj
!     INTEGER( I4B ), INTENT( IN ) :: Order
!     REAL( DFP ), ALLOCATABLE :: Ans( :, : )
!   END FUNCTION lp_refelem_Tetrahedron

!   MODULE FUNCTION lag_elem_refTetrahedron(Obj, Order) RESULT( Ans )
!     CLASS( ReferenceTetrahedron_ ), INTENT( IN ) :: Obj
!     INTEGER( I4B ), INTENT( IN ) :: Order
!     CLASS( ReferenceElement_ ), POINTER :: Ans
!   END FUNCTION lag_elem_refTetrahedron
! END INTERFACE

! INTERFACE
!   MODULE PURE FUNCTION lp_refelem_Hexahedron( Obj, Order ) RESULT( Ans )
!     CLASS( ReferenceHexahedron_ ), INTENT( IN ) :: Obj
!     INTEGER( I4B ), INTENT( IN ) :: Order
!     REAL( DFP ), ALLOCATABLE :: Ans( :, : )
!   END FUNCTION lp_refelem_Hexahedron

!   MODULE FUNCTION lag_elem_refHexahedron(Obj, Order) RESULT( Ans )
!     CLASS( ReferenceHexahedron_ ), INTENT( IN ) :: Obj
!     INTEGER( I4B ), INTENT( IN ) :: Order
!     CLASS( ReferenceElement_ ), POINTER :: Ans
!   END FUNCTION lag_elem_refHexahedron
! END INTERFACE

! INTERFACE
!   MODULE PURE FUNCTION lp_refelem_Prism( Obj, Order ) RESULT( Ans )
!     CLASS( ReferencePrism_ ), INTENT( IN ) :: Obj
!     INTEGER( I4B ), INTENT( IN ) :: Order
!     REAL( DFP ), ALLOCATABLE :: Ans( :, : )
!   END FUNCTION lp_refelem_Prism

!   MODULE FUNCTION lag_elem_refPrism(Obj, Order) RESULT( Ans )
!     CLASS( ReferencePrism_ ), INTENT( IN ) :: Obj
!     INTEGER( I4B ), INTENT( IN ) :: Order
!     CLASS( ReferenceElement_ ), POINTER :: Ans
!   END FUNCTION lag_elem_refPrism
! END INTERFACE

! INTERFACE
!   MODULE PURE FUNCTION lp_refelem_Pyramid( Obj, Order ) RESULT( Ans )
!     CLASS( ReferencePyramid_ ), INTENT( IN ) :: Obj
!     INTEGER( I4B ), INTENT( IN ) :: Order
!     REAL( DFP ), ALLOCATABLE :: Ans( :, : )
!   END FUNCTION lp_refelem_Pyramid

!   MODULE FUNCTION lag_elem_refPyramid(Obj, Order) RESULT( Ans )
!     CLASS( ReferencePyramid_ ), INTENT( IN ) :: Obj
!     INTEGER( I4B ), INTENT( IN ) :: Order
!     CLASS( ReferenceElement_ ), POINTER :: Ans
!   END FUNCTION lag_elem_refPyramid
! END INTERFACE


