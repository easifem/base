SUBMODULE( ElemshapeData_Method ) Constructor
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj
  CALL reallocate( Obj % N, nns, nips )
  CALL reallocate( Obj % dNdXi, nns, xidim, nips )
  CALL reallocate( Obj % Normal, 3, nips )
  CALL reallocate( Obj % dNdXt, nns, nsd, nips )
  CALL reallocate( Obj % Jacobian, nsd, xidim, nips )
  CALL reallocate( Obj % Js, nips )
  CALL reallocate( Obj % Thickness, nips )
  Obj % Thickness = 1.0_DFP
  CALL reallocate( Obj % Coord, nsd, nips )
END PROCEDURE initiate_obj

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_initiate
  INTEGER( I4B ) :: tip, ip
  REAL( DFP ) :: x( 3 )

  tip = SIZE( elemsd % N, 2 )
  IF( ALLOCATED( obj ) ) DEALLOCATE( obj )
  ALLOCATE( obj( tip ) )

  DO ip = 1, tip
    Obj( ip ) % T = elemsd % N( :, ip )
    Obj( ip ) % dTdTheta = elemsd % dNdXi( :, 1, ip )
    Obj( ip ) % Jt = elemsd % Js( ip )
    CALL getQuadraturePoints( Obj = elemsd % Quad, Weight = Obj( ip ) % Wt,&
      & Num = ip, Point = x )
    Obj( ip ) % Theta = x( 1 )
  END DO
END PROCEDURE stsd_initiate

!----------------------------------------------------------------------------
!                                                              DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocate_data

  IF( ALLOCATED( Obj % Normal ) ) DEALLOCATE( Obj % Normal )
  IF( ALLOCATED( Obj % N ) ) DEALLOCATE( Obj % N )
  IF( ALLOCATED( Obj % dNdXi ) ) DEALLOCATE( Obj % dNdXi )
  IF( ALLOCATED( Obj % dNdXt ) ) DEALLOCATE( Obj % dNdXt )
  IF( ALLOCATED( Obj % Jacobian ) ) DEALLOCATE( Obj % Jacobian )
  IF( ALLOCATED( Obj % Js ) ) DEALLOCATE( Obj % Js )
  IF( ALLOCATED( Obj % Ws ) ) DEALLOCATE( Obj % Ws )
  IF( ALLOCATED( Obj % Thickness ) ) DEALLOCATE( Obj % Thickness )
  IF( ALLOCATED( Obj % Coord ) ) DEALLOCATE( Obj % Coord )
  CALL DeallocateData( Obj % Quad )
  CALL DeallocateData( Obj % RefElem )

  SELECT TYPE( Obj )
  TYPE IS (STElemShapeData_)
    IF( ALLOCATED( Obj % T ) ) DEALLOCATE( Obj % T )
    IF( ALLOCATED( Obj % dTdTheta ) ) DEALLOCATE( Obj % dTdTheta )
    IF( ALLOCATED( Obj % dNTdt ) ) DEALLOCATE( Obj % dNTdt )
    IF( ALLOCATED( Obj % dNTdXt ) ) DEALLOCATE( Obj % dNTdXt )
  END SELECT
END PROCEDURE deallocate_data

!------------------------------------------------------------------------------
!                                                                      Display
!------------------------------------------------------------------------------

MODULE PROCEDURE display_obj
  INTEGER( I4B ) :: I

  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = stdout
  END IF

  CALL Blanklines( UnitNo = I )
  CALL Display( msg, UnitNo = I )

  CALL Blanklines( UnitNo = I )
  CALL Display( "SHAPE FUNCTION IN SPACE ::", UnitNo = I )

  CALL Display( Obj % Quad, "Quadrature Point", I )

  IF( ALLOCATED( Obj % N ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( Obj % N, "Obj % N", I )
  END IF

  IF( ALLOCATED( Obj % dNdXi ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( Obj % dNdXi, "Obj % dNdXi", I )
  END IF

  IF( ALLOCATED( Obj % dNdXt ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( Obj % dNdXt, "Obj % dNdXt", I )
  END IF

  IF( ALLOCATED( Obj % Jacobian ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( Obj % Jacobian, "Obj % Jacobian", I )
  END IF

  IF( ALLOCATED( Obj % Js ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( Obj % Js, "Obj % Js", I )
  END IF

  IF( ALLOCATED( Obj % Thickness ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( Obj % Thickness, "Obj % Thickness", I )
  END IF

  IF( ALLOCATED( Obj % Coord ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( Obj % Coord, "Obj % Coord", I )
  END IF

  IF( ALLOCATED( Obj % Normal ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( Obj % Normal, "Obj % Normal", I )
  END IF

  SELECT TYPE( Obj )

  TYPE IS (STElemShapeData_)

    CALL Blanklines( UnitNo = I )
    CALL Display( "SHAPE FUNCTION IN TIME ::", UnitNo = I )

    CALL Blanklines( UnitNo = I )
    CALL Display( Obj%Jt, "Obj % Jt :: ", UnitNo = I  )
    CALL Display( Obj%Theta, "Obj % Theta :: ", UnitNo = I  )
    CALL Display( Obj%Wt, "Obj % Wt :: ", UnitNo = I  )

    IF( ALLOCATED( Obj % T ) ) THEN
      CALL Blanklines( UnitNo = I )
      CALL Display( Obj % T, "Obj % T", UnitNo = I )
    END IF

    IF( ALLOCATED( Obj % dTdTheta ) ) THEN
    CALL Blanklines( UnitNo = I )
      CALL Display( Obj % dTdTheta, "Obj % dTdTheta", UnitNo = I )
    END IF

    IF( ALLOCATED( Obj % dNTdt ) ) THEN
      CALL Blanklines( UnitNo = I )
      CALL Display( Obj % dNTdt, "Obj % dNTdt", UnitNo = I )
    END IF

    IF( ALLOCATED( Obj % dNTdXt ) ) THEN
      CALL Blanklines( UnitNo = I )
      CALL Display( Obj % dNTdXt, "Obj % dNTdXt", UnitNo = I )
    END IF
  END SELECT

END PROCEDURE display_obj

END SUBMODULE Constructor

