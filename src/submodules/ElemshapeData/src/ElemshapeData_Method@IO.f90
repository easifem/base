! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This submodule contains the methods for IO of [[elemshapedata_]] and [[stelemshapedata_]]
!

SUBMODULE( ElemshapeData_Method ) IO
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

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
  CALL Display( Obj%Quad, "Quadrature Point", I )
  IF( ALLOCATED( Obj%N ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( Obj%N, "Obj%N", I )
  END IF
  IF( ALLOCATED( Obj%dNdXi ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( Obj%dNdXi, "Obj%dNdXi", I )
  END IF
  IF( ALLOCATED( Obj%dNdXt ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( Obj%dNdXt, "Obj%dNdXt", I )
  END IF
  IF( ALLOCATED( Obj%Jacobian ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( Obj%Jacobian, "Obj%Jacobian", I )
  END IF
  IF( ALLOCATED( Obj%Js ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( Obj%Js, "Obj%Js", I )
  END IF
  IF( ALLOCATED( Obj%Thickness ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( Obj%Thickness, "Obj%Thickness", I )
  END IF
  IF( ALLOCATED( Obj%Coord ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( Obj%Coord, "Obj%Coord", I )
  END IF
  IF( ALLOCATED( Obj%Normal ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( Obj%Normal, "Obj%Normal", I )
  END IF
  SELECT TYPE( Obj )
  TYPE IS (STElemShapeData_)
    CALL Blanklines( UnitNo = I )
    CALL Display( "SHAPE FUNCTION IN TIME ::", UnitNo = I )
    CALL Blanklines( UnitNo = I )
    CALL Display( Obj%Jt, "Obj%Jt :: ", UnitNo = I  )
    CALL Display( Obj%Theta, "Obj%Theta :: ", UnitNo = I  )
    CALL Display( Obj%Wt, "Obj%Wt :: ", UnitNo = I  )
    IF( ALLOCATED( Obj%T ) ) THEN
      CALL Blanklines( UnitNo = I )
      CALL Display( Obj%T, "Obj%T", UnitNo = I )
    END IF
    IF( ALLOCATED( Obj%dTdTheta ) ) THEN
    CALL Blanklines( UnitNo = I )
      CALL Display( Obj%dTdTheta, "Obj%dTdTheta", UnitNo = I )
    END IF
    IF( ALLOCATED( Obj%dNTdt ) ) THEN
      CALL Blanklines( UnitNo = I )
      CALL Display( Obj%dNTdt, "Obj%dNTdt", UnitNo = I )
    END IF
    IF( ALLOCATED( Obj%dNTdXt ) ) THEN
      CALL Blanklines( UnitNo = I )
      CALL Display( Obj%dNTdXt, "Obj%dNTdXt", UnitNo = I )
    END IF
  END SELECT
END PROCEDURE display_obj

END SUBMODULE IO