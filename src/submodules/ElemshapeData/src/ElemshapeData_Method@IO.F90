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

SUBMODULE(ElemshapeData_Method) IO
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
  CALL Display( obj%Quad, "Quadrature Point", I )
  IF( ALLOCATED( obj%N ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( obj%N, "obj%N", I )
  END IF
  IF( ALLOCATED( obj%dNdXi ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( obj%dNdXi, "obj%dNdXi", I )
  END IF
  IF( ALLOCATED( obj%dNdXt ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( obj%dNdXt, "obj%dNdXt", I )
  END IF
  IF( ALLOCATED( obj%Jacobian ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( obj%Jacobian, "obj%Jacobian", I )
  END IF
  IF( ALLOCATED( obj%Js ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( obj%Js, "obj%Js", I )
  END IF
  IF( ALLOCATED( obj%Thickness ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( obj%Thickness, "obj%Thickness", I )
  END IF
  IF( ALLOCATED( obj%Coord ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( obj%Coord, "obj%Coord", I )
  END IF
  IF( ALLOCATED( obj%Normal ) ) THEN
    CALL Blanklines( UnitNo = I )
    CALL Display( obj%Normal, "obj%Normal", I )
  END IF
  SELECT TYPE( obj )
  TYPE IS (STElemShapeData_)
    CALL Blanklines( UnitNo = I )
    CALL Display( "SHAPE FUNCTION IN TIME ::", UnitNo = I )
    CALL Blanklines( UnitNo = I )
    CALL Display( obj%Jt, "obj%Jt :: ", UnitNo = I  )
    CALL Display( obj%Theta, "obj%Theta :: ", UnitNo = I  )
    CALL Display( obj%Wt, "obj%Wt :: ", UnitNo = I  )
    IF( ALLOCATED( obj%T ) ) THEN
      CALL Blanklines( UnitNo = I )
      CALL Display( obj%T, "obj%T", UnitNo = I )
    END IF
    IF( ALLOCATED( obj%dTdTheta ) ) THEN
    CALL Blanklines( UnitNo = I )
      CALL Display( obj%dTdTheta, "obj%dTdTheta", UnitNo = I )
    END IF
    IF( ALLOCATED( obj%dNTdt ) ) THEN
      CALL Blanklines( UnitNo = I )
      CALL Display( obj%dNTdt, "obj%dNTdt", UnitNo = I )
    END IF
    IF( ALLOCATED( obj%dNTdXt ) ) THEN
      CALL Blanklines( UnitNo = I )
      CALL Display( obj%dNTdXt, "obj%dNTdXt", UnitNo = I )
    END IF
  END SELECT
END PROCEDURE display_obj

END SUBMODULE IO