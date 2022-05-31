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
! summary: Methods for IO of [[elemshapedata_]] and [[stelemshapedata_]]

SUBMODULE(ElemshapeData_Method) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_display_1
  !!
  CALL Blanklines( unitno=unitno )
  !!
  CALL Display( msg, unitno=unitno )
  !!
  CALL Blanklines( unitno=unitno )
  !!
  CALL Display( "# SHAPE FUNCTION IN SPACE: ", unitno=unitno )
  !!
  CALL Display( obj%Quad, "# Quadrature Point: ", unitno=unitno )
  !!
  IF( ALLOCATED( obj%N ) ) THEN
    CALL Blanklines( unitno=unitno )
    CALL Display( obj%N, "# obj%N: ", unitno )
  ELSE
    CALL Display( "# obj%N: NOT ALLOCATED", unitno )
  END IF
  !!
  IF( ALLOCATED( obj%dNdXi ) ) THEN
    CALL Blanklines( unitno=unitno )
    CALL Display( obj%dNdXi, "# obj%dNdXi: ", unitno )
  ELSE
    CALL Display( "# obj%dNdXi: NOT ALLOCATED", unitno )
  END IF
  !!
  IF( ALLOCATED( obj%dNdXt ) ) THEN
    CALL Blanklines( unitno=unitno )
    CALL Display( obj%dNdXt, "# obj%dNdXt: ", unitno )
  ELSE
    CALL Display( "# obj%dNdXt: NOT ALLOCATED", unitno )
  END IF
  !!
  IF( ALLOCATED( obj%Jacobian ) ) THEN
    CALL Blanklines( unitno=unitno )
    CALL Display( obj%Jacobian, "# obj%Jacobian: ", unitno )
  ELSE
    CALL Display( "# obj%Jacobian: NOT ALLOCATED", unitno )
  END IF
  !!
  IF( ALLOCATED( obj%Js ) ) THEN
    CALL Blanklines( unitno=unitno )
    CALL Display( obj%Js, "# obj%Js: ", unitno )
  ELSE
    CALL Display( "# obj%Js: NOT ALLOCATED", unitno )
  END IF
  !!
  IF( ALLOCATED( obj%Thickness ) ) THEN
    CALL Blanklines( unitno=unitno )
    CALL Display( obj%Thickness, "# obj%Thickness: ", unitno )
  ELSE
    CALL Display( "# obj%Thickness: NOT ALLOCATED", unitno )
  END IF
  !!
  IF( ALLOCATED( obj%Coord ) ) THEN
    CALL Blanklines( unitno=unitno )
    CALL Display( obj%Coord, "# obj%Coord: ", unitno )
  ELSE
    CALL Display( "# obj%Coord: NOT ALLOCATED", unitno )
  END IF
  !!
  IF( ALLOCATED( obj%Normal ) ) THEN
    CALL Blanklines( unitno=unitno )
    CALL Display( obj%Normal, "# obj%Normal: ", unitno )
  ELSE
    CALL Display( "# obj%Normal: NOT ALLOCATED", unitno )
  END IF
  !!
  SELECT TYPE( obj )
  TYPE IS (STElemShapeData_)
    CALL Blanklines( unitno=unitno )
    CALL Display( "# SHAPE FUNCTION IN TIME: ", unitno=unitno )
    CALL Blanklines( unitno=unitno )
    !!
    CALL Display( obj%Jt, "# obj%Jt: ", unitno=unitno  )
    CALL Display( obj%Theta, "# obj%Theta: ", unitno=unitno  )
    CALL Display( obj%Wt, "# obj%Wt: ", unitno=unitno  )
    !!
    IF( ALLOCATED( obj%T ) ) THEN
      CALL Blanklines( unitno=unitno )
      CALL Display( obj%T, "# obj%T: ", unitno=unitno )
    ELSE
      CALL Display( "# obj%T: NOT ALLOCATED", unitno=unitno )
    END IF
    !!
    IF( ALLOCATED( obj%dTdTheta ) ) THEN
    CALL Blanklines( unitno=unitno )
      CALL Display( obj%dTdTheta, "# obj%dTdTheta: ", unitno=unitno )
    ELSE
      CALL Display( "# obj%dTdTheta: NOT ALLOCATED", unitno=unitno )
    END IF
    !!
    IF( ALLOCATED( obj%dNTdt ) ) THEN
      CALL Blanklines( unitno=unitno )
      CALL Display( obj%dNTdt, "# obj%dNTdt: ", unitno=unitno )
    ELSE
      CALL Display( "# obj%dNTdt: NOT ALLOCATED", unitno=unitno )
    END IF
    !!
    IF( ALLOCATED( obj%dNTdXt ) ) THEN
      CALL Blanklines( unitno=unitno )
      CALL Display( obj%dNTdXt, "# obj%dNTdXt: ", unitno=unitno )
    ELSE
      CALL Display( "# obj%dNTdXt: NOT ALLOCATED", unitno=unitno )
    END IF
    !!
  END SELECT
  !!
END PROCEDURE elemsd_display_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_display_2
  INTEGER( I4B ) :: ii
  !!
  DO ii = 1, SIZE(obj)
    CALL Display( obj=obj(ii), msg=trim(msg)// "("//tostring(ii)//"): ", &
      & unitno=unitno )
  END DO
  !!
END PROCEDURE elemsd_display_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods