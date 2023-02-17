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

!> author: Vikas Sharma, Ph. D.
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
  CALL Display( msg, unitno=unitno )
  CALL Display( "# SHAPE FUNCTION IN SPACE: ", unitno=unitno )
  CALL Display( obj%Quad, "# Quadrature Point: ", unitno=unitno )
  !!
  IF( ALLOCATED( obj%N ) ) THEN
    CALL Display( obj%N, "# N: ", unitno )
  ELSE
    CALL Display( "# N: NOT ALLOCATED", unitno )
  END IF
  !!
  IF( ALLOCATED( obj%dNdXi ) ) THEN
    CALL Display( obj%dNdXi, "# dNdXi: ", unitno )
  ELSE
    CALL Display( "# dNdXi: NOT ALLOCATED", unitno )
  END IF
  !!
  IF( ALLOCATED( obj%dNdXt ) ) THEN
    CALL Display( obj%dNdXt, "# dNdXt: ", unitno )
  ELSE
    CALL Display( "# dNdXt: NOT ALLOCATED", unitno )
  END IF
  !!
  IF( ALLOCATED( obj%jacobian ) ) THEN
    CALL Display( obj%Jacobian, "# jacobian: ", unitno )
  ELSE
    CALL Display( "# jacobian: NOT ALLOCATED", unitno )
  END IF
  !!
  IF( ALLOCATED( obj%js ) ) THEN
    CALL Display( obj%js, "# js: ", unitno )
  ELSE
    CALL Display( "# js: NOT ALLOCATED", unitno )
  END IF
  !!
  IF( ALLOCATED( obj%thickness ) ) THEN
    CALL Display( obj%thickness, "# thickness: ", unitno )
  ELSE
    CALL Display( "# thickness: NOT ALLOCATED", unitno )
  END IF
  !!
  IF( ALLOCATED( obj%coord ) ) THEN
    CALL Display( obj%coord, "# coord: ", unitno )
  ELSE
    CALL Display( "# coord: NOT ALLOCATED", unitno )
  END IF
  !!
  IF( ALLOCATED( obj%normal ) ) THEN
    CALL Display( obj%normal, "# normal: ", unitno )
  ELSE
    CALL Display( "# normal: NOT ALLOCATED", unitno )
  END IF
  !!
  SELECT TYPE( obj ); TYPE IS (STElemShapeData_)
    CALL Display( "# SHAPE FUNCTION IN TIME: ", unitno=unitno )
    !!
    CALL Display( obj%jt, "# jt: ", unitno=unitno  )
    CALL Display( obj%theta, "# theta: ", unitno=unitno  )
    CALL Display( obj%wt, "# wt: ", unitno=unitno  )
    !!
    IF( ALLOCATED( obj%T ) ) THEN
      CALL Display( obj%T, "# T: ", unitno=unitno )
    ELSE
      CALL Display( "# T: NOT ALLOCATED", unitno=unitno )
    END IF
    !!
    IF( ALLOCATED( obj%dTdTheta ) ) THEN
      CALL Display( obj%dTdTheta, "# dTdTheta: ", unitno=unitno )
    ELSE
      CALL Display( "# dTdTheta: NOT ALLOCATED", unitno=unitno )
    END IF
    !!
    IF( ALLOCATED( obj%dNTdt ) ) THEN
      CALL Display( obj%dNTdt, "# dNTdt: ", unitno=unitno )
    ELSE
      CALL Display( "# dNTdt: NOT ALLOCATED", unitno=unitno )
    END IF
    !!
    IF( ALLOCATED( obj%dNTdXt ) ) THEN
      CALL Display( obj%dNTdXt, "# dNTdXt: ", unitno=unitno )
    ELSE
      CALL Display( "# dNTdXt: NOT ALLOCATED", unitno=unitno )
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