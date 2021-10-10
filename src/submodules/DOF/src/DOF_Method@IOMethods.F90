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
! date: 	28 Feb 2021
! summary: 	This submodule contains IO method for [[DOF_]]

SUBMODULE(DOF_Method) IOMethods
USE BaseMethod
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_Display1
  INTEGER( I4B ) :: n, j

  IF( LEN_TRIM( msg) .NE. 0 ) THEN
    CALL Display( "# "//TRIM( msg ), unitNo=unitNo )
  END IF
  IF( ALLOCATED( obj%Map ) ) THEN
    ASSOCIATE( Map => obj%Map, ValMap => obj%ValMap )
      n = SIZE( Map, 1 ) - 1
      CALL Display( n, "# Total Physical Variables :", unitNo=unitNo )
      DO j = 1, n
        CALL Display("# Name : " // CHAR( Map( j, 1 ) ), unitNo=unitNo )
        IF( Map( j, 2 ) .LT. 0 ) THEN
          CALL Display( "# Space Components : " // "Scalar", unitNo=unitNo)
        ELSE
          CALL Display( Map( j, 2 ), "# Space Components : ", unitNo=unitNo)
        END IF
        CALL Display( Map( j, 3 ), "# Time Components : ", unitNo=unitNo)
        CALL Display( Map( j, 6 ), "# Total Nodes : ", unitNo=unitNo)
      END DO
      SELECT CASE( obj%StorageFMT )
      CASE( DOF_FMT )
        CALL Display( "# Storage Format : DOF", unitNo=unitNo )
      CASE( Nodes_FMT )
        CALL Display( "# Storage Format : Nodes", unitNo=unitNo )
      END SELECT
      CALL Display( obj%ValMap, "# Value Map : ", unitNo=unitNo )
    END ASSOCIATE
  ELSE
    CALL Display( "# obj%Map : NOT ALLOCATED")
  END IF
END PROCEDURE dof_Display1

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_display2
  INTEGER( I4B ) :: jj, tnames, idof, a(3)
  !> main
  CALL Display( obj, '# DOF data : ', unitNo = unitNo  )
  tnames = .tNames. obj
  DO jj = 1, tnames
    CALL Display( ACHAR(obj%Map(jj,1)), "# VAR : ", unitNo)
    DO idof = obj%Map( jj, 5 ), obj%Map( jj+1, 5 ) - 1
      a = getNodeLOC( obj=obj, idof=idof )
      CALL Display( Vec(a(1):a(2):a(3)),  &
        & msg="DOF-"//TOSTRING( idof ), unitNo=unitNo, advance="NO" )
    END DO
    CALL Display( " ", unitNo=unitNo, advance=.TRUE. )
  END DO
END PROCEDURE dof_display2

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_display3
  IF( ALLOCATED( vec%val )  ) THEN
    CALL Display( vec=vec%val, obj=obj, msg=msg, unitNo=unitNo  )
  END IF
END PROCEDURE dof_display3
END SUBMODULE IOMethods