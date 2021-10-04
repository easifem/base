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

SUBMODULE(DOF_Method) IO
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
        CALL Display("# Name : " // CHAR( Map( j, 1 ) ), unitNo=UnitNo )
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
  INTEGER( I4B ) :: I, j, n, tdof, idof, k
  !> main
  tdof = .tdof. obj
  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = stdout
  END IF
  CALL Display( obj, 'Degree of freedom info=', Unitno = I )
  n = SIZE( obj%Map, 1 ) - 1
  SELECT CASE( obj%StorageFMT )
  CASE( FMT_Nodes  )
    DO j = 1, n
      CALL BlankLines( UnitNo = I )
      WRITE( I, "(4X, A)" ) "VAR : "//ACHAR( obj%Map( j, 1 )  )
      DO idof = obj%Map( j, 5 ), obj%Map( j+1, 5 ) - 1
        WRITE( I, "( 6X, A )", ADVANCE="NO" ) "--------------"
      END DO
      WRITE( I, "(A)", ADVANCE="YES" ) " "
      DO idof = obj%Map( j, 5 ), obj%Map( j+1, 5 ) - 1
        WRITE( I, "(6X, 4X, A, 4X )", ADVANCE="NO" ) "DOF-"//TRIM( INT2STR( idof ) )
      END DO
      WRITE( I, "(A)", ADVANCE="YES" ) " "
      DO idof = obj%Map( j, 5 ), obj%Map( j+1, 5 ) - 1
        WRITE( I, "( 6X, A )", ADVANCE="NO" ) "--------------"
      END DO
      WRITE( I, "(A)", ADVANCE="YES" ) " "
      DO k = 1, obj%Map( j,  6)
        DO idof = obj%Map( j, 5 ), obj%Map( j+1, 5 ) - 1
          WRITE( I, "(I6, 2X, G10.2, 2X )", ADVANCE="NO" ) k, &
            & Vec(  ( k - 1 ) * tdof + idof )
        END DO
        WRITE( I, "(A)", ADVANCE="YES" ) " "
      END DO
    END DO
  CASE( FMT_DOF )
  END SELECT
END PROCEDURE dof_display2

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_display3
  IF( ALLOCATED( vec%val )  ) THEN
    CALL Display( vec=vec%val, obj=obj, msg=msg, unitNo=unitNo  )
  END IF
END PROCEDURE dof_display3
END SUBMODULE IO