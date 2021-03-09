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

SUBMODULE( DOF_Method ) IO
USE BaseMethod
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE display_obj
  INTEGER( I4B ) :: I, n, j
  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = stdout
  END IF

  IF( LEN_TRIM( msg ) .NE. 0 ) THEN
    CALL BlankLines( I, 1 )
    WRITE( I, "(A)" ) TRIM( msg )
  END IF

  IF( ALLOCATED( Obj % Map ) ) THEN
    CALL DashLine( UnitNo = I )
    ASSOCIATE( Map => Obj % Map, ValMap => Obj % ValMap )
      n = SIZE( Map, 1 ) - 1
      CALL BlankLines( I, 1 )
      WRITE( I, "(A, I4 )") "Number of Physical Quantities :: ", n
      DO j = 1, n
        CALL BlankLines( I, 1 )
        WRITE( I, "(A)") "Name :: " // CHAR( Map( j, 1 ) )
        IF( Map( j, 2 ) .LT. 0 ) THEN
          WRITE( I, "(A)") "Space Components :: " // "Scalar"
        ELSE
          WRITE( I, "(A, I4)") "Space Components :: ", Map( j, 2 )
        END IF
        WRITE( I, "(A, I4)") "Time Components :: ", Map( j, 3 )
        WRITE( I, "(A, I6)") "Total Nodes :: ", Map( j, 6 )
      END DO
      SELECT CASE( Obj % StorageFMT )
      CASE( dof_FMT )
        WRITE( I, "(A)") "Storage Format :: DOF"
      CASE( Nodes_FMT )
        WRITE( I, "(A)") "Storage Format :: Nodes"
      END SELECT
      CALL Display( Obj % ValMap, "Value Map :: " )
    END ASSOCIATE
    CALL DashLine( UnitNo = I )
  END IF
END PROCEDURE display_obj

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_display_vec
  INTEGER( I4B ) :: I, j, n, tdof, idof, k

  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = stdout
  END IF

  tdof = .tdof. Obj

  CALL Display( Obj, 'Degree of freedom info=', Unitno = I )

  n = SIZE( Obj % Map, 1 ) - 1

  SELECT CASE( Obj % StorageFMT )
  CASE( FMT_Nodes  )

    DO j = 1, n

      CALL BlankLines( UnitNo = I )
      WRITE( I, "(4X, A)" ) "VAR : "//ACHAR( Obj % Map( j, 1 )  )

      DO idof = Obj % Map( j, 5 ), Obj % Map( j+1, 5 ) - 1
        WRITE( I, "( 6X, A )", ADVANCE="NO" ) "--------------"
      END DO
      WRITE( I, "(A)", ADVANCE="YES" ) " "

      DO idof = Obj % Map( j, 5 ), Obj % Map( j+1, 5 ) - 1
        WRITE( I, "(6X, 4X, A, 4X )", ADVANCE="NO" ) "DOF-"//TRIM( INT2STR( idof ) )
      END DO
      WRITE( I, "(A)", ADVANCE="YES" ) " "

      DO idof = Obj % Map( j, 5 ), Obj % Map( j+1, 5 ) - 1
        WRITE( I, "( 6X, A )", ADVANCE="NO" ) "--------------"
      END DO
      WRITE( I, "(A)", ADVANCE="YES" ) " "

      DO k = 1, Obj % Map( j,  6)
        DO idof = Obj % Map( j, 5 ), Obj % Map( j+1, 5 ) - 1
          WRITE( I, "(I6, 2X, G10.2, 2X )", ADVANCE="NO" ) k, &
            & Vec(  ( k - 1 ) * tdof + idof )
        END DO
        WRITE( I, "(A)", ADVANCE="YES" ) " "
      END DO
    END DO

  CASE( FMT_DOF )
  END SELECT
END PROCEDURE dof_display_vec

END SUBMODULE IO