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
SUBMODULE( txtFile_Method ) WriteMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 SaveFile
!----------------------------------------------------------------------------

MODULE PROCEDURE save_to_file_r0
  SELECT TYPE( Val )
  TYPE IS( CHARACTER( * ) )
    WRITE( Obj%UnitNo, "(A)") TRIM(Val)
  TYPE IS( Real(Real64) )
    WRITE( Obj%UnitNo, "(A)" ) STR(Val)
  TYPE IS( Real(Real32) )
    WRITE( Obj%UnitNo, "(A)" ) STR(Val)
  TYPE IS( Integer(INT32) )
    WRITE( Obj%UnitNo, "(A)" ) STR(Val)
  TYPE IS( Integer(INT64) )
    WRITE( Obj%UnitNo, "(A)" ) STR(Val)
  END SELECT
END PROCEDURE save_to_file_r0

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE write_data_ascii_r1
  INTEGER( I4B ) :: ii

  SELECT TYPE( Val )
  TYPE IS ( Integer(Int32) )
    IF( PRESENT( col ) ) THEN
      DO ii =1, SIZE(Val)
        WRITE(Obj%UnitNo, "(A)") STR(Val( ii ))
      END DO
      RETURN
    END IF

    WRITE(Obj%UnitNo, "(A)") STR(Val, Separator = Obj%Separator)

  TYPE IS ( Integer(Int64) )
    IF( PRESENT( col ) ) THEN
      DO ii =1, SIZE(Val)
        WRITE(Obj%UnitNo, "(A)") STR(Val( ii ))
      END DO
      RETURN
    END IF

    WRITE(Obj%UnitNo, "(A)") STR(Val, Separator = Obj%Separator)

  TYPE IS ( Real(Real32) )
    IF( PRESENT( col ) ) THEN
      DO ii =1, SIZE(Val)
        WRITE(Obj%UnitNo, "(A)") STR(Val( ii ))
      END DO
      RETURN
    END IF

    WRITE(Obj%UnitNo, "(A)") STR(Val, Separator = Obj%Separator)

  TYPE IS ( Real(Real64) )
    IF( PRESENT( col ) ) THEN
      DO ii =1, SIZE(Val)
        WRITE(Obj%UnitNo, "(A)") STR(Val( ii ))
      END DO
      RETURN
    END IF

    WRITE(Obj%UnitNo, "(A)") STR(Val, Separator = Obj%Separator)
  END SELECT

END PROCEDURE write_data_ascii_r1

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE write_data_ascii_r2
  INTEGER( I4B ) :: ii, jj, m, n

  SELECT TYPE( Val )

  TYPE IS ( Integer(Int32) )
    ! print column wise
    m = SIZE( Val, 1 )
    n = SIZE( Val, 2 )
    IF(transpose) THEN

      DO jj=1,n
        DO ii = 1, m
          IF( ii .EQ. m ) THEN
            WRITE(Obj%UnitNo, "(A)", ADVANCE="YES" ) TRIM(STR(val( ii, jj )))
          ELSE
            WRITE(Obj%UnitNo, "(A)", ADVANCE="NO" ) &
              & TRIM(STR(val( ii, jj ))) // Obj%Separator
          END IF
        END DO
      END DO

    ELSE
      ! print rowwise
      DO ii =1, m
        DO jj=1,n
          IF( jj .EQ. n ) THEN
            WRITE(Obj%UnitNo, "(A)", ADVANCE="YES" ) TRIM(STR(val( ii, jj )))
          ELSE
            WRITE(Obj%UnitNo, "(A)", ADVANCE="NO" ) &
              & TRIM(STR(val( ii, jj ))) // Obj%Separator
          END IF
        END DO
      END DO
    END IF

  TYPE IS ( Integer(Int64) )
    ! print column wise
    m = SIZE( Val, 1 )
    n = SIZE( Val, 2 )
    IF(transpose) THEN

      DO jj=1,n
        DO ii = 1, m
          IF( ii .EQ. m ) THEN
            WRITE(Obj%UnitNo, "(A)", ADVANCE="YES" ) TRIM(STR(val( ii, jj )))
          ELSE
            WRITE(Obj%UnitNo, "(A)", ADVANCE="NO" ) &
              & TRIM(STR(val( ii, jj ))) // Obj%Separator
          END IF
        END DO
      END DO

    ELSE
      ! print rowwise
      DO ii =1, m
        DO jj=1,n
          IF( jj .EQ. n ) THEN
            WRITE(Obj%UnitNo, "(A)", ADVANCE="YES" ) TRIM(STR(val( ii, jj )))
          ELSE
            WRITE(Obj%UnitNo, "(A)", ADVANCE="NO" ) &
              & TRIM(STR(val( ii, jj ))) // Obj%Separator
          END IF
        END DO
      END DO
    END IF

  TYPE IS ( Real(Real32) )
    ! print column wise
    m = SIZE( Val, 1 )
    n = SIZE( Val, 2 )
    IF(transpose) THEN

      DO jj=1,n
        DO ii = 1, m
          IF( ii .EQ. m ) THEN
            WRITE(Obj%UnitNo, "(A)", ADVANCE="YES" ) TRIM(STR(val( ii, jj )))
          ELSE
            WRITE(Obj%UnitNo, "(A)", ADVANCE="NO" ) &
              & TRIM(STR(val( ii, jj ))) // Obj%Separator
          END IF
        END DO
      END DO

    ELSE
      ! print rowwise
      DO ii =1, m
        DO jj=1,n
          IF( jj .EQ. n ) THEN
            WRITE(Obj%UnitNo, "(A)", ADVANCE="YES" ) TRIM(STR(val( ii, jj )))
          ELSE
            WRITE(Obj%UnitNo, "(A)", ADVANCE="NO" ) &
              & TRIM(STR(val( ii, jj ))) // Obj%Separator
          END IF
        END DO
      END DO
    END IF

  TYPE IS ( Real(Real64) )
    ! print column wise
    m = SIZE( Val, 1 )
    n = SIZE( Val, 2 )
    IF(transpose) THEN

      DO jj=1,n
        DO ii = 1, m
          IF( ii .EQ. m ) THEN
            WRITE(Obj%UnitNo, "(A)", ADVANCE="YES" ) TRIM(STR(val( ii, jj )))
          ELSE
            WRITE(Obj%UnitNo, "(A)", ADVANCE="NO" ) &
              & TRIM(STR(val( ii, jj ))) // Obj%Separator
          END IF
        END DO
      END DO

    ELSE
      ! print rowwise
      DO ii =1, m
        DO jj=1,n
          IF( jj .EQ. n ) THEN
            WRITE(Obj%UnitNo, "(A)", ADVANCE="YES" ) TRIM(STR(val( ii, jj )))
          ELSE
            WRITE(Obj%UnitNo, "(A)", ADVANCE="NO" ) &
              & TRIM(STR(val( ii, jj ))) // Obj%Separator
          END IF
        END DO
      END DO
    END IF
  END SELECT

END PROCEDURE write_data_ascii_r2

END SUBMODULE WriteMethods