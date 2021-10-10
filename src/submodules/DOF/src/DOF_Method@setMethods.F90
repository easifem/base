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

SUBMODULE(DOF_Method) setMethods
  !! This submodule defines the methods for setting/changing values in
  !! fortran real vector using [[dof_]] object

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_set1
  INTEGER( I4B ) :: tdof, idof, i, n, m

  tdof = .tdof. obj
  n = SIZE( Nptrs ); m = SIZE( Val )

  ASSOCIATE( vm => obj%ValMap )
    SELECT CASE( obj% StorageFMT )
    CASE( dof_FMT )
      IF( m .NE. n ) THEN
        IF( m .EQ. 1 ) THEN
          DO idof = 1, tdof
            Vec( vm( idof ) - 1 + Nptrs ) = Val( 1 )
          END DO
        ELSE IF( m .EQ. tdof * n ) THEN
          IF( Conversion( 1 ) .EQ. nodesToDOF ) THEN
            DO idof = 1, tdof
              DO i = 1, n
                Vec( vm(idof)-1+Nptrs( i ) ) = Val( ( i - 1 ) * tdof + idof  )
              END DO
            END DO
          ELSE
            DO idof = 1, tdof
              Vec( vm(idof)-1+Nptrs) = Val( ( idof - 1 ) * n + 1 : idof * n )
            END DO
          END IF
        END IF
      ELSE
        DO idof = 1, tdof
          Vec( vm( idof ) - 1 + Nptrs ) = Val( : )
        END DO
      END IF
    !
    CASE( Nodes_FMT )
      IF( m .NE. n ) THEN
        IF( m .EQ. 1 ) THEN
          DO idof = 1, tdof
            Vec( (Nptrs - 1 ) * tdof + idof ) = Val( 1 )
          END DO
        ELSE IF( m .EQ. tdof * n ) THEN
          IF( Conversion( 1 ) .EQ. DOFToNodes ) THEN
            DO idof = 1, tdof
              DO i = 1, n
                Vec( ( Nptrs( i ) - 1 ) * tdof + idof ) &
                  & = Val( ( idof - 1 ) * n + i  )
              END DO
            END DO
          ELSE
            DO idof = 1, tdof
              DO i = 1, n
                Vec( ( Nptrs( i ) - 1 ) * tdof + idof ) &
                  & = Val( ( i - 1 ) * tdof + idof )
              END DO
            END DO
          END IF
        END IF
      ELSE
        DO idof = 1, tdof
          Vec( (Nptrs - 1) * tdof + idof ) = Val( : )
        END DO
      END IF
    END SELECT
  END ASSOCIATE
END PROCEDURE dof_set1

!----------------------------------------------------------------------------
!                                                                 setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_set2
  INTEGER( I4B ) :: n, m, tdof
  tdof = obj%Map( SIZE( obj%Map, 1 ), 4 ); n = SIZE(Nptrs); m=SIZE(Val)
  ASSOCIATE( vm => obj%ValMap )
    SELECT CASE( obj%StorageFMT )
    CASE( dof_FMT )
      IF( m .EQ. n ) THEN
        Vec( vm( dofno ) - 1 + Nptrs ) = Val( : )
      ELSE
        Vec( vm( dofno ) - 1 + Nptrs ) = Val( 1 )
      END IF
    CASE( Nodes_FMT )
      IF( m .EQ. n ) THEN
        Vec( ( Nptrs - 1 ) * tdof + dofno ) = Val( : )
      ELSE
        Vec( ( Nptrs - 1 ) * tdof + dofno ) = Val( 1 )
      END IF
    END SELECT
  END ASSOCIATE
END PROCEDURE dof_set2

!----------------------------------------------------------------------------
!                                                                 set
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_set3
  CALL set( Vec%val, obj, Nptrs, Val, Conversion )
END PROCEDURE dof_set3

!----------------------------------------------------------------------------
!                                                                 set
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_set4
  CALL set( Vec%val, obj, Nptrs, Val, dofno )
END PROCEDURE dof_set4

!----------------------------------------------------------------------------
!                                                            AddContribution
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_add1
  INTEGER( I4B ) :: tdof, idof, i, n, m

  tdof = .tdof. obj
  n = SIZE( Nptrs ); m = SIZE( Val )

  ASSOCIATE( vm => obj%ValMap )
    SELECT CASE( obj% StorageFMT )
    CASE( dof_FMT )
      IF( m .NE. n ) THEN
        ! Vec( Nptrs ) += scale * Val( 1 )
        IF( m .EQ. 1 ) THEN
          DO idof = 1, tdof
            Vec( vm( idof ) - 1 + Nptrs ) &
              & = Vec( vm( idof ) - 1 + Nptrs ) &
              & + scale * Val( 1 )
          END DO

        ! Vec_dof_i( Nptrs ) += scale * Val_dof_i( : )
        ELSE IF( m .EQ. tdof * n ) THEN

          IF( Conversion( 1 ) .EQ. nodesToDOF ) THEN

            DO idof = 1, tdof
              DO i = 1, n
                Vec( vm(idof)-1+Nptrs( i ) ) &
                  & = Vec( vm(idof)-1+Nptrs( i ) ) &
                  & + scale * Val( ( i - 1 ) * tdof +idof)
              END DO
            END DO

          ELSE

            DO idof = 1, tdof
              Vec( vm(idof)-1+Nptrs) &
                & = Vec( vm(idof)-1+Nptrs) &
                & + scale * Val( ( idof - 1 ) * n + 1 : idof * n )
            END DO

          END IF

        END IF

      ! Vec_dof_i(Nptrs) += scale * Val
      ELSE

        DO idof = 1, tdof
          Vec( vm( idof ) - 1 + Nptrs ) &
            & = Vec( vm( idof ) - 1 + Nptrs ) &
            & + scale * Val( : )
        END DO

      END IF
    !
    CASE( Nodes_FMT )

      IF( m .NE. n ) THEN

        IF( m .EQ. 1 ) THEN

          DO idof = 1, tdof
            Vec( (Nptrs - 1 ) * tdof + idof ) &
              & = Vec( (Nptrs - 1 ) * tdof + idof ) &
              & + scale * Val( 1 )
          END DO

        ELSE IF( m .EQ. tdof * n ) THEN

          IF( Conversion( 1 ) .EQ. DOFToNodes ) THEN

            DO idof = 1, tdof
              DO i = 1, n
                Vec( ( Nptrs( i ) - 1 ) * tdof + idof ) &
                  & = Vec( ( Nptrs( i ) - 1 ) * tdof + idof ) &
                  & + scale * Val( ( idof - 1 ) * n + i  )
              END DO
            END DO

          ELSE

            DO idof = 1, tdof
              DO i = 1, n
                Vec( ( Nptrs( i ) - 1 ) * tdof + idof ) &
                  & = Vec( ( Nptrs( i ) - 1 ) * tdof + idof ) &
                  & + scale * Val( ( i - 1 ) * tdof + idof )
              END DO
            END DO

          END IF
        END IF

      ELSE

        DO idof = 1, tdof
          Vec( (Nptrs - 1) * tdof + idof ) &
            & = Vec( (Nptrs - 1) * tdof + idof ) &
            & + scale * Val( : )
        END DO

      END IF

    END SELECT
  END ASSOCIATE
END PROCEDURE dof_add1

!----------------------------------------------------------------------------
!                                                            AddContribution
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_add2
  INTEGER( I4B ) :: n, m, tdof

  tdof = obj%Map( SIZE( obj%Map, 1 ), 4 ); n = SIZE(Nptrs); m=SIZE(Val)

  ASSOCIATE( vm => obj%ValMap )
    SELECT CASE( obj%StorageFMT )

    CASE( dof_FMT )

      IF( m .EQ. n ) THEN
        Vec( vm( dofno ) - 1 + Nptrs ) &
          & = Vec( vm( dofno ) - 1 + Nptrs ) &
          & + scale * Val( : )
      ELSE
        Vec( vm( dofno ) - 1 + Nptrs ) &
          & = Vec( vm( dofno ) - 1 + Nptrs ) &
          & + scale * Val( 1 )
      END IF

    CASE( Nodes_FMT )

      IF( m .EQ. n ) THEN
        Vec( ( Nptrs - 1 ) * tdof + dofno ) &
          & = Vec( ( Nptrs - 1 ) * tdof + dofno ) &
          & + scale * Val( : )
      ELSE
        Vec( ( Nptrs - 1 ) * tdof + dofno ) &
          & = Vec( ( Nptrs - 1 ) * tdof + dofno ) &
          & + scale * Val( 1 )
      END IF
    END SELECT
  END ASSOCIATE
END PROCEDURE dof_add2

END SUBMODULE setMethods