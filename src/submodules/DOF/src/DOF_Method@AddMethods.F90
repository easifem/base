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

SUBMODULE(DOF_Method) AddMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      add
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_add1
  INTEGER( I4B ) :: tdof, idof, i, n, m
  !!
  tdof = .tdof. obj
  n = SIZE( nodenum )
  m = SIZE( value )
  !!
  !!
  SELECT CASE( obj% StorageFMT )
  !!
  !!
  !!
  CASE( DOF_FMT )
    !!
    IF( m .NE. n ) THEN
      !! vec( nodenum ) += scale * value( 1 )
      IF( m .EQ. 1 ) THEN
        !!
        DO idof = 1, tdof
          vec( obj%valmap( idof ) - 1 + nodenum ) &
            & = vec( obj%valmap( idof ) - 1 + nodenum ) &
            & + scale * value( 1 )
        END DO
        !!
      ! Vec_dof_i( nodenum ) += scale * val_dof_i( : )
      ELSE IF( m .EQ. tdof * n ) THEN
        !!
        IF( Conversion( 1 ) .EQ. nodesToDOF ) THEN
          !!
          DO idof = 1, tdof
            DO i = 1, n
              vec( obj%valmap(idof)-1+nodenum( i ) ) &
                & = vec( obj%valmap(idof)-1+nodenum( i ) ) &
                & + scale * value( ( i - 1 ) * tdof +idof)
            END DO
          END DO
          !!
        ELSE
          !!
          DO idof = 1, tdof
            vec( obj%valmap(idof)-1+nodenum) &
              & = vec( obj%valmap(idof)-1+nodenum) &
              & + scale * value( ( idof - 1 ) * n + 1 : idof * n )
          END DO
          !!
        END IF
      END IF
      !!
    ELSE
      !!
      DO idof = 1, tdof
        vec( obj%valmap( idof ) - 1 + nodenum ) &
          & = vec( obj%valmap( idof ) - 1 + nodenum ) &
          & + scale * value( : )
      END DO
      !!
    END IF
  !!
  !!
  !!
  CASE( NODES_FMT )
    !!
    IF( m .NE. n ) THEN
      !!
      IF( m .EQ. 1 ) THEN
        !!
        DO idof = 1, tdof
          vec( (nodenum - 1 ) * tdof + idof ) &
            & = vec( (nodenum - 1 ) * tdof + idof ) &
            & + scale * value( 1 )
        END DO
        !!
      ELSE IF( m .EQ. tdof * n ) THEN
        !!
        IF( Conversion( 1 ) .EQ. DOFToNodes ) THEN
          !!
          DO idof = 1, tdof
            DO i = 1, n
              vec( ( nodenum( i ) - 1 ) * tdof + idof ) &
                & = vec( ( nodenum( i ) - 1 ) * tdof + idof ) &
                & + scale * value( ( idof - 1 ) * n + i  )
            END DO
          END DO
          !!
        ELSE
          !!
          DO idof = 1, tdof
            DO i = 1, n
              vec( ( nodenum( i ) - 1 ) * tdof + idof ) &
                & = vec( ( nodenum( i ) - 1 ) * tdof + idof ) &
                & + scale * value( ( i - 1 ) * tdof + idof )
            END DO
          END DO
          !!
        END IF
      END IF
      !!
    ELSE
      !!
      DO idof = 1, tdof
        vec( (nodenum - 1) * tdof + idof ) &
          & = vec( (nodenum - 1) * tdof + idof ) &
          & + scale * value( : )
      END DO
      !!
    END IF
    !!
  END SELECT
  !!
END PROCEDURE dof_add1

!----------------------------------------------------------------------------
!                                                                      add
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_add2
  INTEGER( I4B ), ALLOCATABLE :: indx(:)
  indx = getIndex(obj=obj, nodenum=nodenum)
  vec( indx ) = vec( indx ) + scale * value
  DEALLOCATE( indx )
END PROCEDURE dof_add2

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_add3
  INTEGER( I4B ), ALLOCATABLE :: indx(:)
  !!
  indx = getNodeLoc( &
    & obj=obj,&
    & nodenum=nodenum,&
    & idof=idof)
  !!
  IF( SIZE(nodenum) .EQ. SIZE(value) ) THEN
    !!
    vec( indx ) = vec( indx ) + scale * value( : )
    !!
  ELSE
    !!
    vec( indx ) = vec( indx ) + scale * value( 1 )
    !!
  END IF
  !!
  DEALLOCATE( indx )
  !!
END PROCEDURE dof_add3

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_add4
  INTEGER( I4B ), ALLOCATABLE :: indx(:)
  !!
  indx = getNodeLoc( &
    & obj=obj,&
    & nodenum=nodenum,&
    & idof=idof,&
    & ivar=ivar)
  !!
  IF( SIZE(nodenum) .EQ. SIZE(value) ) THEN
    vec( indx ) = vec( indx ) + scale * value( : )
  ELSE
    vec( indx ) = vec( indx ) + scale * value( 1 )
  END IF
  !!
  DEALLOCATE(indx)
  !!
END PROCEDURE dof_add4

!----------------------------------------------------------------------------
!                                                                      add
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_add5
  INTEGER( I4B ), ALLOCATABLE :: indx(:)
  !!
  indx = getNodeLoc( &
    & obj=obj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)
  !!
  IF( SIZE(nodenum) .EQ. SIZE(value) ) THEN
    vec( indx ) = vec( indx ) + scale * value( : )
  ELSE
    vec( indx ) = vec( indx ) + scale * value( 1 )
  END IF
  !!
  DEALLOCATE(indx)
  !!
END PROCEDURE dof_add5

!----------------------------------------------------------------------------
!                                                                      add
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_add6
  INTEGER( I4B ), ALLOCATABLE :: indx(:)
  !!
  indx = getNodeLoc(obj=obj, nodenum=nodenum, ivar=ivar, &
    & spacecompo=spacecompo, timecompo=timecompo)
  !!
  IF( SIZE(nodenum) .EQ. SIZE(value) ) THEN
    vec( indx ) = vec( indx ) + scale * value( : )
  ELSE
    vec( indx ) = vec( indx ) + scale * value( 1 )
  END IF
  !!
  DEALLOCATE(indx)
  !!
END PROCEDURE dof_add6

!----------------------------------------------------------------------------
!                                                                      add
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_add7
  INTEGER( I4B ), ALLOCATABLE :: indx(:)
  !!
  indx = getNodeLoc( &
    & obj=obj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)
  !!
  IF( SIZE(nodenum) .EQ. SIZE(value) ) THEN
    vec( indx ) = vec( indx ) + scale * value( : )
  ELSE
    vec( indx ) = vec( indx ) + scale * value( 1 )
  END IF
  !!
  DEALLOCATE(indx)
  !!
END PROCEDURE dof_add7

!----------------------------------------------------------------------------
!                                                                      add
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_add8
  INTEGER( I4B ), ALLOCATABLE :: indx(:)
  indx = getIndex(obj=obj, nodenum=nodenum)
  vec( indx ) = vec( indx ) + scale * value
  DEALLOCATE( indx )
END PROCEDURE dof_add8

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_add9
  INTEGER( I4B ) :: indx
  !!
  indx = getNodeLoc( &
    & obj=obj, &
    & nodenum=nodenum, &
    & idof=idof )
  !!
  vec( indx ) = vec( indx ) + scale * value
END PROCEDURE dof_add9

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_add10
  INTEGER( I4B ) :: indx
  !!
  indx = getNodeLoc( &
    & obj=obj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & idof=idof )
  !!
  vec( indx ) = vec( indx ) + scale * value
END PROCEDURE dof_add10

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_add11
  INTEGER( I4B ) :: indx
  !!
  indx = getNodeLoc( &
    & obj=obj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo )
  !!
  vec( indx ) = vec( indx ) + scale * value
END PROCEDURE dof_add11

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_add12
  INTEGER( I4B ), ALLOCATABLE :: indx( : )
  !!
  indx = getNodeLoc( &
    & obj=obj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo )
  !!
  vec( indx ) = vec( indx ) + scale * value
  !!
  DEALLOCATE(indx)
  !!
END PROCEDURE dof_add12

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_add13
  INTEGER( I4B ), ALLOCATABLE :: indx( : )
  !!
  indx = getNodeLoc( &
    & obj=obj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo )
  !!
  vec( indx ) = vec( indx ) + scale * value
  !!
  DEALLOCATE(indx)
  !!
END PROCEDURE dof_add13

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE AddMethods