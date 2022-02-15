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

SUBMODULE(DOF_Method) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              DOFStartIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_DOFStartIndex
  ans = obj%map(ivar,5)
END PROCEDURE dof_DOFStartIndex

!----------------------------------------------------------------------------
!                                                              DOFEndIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_DOFEndIndex
  ans = obj%map(ivar+1,5)-1
END PROCEDURE dof_DOFEndIndex

!----------------------------------------------------------------------------
!                                                                     tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_tNodes1
  IF( ALLOCATED( obj%map ) ) THEN
    ans = obj%map( SIZE( obj%map, 1 ), 6 )
  ELSE
    ans = 0
  END IF
END PROCEDURE dof_tNodes1

!----------------------------------------------------------------------------
!                                                                     tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_tNodes2
  ans = 0
  IF( ALLOCATED( obj%valmap ) ) THEN
    ans = obj%valmap( idof + 1 ) - obj%valmap( idof )
  ELSE
    ans = 0
  END IF
END PROCEDURE dof_tNodes2

!----------------------------------------------------------------------------
!                                                                     tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_tNodes3
  ans = obj .tNodes. NameToIndex( obj, varName )
END PROCEDURE dof_tNodes3

!----------------------------------------------------------------------------
!                                                                     tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_tNodes4
  INTEGER( I4B ) :: ii
  ans = 0
  DO ii = 1, SIZE( idof )
    ans = ans + (obj .tNodes. idof( ii ))
  END DO
END PROCEDURE dof_tNodes4

!----------------------------------------------------------------------------
!                                                                       tDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_tdof1
  IF( ALLOCATED( obj%map ) ) THEN
    ans = obj%map( SIZE( obj%map, 1 ), 4 )
  ELSE
    ans = 0
  END IF
END PROCEDURE dof_tdof1

!----------------------------------------------------------------------------
!                                                                       tDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_tdof2
  INTEGER( I4B ) :: i, k
  k = ICHAR( Name )
  IF( ALLOCATED( obj%map ) ) THEN
    DO i = 1, SIZE( obj%map, 1 ) - 1
      IF( obj%map( i, 1 ) .EQ. k ) ans = obj%map( i, 4 )
    END DO
  ELSE
    ans = 0
  END IF
END PROCEDURE dof_tdof2

!----------------------------------------------------------------------------
!                                                                       tDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_tdof3
  INTEGER( I4B ) :: i

  i = SIZE( obj%map, 1 ) - 1
  IF( ALLOCATED( obj%map ) .AND. ivar .LE. i ) THEN
    ans = obj%map( ivar, 4 )
  ELSE
    ans = 0
  END IF
END PROCEDURE dof_tdof3

!----------------------------------------------------------------------------
!                                                                     tNames
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_tNames
  IF( ALLOCATED( obj%map ) ) THEN
    ans = SIZE( obj%map, 1 ) - 1
  ELSE
    ans = 0
  END IF
END PROCEDURE dof_tNames

!----------------------------------------------------------------------------
!                                                                     Names
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_names1
  INTEGER( I4B ) :: ii, n

  n = SIZE( obj%map, 1 ) - 1
  ALLOCATE( ans( n ) )

  DO ii = 1, n
    ans( ii ) = ACHAR( obj%map( ii, 1 ) )
  END DO
END PROCEDURE dof_names1

!----------------------------------------------------------------------------
!                                                                     Names
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_names2
  ans = ACHAR( obj%map( ii, 1 ) )
END PROCEDURE dof_names2

!----------------------------------------------------------------------------
!                                                                 IndexOF
!----------------------------------------------------------------------------

MODULE PROCEDURE NameToIndex
  INTEGER( I4B ) :: n, i, ic
  n = SIZE( obj%map, 1 ) - 1
  ic = ICHAR( Name )
  ans = 0
  DO i =1, n
    IF( obj%map( i, 1 ) .EQ. ic ) THEN
      ans = i
      EXIT
    END IF
  END DO
END PROCEDURE NameToIndex

!----------------------------------------------------------------------------
!                                                            tspacecomponents
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_tspacecomponents
  INTEGER( I4B ) :: n, i
  n = SIZE( obj%map, 1 ) - 1
  ans = 0
  DO i = 1, n
    IF( obj%map( i, 2 ) .GT. 0 ) ans = ans + 1
  END DO
END PROCEDURE dof_tspacecomponents

!----------------------------------------------------------------------------
!                                                            spacecomponents
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_spacecomponents1
  INTEGER( I4B ) :: n, i
  CALL Reallocate( ans, SIZE( obj%map, 1 ) - 1 )
  DO i = 1, SIZE(ans)
    ans( i ) = obj%map( i, 2 )
  END DO
END PROCEDURE dof_spacecomponents1

!----------------------------------------------------------------------------
!                                                            spacecomponents
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_spacecomponents2
  ans = obj%map( ivar, 2 )
END PROCEDURE dof_spacecomponents2

!----------------------------------------------------------------------------
!                                                            ttimecomponents
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_ttimecomponents
  INTEGER( I4B ) :: n, i
  n = SIZE( obj%map, 1 ) - 1
  ans = 0
  DO i = 1, n
    IF( obj%map( i, 3 ) .GT. 1 ) ans = ans + 1
  END DO
END PROCEDURE dof_ttimecomponents

!----------------------------------------------------------------------------
!                                                            timecomponents
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_timecomponents1
  INTEGER( I4B ) :: n, i
  CALL Reallocate( ans, SIZE( obj%map, 1 ) - 1 )
  DO i = 1, SIZE(ans)
    ans( i ) = obj%map( i, 3 )
  END DO
END PROCEDURE dof_timecomponents1

!----------------------------------------------------------------------------
!                                                            timecomponents
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_timecomponents2
  ans = obj%map( ivar, 3 )
END PROCEDURE dof_timecomponents2

!----------------------------------------------------------------------------
!                                                                 EQ
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_isEqual
  ans = .TRUE.
  IF( obj1%storageFMT .NE. obj2%storageFMT ) ans = .FALSE.
  IF( ANY( obj1%map( :, 2: ) .NE. obj2%map( :, 2: ) ) ) ans = .FALSE.
  IF( ANY( obj1%valmap .NE. obj2%valmap ) ) ans = .FALSE.
END PROCEDURE dof_isEqual

!----------------------------------------------------------------------------
!                                                                 NE
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_isNE
  ans = .NOT. ( dof_isEqual( obj1, obj2 ) )
END PROCEDURE dof_isNE

!----------------------------------------------------------------------------
!                                                                 getIDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIDOF1
  ans = spacecompo + (timecompo-1)*tspacecompo
END PROCEDURE dof_getIDOF1

!----------------------------------------------------------------------------
!                                                                 getIDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIDOF2
  ans   = (obj .DOFStartIndex. ivar) &
    & + spacecompo - 1 &
    & + (timecompo-1)*(obj .spacecomponents. ivar)
END PROCEDURE dof_getIDOF2

!----------------------------------------------------------------------------
!                                                                 getIDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIDOF3
  ans   = (obj .DOFStartIndex. ivar) &
    & + spacecompo - 1 &
    & + (timecompo-1)*(obj .spacecomponents. ivar)
END PROCEDURE dof_getIDOF3

!----------------------------------------------------------------------------
!                                                                 getIDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIDOF4
  ans   = (obj .DOFStartIndex. ivar) &
    & + spacecompo - 1 &
    & + (timecompo-1)*(obj .spacecomponents. ivar)
END PROCEDURE dof_getIDOF4

!----------------------------------------------------------------------------
!                                                                 getIDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIDOF5
  ans = spacecompo + (timecompo-1)*tspacecompo
END PROCEDURE dof_getIDOF5

!----------------------------------------------------------------------------
!                                                                 getIDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIDOF6
  ans = spacecompo + (timecompo-1)*tspacecompo
END PROCEDURE dof_getIDOF6

!----------------------------------------------------------------------------
!                                                                 getIDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIDOF7
  ans   = (obj .DOFStartIndex. ivar) + idof - 1
END PROCEDURE dof_getIDOF7

!----------------------------------------------------------------------------
!                                                                 getIDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIDOF8
  ans = (obj .DOFStartIndex. ivar) &
    & + arange(1, obj .tdof. ivar ) &
    & - 1
END PROCEDURE dof_getIDOF8

!----------------------------------------------------------------------------
!                                                               getNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getNodeLoc1
  IF( obj%storageFMT .EQ. NODES_FMT ) THEN
    ans = (nodenum-1)*(.tdof. obj) + idof
  ELSE
    ans = obj%valmap(idof) + nodenum - 1
  END IF
END PROCEDURE dof_getNodeLoc1

!----------------------------------------------------------------------------
!                                                               getNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getNodeLoc2
  IF( obj%storageFMT .EQ. NODES_FMT ) THEN
    ans = (nodenum-1)*(.tdof. obj) + idof
  ELSE
    ans = obj%valmap(idof) - 1 + nodenum
  END IF
END PROCEDURE dof_getNodeLoc2

!----------------------------------------------------------------------------
!                                                               getNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getNodeLoc3
  IF( obj%storageFMT .EQ. NODES_FMT ) THEN
    ans = (nodenum-1)*(.tdof. obj) + idof
  ELSE
    ans = obj%valmap(idof) - 1 + nodenum
  END IF
END PROCEDURE dof_getNodeLoc3

!----------------------------------------------------------------------------
!                                                               getNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getNodeLoc4
  IF( obj%storageFMT .EQ. NODES_FMT ) THEN
    ans = [idof, .tnodes. obj, .tdof. obj ]
  ELSE
    ans = [obj%valmap( idof ), obj%valmap(idof+1)-1, 1 ]
  END IF
END PROCEDURE dof_getNodeLoc4

!----------------------------------------------------------------------------
!                                                                getNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getNodeLoc5
  !! main
  IF ( obj%storageFMT .EQ. DOF_FMT ) THEN
    ans = obj%valmap( obj%map( ivar, 5) - 1 + idof ) + nodenum - 1
  ELSE
    ans = (nodenum-1)*(.tdof. obj) + (obj%map( ivar, 5) - 1 + idof)
  END IF
  !!
END PROCEDURE dof_getNodeLoc5

!----------------------------------------------------------------------------
!                                                                getNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getNodeLoc6
  !! main
  IF ( obj%storageFMT .EQ. DOF_FMT ) THEN
    ans = obj%valmap( obj%map( ivar, 5) - 1 + idof ) + nodenum - 1
  ELSE
    ans = (nodenum-1)*(.tdof. obj) + (obj%map( ivar, 5) - 1 + idof)
  END IF
  !!
END PROCEDURE dof_getNodeLoc6

!----------------------------------------------------------------------------
!                                                                getNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getNodeLoc7
  ans = getNodeLoc(obj=obj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & idof=GetIDOF( &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & tspacecompo=obj .spacecomponents. ivar) )
END PROCEDURE dof_getNodeLoc7

!----------------------------------------------------------------------------
!                                                                getNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getNodeLoc8
  ans = getNodeLoc(obj=obj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & idof=GetIDOF( &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & tspacecompo=obj .spacecomponents. ivar) )
END PROCEDURE dof_getNodeLoc8

!----------------------------------------------------------------------------
!                                                                getNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getNodeLoc9
  INTEGER( I4B ) :: ii
  !!
  !! main
  !!
  IF ( obj%storageFMT .EQ. DOF_FMT ) THEN
    !!
    DO ii = 1, size(idof)
      ans(ii) = obj%valmap( obj%map( ivar, 5) - 1 + idof(ii) ) + nodenum - 1
    END DO
    !!
  ELSE
    !!
    DO ii = 1, size(idof)
      ans(ii) = (nodenum-1)*(.tdof. obj) + (obj%map( ivar, 5) - 1 + idof(ii))
    END DO
    !!
  END IF
  !!
END PROCEDURE dof_getNodeLoc9

!----------------------------------------------------------------------------
!                                                                getNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getNodeLoc10
  ans = getNodeLoc(obj=obj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & idof=GetIDOF( &
    & spacecompo=spacecompo,&
    & timecompo=timecompo, &
    & tspacecompo=obj .spacecomponents. ivar) )
END PROCEDURE dof_getNodeLoc10

!----------------------------------------------------------------------------
!                                                                getNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getNodeLoc11
  ans = getNodeLoc(obj=obj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & idof=GetIDOF( &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & tspacecompo=obj .spacecomponents. ivar) )
END PROCEDURE dof_getNodeLoc11

!----------------------------------------------------------------------------
!                                                                getNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getNodeLoc12
  INTEGER( I4B ) ::idofs( size(timecompo) ), ii
  !!
  !!
  idofs = GetIDOF(spacecompo=spacecompo, timecompo=timecompo, &
    & tspacecompo=obj .spacecomponents. ivar)
  !!
  !!
  DO ii =1, size(nodenum)
    !!
    ans( (ii-1)*size(idofs) + 1 : ii*size(idofs) ) = getNodeLoc( &
      & obj=obj, &
      & nodenum=nodenum(ii), &
      & ivar=ivar, &
      & idof=idofs )
    !!
  END DO
  !!
END PROCEDURE dof_getNodeLoc12

!----------------------------------------------------------------------------
!                                                                getNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getNodeLoc13
  INTEGER( I4B ) ::idofs( size(spacecompo) ), ii
  !!
  !!
  idofs = GetIDOF(spacecompo=spacecompo, timecompo=timecompo, &
    & tspacecompo=obj .spacecomponents. ivar)
  !!
  !!
  DO ii =1, size(nodenum)
    !!
    ans( (ii-1)*size(idofs) + 1 : ii*size(idofs) ) = getNodeLoc( &
      & obj=obj, &
      & nodenum=nodenum(ii), &
      & ivar=ivar, &
      & idof=idofs )
    !!
  END DO
  !!
END PROCEDURE dof_getNodeLoc13

!----------------------------------------------------------------------------
!                                                               getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIndex1
  ans = getNodeLoc( &
    & obj=obj, &
    & nodenum=nodenum, &
    & idof=arange(1, .tdof. obj))
END PROCEDURE dof_getIndex1

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIndex2
  ans = getNodeLoc( &
    & obj=obj, &
    & nodenum=nodenum, &
    & idof=arange( obj .DOFStartIndex. ivar, obj .DOFEndIndex. ivar ) )
END PROCEDURE dof_getIndex2

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIndex3
  ans = getIndex( &
    & obj=obj, &
    & ivar=NameToIndex( obj, varName ), &
    & nodenum=nodenum )
END PROCEDURE dof_getIndex3

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIndex4
  INTEGER( I4B ) :: jj, ii, tdof, nn
  !!
  !! main
  !!
  tdof = .tdof. obj
  nn = SIZE(nodenum)
  ALLOCATE(ans(tdof*nn))
  ans = 0
  !!
  IF( obj%storageFMT .EQ. FMT_NODES ) then
    !!
    DO ii = 1, nn
      ans( (ii-1)*tdof+1:ii*tdof ) = getIndex( obj=obj, &
        & nodenum=nodenum(ii) )
    END DO
    !!
  ELSE
    !!
    DO jj = 1, tdof
      DO ii = 1, nn
        ans((jj-1)*nn + ii ) = getNodeLoc(obj=obj, &
          & nodenum=nodenum(ii), idof=jj)
      END DO
    END DO
    !!
  END IF
  !!
END PROCEDURE dof_getIndex4

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIndex5
  INTEGER( I4B ) :: jj, ii, tdof, nn
  !!
  !! main
  !!
  tdof = obj .tdof. ivar
  nn=SIZE(nodenum)
  ALLOCATE( ans(tdof*nn))
  ans=0
  !!
  IF( obj%storageFMT .EQ. FMT_NODES ) THEN
    !!
    DO ii = 1, nn
      ans( (ii-1) * tdof + 1 : ii*tdof ) = getIndex( obj=obj, &
        & nodenum=nodenum(ii), ivar=ivar )
    END DO
    !!
  ELSE
    !!
    tdof = 0 ! using tdof as counter
    DO jj = ( obj .DOFStartIndex. ivar), (obj .DOFEndIndex. ivar)
      tdof=tdof+1
      DO ii = 1, nn
        ans((tdof-1)*nn + ii ) = getNodeLoc(obj=obj, nodenum=nodenum(ii),  &
          & idof=jj)
        ! here tdof is local counter
      END DO
    END DO
  END IF
  !!
END PROCEDURE dof_getIndex5

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIndex6
  ans = getIndex( &
    & obj=obj, &
    & ivar=NameToIndex( obj, varName ), &
    & nodenum=nodenum )
END PROCEDURE dof_getIndex6

END SUBMODULE GetMethods