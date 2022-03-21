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

SUBMODULE(RealVector_Method) GetValueMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_getvalue1
  INTEGER( I4B ) :: ii, jj
  !!
  jj = 0
  !!
  DO ii = istart, iend, stride
    jj = jj + 1
    value%val(jj) = obj%val(ii)
  END DO
  !!
END PROCEDURE realvec_getvalue1

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_getvalue2
  INTEGER( I4B ) :: s(3), ii, jj
  !!
  !!
  !!
  s=GetNodeLoc( obj=dofobj, idof=idof )
  !!
  jj = 0
  !!
  DO ii = s(1), s(2), s(3)
    jj = jj + 1
    value%val(jj) = obj%val(ii)
  END DO
  !!
END PROCEDURE realvec_getvalue2

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_getvalue3
  INTEGER( I4B ) :: s(3), ii, jj
  !!
  !!
  !!
  s=GetNodeLoc( &
    & obj=dofobj, &
    & idof=getIDOF( &
    & obj=dofobj, &
    & ivar=ivar, &
    & idof=idof ))
  !!
  jj = 0
  !!
  DO ii = s(1), s(2), s(3)
    jj = jj + 1
    value%val(jj) = obj%val(ii)
  END DO
  !!
END PROCEDURE realvec_getvalue3

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_getvalue4
  INTEGER( I4B ) :: s(3), ii, jj
  !!
  !!
  !!
  s=GetNodeLoc( &
    & obj=dofobj, &
    & idof=getIDOF( &
    & obj=dofobj, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo ))
  !!
  jj = 0
  !!
  DO ii = s(1), s(2), s(3)
    jj = jj + 1
    value%val(jj) = obj%val(ii)
  END DO
  !!
END PROCEDURE realvec_getvalue4

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_getvalue5
  INTEGER( I4B ) :: p(3), s(3), ii, jj
  !!
  !!
  !!
  s=GetNodeLoc( obj=dofobj, idof=idofobj )
  p=GetNodeLoc( obj=dofvalue, idof=idofvalue )
  !!
  jj = 0
  !!
  DO ii = s(1), s(2), s(3)
    jj = jj + 1
    value%val( p(1) + (jj-1)*p(3) ) = obj%val(ii)
  END DO
  !!
END PROCEDURE realvec_getvalue5

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_getvalue6
  INTEGER( I4B ) :: p(3), s(3), ii, jj, kk
  !!
  !!
  !!
  DO kk = 1, SIZE(idofobj)
    !!
    s=GetNodeLoc( obj=dofobj, idof=idofobj(kk) )
    p=GetNodeLoc( obj=dofvalue, idof=idofvalue(kk) )
    !!
    jj = 0
    !!
    DO ii = s(1), s(2), s(3)
      jj = jj + 1
      value%val( p(1) + (jj-1)*p(3) ) = obj%val(ii)
    END DO
    !!
  END DO
  !!
END PROCEDURE realvec_getvalue6

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_getvalue7
  INTEGER( I4B ) :: p(3), s(3), ii, jj
  !!
  !!
  !!
  s=GetNodeLoc( &
    & obj=dofobj, &
    & idof=getIDOF( &
    & obj=dofobj, &
    & ivar=ivarobj, &
    & idof=idofobj ))
  !!
  p=GetNodeLoc( &
    & obj=dofvalue, &
    & idof=getIDOF( &
    & obj=dofvalue, &
    & ivar=ivarvalue, &
    & idof=idofvalue) )
  !!
  jj = 0
  !!
  DO ii = s(1), s(2), s(3)
    jj = jj + 1
    value%val( p(1) + (jj-1)*p(3) ) = obj%val(ii)
  END DO
  !!
END PROCEDURE realvec_getvalue7

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_getvalue8
  INTEGER( I4B ) :: p(3), s(3), ii, jj, kk
  !!
  !!
  !!
  DO kk = 1, SIZE(idofobj)
    !!
  s=GetNodeLoc( &
    & obj=dofobj, &
    & idof=getIDOF( &
    & obj=dofobj, &
    & ivar=ivarobj, &
    & idof=idofobj(kk) ))
  !!
  p=GetNodeLoc( &
    & obj=dofvalue, &
    & idof=getIDOF( &
    & obj=dofvalue, &
    & ivar=ivarvalue, &
    & idof=idofvalue(kk) ))
    !!
    jj = 0
    !!
    DO ii = s(1), s(2), s(3)
      jj = jj + 1
      value%val( p(1) + (jj-1)*p(3) ) = obj%val(ii)
    END DO
    !!
  END DO
  !!
END PROCEDURE realvec_getvalue8

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_getvalue9
  INTEGER( I4B ) :: p(3), s(3), ii, jj
  !!
  !!
  !!
  s=GetNodeLoc( &
    & obj=dofobj, &
    & idof=getIDOF( &
    & obj=dofobj, &
    & ivar=ivarobj, &
    & spacecompo=spacecompoobj, &
    & timecompo=timecompoobj ))
  !!
  p=GetNodeLoc( &
    & obj=dofvalue, &
    & idof=getIDOF( &
    & obj=dofvalue, &
    & ivar=ivarvalue, &
    & spacecompo=spacecompovalue, &
    & timecompo=timecompovalue ))
  !!
  jj = 0
  !!
  DO ii = s(1), s(2), s(3)
    jj = jj + 1
    value%val( p(1) + (jj-1)*p(3) ) = obj%val(ii)
  END DO
  !!
END PROCEDURE realvec_getvalue9

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_getvalue10
  INTEGER( I4B ) :: p(3), s(3), ii, jj, kk
  !!
  !!
  !!
  DO kk = 1, SIZE(timecompoobj)
    !!
    s=GetNodeLoc( &
      & obj=dofobj, &
      & idof=getIDOF( &
      & obj=dofobj, &
      & ivar=ivarobj, &
      & spacecompo=spacecompoobj, &
      & timecompo=timecompoobj(kk) ))
    !!
    p=GetNodeLoc( &
      & obj=dofvalue, &
      & idof=getIDOF( &
      & obj=dofvalue, &
      & ivar=ivarvalue, &
      & spacecompo=spacecompovalue, &
      & timecompo=timecompovalue(kk) ))
    !!
    jj = 0
    !!
    DO ii = s(1), s(2), s(3)
      jj = jj + 1
      value%val( p(1) + (jj-1)*p(3) ) = obj%val(ii)
    END DO
  END DO
  !!
END PROCEDURE realvec_getvalue10

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_getvalue11
  INTEGER( I4B ) :: p(3), s(3), ii, jj, kk
  !!
  !!
  !!
  DO kk = 1, SIZE(spacecompoobj)
    !!
    s=GetNodeLoc( &
      & obj=dofobj, &
      & idof=getIDOF( &
      & obj=dofobj, &
      & ivar=ivarobj, &
      & spacecompo=spacecompoobj(kk), &
      & timecompo=timecompoobj ))
    !!
    p=GetNodeLoc( &
      & obj=dofvalue, &
      & idof=getIDOF( &
      & obj=dofvalue, &
      & ivar=ivarvalue, &
      & spacecompo=spacecompovalue(kk), &
      & timecompo=timecompovalue ))
    !!
    jj = 0
    !!
    DO ii = s(1), s(2), s(3)
      jj = jj + 1
      value%val( p(1) + (jj-1)*p(3) ) = obj%val(ii)
    END DO
  END DO
  !!
END PROCEDURE realvec_getvalue11

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_getvalue12
  !!
  CALL Getvalue( &
    & v=value, &
    & val=obj%val, &
    & obj=dofobj, &
    & idof=idof, &
    & storageFMT=storageFMT, &
    & nodenum=nodenum )
  !!
END PROCEDURE realvec_getvalue12

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_getvalue13
  !!
  CALL Getvalue( &
    & v=value, &
    & val=obj%val, &
    & obj=dofobj, &
    & idof=idof, &
    & storageFMT=storageFMT )
  !!
END PROCEDURE realvec_getvalue13

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_getvalue14
  !!
  CALL Getvalue( &
    & v=value, &
    & val=obj%val, &
    & obj=dofobj, &
    & idof=idof, &
    & force3D=force3D )
  !!
END PROCEDURE realvec_getvalue14

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_getvalue15
  value = obj%val( &
    & getIndex(obj=dofobj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & idof=idof ))
END PROCEDURE realVec_getvalue15

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_getvalue16
  value = obj%val( &
    & getIndex( &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & idof=idof ))
END PROCEDURE realVec_getvalue16

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_getvalue17
  value = obj%val( &
    & getIndex( &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & ivar=ivar ))
END PROCEDURE realVec_getvalue17

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_getvalue18
  !!
  value = obj%val( &
    & getIndex( &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo ) )
  !!
END PROCEDURE realVec_getvalue18

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_getvalue19
  INTEGER( I4B ) :: s(3), ii, jj
  !!
  s=GetNodeLoc( obj=dofobj, idof=idof )
  CALL Reallocate( value, dofobj .tNodes. idof )
  !!
  jj = 0
  !!
  DO ii = s(1), s(2), s(3)
    jj = jj + 1
    value(jj) = obj%val(ii)
  END DO
  !!
END PROCEDURE realVec_getvalue19

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_getvalue20
  INTEGER( I4B ) :: s(3), ii, jj
  !!
  ii = getIDOF( obj=dofobj, ivar=ivar, idof=idof )
  s=GetNodeLoc( obj=dofobj, idof=ii )
  CALL Reallocate( value, dofobj .tNodes. ii )
  !!
  jj = 0
  !!
  DO ii = s(1), s(2), s(3)
    jj = jj + 1
    value(jj) = obj%val(ii)
  END DO
  !!
END PROCEDURE realVec_getvalue20

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_getvalue21
  INTEGER( I4B ) :: s(3), ii, jj
  !!
  ii = getIDOF( obj=dofobj, ivar=ivar, spacecompo=spacecompo, &
    & timecompo=timecompo )
  s=GetNodeLoc( obj=dofobj, idof=ii )
  CALL Reallocate( value, dofobj .tNodes. ii )
  !!
  jj = 0
  !!
  DO ii = s(1), s(2), s(3)
    jj = jj + 1
    value(jj) = obj%val(ii)
  END DO
  !!
END PROCEDURE realVec_getvalue21

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_getvalue22
  !!
  INTEGER( I4B ) :: m, n, i, k, tdof
  !!
  !!
  !!
  m = SIZE( idof )
  n = SIZE( nodenum )
  !!
  CALL Reallocate( value, m * n )
  !!
  SELECT CASE( dofobj%StorageFMT )
  !!
  !!
  !!
  CASE( DOF_FMT )
    !!
    DO i = 1, m
      value( ( i-1 ) * n + 1 : i * n ) = &
        & obj%val( nodenum + dofobj%valmap( idof( i ) ) - 1 )
    END DO
  !!
  !!
  !!
  CASE( NODES_FMT )
    !!
    tdof = .tdof. dofobj
    !!
    DO i = 1, n
      DO k = 1, m
        value( ( i - 1 ) * m + k ) &
          & = obj%val( ( nodenum( i ) - 1 ) * tdof + idof( k ) )
      END DO
    END DO
    !!
  END SELECT
  !!
END PROCEDURE realvec_getvalue22

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_getvalue23
  !!
  INTEGER( I4B ) :: m, n, i, k, tdof
  !!
  !! main
  !!
  k = 0
  DO i = 1, SIZE( idof )
    k = k + dofobj%valmap( idof( i ) + 1 ) - dofobj%valmap( idof( i ) )
  END DO
  !!
  CALL reallocate( value, k )
  !!
  SELECT CASE( dofobj%StorageFMT )
  !!
  !!
  !!
  CASE( DOF_FMT )
    !!
    m = 0; n = 0
    DO i = 1, SIZE( idof )
      m = n + 1
      n = n + dofobj%valmap( idof( i ) + 1 ) - dofobj%valmap( idof( i ) )
      value( m : n ) = &
        & obj%val( &
        & dofobj%valmap( idof( i ) ) : &
        & dofobj%valmap( idof( i + 1 ) - 1 ) )
    END DO
  !!
  !!
  !!
  CASE( NODES_FMT )
    !!
    tdof = .tdof. dofobj
    m = SIZE( idof )
    !!
    DO i = 1, dofobj%valmap( 2 ) - dofobj%valmap( 1 )
      DO k = 1, m
        value( ( i - 1 ) * m + k ) &
          & = obj%val( ( i - 1 ) * tdof + idof( k ) )
      END DO
    END DO
    !!
  END SELECT
  !!
  !!
END PROCEDURE realvec_getvalue23

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetValueMethods