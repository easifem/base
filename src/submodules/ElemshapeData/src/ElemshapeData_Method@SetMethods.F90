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

SUBMODULE(ElemshapeData_Method) SetMethods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                               setThickness
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_setThickness
  obj%Thickness = MATMUL( Val, N )
END PROCEDURE elemsd_setThickness

!----------------------------------------------------------------------------
!                                                               setThickness
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_setThickness
  CALL setThickness( obj=obj, Val = MATMUL( Val, T ), N=N )
END PROCEDURE stsd_setThickness

!----------------------------------------------------------------------------
!                                                        setBarycentricCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_setBarycentricCoord
  obj%Coord = MATMUL( Val, N )
END PROCEDURE elemsd_setBarycentricCoord

!----------------------------------------------------------------------------
!                                                        setBarycentricCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_setBarycentricCoord
  CALL setBarycentricCoord( obj=obj, Val=MATMUL( Val, T ), N=N )
END PROCEDURE stsd_setBarycentricCoord

!----------------------------------------------------------------------------
!                                                                      setJs
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_setJs
  ! Define internal variable
  INTEGER( I4B ) :: xidim, nsd, nips, ips
  REAL( DFP ) :: aa, bb, ab
  !!
  xidim = obj%RefElem%XiDimension
  nsd = obj%RefElem%nsd
  nips = SIZE( obj%N, 2 )
  !!
  DO ips = 1, nips
    IF( nsd .EQ. xidim ) THEN
      obj%Js( ips ) = det( obj%Jacobian( :, :, ips ) )
    ELSE IF( xidim .EQ. 1 .AND. xidim .NE. nsd ) THEN
      obj%Js( ips ) = &
        & SQRT( DOT_PRODUCT( obj%Jacobian( :, 1, ips ), &
        & obj%Jacobian( :, 1, ips ) ) )
    ELSE IF( xidim .EQ. 2 .AND. xidim .NE. nsd ) THEN
      aa = DOT_PRODUCT( obj%Jacobian( :, 1, ips ), obj%Jacobian(:,1,ips))
      bb = DOT_PRODUCT( obj%Jacobian( :, 2, ips ), obj%Jacobian(:,2,ips))
      ab = DOT_PRODUCT( obj%Jacobian( :, 1, ips ), obj%Jacobian(:,2,ips))
      obj%Js( ips ) = SQRT( aa * bb - ab * ab )
    END IF
  END DO
END PROCEDURE elemsd_setJs

!----------------------------------------------------------------------------
!                                                                  setdNdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_setdNdXt
  ! Define internal variables
  INTEGER( I4B ) :: NSD, XiDim, ips, nips
  REAL( DFP ), ALLOCATABLE :: InvJacobian( :, :, : )

  NSD = obj%RefElem%NSD
  XiDim = obj%RefElem%XiDimension
  IF( NSD .NE. XiDim ) THEN
    obj%dNdXt = 0.0_DFP
  ELSE
    ! Compute inverse of Jacobian
    nips = SIZE( obj%N, 2 )
    ALLOCATE( InvJacobian( NSD, NSD, nips ) )
    CALL Inv( InvA = InvJacobian, A = obj%Jacobian )
    DO ips = 1, nips
      obj%dNdXt( :, :, ips ) = &
        & MATMUL( obj%dNdXi( :, :, ips ), InvJacobian( :, :, ips ) )
    END DO
    DEALLOCATE( InvJacobian )
  END IF
END PROCEDURE elemsd_setdNdXt

!----------------------------------------------------------------------------
!                                                               setJacobian
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_setJacobian
  obj%jacobian = MATMUL( Val, dNdXi )
END PROCEDURE elemsd_setJacobian

!----------------------------------------------------------------------------
!                                                                 setJacobian
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_setJacobian
  obj%jacobian = MATMUL( MATMUL( Val, T ), dNdXi)
END PROCEDURE stsd_setJacobian

!----------------------------------------------------------------------------
!                                                                 setdNTdt
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_setdNTdt
  REAL( DFP ), ALLOCATABLE :: v( :, : )
  INTEGER( I4B ) :: ip

  !! get mesh velocity at space integration points
  v = MATMUL(MATMUL( Val, obj%dTdTheta/obj%Jt ), obj%N )
  CALL Reallocate( obj%dNTdt, SIZE( obj%N, 1 ),  SIZE( obj%T ), &
    & SIZE( obj%N, 2 ) )
  DO ip = 1, SIZE( obj%N, 2 )
    obj%dNTdt( :, :, ip ) = OUTERPROD(obj%N(:,ip), obj%dTdTheta/obj%Jt) &
      & - MATMUL( obj%dNTdXt(:,:,:,ip), v(:,ip) )
  END DO
END PROCEDURE stsd_setdNTdt

!----------------------------------------------------------------------------
!                                                                 setdNTdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_setdNTdXt
  !!
  INTEGER( I4B ) :: ip, j
  REAL( DFP ), ALLOCATABLE :: Q(:,:), Temp(:,:)
  !!
  CALL Reallocate( obj%dNTdXt, SIZE(obj%N,1), SIZE(obj%T), &
    & SIZE(obj%Jacobian,1), SIZE(obj%N,2) )
  !!
  IF( obj%RefElem%XiDimension .NE. obj%RefElem%NSD ) THEN
    RETURN
  END IF
  !!
  Q = obj%Jacobian(:,:,1)
  !!
  DO ip = 1, SIZE(obj%N,2)
    CALL INV( A=obj%Jacobian(:,:,ip), INVA=Q)
    Temp = MATMUL( obj%dNdXi(:,:,ip), Q )
    DO j = 1, SIZE(Q,1)
      obj%dNTdXt(:,:,j,ip) = OUTERPROD( Temp(:,j), obj%T )
    END DO
  END DO
  !!
  DEALLOCATE( Q, Temp )
  !!
END PROCEDURE stsd_setdNTdXt

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_set1
  CALL setJacobian( obj = obj, Val= Val, dNdXi=dNdXi )
  CALL setJs( obj = obj )
  CALL setdNdXt( obj = obj )
  CALL setBarycentricCoord( obj = obj, Val = Val, N=N )
END PROCEDURE elemsd_set1

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE stelemsd_set1
  CALL setJacobian( obj = obj, Val= Val, dNdXi=dNdXi, T=T)
  CALL setJs( obj = obj )
  CALL setdNdXt( obj = obj )
  CALL setBarycentricCoord( obj = obj, Val = Val, N=N, T=T )
  CALL setdNTdXt( obj = obj )
  CALL setdNTdt( obj = obj, Val = Val )
END PROCEDURE stelemsd_set1

!----------------------------------------------------------------------------
!                                                                 setNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_setNormal
  REAL( DFP ) :: vec(3, 3)
  INTEGER( I4B ) :: i, xidim, nsd
  !!
  !!
  !!
  vec = 0.0_DFP
  vec( 3, 2 ) = 1.0_DFP
  !!
  xidim = obj%RefElem%XiDimension
  nsd = obj%refElem%nsd
  !!
  DO i = 1, SIZE(obj%N,2)
    Vec( 1:nsd, 1:xidim ) = obj%Jacobian( 1:nsd, 1:xidim, i )
    obj%Normal( :, i ) = &
      & VectorProduct( Vec(:, 1), Vec(:, 2) ) / obj%Js(i)
  END DO
  !!
END PROCEDURE elemsd_setNormal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods