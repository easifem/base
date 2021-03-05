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

SUBMODULE( ElemshapeData_Method ) setMethod
USE BaseMethod
IMPLICIT NONE

CONTAINS
!----------------------------------------------------------------------------
!                                                               setThickness
!----------------------------------------------------------------------------

MODULE PROCEDURE set_thickness
  Obj % Thickness = MATMUL( Val, N )
END PROCEDURE set_thickness

!----------------------------------------------------------------------------
!                                                               setThickness
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_set_thickness
  CALL setThickness( Obj=Obj, Val = MATMUL( Val, T ), N=N )
END PROCEDURE stsd_set_thickness

!----------------------------------------------------------------------------
!                                                        setBarycentricCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE set_coord
  Obj % Coord = MATMUL( Val, N )
END PROCEDURE set_coord

!----------------------------------------------------------------------------
!                                                        setBarycentricCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_set_coord
  CALL setBarycentricCoord( Obj=Obj, Val=MATMUL( Val, T ), N=N )
END PROCEDURE stsd_set_coord

!----------------------------------------------------------------------------
!                                                                      setJs
!----------------------------------------------------------------------------

MODULE PROCEDURE set_Js
  ! Define internal variable
  INTEGER( I4B ) :: xidim, nsd, nips, ips
  REAL( DFP ) :: aa, bb, ab

  xidim = Obj % RefElem % XiDimension
  nsd = Obj % RefElem % nsd
  nips = SIZE( Obj % N, 2 )

  DO ips = 1, nips
    IF( nsd .EQ. xidim ) THEN
      Obj % Js( ips ) = det( Obj % Jacobian( :, :, ips ) )
    ELSE IF( xidim .EQ. 1 .AND. xidim .NE. nsd ) THEN
      Obj % Js( ips ) = &
        & SQRT( DOT_PRODUCT( Obj % Jacobian( :, 1, ips ), &
        & Obj % Jacobian( :, 1, ips ) ) )
    ELSE IF( xidim .EQ. 2 .AND. xidim .NE. nsd ) THEN
      aa = DOT_PRODUCT( Obj % Jacobian( :, 1, ips ), Obj % Jacobian(:,1,ips))
      bb = DOT_PRODUCT( Obj % Jacobian( :, 2, ips ), Obj % Jacobian(:,2,ips))
      ab = DOT_PRODUCT( Obj % Jacobian( :, 1, ips ), Obj % Jacobian(:,2,ips))
      Obj % Js( ips ) = SQRT( aa * bb - ab * ab )
    END IF
  END DO
END PROCEDURE set_Js

!----------------------------------------------------------------------------
!                                                                  setdNdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE set_dNdXt_internally
  ! Define internal variables
  INTEGER( I4B ) :: NSD, XiDim, ips, nips
  REAL( DFP ), ALLOCATABLE :: InvJacobian( :, :, : )

  NSD = Obj % RefElem % NSD
  XiDim = Obj % RefElem % XiDimension
  IF( NSD .NE. XiDim ) THEN
    Obj % dNdXt = 0.0_DFP
  ELSE
    ! Compute inverse of Jacobian
    nips = SIZE( Obj % N, 2 )
    ALLOCATE( InvJacobian( NSD, NSD, nips ) )
    CALL Inv( InvA = InvJacobian, A = Obj % Jacobian )
    DO ips = 1, nips
      Obj % dNdXt( :, :, ips ) = &
        & MATMUL( Obj % dNdXi( :, :, ips ), InvJacobian( :, :, ips ) )
    END DO
    DEALLOCATE( InvJacobian )
  END IF
END PROCEDURE set_dNdXt_internally

!----------------------------------------------------------------------------
!                                                               setJacobian
!----------------------------------------------------------------------------

MODULE PROCEDURE set_Jacobian
  Obj%jacobian = MATMUL( Val, dNdXi )
END PROCEDURE set_Jacobian

!----------------------------------------------------------------------------
!                                                                 setJacobian
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_set_jacobian
  Obj%jacobian = MATMUL( MATMUL( Val, T ), dNdXi)
END PROCEDURE stsd_set_jacobian

!----------------------------------------------------------------------------
!                                                                 setdNTdt
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_set_dNTdt
  REAL( DFP ), ALLOCATABLE :: v( :, : )
  INTEGER( I4B ) :: ip

  !! get mesh velocity at space integration points
  v = MATMUL(MATMUL( Val, Obj%dTdTheta/Obj % Jt ), Obj%N )
  CALL Reallocate( Obj % dNTdt, SIZE( Obj % N, 1 ),  SIZE( Obj % T ), &
    & SIZE( Obj % N, 2 ) )
  DO ip = 1, SIZE( Obj % N, 2 )
    Obj % dNTdt( :, :, ip ) = OUTERPROD(Obj%N(:,ip), Obj%dTdTheta/Obj%Jt) &
      & - MATMUL( Obj%dNTdXt(:,:,:,ip), v(:,ip) )
  END DO
END PROCEDURE stsd_set_dNTdt

!----------------------------------------------------------------------------
!                                                                 setdNTdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_set_dNTdXt_internally
  INTEGER( I4B ) :: ip, j
  REAL( DFP ), ALLOCATABLE :: Q(:,:), Temp(:,:)

  CALL Reallocate( Obj%dNTdXt, SIZE(Obj%N,1), SIZE(Obj%T), &
    & SIZE(Obj%Jacobian,1), SIZE(Obj%N,2) )

  IF( Obj%RefElem%XiDimension .NE. Obj%RefElem%NSD ) THEN
    RETURN
  END IF

  Q = Obj%Jacobian(:,:,1)

  DO ip = 1, SIZE(Obj%N,2)
    CALL INV( A=Obj%Jacobian(:,:,ip), INVA=Q)
    Temp = MATMUL( Obj%dNdXi(:,:,ip), Q )
    DO j = 1, SIZE(Q,1)
      Obj%dNTdXt(:,:,j,ip) = OUTERPROD( Temp(:,j), Obj%T )
    END DO
  END DO

  DEALLOCATE( Q, Temp )

END PROCEDURE stsd_set_dNTdXt_internally

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE set_value
  CALL setJacobian( Obj = Obj, Val= Val, dNdXi=dNdXi )
  CALL setJs( Obj = Obj )
  CALL setdNdXt( Obj = Obj )
  CALL setBarycentricCoord( Obj = Obj, Val = Val, N=N )
END PROCEDURE set_value

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_set_value
  CALL setJacobian( Obj = Obj, Val= Val, dNdXi=dNdXi, T=T)
  CALL setJs( Obj = Obj )
  CALL setdNdXt( Obj = Obj )
  CALL setBarycentricCoord( Obj = Obj, Val = Val, N=N, T=T )
  CALL setdNTdXt( Obj = Obj )
  CALL setdNTdt( Obj = Obj, Val = Val )
END PROCEDURE stsd_set_value

!----------------------------------------------------------------------------
!                                                                 setNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE set_normal
  REAL( DFP ) :: vec(3, 3)
  INTEGER( I4B ) :: i, xidim, nsd

  vec = 0.0_DFP
  vec( 3, 2 ) = 1.0_DFP

  xidim = Obj%RefElem%XiDimension
  nsd = obj%refElem%nsd

  DO i = 1, SIZE(obj%N,2)
    Vec( 1:nsd, 1:xidim ) = Obj%Jacobian( 1:nsd, 1:xidim, i )
    Obj%Normal( :, i ) = &
      & VectorProduct( Vec(:, 1), Vec(:, 2) ) / obj%Js(i)
  END DO
END PROCEDURE set_normal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE setMethod