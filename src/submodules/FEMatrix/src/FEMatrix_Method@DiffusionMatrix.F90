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

SUBMODULE(FEMatrix_Method) DiffusionMatrix
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Space_DiffusionMatrix
  ! Define internal variable
  REAL( DFP ), ALLOCATABLE :: RealVal( : )
  INTEGER( I4B ) :: i, j, nsd

  i = SIZE( Test%N, 1 )
  j = SIZE( Trial%N, 1 )
  ALLOCATE( Ans( i, j ) ); Ans = 0.0_DFP
  nsd = Trial%RefElem%NSD
  RealVal = Trial%Js * Trial%Ws * Trial%Thickness
  DO i = 1, SIZE( Trial%N, 2 )
    DO j = 1, nsd
      Ans = Ans + &
        & OUTERPROD( a = Test%dNdXt( :, j, i ), b = &
        & Trial%dNdXt( :, j, i ) ) &
        & * RealVal( i )
    END DO
  END DO
  DEALLOCATE( RealVal )
  IF( PRESENT( nCopy ) ) THEN
    CALL MakeDiagonalCopies( Ans, nCopy )
  END IF
END PROCEDURE Space_DiffusionMatrix

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Space_DiffusionMatrix_K

  REAL( DFP ), ALLOCATABLE :: KBar( :, :, : ), RealVal( : )
  INTEGER( I4B ) :: i
  INTEGER( I4B ), ALLOCATABLE :: S( : )

  S = SHAPE( K )
  ALLOCATE( KBar( S(1), S(2), SIZE( Trial%N, 2 ) ) )
  CALL getInterpolation( obj=Trial, Interpol=KBar, Val=K )
  RealVal = Trial%Js * Trial%Ws * Trial%Thickness
  ALLOCATE( Ans( SIZE( Test%N, 1 ), SIZE( Trial%N, 1 ) ) )
  Ans = 0.0_DFP
  DO i = 1, SIZE( Trial%N, 2 )
    Ans = Ans + RealVal( i ) * MATMUL( &
      & MATMUL( Test%dNdXt( :, :, i ), KBar( :, :, i ) ), &
      & TRANSPOSE( Trial%dNdXt( :, :, i ) ) )
  END DO
  DEALLOCATE( KBar, RealVal, S )
  IF( PRESENT( nCopy ) ) THEN
    CALL MakeDiagonalCopies( Ans, nCopy )
  END IF
END PROCEDURE Space_DiffusionMatrix_K

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Space_DiffusionMatrix_C
  REAL( DFP ), ALLOCATABLE :: C1Bar( :, : ), C2Bar( :, : ), RealVal( : )
  INTEGER( I4B ) :: i

  CALL getProjectionOfdNdXt( obj=Test, cdNdXt=C1Bar, Val=C1 )
  CALL getProjectionOfdNdXt( obj=Trial, cdNdXt=C2Bar, Val=C2 )
  RealVal = Trial%Js * Trial%Ws * Trial%Thickness
  ALLOCATE( Ans ( SIZE( Test%N, 1 ), SIZE( Trial%N, 1 ) ) )
  Ans = 0.0_DFP
  DO i = 1, SIZE( Trial%N, 2 )
    Ans = Ans + RealVal( i ) * OUTERPROD( C1Bar( :, i ), C2Bar( :, i ) )
  END DO
  IF( PRESENT( nCopy  ) ) THEN
    CALL MakeDiagonalCopies( Ans, nCopy )
  END IF
  DEALLOCATE( RealVal, C1Bar, C2Bar )
END PROCEDURE Space_DiffusionMatrix_C

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE st_diffusionMatrix
  REAL( DFP ), ALLOCATABLE :: Mat4(:,:,:,:), RealVal( : )
  INTEGER( I4B ) :: ips, ipt, a, b

  ALLOCATE( Mat4( SIZE( Test( 1 )%N, 1  ), &
    & SIZE( Trial( 1 )%N, 1 ), &
    & SIZE( Test( 1 )%T ), &
    & SIZE( Trial( 1 )%T ) ) )
  Mat4 = 0.0_DFP

  DO ipt = 1, SIZE( Trial )
    RealVal = Trial(ipt)%Js * Trial(ipt)%Ws * Trial(ipt)%Thickness &
      & * Trial(ipt)%Wt * Trial(ipt)%Jt
    DO ips = 1, SIZE( Test )
      DO b = 1, SIZE( Trial(1)%T )
        DO a = 1, SIZE( Test(1)%T )
          Mat4( :, :, a, b ) = Mat4( :, :, a, b ) + RealVal( ips ) &
            & * MATMUL( Test( ipt )%dNTdXt( :, a, :, ips ), &
            & TRANSPOSE( Trial( ipt )%dNTdXt( :, b, :, ips ) ) )
        END DO
      END DO
    END DO
  END DO
  CALL Convert( From=Mat4, To=Ans )
  IF( PRESENT( nCopy ) ) THEN
    CALL MakeDiagonalCopies( Ans, nCopy )
  END IF
  DEALLOCATE( Mat4, RealVal )
END PROCEDURE st_diffusionMatrix

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE st_diffusionMatrix_K
  REAL( DFP ), ALLOCATABLE :: Mat4(:,:,:,:)
  REAL( DFP ), ALLOCATABLE :: RealVal( : )
  REAL( DFP ), ALLOCATABLE :: Dummy(:,:)
  REAL( DFP ), ALLOCATABLE :: KBar(:,:,:,:)
  INTEGER( I4B ) :: ips, ipt, a, b

  ALLOCATE( Mat4( SIZE( Test( 1 )%N, 1  ), &
    & SIZE( Trial( 1 )%N, 1 ), &
    & SIZE( Test( 1 )%T ), &
    & SIZE( Trial( 1 )%T ) ) )
  Mat4 = 0.0_DFP
  CALL getInterpolation( obj=Trial, Val=K, Interpol=KBar )
  DO ipt = 1, SIZE( Trial )
    RealVal = Trial(ipt)%Js * Trial(ipt)%Ws * Trial(ipt)%Thickness &
      & * Trial(ipt)%Wt * Trial(ipt)%Jt
    DO ips = 1, SIZE( Test )
      DO b = 1, SIZE( Trial(1)%T )
        Dummy = RealVal( ips ) * MATMUL(KBar(:,:,ips,ipt), &
          & TRANSPOSE(Trial(ipt)%dNTdXt(:,b,:,ips)))
        DO a = 1, SIZE( Test(1)%T )
          Mat4( :, :, a, b ) = Mat4( :, :, a, b ) +  &
            & MATMUL( Test(ipt)%dNTdXt(:,a,:,ips), Dummy )
        END DO
      END DO
    END DO
  END DO
  CALL Convert( From=Mat4, To=Ans )
  IF( PRESENT( nCopy ) ) THEN
    CALL MakeDiagonalCopies( Ans, nCopy )
  END IF
  DEALLOCATE( Mat4, RealVal, KBar, Dummy )
END PROCEDURE st_diffusionMatrix_K

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE st_diffusionMatrix_C
  REAL( DFP ), ALLOCATABLE :: Mat4(:,:,:,:)
  REAL( DFP ), ALLOCATABLE :: RealVal( : )
  REAL( DFP ), ALLOCATABLE :: c1dNTdXt(:,:,:), c2dNTdXT(:,:,:)
  INTEGER( I4B ) :: ips, ipt, a, b

  ALLOCATE( &
    & Mat4( SIZE( Test( 1 )%N, 1  ), &
    & SIZE( Trial( 1 )%N, 1 ), &
    & SIZE( Test( 1 )%T ), &
    & SIZE( Trial( 1 )%T ) ) )
  Mat4 = 0.0_DFP
  DO ipt = 1, SIZE( Trial )
    RealVal = Trial(ipt)%Js * Trial(ipt)%Ws * Trial(ipt)%Thickness &
      & * Trial(ipt)%Wt * Trial(ipt)%Jt
    CALL getProjectionOfdNTdXt(obj=Test(ipt), cdNTdXt=c1dNTdXt, Val=C1)
    CALL getProjectionOfdNTdXt(obj=Trial(ipt), cdNTdXt=c2dNTdXt, Val=C2)
    DO ips = 1, SIZE( Test )
      DO b = 1, SIZE( Trial(1)%T )
        DO a = 1, SIZE( Test(1)%T )
          Mat4( :, :, a, b ) = Mat4( :, :, a, b ) +  &
            & RealVal( ips ) * &
            & OUTERPROD( a=c1dNTdXt(:,a,ips), b=c2dNTdXt(:,b,ips))
        END DO
      END DO
    END DO
  END DO
  CALL Convert( From=Mat4, To=Ans )
  IF( PRESENT( nCopy ) ) THEN
    CALL MakeDiagonalCopies( Ans, nCopy )
  END IF
  DEALLOCATE( Mat4, RealVal, c1dNTdXt, c2dNTdXT )
END PROCEDURE st_diffusionMatrix_C

END SUBMODULE DiffusionMatrix