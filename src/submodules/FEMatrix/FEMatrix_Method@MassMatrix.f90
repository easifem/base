SUBMODULE( FEMatrix_Method ) MassMatrix
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Space_MassMatrix
  ! Define internal variable
  REAL( DFP ), ALLOCATABLE :: RealVal( : ), RhoBar( : )
  INTEGER( I4B ) :: nns1, nns2, nips, ips
  LOGICAL( LGT ) :: isNodal

  nns1 = SIZE( Test % N, 1 )
  nns2 = SIZE( Trial % N, 1 )
  ALLOCATE( Ans( nns1, nns2 ) )
  Ans = 0.0_DFP
  nips = SIZE( Trial % N, 2 )
  ALLOCATE( RhoBar( nips ) )

  ! making rho at quadrature
  IF( PRESENT( Rho ) ) THEN
    CALL getInterpolation( Obj=Trial, Val=Rho, Interpol=RhoBar )
  ELSE
    RhoBar = 1.0_DFP
  END IF

  ! performing scalar computation
  RealVal = Trial % Js * Trial % Ws * Trial % Thickness * RhoBar
  DEALLOCATE( RhoBar )

  ! performing outerproduct
  DO ips = 1, nips
    Ans = Ans + &
      & OUTERPROD( a = Test % N( :, ips ), b = Trial % N( :, ips ) ) &
      & * RealVal( ips )
  END DO
  IF( ALLOCATED( RealVal ) ) DEALLOCATE( RealVal )

  !making n-diagonal copies
  IF( PRESENT( nCopy ) ) THEN
    CALL MakeDiagonalCopies( Ans, nCopy )
  END IF

END PROCEDURE Space_MassMatrix

!----------------------------------------------------------------------------
!                                                                  MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE st_massMatrix_a

  REAL( DFP ), ALLOCATABLE :: Mat4( :, :, :, : ), RhoBar( :, : ), RealVal(:)

  INTEGER( I4B ) :: a, b, ipt, ips

  ALLOCATE( &
    & Mat4( SIZE( Test( 1 ) % N, 1 ), &
    & SIZE( Trial( 1 ) % N, 1 ), &
    & SIZE( Test( 1 ) % T ), &
    & SIZE( Trial( 1 ) % T ) ) )
  Mat4 = 0.0_DFP
  ALLOCATE( RhoBar( SIZE( Trial( 1 ) % N, 2 ), SIZE( Trial ) ) )

  IF( PRESENT( Rho ) ) THEN
    CALL getInterpolation( Obj=Trial, Val=Rho, Interpol=RhoBar )
  ELSE
    RhoBar = 1.0_DFP
  END IF

  IF( Term1 .EQ. 0 .AND. Term2 .EQ. 0 ) THEN
    DO ipt = 1, SIZE( Trial )
      RealVal = Trial(ipt)%Js * Trial(ipt)%Ws * Trial(ipt)%Thickness &
        & * RhoBar(:,ipt) * Trial(ipt)%Wt * Trial(ipt)%Jt
      DO ips = 1, SIZE( Trial(1) % N, 2 )
        DO b = 1, SIZE( Trial(1) % T )
          DO a = 1, SIZE( Test(1) % T )
            Mat4(:,:,a,b) = Mat4(:,:,a,b) &
              & + RealVal(ips) &
              & * Test(ipt) % T(a) &
              & * Trial(ipt) % T(b) &
              & * OUTERPROD( a=Test(ipt)%N(:,ips), b=Trial(ipt)%N(:,ips) )
          END DO
        END DO
      END DO
    END DO

  ELSE IF( Term1 .EQ. 1 .AND. Term2 .EQ. 1 ) THEN
    DO ipt = 1, SIZE( Trial )
      RealVal = Trial(ipt)%Js * Trial(ipt)%Ws * Trial(ipt)%Thickness &
        & * RhoBar(:,ipt) * Trial(ipt)%Wt * Trial(ipt)%Jt
      DO ips = 1, SIZE( Trial(1) % N, 2 )
        DO b = 1, SIZE( Trial(1) % T )
          DO a = 1, SIZE( Test(1) % T )
            Mat4(:,:,a,b) = Mat4(:,:,a,b) &
              & + RealVal(ips) &
              & * OUTERPROD( a=Test(ipt)%dNTdt( :, a, ips ), &
                  & b=Trial(ipt)%dNTdt( :, b, ips ) )
          END DO
        END DO
      END DO
    END DO
  ELSE IF( Term1 .EQ. 0 .AND. Term2 .EQ. 1 ) THEN
    DO ipt = 1, SIZE( Trial )
      RealVal = Trial(ipt)%Js * Trial(ipt)%Ws * Trial(ipt)%Thickness &
        & * RhoBar(:,ipt) * Trial(ipt)%Wt * Trial(ipt)%Jt
      DO ips = 1, SIZE( Trial(1) % N, 2 )
        DO b = 1, SIZE( Trial(1) % T )
          DO a = 1, SIZE( Test(1) % T )
            Mat4(:,:,a,b) = Mat4(:,:,a,b) &
              & + RealVal(ips) &
              & * Test(ipt) % T(a) &
              & * OUTERPROD( a=Test(ipt)%N( :, ips ), &
                  & b=Trial(ipt)%dNTdt( :, b, ips ) )
          END DO
        END DO
      END DO
    END DO
  ELSE IF( Term1 .EQ. 1 .AND. Term2 .EQ. 0 ) THEN
    DO ipt = 1, SIZE( Trial )
      RealVal = Trial(ipt)%Js * Trial(ipt)%Ws * Trial(ipt)%Thickness &
        & * RhoBar(:,ipt) * Trial(ipt)%Wt * Trial(ipt)%Jt
      DO ips = 1, SIZE( Trial(1) % N, 2 )
        DO b = 1, SIZE( Trial(1) % T )
          DO a = 1, SIZE( Test(1) % T )
            Mat4(:,:,a,b) = Mat4(:,:,a,b) &
              & + RealVal(ips) &
              & * Trial(ipt) % T(b) &
              & * OUTERPROD( a=Test(ipt)%dNTdt( :, a, ips ), &
                  & b=Trial(ipt)%N( :,ips ) )
          END DO
        END DO
      END DO
    END DO

  END IF

  CALL Convert( From = Mat4, To = Ans )
  IF( PRESENT( nCopy ) ) THEN
    CALL MakeDiagonalCopies( Ans, nCopy )
  END IF

  DEALLOCATE( Mat4, RhoBar, RealVal )

END PROCEDURE st_massMatrix_a

END SUBMODULE MassMatrix


