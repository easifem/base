SUBMODULE( FEMatrix_Module ) NitscheMatrix
!! This Submodule implements method to get Nitsche matrix

USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                              NitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE space_nitsche_mat_1
  LOGICAL( LGT ) :: isLamNod, isMuNod, isEvecNod
  REAL( DFP ), ALLOCATABLE :: LamBar( : ), MuBar( : ), RealVal( : ), &
    & EvecBar( :, : )
  INTEGER( I4B ) :: nns1, nns2, nips, nsd, ips, r1, r2, i, j
  REAL( DFP ), ALLOCATABLE :: SBar( :, : ), DummyMat( :, : )

  nns1 = SIZE( Test % N, 1 ); nns2 = SIZE( Trial % N, 1 )
  nips = SIZE( Trial % N, 2 ); nsd = Trial % RefElem % NSD
  isLamNod = .FALSE.; isMuNod = .FALSE.; isEvecNod = .FALSE.
  IF( Lambda % DefineOn .EQ. Nodal ) isLamNod = .TRUE.
  IF( Mu % DefineOn .EQ. Nodal ) isMuNod = .TRUE.
  IF( EVec % DefineOn .EQ. Nodal ) isEvecNod = .TRUE.

  !<--- LamBar and MuBar contains space varying values of Lam and Mu
  SELECT CASE( Lambda % VarType )
  CASE( Constant )

    ALLOCATE( LamBar( nips ) )
    LamBar = getValues( Lambda, TypeFEVariableScalar, &
      & TypeFEVariableConstant )

  CASE( Space )

    RealVal = getValues( Lambda, TypeFEVariableScalar, &
      & TypeFEVariableSpace )

    IF( isLamNod ) THEN
      LamBar = Interpolation( Trial, RealVal )
    ELSE
      LamBar = RealVal
    END IF
  END SELECT

  SELECT CASE( Mu % VarType )
  CASE( Constant )

    ALLOCATE( MuBar( nips ) )
    MuBar = getValues( Mu, TypeFEVariableScalar, &
      & TypeFEVariableConstant )

  CASE( Space )

    RealVal = getValues( Mu, TypeFEVariableScalar, &
      & TypeFEVariableSpace )

    IF( isMuNod ) THEN
      MuBar = Interpolation( Trial, RealVal )
    ELSE
      MuBar = RealVal
    END IF
  END SELECT

  SELECT CASE( Evec % VarType )
  CASE( Constant )

    ALLOCATE( EvecBar( nsd, nips ) )
    EvecBar( :, 1 ) = getValues( Evec, TypeFEVariableVector, &
      & TypeFEVariableConstant )
    DO i = 2, nsd
      EvecBar( :, i ) = EvecBar( :, 1 )
    END DO

  CASE( Space )

    Ans = getValues( Evec, TypeFEVariableVector, &
      & TypeFEVariableSpace )

    IF( isEvecNod ) THEN
      EvecBar = Interpolation( Trial, Ans )
    ELSE
      EvecBar = Ans
    END IF

    DEALLOCATE( Ans )
  END SELECT

  !<--- make integration parameters
  RealVal = Trial % Ws * Trial % Js * Trial % Thickness

  !<--- allocate Ans
  ALLOCATE( Ans( nns1 * nsd, nns2 * nsd ) ); Ans = 0.0_DFP

  !<---
  DO ips = 1, nips
    DummyMat = &
    &   LamBar( ips ) * RealVal( ips ) &
    & * DOT_PRODUCT( Trial % normal(1:nsd, ips),  EvecBar(1:nsd, ips) ) &
    & * Eye3( 1:nsd, 1:nsd ) &
    & + 2.0 * MuBar( ips ) * RealVal( ips ) &
    & * OUTERPROD(  a = Trial % normal( 1 : nsd, ips ), &
                & b = EvecBar( 1 : nsd, ips ), &
                & Sym = .TRUE. )

    SBar = MATMUL( Trial % dNdXt( :, :, ips ), DummyMat )

    DummyMat = RESHAPE( SBar, [nsd*nns2, 1] )

    SBar = OUTERPROD( Test % N( :, ips ), DummyMat( :, 1 ) )

    r1 = 0; r2 = 0
    DO i = 1, nsd
      r1 = r2 + 1; r2 = i * nns1
      Ans( r1:r2, : ) = Ans( r1:r2, : ) + EvecBar( i, ips ) * SBar( :, : )
    END DO
  END DO

  DEALLOCATE( LamBar, MuBar, RealVal, EvecBar, SBar, DummyMat )
END PROCEDURE space_nitsche_mat_1

!----------------------------------------------------------------------------
!                                                              NitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE space_nitsche_mat_3
  LOGICAL( LGT ) :: isEvecNod
  REAL( DFP ), ALLOCATABLE :: RealVal( : ), EvecBar( :, : ), &
    & SBar( :, : ), DummyMat( :, : )
  INTEGER( I4B ) :: nns1, nns2, nips, nsd, ips, r1, r2, i, j

  nns1 = SIZE( Test % N, 1 ); nns2 = SIZE( Trial % N, 1 )
  nips = SIZE( Trial % N, 2 ); nsd = Trial % RefElem % NSD
  isEvecNod = .FALSE.

  IF( EVec % DefineOn .EQ. Nodal ) isEvecNod = .TRUE.

  SELECT CASE( Evec % VarType )
  CASE( Constant )

    ALLOCATE( EvecBar( nsd, nips ) )
    EvecBar( 1, : ) = getValues( Evec, TypeFEVariableVector, &
      & TypeFEVariableConstant )
    DO i = 2, nsd
      EvecBar( i, : ) = EvecBar( 1, : )
    END DO

  CASE( Space )

    Ans = getValues( Evec, TypeFEVariableVector, &
      & TypeFEVariableSpace )

    IF( isEvecNod ) THEN
      EvecBar = Interpolation( Trial, Ans )
    ELSE
      EvecBar = Ans
    END IF

    DEALLOCATE( Ans )
  END SELECT

  !<--- make integration parameters
  RealVal = Trial % Ws * Trial % Thickness * Trial % Js

  !<--- allocate Ans
  ALLOCATE( Ans( nns1 * nsd, nns2 * nsd ) ); Ans = 0.0_DFP

  !<---
  DO ips = 1, nips
    DummyMat = &
    &   Lambda * RealVal( ips ) &
    & * DOT_PRODUCT( Trial % normal(1:nsd, ips),  EvecBar(1:nsd, ips) ) &
    & * Eye3( 1:nsd, 1:nsd ) &
    & + 2.0 * Mu * RealVal( ips ) &
    & * OUTERPROD(  a = Trial % normal( 1 : nsd, ips ), &
                & b = EvecBar( 1 : nsd, ips ), &
                & Sym = .TRUE. )

    SBar = MATMUL( Trial % dNdXt( :, :, ips ), DummyMat )
    DummyMat = RESHAPE( SBar, [nsd*nns2, 1] )
    SBar = OUTERPROD( a = Test % N( :, ips ), b = DummyMat( :, 1 ) )

    r1 = 0; r2 = 0
    DO i = 1, nsd
      r1 = r2 + 1; r2 = i * nns1
      Ans( r1:r2, : ) = Ans( r1:r2, : ) + EvecBar( i, ips ) * SBar( :, : )
    END DO
  END DO

  DEALLOCATE( RealVal, EvecBar, SBar, DummyMat )
END PROCEDURE space_nitsche_mat_3

!----------------------------------------------------------------------------
!                                                              NitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE space_nitsche_mat_5
  REAL( DFP ), ALLOCATABLE :: RealVal( : ), SBar( : ), cdNdXt( :, : )
  INTEGER( I4B ) :: nns1, nns2, nips, nsd, ips, r1, r2, i, j, c1, c2

  nns1 = SIZE( Test % N, 1 ); nns2 = SIZE( Trial % N, 1 )
  nips = SIZE( Trial % N, 2 ); nsd = Trial % RefElem % NSD
  !<--- make integration parameters
  RealVal = Trial % Ws * Trial % Thickness * Trial % Js
  !<--- allocate Ans
  ALLOCATE( Ans( nns1 * nsd, nns2 * nsd ) ); Ans = 0.0_DFP
  ALLOCATE( cdNdXt( SIZE(Trial%N,1), SIZE(Trial%N,2) ) )
  DO i = 1, SIZE(Trial%N,2)
    cdNdXt( :, i ) = MATMUL( Trial % dNdXt( :, :, i ), Trial%Normal(1:nsd,i))
  END DO

  DO ips = 1, nips
    c1 = 0; c2 = 0
    DO j = 1, nsd
      c1 = c2+1; c2 = j*nns2
      r1 = 0; r2 = 0
      DO i = 1, nsd
        r1 = r2 + 1; r2 = i * nns1
        IF( i .EQ. j ) THEN
          SBar = Lambda*trial%normal(i,ips)*trial%dNdXt(:,j,ips) &
            & + Mu*cdNdXt(:,ips) &
            & + Mu*trial%normal(j,ips)*trial%dNdXt(:,i,ips)
        ELSE
          SBar = Lambda*trial%normal(i,ips)*trial%dNdXt(:,j,ips) &
            & + Mu*trial%normal(j,ips)*trial%dNdXt(:,i,ips)
        END IF
        Ans( r1:r2, c1:c2 ) = Ans( r1:r2, c1:c2 ) &
          & + RealVal( ips ) * OUTERPROD( Test%N(:,ips), SBar )
      END DO
    END DO
  END DO

  DEALLOCATE( RealVal, SBar, cdNdXt )
END PROCEDURE space_nitsche_mat_5

!----------------------------------------------------------------------------
!                                                              NitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE space_nitsche_mat_7
  REAL( DFP ), ALLOCATABLE :: RealVal( : ), SBar( : ), cdNdXt( :, : ), &
    & LamBar(:), MuBar(:)
  INTEGER( I4B ) :: nns1, nns2, nips, nsd, ips, r1, r2, i, j, c1, c2

  nns1 = SIZE( Test % N, 1 ); nns2 = SIZE( Trial % N, 1 )
  nips = SIZE( Trial % N, 2 ); nsd = Trial % RefElem % NSD

  SELECT CASE( Lambda % VarType )
  CASE( Constant )

    ALLOCATE( LamBar( nips ) )
    LamBar = getValues( Lambda, TypeFEVariableScalar, &
      & TypeFEVariableConstant )

  CASE( Space )

    RealVal = getValues( Lambda, TypeFEVariableScalar, &
      & TypeFEVariableSpace )

    IF( Lambda % DefineOn .EQ. Nodal  ) THEN
      LamBar = Interpolation( Trial, RealVal )
    ELSE
      LamBar = RealVal
    END IF
  END SELECT

  SELECT CASE( Mu % VarType )
  CASE( Constant )

    ALLOCATE( MuBar( nips ) )
    MuBar = getValues( Mu, TypeFEVariableScalar, &
      & TypeFEVariableConstant )

  CASE( Space )

    RealVal = getValues( Mu, TypeFEVariableScalar, &
      & TypeFEVariableSpace )

    IF( Mu % DefineOn .EQ. Nodal  ) THEN
      MuBar = Interpolation( Trial, RealVal )
    ELSE
      MuBar = RealVal
    END IF
  END SELECT

  !<--- make integration parameters
  RealVal = Trial % Ws * Trial % Thickness * Trial % Js
  !<--- allocate Ans
  ALLOCATE( Ans( nns1 * nsd, nns2 * nsd ) ); Ans = 0.0_DFP
  ALLOCATE( cdNdXt( SIZE(Trial%N,1), SIZE(Trial%N,2) ) )
  DO i = 1, SIZE(Trial%N,2)
    cdNdXt( :, i ) = MATMUL( Trial % dNdXt( :, :, i ), Trial%Normal(1:nsd,i))
  END DO

  DO ips = 1, nips
    c1 = 0; c2 = 0
    DO j = 1, nsd
      c1 = c2+1; c2 = j*nns2
      r1 = 0; r2 = 0
      DO i = 1, nsd
        r1 = r2 + 1; r2 = i * nns1
        IF( i .EQ. j ) THEN
          SBar = LamBar(ips)*trial%normal(i,ips)*trial%dNdXt(:,j,ips) &
            & + MuBar(ips)*cdNdXt(:,ips) &
            & + MuBar(ips)*trial%normal(j,ips)*trial%dNdXt(:,i,ips)
        ELSE
          SBar = LamBar(ips)*trial%normal(i,ips)*trial%dNdXt(:,j,ips) &
            & + MuBar(ips)*trial%normal(j,ips)*trial%dNdXt(:,i,ips)
        END IF
        Ans( r1:r2, c1:c2 ) = Ans( r1:r2, c1:c2 ) &
          & + RealVal( ips ) * OUTERPROD( Test%N(:,ips), SBar )
      END DO
    END DO
  END DO

  DEALLOCATE( RealVal, SBar, cdNdXt, LamBar, MuBar )
END PROCEDURE space_nitsche_mat_7

!----------------------------------------------------------------------------
!                                                              NitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE space_nitsche_mat_2

  LOGICAL( LGT ) :: isAlphaNod, isEvecNod
  INTEGER( I4B ) :: nns1, nns2, nsd, nips, ips, i, j, r1, r2, c1, c2
  REAL( DFP ), ALLOCATABLE :: AlphaBar( : ), EvecBar( :, : ), RealVal( : )
  REAL( DFP ), ALLOCATABLE :: DummyMat( :, : )

  nns1 = SIZE( Test % N, 1 ); nns2 = SIZE( Trial % N, 1 )
  nips = SIZE( Trial % N, 2 ); nsd = Trial % RefElem % NSD

  isAlphaNod = .FALSE.; isEvecNod = .FALSE.
  IF( Alpha % DefineOn .EQ. Nodal ) isAlphaNod = .TRUE.
  IF( Evec % DefineOn .EQ. Nodal ) isEvecNod = .TRUE.

  SELECT CASE( Alpha % VarType )
  CASE( Constant )

    ALLOCATE( AlphaBar( nips ) )
    AlphaBar = getValues( Alpha, TypeFEVariableScalar, &
      & TypeFEVariableConstant )

  CASE( Space )

    RealVal = getValues( Alpha, TypeFEVariableScalar, &
      & TypeFEVariableSpace )

    IF( isAlphaNod ) THEN
      AlphaBar = Interpolation( Trial, RealVal )
    ELSE
      AlphaBar = RealVal
    END IF
  END SELECT

  SELECT CASE( Evec % VarType )
  CASE( Constant )

    ALLOCATE( EvecBar( nsd, nips ) )
    EvecBar( 1, : ) = getValues( Evec, TypeFEVariableVector, &
      & TypeFEVariableConstant )
    DO i = 2, nsd
      EvecBar( i, : ) = EvecBar( 1, : )
    END DO

  CASE( Space )

    Ans = getValues( Evec, TypeFEVariableVector, &
      & TypeFEVariableSpace )

    IF( isEvecNod ) THEN
      EvecBar = Interpolation( Trial, Ans )
    ELSE
      EvecBar = Ans
    END IF

    DEALLOCATE( Ans )
  END SELECT

  RealVal = Trial % Ws * Trial % Js * Trial % Thickness * AlphaBar
  ALLOCATE( Ans( nns1 * nsd, nns2 * nsd ) ); Ans = 0.0_DFP

  DO ips = 1, nips
    DummyMat = RealVal( ips ) * &
      & OUTERPROD( a = Test % N( :, ips ), b = Trial % N( :, ips ) )
    c1 = 0; c2 = 0
    DO j = 1, nsd
      c1 = c2 + 1; c2 = j * nns2; r1 = 0; r2 = r1
      DO i = 1, nsd
        r1 = r2 + 1; r2 = i * nns1
        Ans( r1:r2, c1:c2 ) = Ans( r1:r2, c1:c2 ) + &
          & EvecBar( i, ips ) * EvecBar( j, ips ) * DummyMat
      END DO
    END DO
  END DO

  DEALLOCATE( AlphaBar, EvecBar, RealVal, DummyMat )
END PROCEDURE space_nitsche_mat_2

!----------------------------------------------------------------------------
!                                                              NitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE space_nitsche_mat_4

  LOGICAL( LGT ) :: isEvecNod
  INTEGER( I4B ) :: nns1, nns2, nsd, nips, ips, i, j, r1, r2, c1, c2
  REAL( DFP ), ALLOCATABLE :: EvecBar( :, : ), RealVal( : )
  REAL( DFP ), ALLOCATABLE :: DummyMat( :, : )

  nns1 = SIZE( Test % N, 1 ); nns2 = SIZE( Trial % N, 1 )
  nips = SIZE( Trial % N, 2 ); nsd = Trial % RefElem % NSD

  IF( Evec % DefineOn .EQ. Nodal ) THEN
    isEvecNod = .TRUE.
  ELSE
    isEvecNod = .FALSE.
  END IF

  SELECT CASE( Evec % VarType )
  CASE( Constant )

    ALLOCATE( EvecBar( nsd, nips ) )
    EvecBar( 1, : ) = getValues( Evec, TypeFEVariableVector, &
      & TypeFEVariableConstant )
    DO i = 2, nsd
      EvecBar( i, : ) = EvecBar( 1, : )
    END DO

  CASE( Space )

    Ans = getValues( Evec, TypeFEVariableVector, &
      & TypeFEVariableSpace )

    IF( isEvecNod ) THEN
      EvecBar = Interpolation( Trial, Ans )
    ELSE
      EvecBar = Ans
    END IF

    DEALLOCATE( Ans )
  END SELECT

  RealVal = Trial % Ws * Trial % Js * Trial % Thickness * Alpha
  ALLOCATE( Ans( nns1 * nsd, nns2 * nsd ) ); Ans = 0.0_DFP

  DO ips = 1, nips
    DummyMat = RealVal( ips ) * &
      & OUTERPROD( a = Test % N( :, ips ), b = Trial % N( :, ips ) )
    c1 = 0; c2 = 0
    DO j = 1, nsd
      c1 = c2 + 1; c2 = j * nns2; r1 = 0; r2 = r1
      DO i = 1, nsd
        r1 = r2 + 1; r2 = i * nns1
        Ans( r1:r2, c1:c2 ) = Ans( r1:r2, c1:c2 ) + &
          & EvecBar( i, ips ) * EvecBar( j, ips ) * DummyMat
      END DO
    END DO
  END DO

  DEALLOCATE( EvecBar, RealVal, DummyMat )
END PROCEDURE space_nitsche_mat_4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE NitscheMatrix