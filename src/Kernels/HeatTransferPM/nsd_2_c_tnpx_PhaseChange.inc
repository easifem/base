
SUBROUTINE ComputePhaseChangeQuantity_1( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj

  IF( .NOT. obj%PCFlag(3,a) ) THEN
    RETURN
  END IF

  temp_gp = Interpolation( obj=spacesd, val=temp_nodes )
  temp0_gp = Interpolation( obj=spacesd, val=temp0_nodes )

  ! loop over gauss points
  DO kk = 1, NUM_GP
    dummyReal = Obj%enthalapy%Val(2, kk, a ) &
      & -  Obj%enthalapy%Val(3, kk, a ) &
      & + Water%density * Water%latentHeatMelt

    noPCflag =ABS(obj%volContent%val(2,kk,a)-obj%volContent0%val(2,kk,a)) &
      & .LE. 1.0E-10

    ! IF(  noPCflag ) THEN
    !   HDelta_tanmat( kk ) = 0.0_DFP
    !   HDelta_rhs( kk ) = 0.0_DFP
    ! ELSE
    !   HDelta_tanmat( kk ) = dummyReal &
    !     & * (obj%volContent%val(2,kk,a) - obj%volContent0%val(2,kk,a))&
    !     & / (temp_gp(kk)-temp0_gp(kk)) / Obj%dt
    !   HDelta_rhs( kk ) = dummyReal &
    !     & * (obj%volContent%val(2,kk,a) - obj%volContent0%val(2,kk,a))&
    !     & / Obj%dt
    ! END IF

    HDelta_tanmat( kk ) = dummyReal &
      & * obj%soils(matID)%SFCCModel%getSlope(Temp=temp_gp(kk)) &
      & / Obj%dt

    HDelta_rhs( kk ) = dummyReal &
      & * obj%soils(matID)%SFCCModel%getSlope(Temp=temp_gp(kk)) &
      & * (temp_gp(kk) - temp0_gp(kk))/obj%dt

  END DO

  HDelta_fevar = QuadratureVariable( HDelta_tanmat, &
    & typefevariableScalar, typeFeVariableSpace )
  Be = MassMatrix( Test=spacesd, Trial=spacesd, Rho=HDelta_fevar)

  DO kk = 1, NUM_GP
    dummyReal = spacesd%Js(kk) * spacesd%Ws(kk) * spacesd%Thickness(kk)
    rhs_e( : ) = rhs_e( : ) - dummyReal * spacesd%N(:,kk)*HDelta_rhs(kk)
  END DO

END SUBROUTINE ComputePhaseChangeQuantity_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ComputePhaseChangeQuantity_2( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj

  REAL( DFP ) :: xij_new(2,6), temp_new( 6 ), temp0_new( 6 )
  REAL( DFP ) :: H_s, H_w, H_i, theta_w, theta0_w
  INTEGER( I4B ) :: nptrs_new( 3 ), NPTRS_ALL(3,4), ii
  REAL( DFP ), PARAMETER :: xij0_new(2,6) = RESHAPE([ &
    & 0., 0., &
    & 1., 0., &
    & 0., 1., &
    & 0.5, 0., &
    & 0.5, 0.5, &
    & 0.0, 0.5 &
    &],[2,6])
  REAL( DFP ) :: xi, eta

  IF( .NOT. obj%PCFlag(3,a) ) THEN
    RETURN
  END IF

  xij_new( 1:2, 1:3 ) = xij( 1:2, 1:3 )
  xij_new( 1:2, 4 ) = 0.5_DFP * (xij( 1:2, 1 ) + xij(1:2, 2))
  xij_new( 1:2, 5 ) = 0.5_DFP * (xij( 1:2, 2 ) + xij(1:2, 3))
  xij_new( 1:2, 6 ) = 0.5_DFP * (xij( 1:2, 3 ) + xij(1:2, 1))

  temp_new( 1:3 ) = temp_nodes( 1:3 )
  temp_new( 4 ) = 0.5_DFP * (temp_new( 1 ) + temp_new( 2 ))
  temp_new( 5 ) = 0.5_DFP * (temp_new( 2 ) + temp_new( 3 ))
  temp_new( 6 ) = 0.5_DFP * (temp_new( 3 ) + temp_new( 1 ))

  temp0_new( 1:3 ) = temp0_nodes( 1:3 )
  temp0_new( 4 ) = 0.5_DFP * (temp0_new( 1 ) + temp0_new( 2 ))
  temp0_new( 5 ) = 0.5_DFP * (temp0_new( 2 ) + temp0_new( 3 ))
  temp0_new( 6 ) = 0.5_DFP * (temp0_new( 3 ) + temp0_new( 1 ))

  NPTRS_ALL( 1:3, 1 ) = [1,4,6]
  NPTRS_ALL( 1:3, 2 ) = [4,2,5]
  NPTRS_ALL( 1:3, 3 ) = [6,5,3]
  NPTRS_ALL( 1:3, 4 ) = [5,6,4]

  DO ii =1, 4
    nptrs_new = NPTRS_ALL( :, ii )
    CALL Initiate( spaceQuad_A, tXi=2, tPoints=3 )
    spaceQuad_A%Points(1:2, 1:3) = Interpolation( obj=spacesd, &
      & val=xij0_new(1:2, nptrs_new) )
    spaceQuad_A%Points(3, 1:NUM_GP) = spaceQuad%Points(3, 1:NUM_GP)

    CALL initiate( obj = spacesd_A, quad = spacequad_A, &
    & refelem = Elem%refElem, &
    & continuityType= obj%spaceContinuity, &
    & interpolType = obj%spaceInterpol )

    CALL setValue(obj=spacesd_A, val=xij_new(1:2, nptrs_new), N=spacesd%N, &
      & dNdXi=spacesd%dNdXi)

    temp_gp = Interpolation( obj=spacesd_A, val=temp_nodes )
    temp0_gp = Interpolation( obj=spacesd_A, val=temp0_nodes )

    ! loop over gauss points
    DO kk = 1, NUM_GP
      theta_w = obj%soils(matID)%SFCCModel%getValue(Temp=temp_gp(kk))
      theta0_w = obj%soils(matID)%SFCCModel%getValue(Temp=temp0_gp(kk))

      noPCflag = ABS(theta_w-theta0_w) .LE. 1.0E-5

      IF(  noPCflag ) THEN
        HDelta_tanmat( kk ) = 0.0_DFP
        HDelta_rhs( kk ) = 0.0_DFP
      ELSE
        H_w = volHeatCap_water() * temp_gp( kk )
        H_i = volHeatCap_ice() * temp_gp( kk )
        dummyReal = H_w -  H_i + Water%density * Water%latentHeatMelt

        HDelta_tanmat( kk ) = dummyReal &
          & * (theta_w - theta0_w)&
          & / (temp_gp(kk)-temp0_gp(kk)) / Obj%dt

        HDelta_rhs( kk ) = dummyReal &
          & * (theta_w - theta0_w) / Obj%dt
      END IF
    END DO

    HDelta_fevar = QuadratureVariable( HDelta_tanmat, &
      & typefevariableScalar, typeFeVariableSpace )
    Be = MassMatrix( Test=spacesd_A, Trial=spacesd_A, Rho=HDelta_fevar)
    tanmat( :, : ) = tanmat( :, : ) + Be( :, : )

    DO kk = 1, NUM_GP
      dummyReal = spacesd_A%Js(kk) * spacesd_A%Ws(kk) * spacesd_A%Thickness(kk)
      rhs_e( : ) = rhs_e( : ) - dummyReal * spacesd_A%N(:,kk)*HDelta_rhs(kk)
    END DO
  END DO

END SUBROUTINE ComputePhaseChangeQuantity_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ComputePhaseChangeQuantity_3( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj

  REAL( DFP ) :: xij_new(2,6), temp_new( 6 ), temp0_new( 6 )
  REAL( DFP ) :: H_s, H_w, H_i, theta_w, theta0_w, xi1, xi2, xi3, xi
  INTEGER( I4B ) :: nptrs_new( 3 ),NPTRS_ALL(3,4), ii
  REAL( DFP ) :: xij0_new(2,6) = RESHAPE([ &
    & 0., 0., &
    & 1., 0., &
    & 0., 1., &
    & 0.5, 0., &
    & 0.5, 0.5, &
    & 0.0, 0.5 &
    &],[2,6])
  REAL( DFP ) :: Temp_f, dummy_real, tol_1, tol_2
  CHARACTER( LEN = 3 ) :: FrontOnLine

  IF( .NOT. obj%PCFlag(3,a) ) THEN
    RETURN
  END IF

  tol_1 = 1.0E-10
  tol_2 = 1.0E-10

  FrontOnLine = "TTT"

  Temp_f = 270.15_DFP

  xij_new( 1:2, 1:3 ) = xij( 1:2, 1:3 )
  temp_new( 1:3 ) = temp_nodes( 1:3 )
  temp0_new( 1:3 ) = temp0_nodes( 1:3 )

  !-----------------------------------------
  ! Search on line 1-2
  dummy_real = ( temp0_new( 2 ) - temp0_new( 1 ) )
  IF( ABS( dummy_real ) .GE. tol_1 ) THEN
    xi = ( Temp_f - temp0_new( 1 ) ) / ( temp0_new( 2 ) - temp0_new( 1 ) )
    IF( ABS( xi - 1.0_DFP ) .LE. tol_2 .OR. xi .GE. 1.0_DFP ) THEN
      FrontOnLine(1:1) = "F"
    ELSE IF ( xi .LE. tol_2 ) THEN
      FrontOnLine(1:1) = "F"
    END IF
  ELSE
    FrontOnLine(1:1) = "F"
  END IF

  IF( FrontOnLine(1:1) .EQ. "F" ) THEN
    xi = 0.5_DFP
  END IF
  xi1 = xi

  !-----------------------------------------
  ! Search on line 2-3
  dummy_real = temp0_new( 2 ) - temp0_new( 3 )
  IF( ABS( dummy_real ) .GE. tol_1 ) THEN
    xi = ( Temp_f - temp0_new( 3 ) ) / ( temp0_new( 2 ) - temp0_new( 3 ) )
    IF( ABS( xi - 1.0_DFP ) .LE. tol_2 .OR. xi .GE. 1.0_DFP ) THEN
      FrontOnLine(2:2) = "F"
    ELSE IF ( xi .LE. tol_2 ) THEN
      FrontOnLine(2:2) = "F"
    END IF
  ELSE
    FrontOnLine(2:2) = "F"
  END IF
  !
  IF( FrontOnLine(2:2) .EQ. "F" ) THEN
    xi = 0.5_DFP
  END IF
  xi2 = xi

  !---------------------------------------
  ! Search on line 3-1
  dummy_real = temp0_new( 3 ) - temp0_new( 1 )
  IF( ABS( dummy_real ) .GE. tol_1 ) THEN
    xi = ( Temp_f - temp0_new( 1 ) ) / ( temp0_new( 3 ) - temp0_new( 1 ) )
    IF( ABS( xi - 1.0_DFP ) .LE. tol_2 .OR. xi .GE. 1.0_DFP ) THEN
      FrontOnLine(3:3) = "F"
    ELSE IF ( xi .LT. tol_2 ) THEN
      FrontOnLine(3:3) = "F"
    END IF
  ELSE
    FrontOnLine(3:3) = "F"
  END IF
  !
  IF( FrontOnLine(3:3) .EQ. "F" ) THEN
    xi = 0.5_DFP
  END IF

  xi3 = xi

  xij0_new( 1:2, 4 ) = [xi1, 0.0_DFP]
  temp_new( 4 ) = ( 1.0_DFP - xi1 ) * temp_new( 1 ) + xi1 * temp_new( 2 )
  temp0_new( 4 ) = ( 1.0_DFP - xi1 ) * temp0_new( 1 ) + xi1 * temp0_new( 2 )
  xij_new( :,4 ) = ( 1.0_DFP - xi1 ) * xij_new( :,1 ) + xi1 * xij_new( :,2 )

  xij0_new( 1:2, 5 ) = [xi2, 1-xi2]
  temp_new( 5 ) = temp_new( 3 ) + (temp_new(2) - temp_new(3)) * xi2
  temp0_new( 5 ) = temp0_new( 3 ) + (temp0_new(2) - temp0_new(3)) * xi2
  xij_new( :,5 ) = xij_new( :,3 ) + (xij_new(:,2) - xij_new(:,3)) * xi2

  xij0_new( 1:2, 6 ) = [0.0_DFP, xi3]
  temp_new( 6 ) = temp_new( 1 ) + (temp_new(3) - temp_new(1)) * xi3
  temp0_new( 6 ) = temp0_new( 1 ) + (temp0_new(3) - temp0_new(1)) * xi3
  xij_new(:, 6 ) = xij_new(:, 1 ) + (xij_new(:,3) - xij_new(:,1)) * xi3

  SELECT CASE( FrontOnLine )
  CASE( "FTT", "TTF", "TFT" )
    NPTRS_ALL( 1:3, 1 ) = [1,4,6]
    NPTRS_ALL( 1:3, 2 ) = [4,2,5]
    NPTRS_ALL( 1:3, 3 ) = [6,5,3]
    NPTRS_ALL( 1:3, 4 ) = [5,6,4]

  CASE( "FTF" )
    NPTRS_ALL( 1:3, 1 ) = [5,1,4]
    NPTRS_ALL( 1:3, 2 ) = [5,4,2]
    NPTRS_ALL( 1:3, 3 ) = [5,3,6]
    NPTRS_ALL( 1:3, 4 ) = [5,6,1]
  CASE( "FFT" )
    NPTRS_ALL( 1:3, 1 ) = [6,1,4]
    NPTRS_ALL( 1:3, 2 ) = [6,4,2]
    NPTRS_ALL( 1:3, 3 ) = [6,2,5]
    NPTRS_ALL( 1:3, 4 ) = [6,5,3]
  CASE( "TFF" )
    NPTRS_ALL( 1:3, 1 ) = [4,2,5]
    NPTRS_ALL( 1:3, 2 ) = [4,5,3]
    NPTRS_ALL( 1:3, 3 ) = [4,3,6]
    NPTRS_ALL( 1:3, 4 ) = [4,6,1]
  CASE DEFAULT
    RETURN
  END SELECT

  DO ii =1, 4
    nptrs_new = NPTRS_ALL( :, ii )
    CALL Initiate( spaceQuad_A, tXi=2, tPoints=3 )
    spaceQuad_A%Points(1:2, 1:3) = Interpolation( obj=spacesd, &
      & val=xij0_new(1:2, nptrs_new) )
    spaceQuad_A%Points(3, 1:NUM_GP) = spaceQuad%Points(3, 1:NUM_GP)

    CALL initiate( obj = spacesd_A, quad = spacequad_A, &
    & refelem = Elem%refElem, &
    & continuityType= obj%spaceContinuity, &
    & interpolType = obj%spaceInterpol )

    CALL setValue(obj=spacesd_A, val=xij_new(1:2, nptrs_new), N=spacesd%N, &
      & dNdXi=spacesd%dNdXi)

    temp_gp = Interpolation( obj=spacesd_A, val=temp_nodes )
    temp0_gp = Interpolation( obj=spacesd_A, val=temp0_nodes )

    ! loop over gauss points
    DO kk = 1, NUM_GP
      theta_w = obj%soils(matID)%SFCCModel%getValue(Temp=temp_gp(kk))
      theta0_w = obj%soils(matID)%SFCCModel%getValue(Temp=temp0_gp(kk))

      ! noPCflag = ABS(theta_w-theta0_w) .LE. tol_2

      ! IF(  noPCflag ) THEN
      !   HDelta_tanmat( kk ) = 0.0_DFP
      !   HDelta_rhs( kk ) = 0.0_DFP
      ! ELSE
        H_w = volHeatCap_water() * temp_gp( kk )
        H_i = volHeatCap_ice() * temp_gp( kk )
        dummyReal = H_w -  H_i + Water%density * Water%latentHeatMelt

        HDelta_tanmat( kk ) = dummyReal &
          & * obj%soils(matID)%SFCCModel%getSlope(Temp=temp_gp(kk)) &
          & / Obj%dt

        ! HDelta_tanmat( kk ) = dummyReal &
        !   & * (theta_w - theta0_w) &
        !   & / (temp_gp(kk)-temp0_gp(kk)) / Obj%dt

        HDelta_rhs( kk ) = dummyReal &
          & * obj%soils(matID)%SFCCModel%getSlope(Temp=temp_gp(kk)) &
          & * (temp_gp(kk) - temp0_gp(kk))/obj%dt
        ! HDelta_rhs( kk ) = dummyReal &
        !   & * (theta_w - theta0_w) / Obj%dt
      ! END IF
    END DO

    HDelta_fevar = QuadratureVariable( HDelta_tanmat, &
      & typefevariableScalar, typeFeVariableSpace )
    Be = MassMatrix( Test=spacesd_A, Trial=spacesd_A, Rho=HDelta_fevar)
    tanmat( :, : ) = tanmat( :, : ) + Be( :, : )

    DO kk = 1, NUM_GP
      dummyReal = spacesd_A%Js(kk) * spacesd_A%Ws(kk) * spacesd_A%Thickness(kk)
      rhs_e( : ) = rhs_e( : ) - dummyReal * spacesd_A%N(:,kk)*HDelta_rhs(kk)
    END DO
  END DO

END SUBROUTINE ComputePhaseChangeQuantity_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ComputePhaseChangeQuantity_4( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj

  REAL( DFP ) :: xij_new(2,6), temp_new( 6 ), temp0_new( 6 )
  REAL( DFP ) :: H_s, H_w, H_i, theta_w, theta0_w, xi_s, xi1, xi2, xi3, xi_l
  INTEGER( I4B ) :: nptrs_new( 3 ),NPTRS_ALL(3,4), ii
  REAL( DFP ) :: xij0_new(2,6) = RESHAPE([ &
    & 0., 0., &
    & 1., 0., &
    & 0., 1., &
    & 0.5, 0., &
    & 0.5, 0.5, &
    & 0.0, 0.5 &
    &],[2,6])
  REAL( DFP ) :: Temp_l, Temp_s, dummy_real, tol_1, tol_2
  CHARACTER( LEN = 3 ) :: FrontOnLine
  CHARACTER( LEN=1 ) :: c_s, c_l

  IF( .NOT. obj%PCFlag(3,a) ) THEN
    RETURN
  END IF

  tol_1 = 1.0E-10
  tol_2 = 1.0E-10

  FrontOnLine = "TTT"

  Temp_s = 270.15_DFP
  Temp_l = 270.15_DFP

  xij_new( 1:2, 1:3 ) = xij( 1:2, 1:3 )
  temp_new( 1:3 ) = temp_nodes( 1:3 )
  temp0_new( 1:3 ) = temp0_nodes( 1:3 )

  !-----------------------------------------
  ! Search on line 1-2
  c_s="T"; c_l="T"
  dummy_real = ( temp0_new( 2 ) - temp0_new( 1 ) )
  IF( ABS( dummy_real ) .GE. tol_1 ) THEN
    xi_s = ( Temp_s - temp0_new( 1 ) ) / ( temp0_new( 2 ) - temp0_new( 1 ) )
    xi_l = ( Temp_l - temp0_new( 1 ) ) / ( temp0_new( 2 ) - temp0_new( 1 ) )
    IF( ABS( xi_s - 1.0_DFP ) .LE. tol_2 .OR. xi_s .GE. 1.0_DFP ) THEN
      c_s = "F"
    ELSE IF ( xi_s .LE. tol_2 ) THEN
      c_s = "F"
    END IF
    IF( ABS( xi_l - 1.0_DFP ) .LE. tol_2 .OR. xi_l .GE. 1.0_DFP ) THEN
      c_l = "F"
    ELSE IF ( xi_l .LE. tol_2 ) THEN
      c_l = "F"
    END IF
  ELSE
    c_s = "F"
    c_l = "F"
  END IF

  SELECT CASE( c_s//c_l )
  CASE( "FF" )
    FrontOnLine(1:1) = "F"
    xi1 = 0.5_DFP
    xi_s = 0.5_DFP
    xi_l = 0.5_DFP
  CASE( "FT")
    FrontOnLine(1:1) = "T"
    xi1 = xi_l
  CASE( "TF")
    FrontOnLine(1:1) = "T"
    xi1 = xi_s
  CASE( "TT")
    FrontOnLine(1:1) = "T"
    xi1 = xi_l
  END SELECT

  !-----------------------------------------
  ! Search on line 2-3
  c_s="T"; c_l="T"
  dummy_real = temp0_new( 2 ) - temp0_new( 3 )
  IF( ABS( dummy_real ) .GE. tol_1 ) THEN
    xi_s = ( Temp_s - temp0_new( 3 ) ) / ( temp0_new( 2 ) - temp0_new( 3 ) )
    xi_l = ( Temp_l - temp0_new( 3 ) ) / ( temp0_new( 2 ) - temp0_new( 3 ) )
    IF( ABS( xi_s - 1.0_DFP ) .LE. tol_2 .OR. xi_s .GE. 1.0_DFP ) THEN
      c_s = "F"
    ELSE IF ( xi_s .LE. tol_2 ) THEN
      c_s = "F"
    END IF

    IF( ABS( xi_l - 1.0_DFP ) .LE. tol_2 .OR. xi_l .GE. 1.0_DFP ) THEN
      c_l = "F"
    ELSE IF ( xi_l .LE. tol_2 ) THEN
      c_l = "F"
    END IF
  ELSE
    c_s = "F"
    c_l = "F"
  END IF
  !
  SELECT CASE( c_s//c_l )
  CASE( "FF" )
    FrontOnLine(2:2) = "F"
    xi2 = 0.5_DFP
    xi_s = 0.5_DFP
    xi_l = 0.5_DFP
  CASE( "FT")
    FrontOnLine(2:2) = "T"
    xi2 = xi_l
  CASE( "TF")
    FrontOnLine(2:2) = "T"
    xi2 = xi_s
  CASE( "TT")
    FrontOnLine(2:2) = "T"
    xi2 = xi_l
  END SELECT
  !
  !---------------------------------------
  ! Search on line 3-1
  c_s="T"; c_l="T"
  dummy_real = temp0_new( 3 ) - temp0_new( 1 )
  IF( ABS( dummy_real ) .GE. tol_1 ) THEN
    xi_s = ( Temp_s - temp0_new( 1 ) ) / ( temp0_new( 3 ) - temp0_new( 1 ) )
    xi_l = ( Temp_l - temp0_new( 1 ) ) / ( temp0_new( 3 ) - temp0_new( 1 ) )

    IF( ABS( xi_s - 1.0_DFP ) .LE. tol_2 .OR. xi_s .GE. 1.0_DFP ) THEN
      c_s = "F"
    ELSE IF ( xi_s .LT. tol_2 ) THEN
      c_s = "F"
    END IF

    IF( ABS( xi_l - 1.0_DFP ) .LE. tol_2 .OR. xi_l .GE. 1.0_DFP ) THEN
      c_l = "F"
    ELSE IF ( xi_s .LT. tol_2 ) THEN
      c_l = "F"
    END IF
  ELSE
    c_s = "F"
    c_l = "F"
  END IF
  !
  SELECT CASE( c_s//c_l )
  CASE( "FF" )
    FrontOnLine(3:3) = "F"
    xi3 = 0.5_DFP
    xi_s = 0.5_DFP
    xi_l = 0.5_DFP
  CASE( "FT")
    FrontOnLine(3:3) = "T"
    xi3 = xi_l
  CASE( "TF")
    FrontOnLine(3:3) = "T"
    xi3 = xi_s
  CASE( "TT")
    FrontOnLine(3:3) = "T"
    xi3 = xi_l
  END SELECT

  xij0_new( 1:2, 4 ) = [xi1, 0.0_DFP]
  temp_new( 4 ) = ( 1.0_DFP - xi1 ) * temp_new( 1 ) + xi1 * temp_new( 2 )
  temp0_new( 4 ) = ( 1.0_DFP - xi1 ) * temp0_new( 1 ) + xi1 * temp0_new( 2 )
  xij_new( :,4 ) = ( 1.0_DFP - xi1 ) * xij_new( :,1 ) + xi1 * xij_new( :,2 )

  xij0_new( 1:2, 5 ) = [xi2, 1-xi2]
  temp_new( 5 ) = temp_new( 3 ) + (temp_new(2) - temp_new(3)) * xi2
  temp0_new( 5 ) = temp0_new( 3 ) + (temp0_new(2) - temp0_new(3)) * xi2
  xij_new( :,5 ) = xij_new( :,3 ) + (xij_new(:,2) - xij_new(:,3)) * xi2

  xij0_new( 1:2, 6 ) = [0.0_DFP, xi3]
  temp_new( 6 ) = temp_new( 1 ) + (temp_new(3) - temp_new(1)) * xi3
  temp0_new( 6 ) = temp0_new( 1 ) + (temp0_new(3) - temp0_new(1)) * xi3
  xij_new(:, 6 ) = xij_new(:, 1 ) + (xij_new(:,3) - xij_new(:,1)) * xi3

  SELECT CASE( FrontOnLine )
  CASE( "FTT", "TTF", "TFT" )
    NPTRS_ALL( 1:3, 1 ) = [1,4,6]
    NPTRS_ALL( 1:3, 2 ) = [4,2,5]
    NPTRS_ALL( 1:3, 3 ) = [6,5,3]
    NPTRS_ALL( 1:3, 4 ) = [5,6,4]

  CASE( "FTF" )
    NPTRS_ALL( 1:3, 1 ) = [5,1,4]
    NPTRS_ALL( 1:3, 2 ) = [5,4,2]
    NPTRS_ALL( 1:3, 3 ) = [5,3,6]
    NPTRS_ALL( 1:3, 4 ) = [5,6,1]
  CASE( "FFT" )
    NPTRS_ALL( 1:3, 1 ) = [6,1,4]
    NPTRS_ALL( 1:3, 2 ) = [6,4,2]
    NPTRS_ALL( 1:3, 3 ) = [6,2,5]
    NPTRS_ALL( 1:3, 4 ) = [6,5,3]
  CASE( "TFF" )
    NPTRS_ALL( 1:3, 1 ) = [4,2,5]
    NPTRS_ALL( 1:3, 2 ) = [4,5,3]
    NPTRS_ALL( 1:3, 3 ) = [4,3,6]
    NPTRS_ALL( 1:3, 4 ) = [4,6,1]
  CASE DEFAULT
    RETURN
  END SELECT

  DO ii =1, 4
    nptrs_new = NPTRS_ALL( :, ii )
    CALL Initiate( spaceQuad_A, tXi=2, tPoints=3 )
    spaceQuad_A%Points(1:2, 1:3) = Interpolation( obj=spacesd, &
      & val=xij0_new(1:2, nptrs_new) )
    spaceQuad_A%Points(3, 1:NUM_GP) = spaceQuad%Points(3, 1:NUM_GP)

    CALL initiate( obj = spacesd_A, quad = spacequad_A, &
    & refelem = Elem%refElem, &
    & continuityType= obj%spaceContinuity, &
    & interpolType = obj%spaceInterpol )

    CALL setValue(obj=spacesd_A, val=xij_new(1:2, nptrs_new), N=spacesd%N, &
      & dNdXi=spacesd%dNdXi)

    temp_gp = Interpolation( obj=spacesd_A, val=temp_nodes )
    temp0_gp = Interpolation( obj=spacesd_A, val=temp0_nodes )

    ! loop over gauss points
    DO kk = 1, NUM_GP
      theta_w = obj%soils(matID)%SFCCModel%getValue(Temp=temp_gp(kk))
      theta0_w = obj%soils(matID)%SFCCModel%getValue(Temp=temp0_gp(kk))

      ! noPCflag = ABS(theta_w-theta0_w) .LE. tol_2

      ! IF(  noPCflag ) THEN
      !   HDelta_tanmat( kk ) = 0.0_DFP
      !   HDelta_rhs( kk ) = 0.0_DFP
      ! ELSE
        H_w = volHeatCap_water() * temp_gp( kk )
        H_i = volHeatCap_ice() * temp_gp( kk )
        dummyReal = H_w -  H_i + Water%density * Water%latentHeatMelt

        HDelta_tanmat( kk ) = dummyReal &
          & * obj%soils(matID)%SFCCModel%getSlope(Temp=temp_gp(kk)) &
          & / Obj%dt

        ! HDelta_tanmat( kk ) = dummyReal &
        !   & * (theta_w - theta0_w) &
        !   & / (temp_gp(kk)-temp0_gp(kk)) / Obj%dt

        HDelta_rhs( kk ) = dummyReal &
          & * obj%soils(matID)%SFCCModel%getSlope(Temp=temp_gp(kk)) &
          & * (temp_gp(kk) - temp0_gp(kk))/obj%dt
        ! HDelta_rhs( kk ) = dummyReal &
        !   & * (theta_w - theta0_w) / Obj%dt
      ! END IF
    END DO

    HDelta_fevar = QuadratureVariable( HDelta_tanmat, &
      & typefevariableScalar, typeFeVariableSpace )
    Be = MassMatrix( Test=spacesd_A, Trial=spacesd_A, Rho=HDelta_fevar)
    tanmat( :, : ) = tanmat( :, : ) + Be( :, : )

    DO kk = 1, NUM_GP
      dummyReal = spacesd_A%Js(kk) * spacesd_A%Ws(kk) * spacesd_A%Thickness(kk)
      rhs_e( : ) = rhs_e( : ) - dummyReal * spacesd_A%N(:,kk)*HDelta_rhs(kk)
    END DO
  END DO

END SUBROUTINE ComputePhaseChangeQuantity_4
