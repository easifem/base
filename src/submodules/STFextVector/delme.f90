
  SELECT CASE( TRIM( ADJUSTL( FextType ) ) )
  CASE( 'Nodal', 'Nodal Values', 'NodalValues', &
       'STNodalValues', 'ST Nodal Values' )

     SELECT CASE( TRIM( ADJUSTL( Term1 ) ) )

     CASE( 'dx', 'dx1', 'dX', 'dX1', 'x', 'X', 'x1', 'X1' )

        DO a = 1, NNT

           DO I = 1, NNS 

              DO IPT = 1, NIPT

                 DO IPS = 1, NIPS

                    Ws = Obj % SD( IPS, IPT ) % getWs( )
                    Wt = Obj % SD( IPS, IPT ) % getWt( )

                    Js = Obj % SD( IPS, IPT ) % getJs_Xi2Xt( )
                    Jt = Obj % SD( IPS, IPT ) % getJt( )

                    thick = Obj % SD( IPS, IPT ) % getThickness( )

                    CALL Obj % SD( IPS, IPT ) % getInterpolationOfVector( &
                         STNodalValues = Fext, Val = Fbar )

                    CALL Obj % SD( IPS, IPT ) % getdNTdXt( dNTdXt )

                    RealVal = Ws * Wt * Js * Jt * thick * dNTdXt( I, 1, a )

                    Obj % Vec3( :, I, a ) = Obj % Vec3( :, I, a ) + &
                         Fbar * RealVal

                 END DO

              END DO

           END DO

        END DO

     CASE( 'dy', 'dx2', 'dY', 'dX2', 'y', 'Y', 'x2', 'X2' )

        DO a = 1, NNT

           DO I = 1, NNS 

              DO IPT = 1, NIPT

                 DO IPS = 1, NIPS

                    Ws = Obj % SD( IPS, IPT ) % getWs( )
                    Wt = Obj % SD( IPS, IPT ) % getWt( )

                    Js = Obj % SD( IPS, IPT ) % getJs_Xi2Xt( )
                    Jt = Obj % SD( IPS, IPT ) % getJt( )

                    thick = Obj % SD( IPS, IPT ) % getThickness( )

                    CALL Obj % SD( IPS, IPT ) % getInterpolationOfVector( &
                         STNodalValues = Fext, Val = Fbar )

                    CALL Obj % SD( IPS, IPT ) % getdNTdXt( dNTdXt )

                    RealVal = Ws * Wt * Js * Jt * thick * dNTdXt( I, 2, a )

                    Obj % Vec3( :, I, a ) = Obj % Vec3( :, I, a ) + &
                         Fbar * RealVal

                 END DO

              END DO

           END DO

        END DO

     CASE( 'dz', 'dx3', 'dZ', 'dX3', 'z', 'Z', 'x3', 'X3' )

        DO a = 1, NNT

           DO I = 1, NNS 

              DO IPT = 1, NIPT

                 DO IPS = 1, NIPS

                    Ws = Obj % SD( IPS, IPT ) % getWs( )
                    Wt = Obj % SD( IPS, IPT ) % getWt( )

                    Js = Obj % SD( IPS, IPT ) % getJs_Xi2Xt( )
                    Jt = Obj % SD( IPS, IPT ) % getJt( )

                    thick = Obj % SD( IPS, IPT ) % getThickness( )

                    CALL Obj % SD( IPS, IPT ) % getInterpolationOfVector( &
                         STNodalValues = Fext, Val = Fbar )

                    CALL Obj % SD( IPS, IPT ) % getdNTdXt( dNTdXt )

                    RealVal = Ws * Wt * Js * Jt * thick * dNTdXt( I, 3, a )

                    Obj % Vec3( :, I, a ) = Obj % Vec3( :, I, a ) + &
                         Fbar * RealVal

                 END DO

              END DO

           END DO

        END DO

     CASE( 'dt', 'dT', 'Dt', 't' )

        DO a = 1, NNT

           DO I = 1, NNS 

              DO IPT = 1, NIPT

                 DO IPS = 1, NIPS

                    Ws = Obj % SD( IPS, IPT ) % getWs( )
                    Wt = Obj % SD( IPS, IPT ) % getWt( )

                    Js = Obj % SD( IPS, IPT ) % getJs_Xi2Xt( )
                    Jt = Obj % SD( IPS, IPT ) % getJt( )

                    thick = Obj % SD( IPS, IPT ) % getThickness( )

                    CALL Obj % SD( IPS, IPT ) % getInterpolationOfVector( &
                         STNodalValues = Fext, Val = Fbar )

                    CALL Obj % SD( IPS, IPT ) % getdNTdt( dNTdt )

                    RealVal = Ws * Wt * Js * Jt * thick * dNTdt( I, a )

                    Obj % Vec3( :, I, a ) = Obj % Vec3( :, I, a ) + &
                         Fbar * RealVal

                 END DO

              END DO

           END DO

        END DO

     CASE DEFAULT

        CALL Err_Msg( 'STFextVector_Class.f90>>FextVector_24.part', &
             & 'getFextVector_24()', &
             & 'No case found for Term1, It should be, &
             & [dx, dx1, dX, dX1, x, X, x1, X1], &
             & [dy, dx2, dY, dX2, y, Y, x2, X2], &
             & [dz, dx3, dZ, dX3, z, Z, x3, X3], &
             & [dt, dT, t, Dt]' )
        Error_Flag = .TRUE.
        RETURN

     END SELECT



  CASE( 'Integration', 'Integration Points', 'IntegrationPoints', &
       'Quad', 'QuadPoints', 'Quad Points' )

     SELECT CASE( TRIM( ADJUSTL( Term1 ) ) )

     CASE( 'dx', 'dx1', 'dX', 'dX1', 'x', 'X', 'x1', 'X1' )

        DO a = 1, NNT

           DO I = 1, NNS 

              DO IPT = 1, NIPT

                 DO IPS = 1, NIPS

                    Ws = Obj % SD( IPS, IPT ) % getWs( )
                    Wt = Obj % SD( IPS, IPT ) % getWt( )

                    Js = Obj % SD( IPS, IPT ) % getJs_Xi2Xt( )
                    Jt = Obj % SD( IPS, IPT ) % getJt( )

                    thick = Obj % SD( IPS, IPT ) % getThickness( )

                    Fbar = Fext( :, IPS, IPT )

                    CALL Obj % SD( IPS, IPT ) % getdNTdXt( dNTdXt )

                    RealVal = Ws * Wt * Js * Jt * thick * dNTdXt( I, 1, a )

                    Obj % Vec3( :, I, a ) = Obj % Vec3( :, I, a ) + &
                         Fbar * RealVal

                 END DO

              END DO

           END DO

        END DO

     CASE( 'dy', 'dx2', 'dY', 'dX2', 'y', 'Y', 'x2', 'X2' )

        DO a = 1, NNT

           DO I = 1, NNS 

              DO IPT = 1, NIPT

                 DO IPS = 1, NIPS

                    Ws = Obj % SD( IPS, IPT ) % getWs( )
                    Wt = Obj % SD( IPS, IPT ) % getWt( )

                    Js = Obj % SD( IPS, IPT ) % getJs_Xi2Xt( )
                    Jt = Obj % SD( IPS, IPT ) % getJt( )

                    thick = Obj % SD( IPS, IPT ) % getThickness( )

                    Fbar = Fext( :, IPS, IPT )

                    CALL Obj % SD( IPS, IPT ) % getdNTdXt( dNTdXt )

                    RealVal = Ws * Wt * Js * Jt * thick * dNTdXt( I, 2, a )

                    Obj % Vec3( :, I, a ) = Obj % Vec3( :, I, a ) + &
                         Fbar * RealVal

                 END DO

              END DO

           END DO

        END DO

     CASE( 'dz', 'dx3', 'dZ', 'dX3', 'z', 'Z', 'x3', 'X3' )

        DO a = 1, NNT

           DO I = 1, NNS 

              DO IPT = 1, NIPT

                 DO IPS = 1, NIPS

                    Ws = Obj % SD( IPS, IPT ) % getWs( )
                    Wt = Obj % SD( IPS, IPT ) % getWt( )

                    Js = Obj % SD( IPS, IPT ) % getJs_Xi2Xt( )
                    Jt = Obj % SD( IPS, IPT ) % getJt( )

                    thick = Obj % SD( IPS, IPT ) % getThickness( )

                    Fbar = Fext( :, IPS, IPT )

                    CALL Obj % SD( IPS, IPT ) % getdNTdXt( dNTdXt )

                    RealVal = Ws * Wt * Js * Jt * thick * dNTdXt( I, 3, a )

                    Obj % Vec3( :, I, a ) = Obj % Vec3( :, I, a ) + &
                         Fbar * RealVal

                 END DO

              END DO

           END DO

        END DO

     CASE( 'dt', 'dT', 'Dt', 't' )

        DO a = 1, NNT

           DO I = 1, NNS 

              DO IPT = 1, NIPT

                 DO IPS = 1, NIPS

                    Ws = Obj % SD( IPS, IPT ) % getWs( )
                    Wt = Obj % SD( IPS, IPT ) % getWt( )

                    Js = Obj % SD( IPS, IPT ) % getJs_Xi2Xt( )
                    Jt = Obj % SD( IPS, IPT ) % getJt( )

                    thick = Obj % SD( IPS, IPT ) % getThickness( )

                    Fbar = Fext( :, IPS, IPT )

                    CALL Obj % SD( IPS, IPT ) % getdNTdt( dNTdt )

                    RealVal = Ws * Wt * Js * Jt * thick * dNTdt( I, a )

                    Obj % Vec3( :, I, a ) = Obj % Vec3( :, I, a ) + &
                         Fbar * RealVal

                 END DO

              END DO

           END DO

        END DO

     CASE DEFAULT

        CALL Err_Msg( 'STFextVector_Class.f90>>FextVector_24.part', &
             & 'getFextVector_24()', &
             & 'No case found for Term1, It should be, &
             & [dx, dx1, dX, dX1, x, X, x1, X1], &
             & [dy, dx2, dY, dX2, y, Y, x2, X2], &
             & [dz, dx3, dZ, dX3, z, Z, x3, X3], &
             & [dt, dT, t, Dt]' )
        Error_Flag = .TRUE.
        RETURN

     END SELECT

  CASE DEFAULT

     CALL Err_Msg("STFextVector_Class.f90", &
          "getFextVector_24(), Flag-1", &
          "No case found for FextType, It should be &
          'Nodal', 'Nodal Values', 'NodalValues', 'STNodalValues', 'ST Nodal Values', &
          & 'Integration', 'Integration Points', 'IntegrationPoints', 'Quad', 'QuadPoints', &
          & 'Quad Points'" )
     Error_Flag = .TRUE.
     RETURN

  END SELECT