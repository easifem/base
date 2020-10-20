SUBMODULE(MovingMesh_Class) ElasticityMethod
IMPLICIT NONE
CONTAINS
!----------------------------------------------------------------------------
!                                                                 setDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE mmt_setdof_elasticity
  INTEGER( I4B ) :: ii

  obj % tdof = obj%nsd
  ALLOCATE( obj%dof( 2 ), obj%nodalVar( 2 ) )
  ! dof(1)=y
  ! dof(2)=rhs

  ! CALL display( '  Initiating dof y')
  obj % dof(1) = dof( tNodes = [ obj % tNodes ], Names = [ 'y' ], &
    & SpaceCompo = [ obj%nsd ], TimeCompo = [ 1 ], StorageFMT = Nodes_FMT  )

  ! CALL display( '  Initiating dof rhs' )
  obj % dof(2) = dof( tNodes = [ Obj % tNodes ], Names = [ 'f' ], &
    & SpaceCompo = [ obj%nsd ], TimeCompo = [ 1 ], StorageFMT = Nodes_FMT )

  DO ii=1, SIZE(obj%dof)
    CALL initiate( obj % nodalVar(ii), obj % dof(ii) )
  END DO

END PROCEDURE mmt_setdof_elasticity

!----------------------------------------------------------------------------
!                                                            AssembleTanmat
!----------------------------------------------------------------------------

MODULE PROCEDURE mmt_assemble_elasticity
  INTEGER( I4B ) :: telem, ii, order, iel, nsd, jj, DOFNo( 3 )
  INTEGER( I4B ), ALLOCATABLE :: nptrs( : )
  INTEGER( I4B ), ALLOCATABLE :: local_nptrs( : )
  TYPE( RealMatrix_ ), ALLOCATABLE :: rmat( : )
  REAL( DFP ), ALLOCATABLE :: matprops( : )
  REAL( DFP ) :: rval( 3 )

  TYPE( QuadraturePoint_ ) :: quad
  TYPE( ElemShapeData_ ) :: elemsd
  TYPE( FEVariable_ ) :: Lambda, Mu, Evec
  CLASS( Element_ ), POINTER :: elem, cell
  CLASS( Mesh_ ), POINTER :: meshobj


  SELECT TYPE( obj )
  CLASS IS ( MovingMesh_ )

    ALLOCATE(rmat(5))
    nsd = obj % nsd
    DOFNo = [1,2,3]
    meshobj => null( ); elem => null( )
    ! CALL display( "  Resetting tangent matrix to zero" )
    CALL setValue( obj = obj % tanmat, val = 0.0_dfp )

    ! CALL display( "  Resetting RHS vector to zero" )
    obj % nodalVar(obj%rhs)%val = 0.0_DFP

    DO ii = 1, SIZE( obj % omegano )

      ! CALL display( obj % omegano(ii), "  Computation over Omega :: ")

      meshobj => obj % dom % omega( obj % omegano( ii ) ) % Ptr
      elem => meshobj % elem( 1 ) % ptr
      telem = meshobj % SIZE( )

      ! CALL display( '    Generating gauss-quadrature rules' )
      order = elem % refelem % order
      quad = GaussLegendreQuadrature( refelem=elem%refelem, order=order )

      ! CALL display( '    Generating H1-Lagrange shape function' )
      CALL initiate( obj = elemsd, quad = quad, refelem = elem % refelem, &
        & ContinuityType= obj % ContinuityType, &
        & InterpolType = obj %  InterpolType )

      ! CALL display( '    Started: element loop' )

      DO iel = 1, telem

        elem => meshobj % elem( iel ) % ptr
        nptrs = .nptrs. Elem !! Global nodes

        IF(obj%smoothing)THEN
          rmat( 1 ) % Val = obj % smoothnodes( 1:nsd, nptrs )
        ELSE
          rmat( 1 ) % Val = obj % dom % nodes( 1:nsd, nptrs )
        END IF

        CALL setValue(obj=elemsd, val=rmat( 1 )%Val, &
          & N=elemsd%N, dNdXi=elemsd%dNdXi)

        matprops = obj % matprops( :, elem % mat_type )

        Lambda = QuadratureVariable( &
          & Val = matprops( elasticity_lam ) &
          & * (elemsd % Js( : ) ** ( -matprops( elasticity_xi ) )), &
          & rank = TypeFEVariableScalar, VarType = TypeFEVariableSpace )

        Mu = QuadratureVariable( &
          & Val = matprops( elasticity_mu ) &
          & * (elemsd % Js( : ) ** ( -matprops( elasticity_xi ) )), &
          & rank = TypeFEVariableScalar, VarType = TypeFEVariableSpace )

        rmat( 3 ) % val = StiffnessMatrix( elemsd, elemsd, Lambda, Mu )

        local_nptrs = obj % local_nptrs( nptrs )

        !! Need to convert global nodes into local nodes
        CALL addcontribution( obj = obj % tanmat, &
          & nptrs=local_nptrs, val = rmat( 3 ) % val, scale = 1.0_dfp, &
          & storageFMT = DOF_FMT )

      END DO
      ! CALL display( '    Ended: element loop' )
    END DO

    !----------------------------------
    !! If moving boundary is present
    !----------------------------------

    IF( ALLOCATED( obj % db(4)%val ) ) THEN
      meshobj => null( )
      DO ii = 1, SIZE( obj % db(4)%val )
        ! CALL display( obj % db(4)%val( ii ), &
        !   & "  Computation over boundary :: ")

        meshobj => obj % dom % boundary( obj % db(4)%val( ii ) ) % ptr
        telem = meshobj % size( )
        elem => meshobj % elem( 1 ) % ptr

        ! CALL display( '    Generating Gauss-quadrature rule' )
        order = 2*elem % refelem % order
        quad = GaussLegendreQuadrature( refelem = elem % refelem,order=order )

        ! CALL display( '    Generating H1-Lagrange shape function' )
        CALL initiate( obj = elemsd, quad = quad, refelem = elem % refelem, &
          & ContinuityType= obj % ContinuityType, &
          & InterpolType = obj %  InterpolType )

        ! CALL display( '    Started: element loop' )

        elem => null( )
        cell => null( )

        IF( obj%isNoSlip ) THEN
          DO iel = 1, telem

            elem => meshobj % elem( iel ) % ptr

            nptrs = elem % cellNptrs( ) !! global nodes
            IF( obj%smoothing ) THEN
              rmat( 1 ) % Val = obj % smoothnodes( 1:nsd, nptrs )
            ELSE
              rmat( 1 ) % Val = obj % dom % nodes( 1:nsd, nptrs )
            END IF

            cell => elem % PointerToCell( )
            rval( 1 ) = MeasureSimplex( cell % RefElem, rmat( 1 ) % Val )

            CALL elem % getElemShapeData( elemsd = elemsd, &
              & quad = quad, xij = rmat( 1 ) % Val, &
              & ContinuityType = obj % ContinuityType, &
              & InterpolType = obj % InterpolType )

            matprops = obj % matprops( :, elem % mat_type )

            matprops( elasticity_lam ) = matprops( elasticity_lam ) &
              & * (elemsd % Js( 1 ) ** ( -matprops( elasticity_xi ) ))

            matprops( elasticity_mu ) = matprops( elasticity_mu ) &
              & * (elemsd % Js( 1 ) ** ( -matprops( elasticity_xi ) ))

            nptrs = elem % getnptrs( ) !! global nodes
            IF( obj%smoothing ) THEN
              rmat( 1 ) % Val = obj % smoothnodes( 1:nsd, nptrs )
            ELSE
              rmat( 1 ) % Val = obj % dom % nodes( 1:nsd, nptrs )
            END IF

            rval( 2 ) = MeasureSimplex( elem % RefElem, rmat( 1 ) % Val )
            rval( 3 ) = obj % Alpha * ABS( rval( 2 ) / rval( 1 ) )
            ! rval( 3 ) = obj % Alpha

            rmat( 5 ) % val = NitscheMatrix( test = elemsd, trial = elemsd, &
              & Lambda = matprops( elasticity_lam ), &
              & Mu = matprops( elasticity_mu ), isnoslip=.true. )

            rmat( 4 ) % val = rval( 3 )*MassMatrix( test = elemsd, &
              & trial = elemsd, ncopy=obj%nsd )

            rmat( 3 ) % val = -2.0_DFP * skewSym( rmat( 5 ) % val ) &
              & + rmat(4)%val

            ! convert global nodes to local nodes
            local_nptrs = obj % local_nptrs( nptrs )

            CALL addcontribution( obj = obj % tanmat, nptrs = local_nptrs, &
              & val = rmat( 3 ) % val,  scale = 1.0_dfp, &
              & StorageFMT = DOF_FMT )
          END DO
        ELSE
          DO iel = 1, telem

            elem => meshobj % elem( iel ) % ptr
            nptrs = elem % cellNptrs( ) !! global nodes
            IF( obj%smoothing ) THEN
              rmat( 1 ) % Val = obj % smoothnodes( 1:nsd, nptrs )
            ELSE
              rmat( 1 ) % Val = obj % dom % nodes( 1:nsd, nptrs )
            END IF

            cell => elem % PointerToCell( )
            rval( 1 ) = MeasureSimplex( cell % RefElem, rmat( 1 ) % Val )

            CALL elem % getElemShapeData( elemsd = elemsd, &
              & quad = quad, xij = rmat( 1 ) % Val, &
              & ContinuityType = obj % ContinuityType, &
              & InterpolType = obj % InterpolType )

            matprops = obj % matprops( :, elem % mat_type )

            matprops( elasticity_lam ) = matprops( elasticity_lam ) &
              & * (elemsd % Js( 1 ) ** ( -matprops( elasticity_xi ) ))

            matprops( elasticity_mu ) = matprops( elasticity_mu ) &
              & * (elemsd % Js( 1 ) ** ( -matprops( elasticity_xi ) ))

            nptrs = elem % getnptrs( ) !! global nodes
            IF( obj%smoothing ) THEN
              rmat( 1 ) % Val = obj % smoothnodes( 1:nsd, nptrs )
            ELSE
              rmat( 1 ) % Val = obj % dom % nodes( 1:nsd, nptrs )
            END IF

            rval( 2 ) = MeasureSimplex( elem % RefElem, rmat( 1 ) % Val )
            rval( 3 ) = obj % Alpha * ABS( rval( 2 ) / rval( 1 ) )
            ! rval( 3 ) = obj % Alpha

            Evec = QuadratureVariable( Val = elemsd % normal, &
              & rank = TypeFEVariableVector, &
              & varType = TypeFEVariableSpace )

            rmat( 5 ) % val = NitscheMatrix( test = elemsd, trial = elemsd, &
              & Lambda = matprops( elasticity_lam ), &
              & Mu = matprops( elasticity_mu ), Evec = Evec )

            rmat( 4 ) % val = NitscheMatrix( test = elemsd, trial = elemsd, &
              & Alpha = rval( 3 ), Evec = Evec )

            rmat( 3 ) % val = -2.0_DFP * skewSym( rmat( 5 ) % val ) &
              & + rmat(4)%val

            ! convert global nodes to local nodes
            local_nptrs = obj % local_nptrs( nptrs )

            CALL addcontribution( obj = obj % tanmat, nptrs = local_nptrs, &
              & val = rmat( 3 ) % val,  scale = 1.0_dfp, &
              & storageFMT = DOF_FMT )
          END DO
        END IF
        ! CALL display( '    Ended: element loop' )
      END DO
    END IF
  END SELECT

  NULLIFY( elem, cell, meshobj )
  DEALLOCATE( nptrs, local_nptrs, rmat, matprops )
  CALL DeallocateData(quad)
  CALL DeallocateData(elemsd)
  CALL DeallocateData(Lambda)
  CALL DeallocateData(Mu)
  CALL DeallocateData(Evec)
END PROCEDURE mmt_assemble_elasticity

!----------------------------------------------------------------------------
!                                                         ApplyInitCondition
!----------------------------------------------------------------------------

MODULE PROCEDURE mmt_apply_initcond_elasticity
  INTEGER( I4B ) :: ii

  IF( SIZE(initVal) .EQ. 1 ) THEN
    obj%nodalvar(obj%y)%val = initVal(1)
  ELSE
    DO ii = 1, SIZE(obj%omegano)
    CALL setValue( Vec=obj%nodalVar(obj%y)%val, obj=obj%dof(obj%y), &
      & Nptrs=obj%local_nptrs(obj%dom%mdomega(obj%omegano(ii))%ptr%nptrs),&
      & Val=initVal, conversion=[DOFtoNodes] )
    END DO
  END IF
END PROCEDURE mmt_apply_initcond_elasticity

!----------------------------------------------------------------------------
!                                                           ApplyDBCondition
!----------------------------------------------------------------------------

MODULE PROCEDURE mmt_applyuniformdisp_elasticity
  SELECT CASE(dim)
  CASE(0)
    CALL setValue( Vec=obj%nodalVar(obj%y)%val, obj=obj%dof(obj%y), &
      & Nptrs=obj%local_nptrs(obj%dom%mdboundary(tag)%ptr%nptrs),&
      & Val=[Val], conversion=[NONE] )
  CASE(1)
    CALL setValue( Vec=obj%nodalVar(obj%y)%val, obj=obj%dof(obj%y), &
      & Nptrs=obj%local_nptrs(obj%dom%mdboundary(tag)%ptr%nptrs),&
      & Val=[Val], dofno=1 )
  CASE(2)
    CALL setValue( Vec=obj%nodalVar(obj%y)%val, obj=obj%dof(obj%y), &
      & Nptrs=obj%local_nptrs(obj%dom%mdboundary(tag)%ptr%nptrs),&
      & Val=[Val], dofno=2 )
  CASE(3)
    CALL setValue( Vec=obj%nodalVar(obj%y)%val, obj=obj%dof(obj%y), &
      & Nptrs=obj%local_nptrs(obj%dom%mdboundary(tag)%ptr%nptrs),&
      & Val=[Val], dofno=3 )
  END SELECT
END PROCEDURE mmt_applyuniformdisp_elasticity

!----------------------------------------------------------------------------
!                                              applyNitscheBoundaryCondition
!----------------------------------------------------------------------------

MODULE PROCEDURE apply_nitsche_elasticity
  INTEGER( I4B ) :: telem, ii, iel, nsd, jj, DOFNo( 3 )
  INTEGER( I4B ), ALLOCATABLE :: nptrs( : )
  INTEGER( I4B ), ALLOCATABLE :: local_nptrs( : )
  TYPE( RealMatrix_ ), ALLOCATABLE :: rmat(:)
  REAL( DFP ), ALLOCATABLE :: ye( : ), matprops( : )
  REAL( DFP ) :: rval( 3 )

  TYPE( QuadraturePoint_ ) :: quad
  TYPE( ElemShapeData_ ) :: elemsd
  TYPE( FEVariable_ ) :: Lambda, Mu, Evec
  CLASS( Element_ ), POINTER :: elem, cell
  CLASS( Mesh_ ), POINTER :: meshobj

  nsd = obj % nsd
  DOFNo = [1,2,3]
  ALLOCATE(rmat(4))

  meshobj => null( ); elem => null( )
  DO ii = 1, SIZE( tag )
    meshobj => obj % dom % boundary( tag( ii ) ) % ptr
    telem = meshobj % size( )
    elem => meshobj % elem( 1 ) % ptr
    quad = GaussLegendreQuadrature( refelem = elem % refelem,&
      & order=2*elem % refelem % order )
    CALL initiate( obj = elemsd, quad = quad, refelem = elem % refelem, &
      & ContinuityType= obj % ContinuityType, &
      & InterpolType = obj %  InterpolType )
    elem => null( )
    cell => null( )
    DO iel = 1, telem
      elem => meshobj % elem( iel ) % ptr
      nptrs = elem % cellNptrs( ) !! global nodes
      IF( obj%smoothing ) THEN
        rmat( 1 ) % Val = obj % smoothnodes( 1:nsd, nptrs )
      ELSE
        rmat( 1 ) % Val = obj % dom % nodes( 1:nsd, nptrs )
      END IF
      cell => elem % PointerToCell( )
      rval( 1 ) = MeasureSimplex( cell % RefElem, rmat( 1 ) % Val )
      CALL elem % getElemShapeData( elemsd = elemsd, &
        & quad = quad, xij = rmat( 1 ) % Val, &
        & ContinuityType = obj % ContinuityType, &
        & InterpolType = obj % InterpolType )

      matprops = obj % matprops( :, elem % mat_type )
      matprops( elasticity_lam ) = matprops( elasticity_lam ) &
        & * (elemsd % Js( 1 ) ** ( -matprops( elasticity_xi ) ))
      matprops( elasticity_mu ) = matprops( elasticity_mu ) &
        & * (elemsd % Js( 1 ) ** ( -matprops( elasticity_xi ) ))

      nptrs = elem % getnptrs( ) !! global nodes
      IF( obj%smoothing ) THEN
        rmat( 1 ) % Val = obj % smoothnodes( 1:nsd, nptrs )
      ELSE
        rmat( 1 ) % Val = obj % dom % nodes( 1:nsd, nptrs )
      END IF
      rval( 2 ) = MeasureSimplex( elem % RefElem, rmat( 1 ) % Val )
      rval( 3 ) = obj % Alpha * ABS( rval( 2 ) / rval( 1 ) )
      ! rval( 3 ) = obj % Alpha

      Evec = QuadratureVariable( Val = elemsd % normal, &
        & rank = TypeFEVariableVector, &
        & varType = TypeFEVariableSpace )

      rmat( 2 ) % val = NitscheMatrix( test = elemsd, trial = elemsd, &
        & Lambda = matprops( elasticity_lam ), &
        & Mu = matprops( elasticity_mu ), Evec = Evec )

      rmat( 3 ) % val = NitscheMatrix( test = elemsd, trial = elemsd, &
        & Alpha = rval( 3 ), Evec = Evec )
      !> this should go in
      local_nptrs = obj % local_nptrs( nptrs )

      ye = ArrayValues( Val=y, obj=obj%dof(obj%y), &
        & DOFNo=DOFNo(1:obj%nsd), StorageFMT=FMT_DOF, Nptrs=local_nptrs )

      rmat( 4 ) % val = rmat( 3 ) % val + transpose( rmat( 2 ) % val )

      ye = matmul( rmat( 4 ) % val, ye*dt )

      CALL addContribution( Vec = obj % nodalVar(obj%rhs)%val, &
        & obj=obj%dof(obj%rhs), Nptrs=local_nptrs, &
        & Val = ye, Scale = 1.0_dfp, Conversion = [dofToNodes] )
    END DO
  END DO
  DEALLOCATE( nptrs, local_nptrs, rmat, ye, matprops )
  NULLIFY( meshobj, elem, cell )
END PROCEDURE apply_nitsche_elasticity

!----------------------------------------------------------------------------
!                                                                  applyDBC
!----------------------------------------------------------------------------

MODULE PROCEDURE apply_dbc_elasticity
  INTEGER( I4B ) :: ii, imesh, jj, kk, ll, mm
  CLASS( meshData_ ), POINTER :: md

  md => NULL( )
  IF( PRESENT(local_nptrs) ) THEN
    SELECT CASE( dim )
    CASE(0)
      DO imesh=1, size(tag)
        md => obj%dom%mdboundary(tag(imesh))%ptr
        DO ii=1,md%tNodes
          jj = md%nptrs(ii)
          DO ll = 1, obj%nsd
            kk = (obj%local_nptrs(jj)-1)*obj%tdof+ll
            mm = (local_nptrs(jj)-1)*obj%tdof+ll
            obj%nodalVar(obj%y)%val(kk) = y(mm)*dt
          END DO
        END DO
      END DO
    CASE DEFAULT
      IF( PRESENT( magnitude ) ) THEN
        DO imesh=1, size(tag)
          md => obj%dom%mdboundary(tag(imesh))%ptr
          DO ii=1,SIZE(md%nptrs)
            jj = md%nptrs(ii)
            kk = (obj%local_nptrs(jj)-1)*obj%tdof+dim
            mm = (local_nptrs(jj)-1)*obj%tdof
            ll = (local_nptrs(jj)-1)*obj%tdof + dim
            obj%nodalVar(obj%y)%val(kk) = NORM2(y(mm+1:mm+obj%tdof))*dt*SIGN( 1.0_DFP, y(ll) )
          END DO
        END DO
      ELSE
        DO imesh=1, size(tag)
          md => obj%dom%mdboundary(tag(imesh))%ptr
          DO ii=1,SIZE(md%nptrs)
            jj = md%nptrs(ii)
            kk = (obj%local_nptrs(jj)-1)*obj%tdof+dim
            mm = (local_nptrs(jj)-1)*obj%tdof+dim
            obj%nodalVar(obj%y)%val(kk) = y(mm)*dt
          END DO
        END DO
      END IF

    END SELECT

  ELSE

    IF( SIZE(y) .NE. SIZE(obj%nodalVar(obj%y)%val) ) THEN
      CALL Display("ERROR: MovingMesh_Class@ElasticityMethod.f90")
      CALL Display(        "apply_dbc_elasticity()")
      CALL Display(           "SIZE(y) .NE. obj%tNodes")
      STOP
    END IF

    SELECT CASE( dim )
    CASE(0)
      DO imesh=1, size(tag)
        md => obj%dom%mdboundary(tag(imesh))%ptr
        DO ii=1,md%tNodes
          jj = md%nptrs(ii)
          DO ll = 1, obj%nsd
            kk = (obj%local_nptrs(jj)-1)*obj%tdof+ll
            obj%nodalVar(obj%y)%val(kk) = y(kk)*dt
          END DO
        END DO
      END DO
    CASE DEFAULT
      DO imesh=1, size(tag)
        md => obj%dom%mdboundary(tag(imesh))%ptr
        DO ii=1,md%tNodes
          jj = md%nptrs(ii)
          kk = (obj%local_nptrs(jj)-1)*obj%tdof+dim
          obj%nodalVar(obj%y)%val(kk) = y(kk)*dt
        END DO
      END DO
    END SELECT
  END IF

  NULLIFY(md)

END PROCEDURE apply_dbc_elasticity

!----------------------------------------------------------------------------
!                                                             AssembleTanmat
!----------------------------------------------------------------------------

MODULE PROCEDURE mmt_solve_elasticity
  INTEGER( I4B ) :: ii, jj
  SELECT TYPE(Obj)
  CLASS IS( MovingMesh_ )
    CALL obj % linsol % setMatrix( obj % tanmat )
    CALL obj % linsol % solve( rhs = obj%nodalVar(obj%rhs)%val, &
      & sol=obj%nodalVar(obj%y)%val )

    IF( obj%smoothing ) THEN
      obj%smoothing = .FALSE.
      DO ii = 1, obj % tNodes
        DO jj = 1, obj % tdof
          obj%nodalVar(obj%y)%val((ii-1)*obj%tdof + jj) &
            & = obj%nodalVar(obj%y)%val((ii-1)*obj%tdof + jj) &
            & + obj%smoothnodes(jj, obj%nptrs(ii) ) &
            & - obj%dom%nodes(jj, obj%nptrs(ii) )
          obj%dom%Nodalvelocity(jj,obj%nptrs(ii)) = &
            & obj%nodalVar(obj%y)%val((ii-1)*obj%tdof + jj)/obj%dt
        END DO
      END DO
    ELSE
      DO ii = 1, obj % tNodes
        DO jj = 1, obj % tdof
          obj%dom%Nodalvelocity(jj,obj%nptrs(ii)) = &
            & obj%nodalVar(obj%y)%val((ii-1)*obj%tdof + jj)/obj%dt
        END DO
      END DO
    END IF
  END SELECT
END PROCEDURE mmt_solve_elasticity

!----------------------------------------------------------------------------
!                                                                     Update
!----------------------------------------------------------------------------

MODULE PROCEDURE mmt_update_elasticity
  INTEGER( I4B ) :: ii

  SELECT TYPE( Obj )
  CLASS IS( MovingMesh_ )

    SELECT CASE( obj % nsd )

    CASE( 1 )

      DO ii = 1, obj % tnodes
        obj % dom % nodes( 1, obj % nptrs( ii )  ) = &
          & obj % dom % nodes( 1, obj % nptrs( ii )  ) &
          & + obj % nodalVar(obj%y)%val( ( ii - 1 ) * obj % tdof + 1  )

        obj%dom%Nodalvelocity(1,obj%nptrs(ii)) = &
            & obj%nodalVar(obj%y)%val((ii-1)*obj%tdof + 1)/obj%dt

      END DO

    CASE( 2 )

      DO ii = 1, obj % tnodes
        obj % dom % nodes( 1, obj % nptrs( ii )  ) = &
          & obj % dom % nodes( 1, obj % nptrs( ii )  ) &
          & + obj % nodalVar(obj%y)%val( ( ii - 1 ) * obj % tdof + 1  )

        obj % dom % nodes( 2, obj % nptrs( ii )  ) = &
          & obj % dom % nodes( 2, obj % nptrs( ii )  ) &
          & + obj % nodalVar(obj%y)%val( ( ii - 1 ) * obj % tdof + 2  )

        obj%dom%Nodalvelocity(1,obj%nptrs(ii)) = &
          & obj%nodalVar(obj%y)%val((ii-1)*obj%tdof + 1)/obj%dt

        obj%dom%Nodalvelocity(2,obj%nptrs(ii)) = &
          & obj%nodalVar(obj%y)%val((ii-1)*obj%tdof + 2)/obj%dt

      END DO

    CASE DEFAULT

      DO ii = 1, obj % tnodes
        !x
        obj % dom % nodes( 1, obj % nptrs( ii )  ) = &
          & obj % dom % nodes( 1, obj % nptrs( ii )  ) &
          & + obj % nodalVar(obj%y)%val( ( ii - 1 ) * obj % tdof + 1  )

        !y
        obj % dom % nodes( 2, obj % nptrs( ii )  ) = &
          & obj % dom % nodes( 2, obj % nptrs( ii )  ) &
          & + obj % nodalVar(obj%y)%val( ( ii - 1 ) * obj % tdof + 2  )

        !z
        obj % dom % nodes( 3, obj % nptrs( ii )  ) = &
          & obj % dom % nodes( 3, obj % nptrs( ii )  ) &
          & + obj % nodalVar(obj%y)%val( ( ii - 1 ) * obj % tdof + 3  )

        obj%dom%Nodalvelocity(1,obj%nptrs(ii)) = &
          & obj%nodalVar(obj%y)%val((ii-1)*obj%tdof + 1)/obj%dt

        obj%dom%Nodalvelocity(2,obj%nptrs(ii)) = &
          & obj%nodalVar(obj%y)%val((ii-1)*obj%tdof + 2)/obj%dt

        obj%dom%Nodalvelocity(3,obj%nptrs(ii)) = &
          & obj%nodalVar(obj%y)%val((ii-1)*obj%tdof + 3)/obj%dt

      END DO

    END SELECT

    IF( reset ) THEN
      obj%nodalVar(obj%y)%val = 0.0_DFP
    END IF

  END SELECT
END PROCEDURE mmt_update_elasticity

!----------------------------------------------------------------------------
!                                                                isConverged
!----------------------------------------------------------------------------

MODULE PROCEDURE mmt_isconverg_elasticity
END PROCEDURE mmt_isconverg_elasticity

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE mmt_writedata_elasticity
type( vtk_ ) :: vtu
  TYPE(string) :: pfe(3)

  IF(PRESENT(path)) THEN
    pfe(1)=string(trim(path))
  ELSE
    pfe(1)=string('./')
  END IF

  IF(PRESENT(filename)) THEN
    pfe(2)=string(trim(filename))
  ELSE
    pfe(2)=string('movingmesh')
  END IF

  IF(PRESENT(extension)) THEN
    pfe(3)=string(trim(extension))
  ELSE
    pfe(3)=string('.vtu')
  END IF

  ! step -1
  SELECT TYPE(obj)
  CLASS IS(MovingMesh_)
    CALL vtu % initiate( &
      & path = trim(pfe(1)%chars()), &
      & filename = trim(pfe(2)%chars()), &
      & extension = trim(pfe(3)%chars()), &
      & fmt = "ascii", &
      & meshTopology = "UnstructuredGrid", &
      & indx = indx )

    ! step-2
    CALL vtu%openPiece( )
    call vtu%writeGeometry( MeshObj=obj%dom%omega, &
        & nodes = obj%dom%nodes, tag=obj%omegano, &
        & local2global = obj%nptrs, map=obj%local_nptrs, &
        & lb = LBOUND(obj%local_nptrs,1), ub=UBOUND(obj%local_nptrs,1) )
    ! step-3
    CALL vtu%openNodeData( )
    CALL vtu%writeNodeData( x=obj%nodalVar(obj%y)%val,&
      & dofobj=obj%dof(obj%y) )
    CALL vtu%closeNodeData( )

    IF( ALLOCATED(obj%meshq) ) THEN
      CALL vtu%openElementData()
        CALL vtu%writeQuadratureData( val=obj%meshq, name='mesh_quality' )
      CALL vtu%closeElementData()
    END IF

    CALL vtu % closePiece( )
    CALL vtu % Finalize( )

    ! gmsh post process
    IF( PRESENT(gmsh) ) THEN
    CALL gmsh%model%mesh%writenodedata(x=obj%nodalVar(obj%y)%val,&
      & Indx=indx, dofObj=obj%dof(obj%y), Name='y', &
      & local_nptrs=obj%local_nptrs)
    END IF
  END SELECT
END PROCEDURE mmt_writedata_elasticity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ElasticityMethod