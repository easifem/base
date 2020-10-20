SUBMODULE( Seepage_Class ) SpaceTimeAlgorithm
USE h5fortran
IMPLICIT NONE


INTEGER( I4B ), ALLOCATABLE :: local_nptrs( : )

CONTAINS

!----------------------------------------------------------------------------
!                                                                     setDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE seepage_setdof_spaceTime
  INTEGER( I4B ) :: ii

  obj % tdof = obj%nnt
  ALLOCATE( obj%dof( 8 ), obj%nodalVar( 8 ) )
  obj % dof(obj%p ) = dof( tNodes = [ obj % tNodes ], Names = [ 'P' ], &
    & SpaceCompo = [ -1 ], TimeCompo = [ obj%nnt ], StorageFMT = Nodes_FMT  )
  obj % dof(obj%rhs) = dof( tNodes = [ Obj % tNodes ], Names = [ 'f' ], &
    & SpaceCompo = [ -1 ], TimeCompo = [ obj%nnt ], StorageFMT = Nodes_FMT )
  obj % dof(obj%p0) = dof( tNodes = [ Obj % tNodes ], Names = [ 'P' ], &
    & SpaceCompo = [ -1 ], TimeCompo = [ 1 ], StorageFMT = Nodes_FMT )
  obj % dof(obj%v) = dof( tNodes = [ Obj % tNodes ], Names = [ 'v' ], &
    & SpaceCompo = [ obj%nsd ], TimeCompo = [ 1 ], StorageFMT = Nodes_FMT )
  obj % dof(obj%dpdt) = dof( tNodes = [ Obj % tNodes ], Names = [ 'q' ], &
    & SpaceCompo = [ -1 ], TimeCompo = [ 1 ], StorageFMT = Nodes_FMT )
  obj % dof(obj%p_trial) = dof( tNodes = [ Obj % tNodes ], Names = [ 'p' ], &
    & SpaceCompo = [ -1 ], TimeCompo = [ obj%nnt ], StorageFMT = Nodes_FMT )
  obj % dof(obj%v_rhs) = dof( tNodes = [ Obj % tNodes ], Names = [ 'f' ], &
    & SpaceCompo = [ obj%nsd ], TimeCompo = [ 1 ], StorageFMT = Nodes_FMT )
  obj % dof(obj%pw) = dof( tNodes = [ Obj % tNodes ], Names = [ 'p' ], &
    & SpaceCompo = [ -1 ], TimeCompo = [ 1 ], StorageFMT = Nodes_FMT )
  DO ii=1, SIZE(obj%dof)
    CALL initiate( obj % nodalVar(ii), obj % dof(ii) )
  END DO
END PROCEDURE seepage_setdof_spaceTime

!----------------------------------------------------------------------------
!                                                         ApplyInitCondition
!----------------------------------------------------------------------------

MODULE PROCEDURE seepage_apply_initcond_spacetime
  INTEGER( I4B ) :: ii
  type(hdf5_file) :: h5f
  type(file_) :: afile

  IF( PRESENT( Val ) ) THEN

    IF( SIZE(Val) .EQ. 1 ) THEN
      obj%nodalVar(obj%p0)%val = Val(1)
      RETURN
    END IF

    IF( PRESENT( local_nptrs ) ) THEN

      DO ii=1,obj%tnodes
        Obj%nodalVar(obj%p0)%val(ii) = Val(local_nptrs(obj%nptrs(ii)))
      END DO

    ELSE

      IF( SIZE(Val) .EQ. obj%tnodes ) THEN
        obj%nodalVar(obj%p0)%val = Val
      ELSE
        DO ii =1, obj%tnodes
          obj%nodalVar(obj%p0)%val(ii)=Val(obj%nptrs(ii))
        END DO
      END IF

    END IF

    RETURN
  END IF


  IF( PRESENT( filename ) ) THEN
    SELECT CASE( trim(extension ) )
    CASE( ".hdf5" )
      CALL h5f%initialize( &
        & filename= trim(path)//trim(filename)//trim(Extension), &
        & status='old', action='r')
      CALL h5f%read( '/P0', obj%nodalVar(obj%p0)%val)
      CALL  h5f%finalize()
    CASE( ".txt")
      CALL openFileToRead(Obj=afile, Path=path, FileName=filename, &
        & extension=extension )
      DO ii=1, obj%tnodes
        READ(afile%unitno, *) obj%nodalVar(obj%p0)%val(ii)
      END DO
      CALL closeFile(Obj=afile)
    END SELECT

    RETURN
  END IF

  DO ii = 1, obj%tnodes
    obj%nodalVar(obj%p0)%val(ii)=obj%PressureFunc(&
      & obj%dom%nodes(1:obj%nsd, obj%nptrs(ii)))
  END DO
END PROCEDURE seepage_apply_initcond_spacetime

!----------------------------------------------------------------------------
!                                                                   applyDBC
!----------------------------------------------------------------------------

MODULE PROCEDURE seepage_apply_dbc_spacetime
  INTEGER( I4B ) :: ii, idof, jj, kk
  REAL( DFP ) :: x1(3), x2(3), v(3)

  IF( PRESENT( Val ) ) THEN

    IF( SIZE( Val ) .EQ. 1 ) THEN
      DO ii=1,obj%dom%mdboundary(tag)%ptr%tNodes
        kk = obj%dom%mdboundary(tag)%ptr%nptrs(ii)
        jj = obj%local_nptrs(kk)
        DO idof = 1, obj%tdof
          obj%nodalVar(obj%p)%val( (jj-1)*obj%tdof + idof) = Val(1)
        END DO
      END DO
      RETURN
    END IF

    IF (PRESENT(local_nptrs)) THEN
      DO ii=1,obj%dom%mdboundary(tag)%ptr%tNodes
        kk = obj%dom%mdboundary(tag)%ptr%nptrs(ii)
        jj = obj%local_nptrs(kk)
        kk = local_nptrs(kk)
        DO idof = 1, obj%tdof
          obj%nodalVar(obj%p)%val( (jj-1)*obj%tdof + idof) = Val(kk)
        END DO
      END DO

    ELSE

      DO ii=1,obj%dom%mdboundary(tag)%ptr%tNodes
        kk = obj%dom%mdboundary(tag)%ptr%nptrs(ii)
        jj = obj%local_nptrs(kk)
        DO idof = 1, obj%tdof
          obj%nodalVar(obj%p)%val( (jj-1)*obj%tdof + idof) = Val(kk)
        END DO
      END DO

    END IF

  ELSE

    DO ii=1,obj%dom%mdboundary(tag)%ptr%tNodes
      kk = obj%dom%mdboundary(tag)%ptr%nptrs(ii)
      jj = obj%local_nptrs(kk)
      x1(1:obj%nsd) = obj%dom%nodes(1:obj%nsd, kk)
      v(1:obj%nsd) = obj%dom%NodalVelocity(1:obj%nsd, kk)
      x2(1:obj%nsd) = x1(1:obj%nsd) + v(1:obj%nsd)*obj%dt
      obj%nodalVar(obj%p)%val( (jj-1)*obj%tdof + 1) = obj%PressureFunc(x1)
      obj%nodalVar(obj%p)%val( (jj-1)*obj%tdof + 2) = obj%PressureFunc(x2)

      DO idof = 3, obj%nnt
        x2(1:obj%nsd) = x1(1:obj%nsd)+v(1:obj%nsd)*obj%dt*(idof-2)/(obj%nnt-1)
        obj%nodalVar(obj%p)%val( (jj-1)*obj%tdof + idof) = &
          & obj%PressureFunc(x2)
      END DO
    END DO

  END IF

END PROCEDURE seepage_apply_dbc_spacetime

!----------------------------------------------------------------------------
!                                                                 applyNBC
!----------------------------------------------------------------------------

MODULE PROCEDURE seepage_nbc_spacetime
  ! IF( PRESENT( ConstantValue ) ) THEN
  !     CALL seepage_nbc_spacetime_constantvalue(Obj, tag, ConstantValue )
  !     RETURN
  ! END IF
END PROCEDURE seepage_nbc_spacetime

!----------------------------------------------------------------------------
!                                                                 applyNBC
!----------------------------------------------------------------------------

! SUBROUTINE seepage_nbc_spacetime_constantvalue( Obj, tag, ConstantValue )
!   CLASS( Seepage_ ), INTENT( INOUT) :: Obj
!   INTEGER( I4B ), INTENT( IN ) :: tag
!   REAL( DFP ), INTENT( IN ) :: ConstantValue

!   CLASS( Mesh_ ), POINTER :: meshobj
!   CLASS( Element_ ), POINTER :: elem
!   TYPE( ReferenceLine_ ) :: TimeElem
!   TYPE( QuadraturePoint_ ) :: spaceQuad, TimeQuad
!   TYPE( ElemShapeData_ ) :: timesd, spacesd
!   TYPE( STElemShapeData_ ), ALLOCATABLE :: stsd( : )
!   REAL( DFP ), ALLOCATABLE :: xija( :, :, : ), matprops( : )
!   INTEGER( I4B ), ALLOCATABLE :: nptrs( : ), local_nptrs( : )
!   TYPE( RealMatrix_ ), ALLOCATABLE :: rmat( : )
!   TYPE( RealVector_ ), ALLOCATABLE :: rvec( : )
!   INTEGER( I4B ) :: ii, iel, telem, nips, nipt, nns, ips, ipt
!   REAL( DFP ) :: dummyVar, g(3), dummyVar2

!   meshobj => NULL()
!   elem => NULL()
!   g = 0.0_DFP
!   g(obj%nsd) = -9.81

!   ALLOCATE( rmat(3), rvec(2) )
!   ALLOCATE( rmat( 1 ) % Val( 1, obj%NNT ) )
!   rmat(1)%Val( 1, 1 ) = obj%tn
!   rmat(1)%Val( 1, 2 ) = obj%tn+obj%dt
!   DO ii = 3, obj%NNT
!     rmat(1)%Val(1,ii) = obj%tn + (ii-2)*obj%dt/REAL(obj%NNT-1)
!   END DO

!     ! CALL display( '    Generating time element' )
!   TimeElem = ReferenceLine(nsd=1)
!   TimeElem = LagrangeElement( refelem=TimeElem, order=obj%NNT-1 )
!   TimeQuad = GaussLegendreQuadrature( refelem=TimeElem, &
!     & order= TimeElem%order )
!   nipt = SIZE( TimeQuad, 2 )

!   CALL initiate( obj=timesd, quad=TimeQuad, refElem=TimeElem, &
!     & ContinuityType = obj%TimeContinuity, &
!     & InterpolType = obj%TimeInterpol )

!   CALL initiate( obj=stsd, elemsd=timesd )
!   meshobj => obj%dom%boundary( tag )%ptr
!   elem => meshobj%elem(1)%ptr
!   spaceQuad = GaussLegendreQuadrature( refelem=elem%refelem, &
!     & order=elem%refelem%order )
!   nips = SIZE( spaceQuad, 2 )

!   ! CALL display( '  Generating shape data for space element' )
!   CALL initiate( obj = spacesd, quad = spacequad, &
!     & refelem = elem % refelem, &
!     & ContinuityType= Obj % SpaceContinuity, &
!     & InterpolType = Obj %  SpaceInterpol )

!   ! CALL display( '  Generating shape data for space-time element' )
!   DO ii = 1, nipt
!     CALL initiate( &
!       & obj=stsd(ii), quad=spaceQuad, refelem=elem%refelem, &
!       & ContinuityType=obj%SpaceContinuity, InterpolType=obj%SpaceInterpol)
!   END DO

!   nptrs = .Nptrs. elem
!   nns = SIZE( nptrs )
!   CALL Reallocate( xija, obj%nsd, nns, obj%nnt )
!   CALL Reallocate( rmat(3)%val, nns, Obj%nnt )
!   telem = meshobj%size()

!   DO iel = 1, telem
!     elem => meshobj%elem(iel)%ptr
!     nptrs = .Nptrs. elem
!     local_nptrs = obj%local_nptrs(nptrs)

!     rmat(1)%val = obj%dom%nodes(1:obj%nsd, nptrs)
!     rmat(2)%val = obj%dom%NodalVelocity(1:obj%nsd, nptrs)
!     xija(:,:,1) = rmat(1)%val(:,:)
!     xija(:,:,2) = rmat(1)%val + rmat(2)%val*obj%dt
!     DO ii=3,obj%nnt
!       xija(:,:,ii) = rmat(1)%val + rmat(2)%val*obj%dt*(ii-2)/(obj%nnt-1)
!     END DO

!     DO ii=1,size(stsd)
!       CALL setValue(obj=stsd(ii), val=xija, N=stsd(ii)%N, &
!         & dNdXi=stsd(ii)%dNdXi,&
!         & T=stsd(ii)%T )
!       CALL setNormal( stsd(ii) )
!     END DO

!     matprops = obj%matprops( :, elem%mat_type )
!     dummyVar2 = matprops(rho_w) * matprops(ks) / matprops(mu_w)
!     rmat(3)%val = 0.0_DFP

!     DO ipt=1, nipt

!       rvec(1)%val = stsd(ipt)%Js * stsd(ipt)%ws * &
!         & stsd(ipt)%Thickness * stsd(ipt)%wt * &
!         & stsd(ipt)%Jt

!       DO ips = 1, nips
!         dummyVar = dummyVar2*DOT_PRODUCT(stsd(ipt)%Normal(:, ips), g)
!         dummyVar =  rvec(1)%val(ips)*(dummyVar + ConstantValue)
!         rmat(3)%val = rmat(3)%val + dummyVar * OUTERPROD( &
!           & stsd(ipt)%N(:,ips), stsd(ipt)%T(:))
!       END DO

!       rvec(2)%val = RESHAPE( rmat(3)%val, [SIZE(rmat(3)%val)])

!       CALL addcontribution(Vec=obj%nodalVar(obj%rhs)%val,&
!         & Obj=obj%dof(obj%rhs), Nptrs=local_nptrs, &
!         & Val=rvec(2)%val, Scale=1.0_dfp, conversion=[dofToNodes])

!     END DO

!   END DO

!   DEALLOCATE( nptrs, local_nptrs, matprops, xija, stsd, rmat, rvec )
!   CALL DeallocateData(spaceQuad)
!   CALL DeallocateData(timeQuad)
!   CALL DeallocateData(timesd)
!   CALL DeallocateData(spacesd)
!   CALL DeallocateData(timeElem)
!   NULLIFY( elem, meshobj )

! END SUBROUTINE seepage_nbc_spacetime_constantvalue

!----------------------------------------------------------------------------
!                                                             AssembleTanmat
!----------------------------------------------------------------------------

MODULE PROCEDURE seepage_assemble_spacetime
  INTEGER( I4B ) :: ii,jj, telem, iel,  nips, nipt, nns, a
  REAL( DFP ), ALLOCATABLE :: xija( :, :, : )
  REAL( DFP ) :: dummyReal

  !-------------- main code --------------------
  SELECT TYPE( obj )
  CLASS IS ( Seepage_ )

    IF ( ALLOCATED( rmat( 1 ) % val  ) ) DEALLOCATE( rmat( 1 ) % val )
    ALLOCATE( rmat( 1 ) % Val( 1, obj%NNT ) )

    rmat(1)%Val( 1, 1 ) = obj%tn
    rmat(1)%Val( 1, 2 ) = obj%tn+obj%dt

    DO ii = 3, obj%NNT
      rmat(1)%Val(1,ii) = obj%tn + (ii-2)*obj%dt/REAL(obj%NNT-1)
    END DO

    meshobj => null()
    elem => null()
    ! CALL display( "  Resetting tangent matrix to zero" )
    CALL setValue( obj = obj % tanmat, val = 0.0_dfp )
    obj%nodalVar(obj%rhs)%val = 0.0_DFP

    ! CALL display( '    Generating time element' )
    TimeElem = ReferenceLine(nsd=1)
    TimeElem = LagrangeElement( refelem=TimeElem, order=obj%NNT-1 )

    ! CALL display( '    Generating quadrature points for time domain' )
    TimeQuad = GaussLegendreQuadrature( refelem=TimeElem, &
      & order= TimeElem%order*2 )
    nipt = SIZE( TimeQuad, 2 )

    ! generate shape data in time domain
    ! CALL display( '    Generating shape data for time element' )
    CALL initiate( obj=timesd, quad=TimeQuad, refElem=TimeElem, &
      & ContinuityType = obj%TimeContinuity, &
      & InterpolType = obj%TimeInterpol  )
    CALL setValue( obj=timesd, val=rmat(1)%val, N=timesd%N, &
      & dNdXi=timesd%dNdXi )

    ! CALL display( '    Initiating shape data for time element' )
    CALL initiate( obj=stsd, elemsd=timesd )

    a = 0; ks = 0.0_DFP

    DO ii=1,SIZE(obj%omegano)

      ! CALL display( ii, "  Computation over Omega :: ")

      meshobj => obj%dom%omega(obj%omegano(ii))%ptr
      elem => meshobj%elem(1)%ptr
      telem = meshobj%size()

      ! CALL display( '  Generating quadrature points for space element' )
      spaceQuad = GaussLegendreQuadrature( refelem=elem%refelem, &
        & order=2*elem%refelem%order )
      nips = SIZE(spaceQuad, 2)

      ! CALL display( '  Generating shape data for space element' )
      CALL initiate( obj = spacesd, quad = spacequad, &
        & refelem = elem % refelem, &
        & ContinuityType= Obj % SpaceContinuity, &
        & InterpolType = Obj %  SpaceInterpol )

      ! CALL display( '  Generating shape data for space-time element' )
      DO jj = 1,SIZE(stsd)
        CALL initiate( &
          & obj=stsd(jj), quad=spaceQuad, refelem=elem%refelem, &
          & ContinuityType=obj%SpaceContinuity, &
          & InterpolType=obj%SpaceInterpol)
      END DO

      ! start element loop
      nptrs = .Nptrs. elem
      nns = size(nptrs)
      CALL Reallocate( xija, obj%nsd, nns, obj%nnt )
      CALL Reallocate( rvec(2)%val, nns*obj%nnt )

      DO iel = 1, telem
        elem => meshobj%elem(iel)%ptr
        nptrs = .Nptrs. elem
        local_nptrs = obj%local_nptrs(nptrs)

        rmat(1)%val = obj%dom%nodes( 1:obj%nsd, nptrs )
        rmat(2)%val = obj%dom%NodalVelocity(1:obj%nsd, nptrs)
        xija(:,:,1) = rmat(1)%val(:,:)
        xija(:,:,2) = rmat(1)%val + rmat(2)%val*obj%dt
        DO jj=3,obj%nnt
          xija(:,:,jj) = rmat(1)%val + rmat(2)%val*obj%dt*(jj-2)/(obj%nnt-1)
        END DO

        DO jj=1, nipt
          CALL setValue(obj=stsd(jj), val=xija, N=stsd(jj)%N, T=stsd(jj)%T,&
            & dNdXi=stsd(jj)%dNdXi)
        END DO

        a = a + 1
        ! Term1
        dummyReal = obj%ss%val(1,1,a)/obj%rho_w/acc_gravity
        rmat(3)%val = dummyReal * MassMatrix(Test=stsd, Trial=stsd, &
          & Term1=0, Term2=1)

        ! Term2
        CALL setValue(obj=spacesd, val=rmat(1)%val, N=spacesd%N, &
          & dNdXi=spacesd%dNdXi)

        rmat(4)%val = dummyReal * MassMatrix( Test=spacesd, Trial=spacesd)

        rmat(3)%val( 1:size(rmat(4)%val, 1), 1:size(rmat(4)%val, 2) ) = &
          & + rmat(3)%val( 1:size(rmat(4)%val, 1), 1:size(rmat(4)%val, 2) ) &
          & + rmat(4)%val

        rvec(1)%val = ArrayValues( Val = obj%nodalVar(obj%p0)%val, &
          & Obj = obj%dof(obj%p0), &
          & DOFNo=[1], &
          & StorageFMT = FMT_DOF, Nptrs = local_nptrs )

        rvec(2)%val(1:size(rvec(1)%val))=MATMUL( rmat(4)%val, rvec(1)%val )

        CALL addcontribution( Vec=obj%nodalVar(obj%rhs)%val, &
          & Obj=obj%dof(obj%rhs), Nptrs=local_nptrs, Val=rvec(2)%val, &
          & Scale=1.0_dfp, conversion=[dofToNodes] )

        ks(1,1) = obj%ks%val(1, 1, a)/obj%mu_w
        ks(2,2) = obj%ks%val(2, 1, a)/obj%mu_w
        ks(3,3) = obj%ks%val(3, 1, a)/obj%mu_w

        diff_coeff  = QuadratureVariable( ks, typefevariableMATRIX, &
          & typeFeVariableConstant )

        rmat(3)%val = rmat(3)%val + &
          & DiffusionMatrix(Test=stsd, Trial=stsd, K = diff_coeff )

        ! Adding contribution
        !! Need to convert global nodes into local nodes
        CALL addcontribution( obj = obj % tanmat, nptrs=local_nptrs, &
          & val=rmat(3)%val, scale = 1.0_dfp, &
          & storageFMT = DOF_FMT )

      END DO
      ! CALL display( '    Ended: element loop' )
    END DO

    ! ! adding contribution of drainage boundary
    ! meshobj => null( ); elem => null( )

    ! IF( obj%dbcinfo( 2 ) .NE. 0 ) THEN
    !   DO ii = 1, SIZE( obj%db(2)%val )
    !     ! CALL display( Obj%db(2)%val( ii ),"  Computation over boundary :: ")
    !     meshobj => obj%dom%boundary(obj%db(4)%val(ii))%ptr
    !     telem=meshobj%size()
    !     elem=>meshobj%elem(1)%ptr

    !     ! CALL display( '  Generating quadrature points for space element' )
    !     spaceQuad = GaussLegendreQuadrature( refelem=elem%refelem, &
    !       & order=2*elem%refelem%order )

    !     ! CALL display( '  Generating shape data for space element' )
    !     ! CALL display( '  Generating shape data for space-time element' )
    !     DO jj = 1, SIZE(stsd)
    !       CALL initiate( &
    !         & obj=stsd(jj), quad=spaceQuad, refelem=elem%refelem, &
    !         & ContinuityType=obj%SpaceContinuity,&
    !         & InterpolType=obj%SpaceInterpol)
    !     END DO

    !     ! start element loop
    !     nptrs = .Nptrs. elem
    !     CALL Reallocate( xija, obj%nsd, size(nptrs), obj%nnt )

    !     DO iel = 1, telem
    !       elem => meshobj%elem(iel)%ptr
    !       nptrs = .Nptrs. elem

    !       rmat(1)%val = obj%dom%nodes( 1:obj%nsd, nptrs )
    !       rmat(2)%val = obj%dom%NodalVelocity(1:obj%nsd, nptrs)
    !       xija(:,:,1) = rmat(1)%val(:,:)
    !       DO jj=2,obj%nnt
    !         xija(:,:,jj) = rmat(1)%val + rmat(2)%val*obj%dt*(jj-1)/(obj%nnt-1)
    !       END DO
    !       DO jj=1,size(stsd)
    !         CALL setValue(obj=stsd(jj), val=xija, N=stsd(jj)%N, &
    !           & dNdXi=stsd(jj)%dNdXi, T=stsd(jj)%T)
    !       END DO

    !       matprops = obj%matprops( :, elem%mat_type )

    !       ! Term1
    !       rmat(3)%val = MassMatrix( Test=stsd, Trial=stsd, Term1=0, Term2=0)
    !       rmat(3)%val = matprops(kd) * rmat(mu_w)%val
    !       ! Adding contribution
    !       local_nptrs = obj%local_nptrs(nptrs)
    !       !! Need to convert global nodes into local nodes
    !       CALL addcontribution( obj = obj % tanmat, &
    !         & nptrs=local_nptrs, val = rmat( 3 ) % val, scale = 1.0_dfp, &
    !         & conversion = doftonodes )
    !     END DO
    !     ! CALL display( '    Ended: element loop' )
    !   END DO
    ! END IF
  END SELECT

  NULLIFY( MeshObj, Elem )
  DEALLOCATE( xija )
END PROCEDURE seepage_assemble_spacetime

!----------------------------------------------------------------------------
!                                                             AssembleTanmat
!----------------------------------------------------------------------------

MODULE PROCEDURE seepage_solve_spacetime
  INTEGER( I4B ) :: ii, idof
  SELECT TYPE(Obj)
  CLASS IS( Seepage_ )

    CALL obj%linsol%setMatrix( obj%tanmat )
    obj%nodalVar(obj%p_trial)%val=obj%nodalVar(obj%p)%val
    CALL obj%linsol%solve( rhs=obj%nodalVar(obj%rhs)%val, &
      & sol=obj%nodalVar(obj%p)%val )
    obj%err = 0.0_DFP
    obj%err0 = 0.0_DFP

    DO ii = 1, obj%tnodes
      obj%nodalVar(obj%dpdt)%val(ii) = &
        & ( obj%nodalVar(obj%p)%val( (ii-1)*obj%tdof+2 ) &
        & - obj%nodalVar(obj%p)%val( (ii-1)*obj%tdof+1 ) ) &
        & / obj%dt
      obj%err = obj%err &
        & + (obj%nodalVar(obj%p)%val( (ii-1)*obj%tdof+2 ) &
        & - obj%nodalVar(obj%p_trial)%val( (ii-1)*obj%tdof+2 ))**2
      obj%err0 = obj%err0 &
        & + (obj%nodalVar(obj%p0)%val(ii))**2
    END DO

    obj%err = SQRT(obj%err/obj%tnodes)
    obj%err0 = SQRT(obj%err0/obj%tnodes)

  END SELECT
END PROCEDURE seepage_solve_spacetime

!----------------------------------------------------------------------------
!                                                                getVelocity
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
!  Mass lumping is used to calculate the velocity

MODULE PROCEDURE seepage_velocity_explicit_spacetime

  !Internal variables
  INTEGER( I4B ) :: ii, telem, iel,jj, idof, a

  meshobj=>null()
  elem=>null()
  ! CALL display( "  Resetting velocit to zero" )
  obj%nodalVar(obj%v)%val=0.0_DFP
  obj%nodalVar(obj%v_rhs)%val=0.0_DFP

  a = 0; ks = 0.0

  DO ii=1,SIZE(obj%omegano)
    ! CALL display( ii, "  Computation over Omega :: ")
    meshobj=>obj%dom%omega(obj%omegano(ii))%ptr
    elem => meshobj%elem(1)%ptr
    telem = meshobj%size()
    ! CALL display( '  Generating quadrature points for space element' )
    spaceQuad = GaussLegendreQuadrature( refelem=elem%refelem, &
      & order=2*elem%refelem%order )
    ! CALL display( '  Generating shape data for space element' )
    CALL initiate( obj = spacesd, quad = spacequad, &
      & refelem = elem % refelem, &
      & ContinuityType= Obj % SpaceContinuity, &
      & InterpolType = Obj %  SpaceInterpol )

    DO iel=1,telem
      a = a + 1;
      elem=>meshobj%elem(iel)%ptr
      nptrs = .nptrs. elem
      local_nptrs=obj%local_nptrs(nptrs)
      rmat(1)%val=obj%dom%nodes( 1:obj%nsd, nptrs )
      rmat(2)%val = obj%dom%NodalVelocity(1:obj%nsd, nptrs)
      rmat(1)%val=rmat(1)%val+obj%dt*rmat(2)%val
      CALL setValue( obj=spacesd, val=rmat(1)%val, N=spacesd%N,&
        & dNdXi=spacesd%dNdXi)
      rmat(2)%val=MassMatrix(Test=spacesd, Trial=spacesd, nCopy=obj%nsd)

      CALL addcontribution(Vec=obj%nodalVar(obj%v)%val,&
        & Obj=obj%dof(obj%v), Nptrs=local_nptrs, &
        & Val=SUM(rmat(2)%val,2), Scale=1.0_dfp, conversion=[dofToNodes])

      ! matprops = obj%matprops( :, elem%mat_type )
      ! get value of pn+1
      rvec(1)%val=ArrayValues( Val = obj%nodalVar(obj%p)%val, &
        & Obj = obj%dof(obj%p), DOFNo=[2], StorageFMT = FMT_DOF, &
        & Nptrs = local_nptrs )
      ! get dpdXt
      CALL getSpatialGradient(obj=spacesd,Val=rvec(1)%val, &
        & dPhidXt=rmat(2)%val)

      ks(1,1) = obj%ks%val(1, 1, a)/obj%mu_w
      ks(2,2) = obj%ks%val(2, 1, a)/obj%mu_w
      ks(3,3) = obj%ks%val(3, 1, a)/obj%mu_w

      rmat(2)%val = MATMUL( ks(1:obj%nsd, 1:obj%nsd), rmat(2)%val )
      CALL Reallocate( rmat(3)%val, obj%nsd, SIZE(spacesd%N, 1) )

      rmat(3)%val=0.0_DFP
      rvec(2)%val=spacesd%Js * spacesd%Ws * spacesd%Thickness
      DO jj = 1, SIZE(rmat(2)%val,2)
        DO idof=1,obj%nsd
          rmat(3)%val(idof, :)=rmat(3)%val(idof, :) &
            & + (rvec(2)%val(jj)*rmat(2)%val(idof,jj))* spacesd%N(:,jj)
        END DO
      END DO

      rvec(2)%val = RESHAPE(rmat(3)%val, [SIZE(rmat(3)%val)])

      CALL addcontribution(Vec=obj%nodalVar(obj%v_rhs)%val,&
        & Obj=obj%dof(obj%v_rhs), Nptrs=local_nptrs, &
        & Val=rvec(2)%val, Scale=-1.0_dfp, conversion=[NONE])
    END DO
  END DO

  DO ii=1,SIZE(obj%nodalVar(obj%v)%val)
    obj%nodalVar(obj%v)%val(ii)=&
      & obj%nodalVar(obj%v_rhs)%val(ii)/obj%nodalVar(obj%v)%val(ii)
  END DO

  NULLIFY( elem, meshobj )
END PROCEDURE seepage_velocity_explicit_spacetime

!----------------------------------------------------------------------------
!                                                                isConverged
!----------------------------------------------------------------------------

MODULE PROCEDURE seepage_isconverg_spacetime
  LOGICAL(LGT) :: l1, l2

  SELECT TYPE(obj)
  CLASS IS(Seepage_)

    l1 = .TRUE.
    l2 = .TRUE.
    IF( PRESENT(abstol) ) THEN
      IF( obj%err .LE. abstol ) THEN
        l1 = .TRUE.
      ELSE
        l1 = .FALSE.
      END IF
    END IF

    IF( PRESENT(reltol) ) THEN
      IF( obj%err .LE. obj%err0 * reltol ) THEN
        l2 = .TRUE.
      ELSE
        l2 = .FALSE.
      END IF
    END IF
  END SELECT

  Ans = l1 .AND. l2
END PROCEDURE seepage_isconverg_spacetime

!----------------------------------------------------------------------------
!                                                                    Update
!----------------------------------------------------------------------------

MODULE PROCEDURE seepage_update_spacetime
  INTEGER( I4B ) :: ii
  REAL( DFP ) :: y

  SELECT TYPE( Obj )
  CLASS IS( Seepage_ )
    obj%tn=obj%tn+obj%dt
    DO ii =1, obj%tNodes
      obj%nodalVar(obj%p0)%val(ii) = &
        & obj%nodalVar(obj%p)%val((ii-1)*obj%tdof + 2 )

      y = obj%dom%nodes(obj%nsd, obj%nptrs(ii) ) &
        & + obj%dom%NodalVelocity(obj%nsd, obj%nptrs(ii)) * obj%dt

      obj%nodalVar(obj%pw)%val(ii) = obj%nodalVar(obj%p0)%val(ii) &
        & - obj%rho_w * ACC_GRAVITY * y
    END DO
  END SELECT
END PROCEDURE seepage_update_spacetime

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE seepage_writedata_spacetime
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
    pfe(2)=string('seepage')
  END IF

  IF(PRESENT(extension)) THEN
    pfe(3)=string(trim(extension))
  ELSE
    pfe(3)=string('.vtu')
  END IF

  ! step -1
  SELECT TYPE(obj)
  CLASS IS(Seepage_)
    call vtu % initiate( &
      & path = trim(pfe(1)%chars()), &
      & filename = trim(pfe(2)%chars()), &
      & extension = trim(pfe(3)%chars()), &
      & fmt = "ascii", &
      & meshTopology = "UnstructuredGrid", &
      & indx = indx )

    ! step-2
    call vtu%openPiece( )
    call vtu%writeGeometry( MeshObj=obj%dom%omega, &
        & nodes = obj%dom%nodes, tag=obj%omegano, &
        & local2global = obj%nptrs, map=obj%local_nptrs, &
        & lb = LBOUND(obj%local_nptrs,1), ub=UBOUND(obj%local_nptrs,1) )
    ! step-3
    call vtu%openNodeData( )
    call vtu%writeNodeData( x=obj%nodalVar(obj%p0)%val,&
      & dofobj=obj%dof(obj%p0), prefix="_PiezoPressure" )
    call vtu%writeNodeData(x=obj%nodalVar(obj%v)%val,dofobj=obj%dof(obj%v),&
      & prefix="_Velocity")
    call vtu%writeNodeData(x=obj%nodalVar(obj%pw)%val,dofobj=obj%dof(obj%pw),&
      & prefix="_Pressure")
    call vtu % closeNodeData( )

    ! call vtu % startWriteElementData( )
    ! call vtu % stopWriteElementData( )
    call vtu % closePiece( )
    call vtu % Finalize( )

    ! gmsh post process
    IF( PRESENT(gmsh) ) THEN
    call gmsh%model%mesh%writenodedata(x=obj%nodalVar(obj%p0)%val,&
      & Indx=indx, dofObj=obj%dof(obj%p0), Name='P', nodes=obj%dom%nodes, &
      & local_nptrs=obj%local_nptrs)
    call gmsh%model%mesh%writenodedata(x=obj%nodalVar(obj%v)%val,&
      & Indx=indx, dofObj=obj%dof(obj%v), Name='v',  nodes=obj%dom%nodes, &
      & local_nptrs=obj%local_nptrs)
    call gmsh%model%mesh%writenodedata(x=obj%nodalVar(obj%pw)%val,&
      & Indx=indx, dofObj=obj%dof(obj%pw), Name='p', nodes=obj%dom%nodes, &
      & local_nptrs=obj%local_nptrs)
    END IF
  END SELECT
END PROCEDURE seepage_writedata_spacetime

!----------------------------------------------------------------------------
!                                                                 saveState
!----------------------------------------------------------------------------

MODULE PROCEDURE saveState_spacetime

  SELECT TYPE( Obj )
  CLASS IS( Seepage_ )

    SELECT CASE( TRIM(Extension) )

    CASE( '.hdf5', '.h5', 'hdf5', 'h5' )
      CALL saveState_hdf5(obj, path, filename, extension, timestep)

    CASE DEFAULT
      CALL Display( "ERROR:: Seepage_Class@SpaceTimeMethods.f90")
      CALL Display( "          saveState_spacetime()")
      CALL Display( "            unidentified extension")
      CALL Display( "            program stopeed")
      STOP
    END SELECT

  END SELECT

END PROCEDURE saveState_spacetime

!----------------------------------------------------------------------------
!                                                             saveState_hdf5
!----------------------------------------------------------------------------

SUBROUTINE saveState_hdf5( obj, path, filename, extension, TimeStep )
  CLASS( Seepage_ ), INTENT( INOUT) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: path, filename, extension
  INTEGER( I4B ), INTENT( IN ) :: Timestep

  ! main program
  type(hdf5_file) :: h5f
  type( string ) :: grp

  call ExecuteCommand( 'mkdir -p '//trim(path), &
    & 'Seepage_Class@SpaceTimeMethods.f90' )

  call h5f%initialize( &
    & filename= trim(path)//trim(filename)//trim(Extension), &
    & status='new', action='w', comp_lvl=1)

  grp = '/'
  ! grp = '/Seepage_Class/State/TimeStep_' // &
  !   & trim(str(TimeStep,no_sign=.true.)) // '/'
  ! call h5f%write_group( grp%chars() )

  ! adding group attributes
  ! call h5f%writeattr(dname='/Seepage_Class/', attr='nsd', attrval=obj%nsd)
  ! call h5f%writeattr(dname='/Seepage_Class/', attr='nnt', attrval=obj%nnt)
  ! call h5f%writeattr(dname='/Seepage_Class/', attr='tNodes', &
  !   & attrval=obj%tNodes)
  ! call h5f%writeattr(dname='/Seepage_Class/', attr='tdof', &
  !   & attrval=obj%tdof)

  ! call h5f%writeattr(dname=grp%chars(), attr='tn', attrval=obj%tn)
  ! call h5f%writeattr(dname=grp%chars(), attr='dt', attrval=obj%dt)

  ! call h5f%write( trim(grp)//'P', obj%nodalVar(obj%p)%val )
  ! call h5f%writeattr(dname=trim(grp)//'P', &
  !   & attr='variable type', attrval='nodal')
  ! call h5f%writeattr(dname=trim(grp)//'P', attr='rank', attrval='scalar')
  ! call h5f%writeattr(dname=trim(grp)//'P', attr='tdof', attrval=obj%tdof)
  ! call h5f%writeattr(dname=trim(grp)//'P', attr='tNodes', attrval=obj%tNodes)

  call h5f%write( trim(grp)//'P0', obj%nodalVar(obj%p0)%val )
  call h5f%writeattr(dname=trim(grp)//'P0', &
    & attr='variable type', attrval='nodal')
  call h5f%writeattr(dname=trim(grp)//'P0', attr='rank', attrval='scalar')
  call h5f%writeattr(dname=trim(grp)//'P0', attr='tdof', attrval=1)
  call h5f%writeattr(dname=trim(grp)//'P0', attr='tNodes', attrval=obj%tNodes)
  call h5f%writeattr(dname=trim(grp)//'P0', attr='nsd', attrval=obj%nsd)
  call h5f%writeattr(dname=trim(grp)//'P0', attr='nnt', attrval=obj%nnt)
  call h5f%writeattr(dname=trim(grp)//'P0', attr='tdof', attrval=obj%tdof)
  call h5f%writeattr(dname=trim(grp)//'P0', attr='tn', attrval=obj%tn)
  call h5f%writeattr(dname=trim(grp)//'P0', attr='dt', attrval=obj%dt)

  call h5f%finalize()
END SUBROUTINE saveState_hdf5

END SUBMODULE SpaceTimeAlgorithm