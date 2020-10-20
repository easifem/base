SUBMODULE( Seepage_Class ) Constructor
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               setAlgorithm
!----------------------------------------------------------------------------

MODULE PROCEDURE seepage_setalgo
  INTEGER( I4B ) :: ii

  IF( .NOT. PRESENT( mainOption ) ) THEN
    CALL display( "ERROR:: Seepage_Class@Constructor.f90" )
    CALL display( "        seepage_setalgo()" )
    CALL display( "          mainOption should be present" )
    stop
  END IF

  SELECT CASE( mainOption( 1 ) )
  CASE( ALGO_OPT_STATIC )
    obj%p=1
    obj%rhs=2
    i_kx = 1
    i_ky = 2
    i_kz = 3
    obj%applyInitCondition => NULL()
    obj%applyDBC => seepage_apply_dbc_static
    obj%assemble=>seepage_assemble_static
    obj%solve=>seepage_solve_static
    obj%isConverged=>seepage_isconverg_static
    obj%update=>seepage_update_static
    obj%writedata => seepage_writedata_static
    obj%applyNBC => NULL() ! implement later
    obj%getVelocity=> NULL() ! implement later
    CALL seepage_setdof_static(Obj)
    CALL Obj%setLinearSolver(name=lis_cg, tol=1.0d-8)
    CALL Obj%setPrecondition()
    CALL Obj%setTanmat()

  CASE( ALGO_OPT_SEMIDISCRETE )
    CALL display( "ERROR:: Seepage_Class@Constructor.f90" )
    CALL display( "        seepage_setalgo()" )
    CALL display( "          Semi-discrete case not implemented yet" )
    stop

  CASE( ALGO_OPT_SPACETIME )
    obj%p = 1 !! Total pressure
    obj%rhs=2 !! RHS
    obj%p0=3 !! Initial total pressure
    obj%v=4 !! Nodal Velocity
    obj%dpdt=5 !! Rate of change of pressure
    obj%p_trial=6 !! Trial values of total pressure
    obj%v_rhs=7 !! Rhs for obtaining velocities
    obj%pw=8 !! Fluid pressure
    i_Ss=4; i_kx=1; i_ky=2; i_kz=3; i_kd=5

    obj%applyInitCondition => seepage_apply_initcond_spacetime
    obj%applyDBC => seepage_apply_dbc_spacetime
    obj%assemble => seepage_assemble_spacetime
    obj%solve => seepage_solve_spacetime
    obj%isConverged => seepage_isconverg_spacetime
    obj%update => seepage_update_spacetime
    obj%writedata => seepage_writedata_spacetime
    obj%applyNBC => seepage_nbc_spacetime
    obj%getVelocity => seepage_velocity_explicit_spacetime
    obj%saveState => saveState_spacetime

    CALL seepage_setdof_spacetime(Obj)
    CALL Obj%setLinearSolver(name=lis_bicg, tol=1.0d-8)
    CALL Obj%setPrecondition()
    CALL Obj%setTanmat()

  END SELECT

END PROCEDURE seepage_setalgo
!----------------------------------------------------------------------------
!                                                       setMaterialProperties
!----------------------------------------------------------------------------

MODULE PROCEDURE set_matprops
  REAL( DFP ), ALLOCATABLE :: mat_props( :, : )

  Obj%MatProps = matProps
  mat_props = matProps(i_kx:i_kz, :)
  CALL obj%setPermeability( ValFromMatType=mat_props)

  IF( SIZE( matProps, 1 ) .GT. 3 ) THEN
    mat_props = matProps(i_ss:i_ss, :)
    CALL obj%setSpecificStorage( ValFromMatType=mat_props)
  END IF

  IF( ALLOCATED( mat_props ) ) DEALLOCATE( mat_props )
END PROCEDURE set_matprops

!----------------------------------------------------------------------------
!                                                            setPermeability
!----------------------------------------------------------------------------

MODULE PROCEDURE set_perm
  INTEGER( I4B ) :: a, ii, iel, telem

  IF( PRESENT( ValFromMatType ) ) THEN

    IF( SIZE( ValFromMatType, 1 ) .NE. 3 ) THEN
      CALL Display( "ERROR:: In Seeepage_Class@Constructor.f90")
      CALL Display( "        SetPermeability()")
      CALL Display( "          SIZE( ValFromMatType, 1 ) should be 3 ")
      STOP
    END IF

    CALL obj%ks%initiate( tprop=3, tpoint=1, telem=obj%telements )
    meshobj => NULL(); elem => null(); a = 0
    DO ii=1,size(obj%omegano)
      meshobj => obj%dom%omega(obj%omegano(ii))%ptr
      DO iel=1, meshobj%size()
        elem => meshobj%elem(iel)%ptr
        a=a+1
        CALL obj%ks%setValue(ipoint=1, elemnum=a, &
          & Val=ValFromMatType( :, elem%mat_type ) )
      END DO
    END DO
    NULLIFY( meshobj, elem )
  END IF

END PROCEDURE set_perm

!----------------------------------------------------------------------------
!                                                          setSpecificStorage
!----------------------------------------------------------------------------

MODULE PROCEDURE set_ss
  INTEGER( I4B ) :: a, ii, iel, telem

  IF( PRESENT( ValFromMatType ) ) THEN

    IF( SIZE( ValFromMatType, 1 ) .NE. 1 ) THEN
      CALL Display( "ERROR:: In Seeepage_Class@Constructor.f90")
      CALL Display( "        SetSpecificStorage()")
      CALL Display( "          SIZE( ValFromMatType, 1 ) should be 1 ")
      STOP
    END IF

    CALL obj%ss%initiate( tprop=1, tpoint=1, telem=obj%telements )
    meshobj => NULL(); elem => null(); a = 0
    DO ii=1,size(obj%omegano)
      meshobj => obj%dom%omega(obj%omegano(ii))%ptr
      DO iel=1, meshobj%size()
        elem => meshobj%elem(iel)%ptr; a=a+1
        CALL obj%ss%setValue(ipoint=1, elemnum=a, &
          & Val=ValFromMatType( :, elem%mat_type ) )
      END DO
    END DO
    NULLIFY( meshobj, elem )
  END IF
END PROCEDURE set_ss

!----------------------------------------------------------------------------
!                                                                setBoundary
!----------------------------------------------------------------------------

MODULE PROCEDURE seepage_setbc
  INTEGER( I4B ) :: ii
  ! INTEGER( I4B ), ALLOCATABLE :: Nptrs(:)

  ALLOCATE( Obj%DB(2), obj%DBCinfo(2) )
  obj%DBCinfo = 0

  IF( PRESENT( DB ) ) THEN

    IF( .NOT. ALLOCATED( obj%dom%boundary ) ) THEN
      CALL display( "ERROR:: In ", __FILE__ )
      CALL display( __LINE__, "        Line " )
      CALL display( "        seepage_setbc()" )
      CALL display( "          dom % boundary NOT ALLOCATED" )
      stop
    END IF

    IF( ANY( DB .GT. SIZE( obj%dom%boundary ) ) ) THEN
      CALL display( "ERROR:: In ", __FILE__ )
      CALL display( __LINE__, "        Line " )
      CALL display( "        seepage_setbc()" )
      CALL display( "          bound error due to uniformDB" )
      stop
    END IF

    DO ii = 1, SIZE( DB )
      IF( .NOT. ASSOCIATED( obj%dom%boundary( DB( ii ) ) % ptr)) THEN
        CALL display( "ERROR:: In ", __FILE__ )
        CALL display( __LINE__, "        Line " )
        CALL display( "        seepage_setbc()" )
        CALL display( "          some dom % omega are not associated" )
        STOP
      END IF
    END DO
    obj % DB(1) = IntVector(DB)
    obj % dbcinfo( 1 ) = 1
  END IF

  IF( PRESENT( robinB ) ) THEN

    IF( .NOT. ALLOCATED( obj%dom%boundary ) ) THEN
      CALL display( "ERROR:: Seepage_Class@Constructor.f90" )
      CALL display( "        seepage_setbc()" )
      CALL display( "          dom % boundary NOT ALLOCATED" )
      stop
    END IF

    IF( ANY( robinB .GT. SIZE( obj%dom%boundary ) ) ) THEN
      CALL display( "ERROR:: Seepage_Class@Constructor.f90" )
      CALL display( "        seepage_setbc()" )
      CALL display( "          bound error due to robinB" )
      stop
    END IF

    DO ii = 1, SIZE( robinB )
      IF( .NOT. ASSOCIATED( obj%dom%boundary( robinB( ii ) )%ptr ) ) THEN
        CALL display( "ERROR:: Seepage_Class@Constructor.f90" )
        CALL display( "        seepage_setbc()" )
        CALL display( "          some dom % omega are not associated" )
        stop
      END IF
    END DO
    obj % DB(2) = IntVector(robinB)
    obj % dbcinfo(2) = 2
  END IF

  ! CALL display( '  allocating obj%intvec(obj%nnt)')

  IF( ALLOCATED( obj % intvec ) ) DEALLOCATE( obj % intvec )
  ALLOCATE( obj % intvec( obj%tdof ) )

  IF( obj % dbcinfo( 1 ) .NE. 0 ) THEN

    DO ii = 1, SIZE( DB )
      nptrs = obj%dom%mdboundary(DB(ii)) % ptr%Nptrs
      CALL append( obj % intvec( 1 ), obj % local_nptrs( nptrs ) )
    END DO
    CALL RemoveDuplicates( obj % intvec( 1 ) )

    DO ii=2,obj%tdof
      obj%intvec(ii)%val=obj%intvec(1)%val
    END DO

  END IF

  CALL obj%linsol%setDirichletBCNodes( &
    & nptrs = obj % intvec, dofs = arange(start=1,end=obj%tdof) )

  DEALLOCATE( obj%intvec )

END PROCEDURE seepage_setbc

!----------------------------------------------------------------------------
!                                                         getNormalVelocity
!----------------------------------------------------------------------------

MODULE PROCEDURE seepage_norm_vel
  INTEGER( I4B ) :: maxnptrs, minnptrs, ii, tnodes, ncomp, jj, telem, ips, kk
  INTEGER( I4B ), ALLOCATABLE :: nptrs0(:), lp(:)

  ! <improve>
  ! constract local_node pointer
  maxnptrs = 0
  minnptrs = size(obj%dom%nodes,2)
  IF( ALLOCATED( nptrs ) ) DEALLOCATE( nptrs )

  DO ii = 1, SIZE(tag)
    maxnptrs = MAX(maxnptrs, obj%dom%mdBoundary(tag(ii))%ptr%maxNptrs)
    minnptrs = MIN(minnptrs, obj%dom%mdBoundary(tag(ii))%ptr%minNptrs)
    CALL append( nptrs, obj%dom%mdBoundary(tag(ii))%ptr%nptrs)
  END DO
  CALL RemoveDuplicates(nptrs)
  nptrs0 = nptrs

  tnodes = SIZE(nptrs)

  IF( ALLOCATED(local_nptrs) ) DEALLOCATE( local_nptrs )
  ALLOCATE( local_nptrs(1:maxNptrs) )
  local_nptrs(:) = 0
  DO ii=1,tnodes
    local_nptrs(nptrs(ii)) = ii
  END DO

  ! initiate rhs, nodalvar and dofs
  ncomp = obj%nsd
  rhs_dof = dof( tNodes = [ tNodes ], Names = [ 'f' ], &
    & SpaceCompo = [ ncomp ], TimeCompo = [ 1 ], StorageFMT = Nodes_FMT  )
  vn_dof = dof( tNodes = [ tNodes ], Names = [ 'v' ], &
    & SpaceCompo = [ ncomp ], TimeCompo = [ 1 ], StorageFMT = Nodes_FMT  )
  CALL Initiate( rhs, rhs_dof )
  CALL Initiate( vn, vn_dof )

  meshobj=>null()
  elem=>null()
  DO ii = 1, SIZE(tag)
    meshobj => obj%dom%boundary(tag(ii))%ptr
    elem => meshobj%elem(1)%ptr
    spaceQuad = GaussLegendreQuadrature( refelem=elem%refelem, &
      & order=2*elem%refelem%order )

    CALL initiate( obj = spacesd, quad = spacequad, &
      & refelem = elem % refelem, ContinuityType=TypeH1, &
      & InterpolType = TypeLagrangeInterpolation )

    telem = meshobj%size()

    DO jj=1,telem
      elem => meshobj%elem(jj)%ptr
      nptrs = .nptrs. elem
      lp = local_nptrs(nptrs)
      rmat(1)%val=obj%dom%nodes(1:obj%nsd, nptrs)

      CALL setValue(obj=spacesd, val=rmat(1)%val, N=spacesd%N, &
        & dNdXi=spacesd%dNdXi)
      CALL setNormal(spacesd)
      rmat(2)%val=MassMatrix(Test=spacesd, Trial=spacesd, nCopy=ncomp)

      CALL addcontribution(Vec=vn,&
        & Obj=vn_dof, Nptrs=lp, &
        & Val=SUM(rmat(2)%val,2), &
        & Scale=1.0_dfp, conversion=[dofToNodes])

      rmat(3)%val=spacesd%normal(1:obj%nsd, :) ! get normal to the surface
      CALL Reallocate(rmat(4)%val, ncomp, SIZE(spacesd%N,1))
      rvec(1)%val = spacesd%Js*spacesd%Ws*spacesd%thickness

      DO ips=1,SIZE(spacesd%N,2)
        rmat(4)%val = rmat(4)%val + rvec(1)%val(ips) &
          & * OUTERPROD(rmat(3)%val(:,ips), spacesd%N(:,ips))
      END DO

      rvec(2)%val = RESHAPE(rmat(4)%val, [SIZE(rmat(4)%val)])
      CALL addcontribution(Vec=rhs,&
        & Obj=rhs_dof, Nptrs=lp, &
        & Val=rvec(2)%val, &
        & Scale=1.0_dfp, &
        & conversion=[NONE])
    END DO
  END DO

  rvec(1)%val = [0.0, 0.0, 0.0]
  rvec(2)%val = [0.0, 0.0, 0.0]
  rvec(3)%val = [0.0]
  Angle = 0.0_DFP

  DO ii = 1, tnodes
    DO jj = 1, ncomp
      rvec(1)%val(jj)=rhs((ii-1)*ncomp+jj)/vn((ii-1)*ncomp+jj) ! normal vector
      kk=obj%local_nptrs(nptrs0(ii))
      rvec(2)%val(jj)=obj%nodalVar(obj%v)%val((kk-1)*ncomp+jj) ! velocity vec
    END DO

    rvec(3)%val(1) = DOT_PRODUCT(rvec(1)%val(1:ncomp), rvec(2)%val(1:ncomp))
    Angle = Angle + (rvec(3)%val(1)**2) &
      & / DOT_PRODUCT(rvec(2)%val(1:ncomp), rvec(2)%val(1:ncomp))

    DO jj = 1, ncomp
      vn((ii-1)*ncomp+jj) = rvec(1)%val(jj) * rvec(3)%val(1)
    END DO
  END DO

  Angle = ACOS(SQRT(Angle/REAL(tNodes,DFP)))*180.0_DFP/PI

  NULLIFY( elem, meshobj )
  DEALLOCATE( lp )

END PROCEDURE seepage_norm_vel

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Constructor