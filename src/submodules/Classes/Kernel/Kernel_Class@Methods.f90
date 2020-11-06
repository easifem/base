SUBMODULE( Kernel_Class ) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE kernel_init
  obj%nsd=nsd
  IF( PRESENT( nnt ) ) THEN
    obj%nnt = nnt
  ELSE
    obj%nnt=1
  END IF

  IF( PRESENT( dt ) ) THEN
    obj%dt = dt
  ELSE
    obj%dt=1.0_DFP
  END IF

  IF( PRESENT( SpatialCoordType ) ) THEN
    obj%SpatialCoordType=SpatialCoordType
  ELSE
    SELECT CASE( NSD )
    CASE( 1 )
      obj%SpatialCoordType = obj%OneD_Horizontal
    CASE( 2 )
      obj%SpatialCoordType = obj%TwoD_Cartesian
    CASE( 3 )
      obj%SpatialCoordType = obj%ThreeD
    END SELECT
  END IF

  IF( PRESENT( tn ) ) THEN
    Obj%tn=tn
  ELSE
    Obj%tn=0.0_DFP
  END IF

  IF( PRESENT( NTS ) ) THEN
    Obj%NTS=NTS
  ELSE
    Obj%NTS=0
  END IF

  Obj%iter=0
  Obj%its=0

  IF( PRESENT( tol_res ) ) THEN
    Obj%tol_res = tol_res
  ELSE
    Obj%tol_res = 1.0E-5
  END IF

  IF( PRESENT( tol_sol ) ) THEN
    Obj%tol_sol = tol_sol
  ELSE
    Obj%tol_sol = 1.0E-5
  END IF

END PROCEDURE kernel_init

!----------------------------------------------------------------------------
!                                                                 setDomain
!----------------------------------------------------------------------------

MODULE PROCEDURE kernel_setDomain
  INTEGER( I4B ) :: ii, maxnptrs, minNptrs, a
  INTEGER, ALLOCATABLE :: nptrs(:)

  ! CALL display( '  pointing to user supplied domain' )
  obj%dom => dom
  IF( .NOT. ALLOCATED( dom%omega ) ) THEN
    CALL display( "ERROR:: Kernel_Class@Methods.f90" )
    CALL display( "        kernel_setDomain()" )
    CALL display( "          dom%omega not ALLOCATED" )
    STOP
  END IF

  IF( .NOT. associated( dom%nodes ) ) then
    CALL display( "ERROR:: Kernel_Class@Methods.f90" )
    CALL display( "        seepage_init()" )
    CALL display( "          dom%nodes are note associated" )
    stop
  END IF

  ! CHECK
  IF( any( omegano .gt. SIZE( dom%omega ) ) ) then
    CALL display( "ERROR:: Kernel_Class@Methods.f90" )
    CALL display( "        seepage_init()" )
    CALL display( "          bound error due to OmegaNo" )
    stop
  END IF

  ! CHECK
  DO ii = 1, SIZE( omegano )
    IF( .not. associated( dom%omega( omegano( ii ) )%ptr  ) ) then
      CALL display( "ERROR:: Kernel_Class@Methods.f90" )
      CALL display( "        seepage_init()" )
      CALL display( "          some dom%omega are not associated" )
      stop
    END IF
  END DO

  obj%omegano = omegano
  DO ii = 1, SIZE( omegano )
    WRITE( *, "(4x, A)" ) trim( obj%dom%omega_name( omegano( ii ) ) )
  END DO

  DO ii = 1, SIZE( obj%omegano )
    CALL append( obj%nptrs, dom%mdomega( obj%omegano( ii ) )%ptr%nptrs )
  END DO

  CALL RemoveDuplicates( obj%nptrs )

  obj%tnodes = SIZE( obj%nptrs )
  maxnptrs = maxval( obj%nptrs )
  minnptrs = minval( obj%nptrs )
  IF( ALLOCATED( obj%local_nptrs ) ) DEALLOCATE( obj%local_nptrs )
  ALLOCATE( obj%local_nptrs( maxNptrs ) )
  obj%local_nptrs = 0
  DO ii = 1, obj%tnodes
    obj%local_nptrs( obj%nptrs( ii ) ) = ii
  END DO

  obj%tElements=0
  DO ii=1,SIZE(omegano)
    obj%tElements=obj%tElements+dom%omega(omegano(ii))%ptr%size()
  END DO

END PROCEDURE kernel_setDomain

!----------------------------------------------------------------------------
!                                                              setAlgorithm
!----------------------------------------------------------------------------

MODULE PROCEDURE kernel_setalgo
END PROCEDURE kernel_setalgo

!----------------------------------------------------------------------------
!                                                            setLinearSolver
!----------------------------------------------------------------------------

MODULE PROCEDURE kernel_setlinsol
  IF( PRESENT( name ) ) THEN
    obj%solvername=name
  ELSE
    obj%solvername=lis_cg
  END IF

  IF( PRESENT( maxiter ) ) THEN
    obj%maxiter=maxiter
  ELSE
    obj % maxIter = 0.2 * obj % tdof * SIZE( obj % dom % nodes, 2 )
  END IF

  IF( PRESENT( tol ) ) then
    Obj % tol = tol
  ELSE
    obj % tol = 1.0E-5
  END IF

  IF( PRESENT( ipar ) ) obj%lis_ipar=ipar
  IF( PRESENT( fpar ) ) obj%lis_fpar=fpar

  ALLOCATE( sparsekit_:: obj%linsol )
  CALL obj % linsol % initiate( solvername = obj % solverName, &
    & maxIter = obj % maxiter, tol = obj % tol, &
    & fpar = obj%lis_fpar, ipar = obj%lis_ipar )
END PROCEDURE kernel_setlinsol

!----------------------------------------------------------------------------
!                                                            setPrecondition
!----------------------------------------------------------------------------

MODULE PROCEDURE kernel_setprecond

  IF( PRESENT( name ) ) THEN
    obj%precondType = name
  ELSE
    obj%precondType = p_ilud
    obj%precond_fpar(1) = 1.0E-3
  END IF

  IF( PRESENT( ipar ) ) THEN
    obj%precond_ipar(1:SIZE(ipar))=ipar(:)
  END IF

  IF( PRESENT( fpar ) ) THEN
    obj%precond_fpar(1:SIZE(fpar))=fpar(:)
  END IF

  ! CALL display( '  setting precondition')
  CALL obj%linsol%setPrecondition( precondtype = obj%precondtype, &
    & fpar = obj%precond_fpar, ipar = obj%precond_ipar )

END PROCEDURE kernel_setprecond

!----------------------------------------------------------------------------
!                                                          setTangentMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE kernel_settanmat
  INTEGER( I4B ) :: ii

  ALLOCATE( obj%tanmat )
  ! CALL display( '  initiating tangent matrix' )
  CALL initiate( obj = obj % tanmat, tdof = obj % tdof, &
    & tnodes = [ Obj % tNodes ] )

  ! CALL display( '  setting sparsity in tangent matrix' )

  DO ii = 1, SIZE( obj%omegano )

    CALL setSparsity( &
      & Obj = obj%dom%mdOmega( obj%omegano( ii ) ) % ptr, &
      & MeshObj = obj%dom%omega( obj%omegano( ii ) ) % ptr, &
      & mat = obj%tanmat, &
      & map = obj%local_nptrs )

  END DO

  ! CALL display( '  setting sparsity in tangent matrix' )
  CALL setSparsity( obj % tanmat )

  ! CALL display( '  setting sparsity in linear solver')
  CALL obj % linsol % setsparsity( obj % tanmat )
END PROCEDURE kernel_settanmat

!----------------------------------------------------------------------------
!                                                                 Finalize
!----------------------------------------------------------------------------

MODULE PROCEDURE kernel_dealloc
END PROCEDURE kernel_dealloc

!----------------------------------------------------------------------------
!                                                            getMeshQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE kernel_mesh_quality
  INTEGER( I4B ) :: ii, jj, kk, a, b, measure0

  IF( PRESENT( measure ) ) THEN
    measure0 = measure
  ELSE
    measure0 = QualityMeasure%default
  END IF

  ASSOCIATE(omegano=>obj%omegano, dom=>obj%dom)
    IF( PRESENT( q ) ) THEN
      CALL Reallocate(q, Obj%tElements)
    ELSE
      CALL Reallocate(obj%meshq, Obj%tElements)
    END IF

    a=0; b=0

    IF( PRESENT(nodes) ) THEN
      IF( PRESENT( q ) ) THEN
        DO ii=1,SIZE(omegano)
          jj=omegano(ii)
          a=b+1; b=b+dom%omega(jj)%ptr%size()
          q(a:b) = &
            & dom%mdomega(jj)%ptr%meshQuality(&
            & meshobj=dom%omega(jj)%ptr, &
            & nodes=nodes, Measure=measure0)
        END DO
      ELSE
        DO ii=1,SIZE(omegano)
          jj=omegano(ii)
          a=b+1; b=b+dom%omega(jj)%ptr%size()
          obj%meshq(a:b) = &
            & dom%mdomega(jj)%ptr%meshQuality(&
            & meshobj=dom%omega(jj)%ptr, &
            & nodes=nodes, Measure=measure0)
        END DO
      END IF
    ELSE
      IF( PRESENT( q ) ) THEN
        DO ii=1,SIZE(omegano)
          jj=omegano(ii)
          a=b+1; b=b+dom%omega(jj)%ptr%size()
          q(a:b) = &
            & dom%mdomega(jj)%ptr%meshQuality(&
            & meshobj=dom%omega(jj)%ptr, &
            & nodes=dom%nodes, Measure=measure0)
        END DO
      ELSE
        DO ii=1,SIZE(omegano)
          jj=omegano(ii)
          a=b+1; b=b+dom%omega(jj)%ptr%size()
          obj%meshq(a:b) &
            &=dom%mdomega(jj)%ptr%meshQuality(&
            & meshobj=dom%omega(jj)%ptr, &
            & nodes=dom%nodes, Measure=measure0)
        END DO
      END IF
    END IF

    qmin = 100000; qmax = 0; qavg = 0

    IF( PRESENT( q ) ) THEN
      DO jj=1,size(q)
        qmin = MIN( qmin, q(jj))
        qmax = MAX( qmax, q(jj))
        qavg = qavg+q(jj)
      END DO
      qavg = qavg/size(q)
    ELSE
      DO jj=1,size(obj%meshq)
        qmin = MIN( qmin, obj%meshq(jj))
        qmax = MAX( qmax, obj%meshq(jj))
        qavg = qavg+obj%meshq(jj)
      END DO
      qavg = qavg/size(obj%meshq)
    END IF
  END ASSOCIATE
END PROCEDURE kernel_mesh_quality

!----------------------------------------------------------------------------
!                                                     setMaterialProperties
!----------------------------------------------------------------------------

MODULE PROCEDURE set_total_materials
  Obj%tMaterials=tMaterials
END PROCEDURE set_total_materials

!----------------------------------------------------------------------------
!                                                               setThermCond
!----------------------------------------------------------------------------

MODULE PROCEDURE set_material
  INTEGER( I4B ) :: ii

  IF( SIZE( materialNo ) .NE. SIZE( Obj%OmegaNo ) ) THEN
    CALL Display( "ERROR:: in ", __FILE__ )
    CALL Display( __LINE__, "    Line number :: " )
    CALL Display( "        set_material()")
    CALL Display( "          SIZE( materialNo ) should be SIZE(Obj%omegaNo)")
    STOP
  END IF

  CALL Reallocate( Obj%OmegaNoToMaterials, SIZE(materialNo) )

  DO ii = 1, SIZE( materialNo )
    Obj%OmegaNoToMaterials( ii ) = materialNo( ii )
  END DO

END PROCEDURE set_material

END SUBMODULE Methods