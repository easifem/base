SUBMODULE(MovingMesh_Class)Constructor
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                      setMaterialProperties
!----------------------------------------------------------------------------

MODULE PROCEDURE set_matprops
  obj%matprops = matprops
END PROCEDURE set_matprops

!----------------------------------------------------------------------------
!                                                                 setBoundary
!----------------------------------------------------------------------------

MODULE PROCEDURE mmt_setbc
  INTEGER( I4B ), ALLOCATABLE :: nptrs(:)
  INTEGER( I4B ) :: ii, kk, jj, a, b

  ALLOCATE(obj%db(4), obj%dbcinfo(4))
  obj%dbcinfo=0

  IF( PRESENT( xdb ) ) then
    IF( .not. ALLOCATED( obj%dom%boundary ) ) then
      CALL display( "ERROR:: MovingMesh_Class@Methods.f90" )
      CALL display( "        setBoundary()" )
      CALL display( "          obj%dom%boundary not allocated" )
      stop
    END IF

    IF( any( xdb .gt. SIZE( obj%dom%boundary ) ) ) then
      CALL display( "ERROR:: MovingMesh_Class@Methods.f90" )
      CALL display( "        setBoundary()" )
      CALL display( "          bound error due to xdb" )
      stop
    END IF

    DO ii = 1, SIZE( xdb )
      IF( .not. associated( obj%dom%boundary( xdb( ii ) ) % ptr ) ) then
        CALL display( "ERROR:: MovingMesh_Class@Methods.f90" )
        CALL display( "        setBoundary()" )
        CALL display( "          some dom % omega are not associated" )
        stop
      END IF
    END DO

    ! CALL display( '  Dirichlet boundary in x-direction found' )
  ! DO ii = 1, size( xdb )
  ! CALL display( '    boundary( '// trim( int2str( xdb( ii ) ) ) // ' ):: '&
  !   & // trim( obj%dom%boundary_name( xdb( ii ) ) ) )
  ! END DO
    obj % db(1) = intvector(xdb)
    obj % dbcinfo( 1 ) = 1
  END IF

  IF( PRESENT( ydb ) ) then
    IF( .not. ALLOCATED( obj%dom%boundary ) ) then
      CALL display( "ERROR:: MovingMesh_Class@Methods.f90" )
      CALL display( "        setBoundary()" )
      CALL display( "          obj%dom%boundary not allocated" )
      stop
    END IF

    IF( any( ydb .gt. SIZE( obj%dom%boundary ) ) ) then
      CALL display( "ERROR:: MovingMesh_Class@Methods.f90" )
      CALL display( "        setBoundary()" )
      CALL display( "          bound error due to ydb" )
      stop
    END IF

    DO ii = 1, SIZE( ydb )
      IF( .not. associated( obj%dom%boundary( ydb( ii ) ) % ptr ) ) then
        CALL display( "ERROR:: MovingMesh_Class@Methods.f90" )
        CALL display( "        setBoundary()" )
        CALL display( "          some dom % omega are not associated" )
        stop
      END IF
    END DO

  ! CALL display( '  Dirichlet boundary in x-direction found' )
  ! DO ii = 1, size( ydb )
  ! CALL display( '    boundary( '// trim( int2str( ydb( ii ) ) ) // ' ):: '&
  !   & // trim( obj%dom%boundary_name( ydb( ii ) ) ) )
  ! END DO

    obj % db(2) = intvector(ydb)
    obj % dbcinfo( 2 ) = 2
  END IF

  IF( PRESENT( zdb ) ) then
    IF( .not. ALLOCATED( obj%dom%boundary ) ) then
      CALL display( "ERROR:: MovingMesh_Class@Methods.f90" )
      CALL display( "        setBoundary()" )
      CALL display( "          obj%dom%boundary not allocated" )
      stop
    END IF

    IF( any( zdb .gt. SIZE( obj%dom%boundary ) ) ) then
      CALL display( "ERROR:: MovingMesh_Class@Methods.f90" )
      CALL display( "        setBoundary()" )
      CALL display( "          bound error due to zdb" )
      stop
    END IF

    DO ii = 1, SIZE( zdb )
      IF( .not. associated( obj%dom%boundary( zdb( ii ) ) % ptr ) ) then
        CALL display( "ERROR:: MovingMesh_Class@Methods.f90" )
        CALL display( "        setBoundary()" )
        CALL display( "          some dom % omega are not associated" )
        stop
      END IF
    END DO

  ! CALL display( '  Dirichlet boundary in x-direction found' )
  ! DO ii = 1, size( zdb )
  ! CALL display( '    boundary( '// trim( int2str( zdb( ii ) ) ) // ' ):: '&
  !   & // trim( obj%dom%boundary_name( zdb( ii ) ) ) )
  ! END DO

    obj % db(3) = intvector(zdb)
    obj % dbcinfo( 3 ) = 3
  END IF

  IF( PRESENT( mb ) ) then
    IF( .not. allocated( obj%dom%boundary ) ) then
      CALL display( "ERROR:: MovingMesh_Class@Methods.f90" )
      CALL display( "        setBoundary()" )
      CALL display( "          dom % boundary not allocated" )
      stop
    END IF

    IF( any( mb .gt. SIZE( obj%dom%boundary ) ) ) then
      CALL display( "ERROR:: MovingMesh_Class@Methods.f90" )
      CALL display( "        setBoundary()" )
      CALL display( "          bound error due to mb" )
      stop
    END IF

    DO ii = 1, SIZE( mb )
      IF( .not. associated( obj%dom%boundary( mb( ii ) ) % ptr ) ) then
        CALL display( "ERROR:: MovingMesh_Class@Methods.f90" )
        CALL display( "        setBoundary()" )
        CALL display( "          some dom % omega are not associated" )
        stop
      END IF
    END DO

  ! CALL display( '  Moving boundary found' )
  ! DO ii = 1, size( mb )
  ! CALL display( '    boundary( '// trim( int2str( mb( ii ) ) ) // ' ):: '&
  !   & // trim( obj % dom % boundary_name( mb( ii ) ) ) )
  ! END DO
    obj % db(4) = intvector(mb)
    obj % dbcinfo( 4 ) = 4
    obj % solverName = lis_bicgstab
    ! IF( ASSOCIATED( obj%linsol ) ) DEALLOCATE( obj%linsol )
    ! ALLOCATE( sparsekit_:: obj%linsol )
    ! CALL obj%linsol%initiate( solvername = obj % solverName, &
    !   & maxIter = obj % maxiter, tol = obj % tol, &
    !   & fpar = obj%lis_fpar, ipar = obj%lis_ipar )

  END IF

  ! call display( '  allocating obj%intvec(6)')
  ! call display( '    obj%intvec(1):: xDB nptrs')
  ! call display( '    obj%intvec(2):: yDB nptrs')
  ! call display( '    obj%intvec(3):: zDB nptrs')

  IF( allocated( obj % intvec ) ) deallocate( obj % intvec )
  allocate( obj % intvec( 3 ) )
  !! 1 = x
  !! 2 = y
  !! 3 = z

  IF( obj % dbcinfo( 1 ) .ne. 0 ) then
    ! CALL display( "  Dirichlet BC in x-direction found" )
    ! CALL display( "    Getting Nptrs information in xDBNptrs" )
    DO ii = 1, SIZE( xdb )
      nptrs=obj%dom%mdboundary(xdb(ii))%ptr%nptrs
      CALL append( obj % intvec( 1 ), obj % local_nptrs( nptrs ) )
    END DO
    CALL RemoveDuplicates( obj % intvec( 1 ) )
  END IF

  IF( obj % dbcinfo( 2 ) .ne. 0 ) then

    ! CALL display( "  Dirichlet BC in y-direction found")
    ! CALL display( "    Getting Nptrs information in yDBNptrs")

    DO ii = 1, SIZE( ydb )
      nptrs=obj%dom%mdboundary(ydb(ii))%ptr%nptrs
      CALL append( obj % intvec( 2 ), obj % local_nptrs( nptrs ) )
    END DO

    CALL RemoveDuplicates( obj % intvec( 2 ) )

  END IF

  IF( obj % dbcinfo( 3 ) .ne. 0 ) then
    ! CALL display( "  Dirichlet BC in z-direction found")
    ! CALL display( "    Getting Nptrs information in yDBNptrs")
    DO ii = 1, SIZE( zdb )
      nptrs=obj%dom%mdboundary(zdb(ii))%ptr%nptrs
      CALL append( obj % intvec( 3 ), obj % local_nptrs( nptrs ) )
    END DO
    CALL RemoveDuplicates( obj % intvec( 3 ) )
  END IF

  ! CALL display( "  Sending boundary condition info to linsol" )
  CALL obj % linsol % setDirichletBCNodes( &
    & nptrs = obj % intvec, dofs = Obj % dbcinfo( 1 : 3 ) )

  IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )

  IF( PRESENT(isNoSlip ) ) THEN
    Obj%isNoSlip=isNoSlip
  ELSE
    Obj%isNoSlip=.FALSE.
  END IF

END PROCEDURE mmt_setbc

!----------------------------------------------------------------------------
!                                                               setAlgorithm
!----------------------------------------------------------------------------

MODULE PROCEDURE mmt_setalgo
  INTEGER( I4B ) :: ii, jj, a, b


  IF( .NOT. PRESENT( mainOption ) ) THEN
    CALL display( "ERROR:: Seepage_Class@Constructor.f90" )
    CALL display( "        mmt_setalgo()" )
    CALL display( "          mainOption should be present" )
    stop
  END IF

  SELECT CASE( mainOption( 1 ) )
  CASE( opt_elasticity )
    CALL mmt_setdof_elasticity(Obj)
    CALL Obj%setLinearSolver(name=lis_bicg, tol=1.0d-8)
    CALL Obj%setPrecondition()
    CALL Obj%setTanmat()
    obj%applyInitCondition => mmt_apply_initcond_elasticity
    obj%applyUniformDisplacement => mmt_applyuniformdisp_elasticity
    obj%applyWeakDBC => apply_nitsche_elasticity
    obj%applyDBC => apply_dbc_elasticity
    obj%assemble=>mmt_assemble_elasticity
    obj%solve=>mmt_solve_elasticity
    obj%isConverged=>mmt_isconverg_elasticity
    obj%update=>mmt_update_elasticity
    obj%writedata => mmt_writedata_elasticity

    ! Compute mesh quality
    CALL obj%mesh_quality%initiate(tprop=6, tpoint=1, telem=Obj%telements)
    a=0; b=0
    DO ii=1,SIZE(obj%omegano)
      jj=obj%omegano(ii)
      a=b+1; b=b+obj%dom%omega(jj)%ptr%size()
      obj%mesh_quality%Val(1,1, a:b) = &
        & obj%dom%mdomega(jj)%ptr%meshQuality(&
        & meshobj=obj%dom%omega(jj)%ptr, &
        & nodes=obj%dom%nodes, Measure=QualityMeasure%area)
      obj%mesh_quality%Val(2,1, a:b) = &
        & obj%dom%mdomega(jj)%ptr%meshQuality(&
        & meshobj=obj%dom%omega(jj)%ptr, &
        & nodes=obj%dom%nodes, Measure=QualityMeasure%aspectRatio)
    END DO

  END SELECT

END PROCEDURE mmt_setalgo

!----------------------------------------------------------------------------
!                                                            getMeshQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE mmt_getMeshQuality
  INTEGER( I4B ) :: ii, jj, a, b

  a=0; b=0
  IF( PRESENT( nodes ) ) THEN

    DO ii=1,SIZE(obj%omegano)
      jj=obj%omegano(ii)
      a=b+1
      b=b+obj%dom%omega(jj)%ptr%size()

      obj%mesh_quality%Val(3,1, a:b) = &
        & obj%dom%mdomega(jj)%ptr%meshQuality( &
        & meshobj=obj%dom%omega(jj)%ptr, &
        & nodes=nodes, Measure=QualityMeasure%area)

      obj%mesh_quality%Val(4,1, a:b) = &
        & obj%dom%mdomega(jj)%ptr%meshQuality(&
        & meshobj=obj%dom%omega(jj)%ptr, &
        & nodes=nodes, Measure=QualityMeasure%aspectRatio)

    END DO

  ELSE

    DO ii=1,SIZE(obj%omegano)
      jj=obj%omegano(ii)
      a=b+1
      b=b+obj%dom%omega(jj)%ptr%size()

      obj%mesh_quality%Val(3,1, a:b) = &
        & obj%dom%mdomega(jj)%ptr%meshQuality( &
        & meshobj=obj%dom%omega(jj)%ptr, &
        & nodes=obj%dom%nodes, Measure=QualityMeasure%area)

      obj%mesh_quality%Val(4,1, a:b) = &
        & obj%dom%mdomega(jj)%ptr%meshQuality(&
        & meshobj=obj%dom%omega(jj)%ptr, &
        & nodes=obj%dom%nodes, Measure=QualityMeasure%aspectRatio)

    END DO
  END IF

  qmin = 0.0_DFP; qmax = 0.0_DFP; qavg = 0.0_DFP
  obj%fA=0.0_DFP; obj%fAR=0.0_DFP

  DO ii = 1, obj%telements
    obj%mesh_quality%Val(5, 1, ii) = &
      & ABS( LOG10(obj%mesh_quality%Val(3, 1, ii) &
      & / obj%mesh_quality%Val(1,1, ii))/LOG10(2.0_DFP) )
    !
    obj%mesh_quality%Val(6, 1, ii) = &
      & ABS( LOG10(obj%mesh_quality%Val(4, 1, ii) &
      & / obj%mesh_quality%Val(2,1, ii))/LOG10(2.0_DFP) )

    obj%fA = MAX(obj%fA, obj%mesh_quality%Val(5, 1, ii) )
    obj%fAR = MAX(obj%fAR, obj%mesh_quality%Val(6, 1, ii) )
  END DO

END PROCEDURE mmt_getMeshQuality

END SUBMODULE Constructor