SUBMODULE( Seepage_Class ) StaticMethods
IMPLICIT NONE

INTEGER( I4B ), ALLOCATABLE :: local_nptrs( : )

CONTAINS

!----------------------------------------------------------------------------
!                                                                     setDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE seepage_setdof_static
  INTEGER( I4B ) :: ii

  obj % tdof = 1
  ALLOCATE( obj%dof( 2 ), obj%nodalVar( 2 ) )

  obj % dof(1) = dof( tNodes = [ obj % tNodes ], Names = [ 'P' ], &
    & SpaceCompo = [ -1 ], TimeCompo = [ 1 ], StorageFMT = Nodes_FMT  )

  obj % dof(2) = dof( tNodes = [ Obj % tNodes ], Names = [ 'f' ], &
    & SpaceCompo = [ -1 ], TimeCompo = [ 1 ], StorageFMT = Nodes_FMT )

  DO ii=1, SIZE(obj%dof)
    CALL initiate( obj % nodalVar(ii), obj % dof(ii) )
  END DO
END PROCEDURE seepage_setdof_static

!----------------------------------------------------------------------------
!                                                                  ApplyDBC
!----------------------------------------------------------------------------

MODULE PROCEDURE seepage_apply_dbc_static
  INTEGER( I4B ) :: ii, jj, kk
  REAL( DFP ) :: x1(3)

  IF( PRESENT( Val ) ) THEN

    IF( SIZE( Val ) .EQ. 1 ) THEN
      DO ii=1,obj%dom%mdboundary(tag)%ptr%tNodes
        kk = obj%dom%mdboundary(tag)%ptr%nptrs(ii)
        jj = obj%local_nptrs(kk)
        obj%nodalVar(obj%p)%val( jj ) = Val(1)
      END DO
      RETURN
    END IF

    IF (PRESENT(local_nptrs)) THEN
      DO ii=1,obj%dom%mdboundary(tag)%ptr%tNodes
        kk = obj%dom%mdboundary(tag)%ptr%nptrs(ii)
        jj = obj%local_nptrs(kk)
        kk = local_nptrs(kk)
        obj%nodalVar(obj%p)%val( jj ) = Val(kk)
      END DO

    ELSE

      DO ii=1,obj%dom%mdboundary(tag)%ptr%tNodes
        kk = obj%dom%mdboundary(tag)%ptr%nptrs(ii)
        jj = obj%local_nptrs(kk)
        obj%nodalVar(obj%p)%val(jj) = Val(kk)
      END DO

    END IF

  ELSE
    DO ii=1,obj%dom%mdboundary(tag)%ptr%tNodes
      kk = obj%dom%mdboundary(tag)%ptr%nptrs(ii)
      jj = obj%local_nptrs(kk)
      x1(1:obj%nsd) = obj%dom%nodes(1:obj%nsd, kk)
      obj%nodalVar(obj%p)%val( jj ) = obj%PressureFunc(x1)
    END DO
  END IF

END PROCEDURE seepage_apply_dbc_static

!----------------------------------------------------------------------------
!                                                                   Assemble
!----------------------------------------------------------------------------

MODULE PROCEDURE seepage_assemble_static
  INTEGER( I4B ) :: ii,jj, telem, iel, a
  ! TYPE( QuadraturePoint_ ) :: spaceQuad
  ! TYPE( ElemShapeData_ ) :: spacesd
  ! REAL( DFP ) :: ks( 3, 3 )
  INTEGER( I4B ) :: nips, nipt, nns

  SELECT TYPE( obj )
  CLASS IS ( Seepage_ )
    meshobj => null()
    elem => null()
    ! CALL display( "  Resetting tangent matrix to zero" )
    CALL setValue( obj=obj%tanmat, val=0.0_dfp )

    obj%nodalVar(obj%rhs)%val = 0.0_DFP

    a = 0; ks=0.0_DFP

    DO ii=1,SIZE(obj%omegano)
      meshobj => obj%dom%omega(obj%omegano(ii))%ptr
      elem => meshobj%elem(1)%ptr
      telem = meshobj%size()
      spaceQuad = GaussLegendreQuadrature( refelem=elem%refelem, &
        & order=2*(elem%refelem%order-1) )
      nips = SIZE(spaceQuad, 2)

      CALL initiate( obj=spacesd, quad=spacequad, &
        & refelem = elem%refelem, &
        & ContinuityType= Obj % SpaceContinuity, &
        & InterpolType = Obj %  SpaceInterpol )

      ! start element loop
      nptrs = .Nptrs. elem
      nns = size(nptrs)

      DO iel = 1, telem
        a = a+1
        elem => meshobj%elem(iel)%ptr
        nptrs = .Nptrs. elem
        local_nptrs = obj%local_nptrs(nptrs)
        rmat(1)%val = obj%dom%nodes( 1:obj%nsd, nptrs )

        CALL setValue(obj=spacesd, val=rmat(1)%val, N=spacesd%N, &
          & dNdXi=spacesd%dNdXi)

        ks(1,1) = obj%ks%val(1, 1, a)/obj%mu_w
        ks(2,2) = obj%ks%val(2, 1, a)/obj%mu_w
        ks(3,3) = obj%ks%val(3, 1, a)/obj%mu_w

        diff_coeff  = QuadratureVariable( ks(1:obj%nsd, 1:obj%nsd), &
          & typefevariableMATRIX, typeFeVariableConstant )

        ! matprops = obj%matprops( :, elem%mat_type )
        rmat(2)%val = DiffusionMatrix(Test=spacesd, Trial=spacesd, &
          & K=diff_coeff)

        ! Adding contribution to tangent matrix
        !! Need to convert global nodes into local nodes
        CALL addcontribution( obj = obj % tanmat, nptrs=local_nptrs, &
          & val=rmat(2)%val, scale = 1.0_dfp, &
          & storageFMT = DOF_FMT )

      END DO
    END DO
  END SELECT

  NULLIFY( MeshObj, Elem )
  DEALLOCATE(local_nptrs)
END PROCEDURE seepage_assemble_static

!----------------------------------------------------------------------------
!                                                             AssembleTanmat
!----------------------------------------------------------------------------

MODULE PROCEDURE seepage_solve_static
  INTEGER( I4B ) :: ii, idof
  SELECT TYPE(Obj)
  CLASS IS( Seepage_ )

    CALL obj % linsol % setMatrix( obj%tanmat )
    CALL obj%linsol%solve( rhs=obj%nodalVar(obj%rhs)%val, &
      & sol=obj%nodalVar(obj%p)%val )

  END SELECT
END PROCEDURE seepage_solve_static

!----------------------------------------------------------------------------
!                                                                isConverged
!----------------------------------------------------------------------------

MODULE PROCEDURE seepage_isconverg_static
  Ans = .TRUE.
END PROCEDURE seepage_isconverg_static

!----------------------------------------------------------------------------
!                                                                    Update
!----------------------------------------------------------------------------

MODULE PROCEDURE seepage_update_static
END PROCEDURE seepage_update_static

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE seepage_writedata_static
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
    call vtu%writeNodeData( x=obj%nodalVar(obj%p)%val,&
      & dofobj=obj%dof(obj%p), prefix="_PiezoPressure" )
    call vtu % closeNodeData( )

    ! call vtu % startWriteElementData( )
    ! call vtu % stopWriteElementData( )
    call vtu % closePiece( )
    call vtu % Finalize( )

  END SELECT
END PROCEDURE seepage_writedata_static

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE StaticMethods