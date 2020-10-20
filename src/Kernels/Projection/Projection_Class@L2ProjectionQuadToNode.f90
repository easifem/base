SUBMODULE( Projection_Class ) L2ProjectionQuadToNode
IMPLICIT NONE
CONTAINS

MODULE PROCEDURE l2_project_quad_to_node
  SELECT CASE( algoType )
  CASE(SPACETIME_EXPLICIT_PARAM)
    IF( SIZE(quadVar) .NE. SIZE(omegano) ) THEN
      CALL Display("Projection_Class@L2ProjectionQuadToNode.f90")
      CALL Display("  l2_project_quad_to_node()")
      CALL Display("  SPACETIME_EXPLICIT_PARAM not supported")
      STOP
    END IF

  CASE(SPACETIME_IMPLICIT_PARAM)
    IF( SIZE(quadVar) .NE. SIZE(omegano) ) THEN
      CALL Display("Projection_Class@L2ProjectionQuadToNode.f90")
      CALL Display("  l2_project_quad_to_node()")
      CALL Display("  SPACETIME_IMPLICIT_PARAM not supported")
      STOP
    END IF

  CASE(SPACE_EXPLICIT_PARAM)
    CALL l2_project_quad_to_node_space_explicit( Obj, omega, mdomega,omegano,&
    & nodes, quadVar, nodalVar, nodalDOF, rhs, rhsDOF, local_nptrs )

  CASE(SPACE_IMPLICIT_PARAM)
    IF( SIZE(quadVar) .NE. SIZE(omegano) ) THEN
      CALL Display("Projection_Class@L2ProjectionQuadToNode.f90")
      CALL Display("  l2_project_quad_to_node()")
      CALL Display("  SPACE_IMPLICIT_PARAM not supported")
      STOP
    END IF
  END SELECT
END PROCEDURE l2_project_quad_to_node
!----------------------------------------------------------------------------
!                                                               L2Projection
!----------------------------------------------------------------------------

SUBROUTINE l2_project_quad_to_node_space_explicit(Obj, omega, mdomega, &
  & omegano, nodes, quadVar, nodalVar, nodalDOF, rhs, rhsDOF, local_nptrs)
  CLASS( Projection_ ), INTENT( INOUT) :: Obj
  CLASS( MeshPointer_ ), INTENT( INOUT) :: omega(:)
  CLASS( MeshDataPointer_ ), INTENT( INOUT) :: mdomega(:)
  INTEGER( I4B ), INTENT( IN ) :: omegano(:)
  REAL( DFP ), INTENT( IN ) :: nodes(:,:)
  TYPE( QuadratureVariablesPointer_ ), INTENT( IN ) :: quadVar(:)
  REAL( DFP ), INTENT( INOUT ) :: nodalVar(:)
  TYPE( DOF_ ), INTENT( IN ) :: nodalDOF
  REAL( DFP ), INTENT( INOUT ) :: rhs(:)
  TYPE( DOF_ ), INTENT( IN ) :: rhsDOF
  INTEGER( I4B ), INTENT( IN ) :: local_nptrs(:)

  !> define internal variables
  INTEGER( I4B ) :: ii, n, telem, iel, ncomp, ips, nsd
  CLASS( Mesh_ ), POINTER :: meshobj
  CLASS( Element_ ), POINTER :: elem
  CLASS( QuadratureVariables_ ), POINTER :: qvar
  TYPE(QuadraturePoint_) :: spaceQuad
  TYPE(ElemShapeData_) :: spacesd
  INTEGER( I4B ), ALLOCATABLE :: nptrs( : )
  INTEGER( I4B ), ALLOCATABLE :: lp( : )
  TYPE(RealMatrix_), ALLOCATABLE :: rmat(:)
  TYPE(RealVector_), ALLOCATABLE :: rvec(:)

  !> main program
  IF( SIZE(quadVar) .NE. SIZE(omegano) ) THEN
    CALL Display("Projection_Class@L2ProjectionQuadToNode.f90")
    CALL Display("  l2_project_quad_to_node_space_explicit()")
    CALL Display("  SIZE(quadVar) .NE. SIZE(omegano) )")
    STOP
  END IF

  ncomp = quadVar(1)%ptr%tprop
  DO ii=1,SIZE(quadVar)
    qvar=>quadvar(ii)%ptr
    IF( qvar%tprop .NE. ncomp ) THEN
      CALL Display("Projection_Class@L2ProjectionQuadToNode.f90")
      CALL Display("  l2_project_quad_to_node_space_explicit()")
      CALL Display("  ALL(quadVar(:)%tprop .NE. ncomp )")
      STOP
    END IF
  END DO
  qvar => NULL()

  n = SIZE(local_nptrs)
  IF( SIZE(nodalVar) .NE. n*ncomp ) THEN
    CALL Display("Projection_Class@L2ProjectionQuadToNode.f90")
    CALL Display("  l2_project_quad_to_node_space_explicit()")
    CALL Display("  size(nodalVar) should be equal to ncomp*size(local_nptrs")
    STOP
  END IF

  rhs=0.0_DFP
  meshobj=>null()
  elem=>null()
  ALLOCATE(rmat(4), rvec(2))
  DO ii = 1, SIZE(omegano)
    meshobj => omega(omegano(ii))%ptr
    nsd = meshobj%nsd
    elem => meshobj%elem(1)%ptr
    telem = meshobj%size()
    spaceQuad = GaussLegendreQuadrature( refelem=elem%refelem, &
      & order=2*elem%refelem%order )
    ! CALL display( '  Generating shape data for space element' )
    CALL initiate( obj = spacesd, quad = spacequad, &
      & refelem = elem % refelem, ContinuityType=TypeH1, &
      & InterpolType = TypeLagrangeInterpolation )

    qvar => quadVar(ii)%ptr

    DO iel=1,telem
      elem => meshobj%elem(iel)%ptr
      nptrs = .nptrs. elem
      lp = local_nptrs(nptrs)
      rmat(1)%val=nodes( 1:nsd, nptrs )
      CALL setValue(obj=spacesd, val=rmat(1)%val, N=spacesd%N, &
        & dNdXi=spacesd%dNdXi)
      rmat(2)%val=MassMatrix(Test=spacesd, Trial=spacesd, nCopy=ncomp)

      CALL addcontribution(Vec=nodalVar,&
        & Obj=nodalDOF, Nptrs=lp, &
        & Val=SUM(rmat(2)%val,2), Scale=1.0_dfp, conversion=[dofToNodes])

      rmat(3)%val=qvar%ArrayValues(elemnum=iel)
      CALL Reallocate(rmat(4)%val, ncomp, SIZE(spacesd%N,1))
      rvec(1)%val = spacesd%Js*spacesd%Ws*spacesd%thickness
      DO ips=1,SIZE(spacesd%N,2)
        rmat(4)%val = rmat(4)%val + rvec(1)%val(ips) &
          & * OUTERPROD(rmat(3)%val(:,ips), spacesd%N(:,ips))
      END DO
      rvec(2)%val = RESHAPE(rmat(4)%val, [SIZE(rmat(4)%val)])
      CALL addcontribution(Vec=rhs,&
        & Obj=rhsDOF, Nptrs=lp, &
        & Val=rvec(2)%val, Scale=1.0_dfp, conversion=[dofToNodes])
    END DO
  END DO

  DO ii=1,SIZE(rhs)
    nodalVar(ii)=rhs(ii)/nodalVar(ii)
  END DO

  NULLIFY( meshobj, elem, qvar )
  CALL DeallocateData(spaceQuad)
  CALL DeallocateData(spacesd)
  DEALLOCATE(nptrs,lp,rmat,rvec)
END SUBROUTINE l2_project_quad_to_node_space_explicit

END SUBMODULE L2ProjectionQuadToNode