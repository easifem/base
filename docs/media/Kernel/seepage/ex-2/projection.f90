
!> authors: Dr. Vikas Sharma
!
! This program projects nodal values from one mesh to another mesh
! Currently method for triangular elements
! always use local_nptrs to access nodal values

program main
use easifem
use h5fortran
implicit none

! user supply
INTEGER( I4B ), PARAMETER :: tdof=1, nsd=2, maxIter=10000, lis_ipar(2)=0
INTEGER( I4B ), PARAMETER :: spaceCompo=-1, timecompo=1
INTEGER( I4B ), PARAMETER :: omegano_old(2)=[1,2],omegano_new(2)=[1,2]
REAL( DFP ), PARAMETER :: tol=1.0E-8, lis_fpar(2) = 0.0
CHARACTER(LEN=200), PARAMETER :: old_state_file="./state_old/seepage.hdf5"
CHARACTER(LEN=200), PARAMETER :: old_state_dataset="/P0"
CHARACTER(LEN=200), PARAMETER :: new_state_file="./state_new/seepage.hdf5"
CHARACTER(LEN=200), PARAMETER :: new_state_dataset="/P0"

CHARACTER( LEN=100 ), PARAMETER :: prefix="PROJECTION "

! internal variables
INTEGER( I4B ) :: ii, tnodes_old, tnodes_new, iel, telem, ips, nips
INTEGER( I4B ), ALLOCATABLE :: nptrs_old(:), nptrs_new(:), nptrs1(:)
INTEGER( I4B ), ALLOCATABLE :: nptrs2(:)
INTEGER( I4B ), ALLOCATABLE :: elemList(:)
INTEGER( I4B ), ALLOCATABLE :: local_nptrs_old(:), local_nptrs_new(:)

REAL( DFP ), ALLOCATABLE :: rhs( : ), x(:), xij(:,:), me(:,:), xieta_old(:)
REAL( DFP ), ALLOCATABLE :: rhs_e( :,: ), x_old( : ), xe_old(:)
REAL( DFP ) :: realval

type(hdf5_file) :: h5f
TYPE( gmsh_) :: gmsh_old, gmsh_new
TYPE( domain_ ) :: dom_old, dom_new
TYPE( DOF_ ) :: rhs_dof, x_dof, x_dof_old
TYPE( SparseMatrix_ ) :: tanmat
TYPE( Sparsekit_ ) :: linsol
TYPE(QuadraturePoint_) :: spaceQuad, spaceQuad_old
TYPE(ElemShapeData_) :: spacesd, spacesd_old
CLASS( Mesh_ ), POINTER :: meshobj_new, meshobj_old
CLASS( Element_ ), POINTER :: elem_new, elem_old

!----------------------------------------------------------------------------
CALL display( trim(prefix) // ' :: Initiating old gmsh object' )
!----------------------------------------------------------------------------
  ii = gmsh_old%initialize( NSD )
  ii = gmsh_old%open( './state_old/', 'mesh', '.msh' )
  tnodes_old = gmsh_old%model%mesh%totalnodes( )
  CALL gmsh_old%model%mesh%getElements( dom_old )

!----------------------------------------------------------------------------
CALL display( trim(prefix) // ' :: Initiating new gmsh object' )
!----------------------------------------------------------------------------
  ii = gmsh_new%initialize( NSD )
  ii = gmsh_new%open( './state_new/', 'mesh', '.msh' )
  tnodes_new = gmsh_new%model%mesh%totalnodes( )
  CALL gmsh_new%model%mesh%getElements( dom_new )

!----------------------------------------------------------------------------
CALL display( trim(prefix) // ' :: generating nptrs and local_nptrs' )
!----------------------------------------------------------------------------
  DO ii = 1, SIZE( omegano_old )
    CALL append(nptrs_old, dom_old%mdomega(omegano_old(ii))%ptr%nptrs)
  END DO
  CALL RemoveDuplicates( nptrs_old )
  ALLOCATE( local_nptrs_old( maxval( nptrs_old ) ) )
  local_nptrs_old = 0
  DO ii = 1, tnodes_old
    local_nptrs_old( nptrs_old( ii ) ) = ii
  END DO

  DO ii = 1, SIZE( omegano_new )
    CALL append(nptrs_new, dom_new%mdomega(omegano_new(ii))%ptr%nptrs)
  END DO
  CALL RemoveDuplicates( nptrs_new )
  ALLOCATE( local_nptrs_new( maxval( nptrs_new ) ) )
  local_nptrs_new = 0
  DO ii = 1, tnodes_new
    local_nptrs_new( nptrs_new( ii ) ) = ii
  END DO

!----------------------------------------------------------------------------
CALL display( trim(prefix) // ' :: making tangent matrix for problem' )
!----------------------------------------------------------------------------

  CALL initiate( obj = tanmat, tdof = tdof, tnodes = [tNodes_new] )
  DO ii = 1, SIZE( omegano_new )
    CALL setSparsity( &
      & Obj = dom_new%mdOmega( omegano_new( ii ) ) % ptr, &
      & meshobj = dom_new%omega( omegano_new( ii ) ) % ptr, &
      & mat = tanmat, &
      & map = local_nptrs_new )
  END DO
  CALL setSparsity( tanmat )

!----------------------------------------------------------------------------
CALL display( trim(prefix) // ' :: making linear solver for problem' )
!----------------------------------------------------------------------------
  CALL linsol%initiate( solvername = lis_cg, &
      & maxIter = maxIter, tol = tol, &
      & fpar = lis_fpar, ipar = lis_ipar )
  CALL linsol%setPrecondition( precondtype = p_none, fpar =[1.0d-3], ipar=[0])
  CALL linsol%setsparsity( tanmat )

!----------------------------------------------------------------------------
CALL display( trim(prefix) // ' :: making new dof' )
!----------------------------------------------------------------------------
  rhs_dof = dof( tNodes = [tNodes_new], Names = ['f'], &
    & SpaceCompo = [spaceCompo], TimeCompo = [timecompo], &
    & StorageFMT = Nodes_FMT  )
  x_dof = dof( tNodes = [tNodes_new], Names = ['x'], &
    & SpaceCompo = [spaceCompo], TimeCompo = [timecompo], &
    & StorageFMT = Nodes_FMT  )
  x_dof_old = dof( tNodes = [tNodes_old], Names = ['x'], &
    & SpaceCompo = [spaceCompo], TimeCompo = [timecompo], &
    & StorageFMT = Nodes_FMT  )
  CALL Initiate( rhs, rhs_dof )
  CALL Initiate( x, x_dof )
  CALL Initiate( x_old, x_dof_old)

!----------------------------------------------------------------------------
CALL display( trim(prefix) // ' :: reading old dof' )
!----------------------------------------------------------------------------
CALL h5f%initialize( TRIM(old_state_file), status='old',action='r')
CALL h5f%read(trim(old_state_dataset), x_old)
CALL  h5f%finalize()

!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  meshobj_new => NULL(); elem_new => NULL(); elem_old => NULL()
  DO ii = 1, SIZE(omegano_new)
    meshobj_new => dom_new%omega(omegano_new(ii))%ptr
    meshobj_old => dom_old%omega(omegano_old(ii))%ptr
    elem_new => meshobj_new%elem(1)%ptr
    spaceQuad = GaussLegendreQuadrature( refelem=elem_new%refelem, &
      & order=2*elem_new%refelem%order )
    spaceQuad_old = spaceQuad
    nips = SIZE(spaceQuad, 2)
    CALL initiate( obj=spacesd, quad=spacequad, &
      & refelem = elem_new%refelem, &
      & ContinuityType= TypeH1, &
      & InterpolType = TypeLagrangeInterpolation )
    telem = meshobj_new%size()
    DO iel=1, telem
      elem_new => meshobj_new%elem(iel)%ptr
      nptrs1 = elem_new%getnptrs()
      xij = dom_new%nodes(1:nsd, nptrs1)
      CALL setValue( obj=spacesd, val=xij, N=spacesd%N,&
        & dNdXi=spacesd%dNdXi )
      me=MassMatrix(Test=spacesd, Trial=spacesd, nCopy=tdof)

      CALL addcontribution( obj=tanmat, nptrs=local_nptrs_new(nptrs1), &
        & val=me, scale = 1.0_dfp, conversion = doftonodes )

      elemList = dom_old%mdOmega(omegano_old(ii))%ptr%findElement(&
        & meshobj=dom_old%omega(omegano_old(ii))%ptr, &
        & coord=spacesd%coord, nodes=dom_old%nodes )

      ! Currently valid for triangle element only
      ! find local_coordinates\

      DO ips = 1, nips
        elem_old => meshobj_old%elem(elemList(ips))%ptr
        nptrs2 = elem_old%getnptrs()
        xij = dom_old%nodes(1:nsd, nptrs2)
        spaceQuad_old%points(1:nsd, ips) = GET_LOCAL_COORDINATES( xij, &
          & spacesd%coord(:,ips) )
      END DO

      CALL initiate( &
        & obj=spacesd_old, quad=spacequad_old, &
        & refelem=elem_old%refelem, &
        & ContinuityType=TypeH1, &
        & InterpolType=TypeLagrangeInterpolation )

      CALL Reallocate(rhs_e, tdof, size(nptrs1))
      DO ips = 1, nips
        elem_old => meshobj_old%elem(elemList(ips))%ptr
        nptrs2 = elem_old%getnptrs()
        CALL getArrayValues( v=xe_old, Val=x_old, obj=x_dof_old, &
          & DOFNo=arange(1,tdof),  StorageFMT=Nodes_FMT, &
          & Nptrs=local_nptrs_old(nptrs2) )
        realval = spacesd%Js(ips)*spacesd%thickness(ips)*spacesd%ws(ips)
        xe_old=MATMUL(RESHAPE(xe_old, [tdof, size(nptrs2)]), spacesd_old%N(:,ips))
        rhs_e = rhs_e + realval * OUTERPROD( xe_old, spacesd%N( :, ips) )
      END DO

      ! add contribution to rhs
      CALL addcontribution(Vec=rhs,&
        & Obj=rhs_dof, Nptrs=local_nptrs_new(nptrs1), &
        & Val=RESHAPE(rhs_e, [size(rhs_e)]), &
        & Scale=1.0_dfp, conversion=[doftonodes])
    END DO
  END DO

!----------------------------------------------------------------------------
CALL display( trim(prefix) // ' :: solving linear system' )
!----------------------------------------------------------------------------
  CALL linsol%solve( rhs=rhs, sol=x )

!----------------------------------------------------------------------------
CALL display( trim(prefix) // ' :: printing data' )
!----------------------------------------------------------------------------

CALL h5f%initialize( TRIM(new_state_file), status='replace',action='w')
! CALL h5f%write_group(trim(new_state_dataset), x)

CALL h5f%write( trim(new_state_dataset), x )
! call h5f%writeattr(dname=trim(grp)//'P0', &
!   & attr='variable type', attrval='nodal')
! call h5f%writeattr(dname=trim(grp)//'P0', attr='rank', attrval='scalar')
! call h5f%writeattr(dname=trim(grp)//'P0', attr='tdof', attrval=1)
! call h5f%writeattr(dname=trim(grp)//'P0', attr='tNodes', attrval=obj%tNodes)

CALL  h5f%finalize()

call gmsh_new%model%mesh%writenodedata(x=x, Indx=[0], dofObj=x_dof, &
  & Name='x', nodes=dom_new%nodes, local_nptrs=local_nptrs_new)
call gmsh_old%model%mesh%writenodedata(x=x_old, Indx=[0], &
  & dofObj=x_dof_old, Name='x', nodes=dom_old%nodes, &
  & local_nptrs=local_nptrs_old)


CONTAINS
FUNCTION GET_LOCAL_COORDINATES(xij, x) RESULT(xi)
  REAL( DFP ), INTENT( IN ) :: xij( :, : ), x( : )
  REAL( DFP ), ALLOCATABLE :: xi( : )

  REAL( DFP ) :: J, x12, x13, y12, y13, A(2,2), rhs(2)

  x12=xij(1,1)-xij(1,2)
  x13=xij(1,1)-xij(1,3)
  y12=xij(2,1)-xij(2,2)
  y13=xij(2,1)-xij(2,3)
  J = x12*y13 - x13*y12
  A = RESHAPE( [-y13, y12, x13, -x12],[2,2] ) / J
  rhs( 1 ) = x( 1 ) - xij( 1, 1 )
  rhs( 2 ) = x( 2  ) - xij( 2, 1 )
  xi = MATMUL( A, rhs )
END FUNCTION
end program main