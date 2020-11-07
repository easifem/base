type( gmsh_ ) :: gmsh, newgmsh
type( vtk_ ) :: vtu
type( domain_ ) :: dom
real( dfp ), allocatable :: meshprops( :, : ), damprops( :, : )
type( movingmesh_ ) :: meshMove, meshMove_wbc
type( seepage_ ) :: seep
type( file_ ) :: afile

real( dfp )    :: rval( 10 )
integer( i4b ) :: ii, jj, kk, ll
integer( i4b ) :: its
integer( i4b ) :: tnodes
integer( i4b ), parameter :: NSD = 2
integer( i4b ), parameter :: NNT = 2

real( dfp ), parameter :: rho_w=1000.0_dfp
integer( i4b ) :: its0=0
integer( i4b ), parameter :: NTS = 100
real( dfp ) :: dt = 100
logical(lgt), parameter :: noslip = .false.
real( dfp ), parameter :: reltol=1.0E-4
integer( i4b ), parameter :: MAXITER = 20
real( dfp ), parameter :: MESH_QUALITY_fa = 3.0
real( dfp ), parameter :: MESH_QUALITY_far = 3.0
integer( i4b ), parameter :: BOTTOM=1, RIGHT1=2, RIGHT2=3, TOP_R=4, &
  & TOP_L=5, LEFT=6, MID=7
CHARACTER( LEN=100 ), PARAMETER :: path = "./InitCondition/"
CHARACTER( LEN=100 ), PARAMETER :: filename = "seepage"
CHARACTER( LEN=100 ), PARAMETER :: extension = ".hdf5"

real( dfp ) :: VertPos( 0:NTS )
real( dfp ) :: qmin( 0:NTS )
real( dfp ) :: qmax( 0:NTS )
real( dfp ) :: qavg( 0:NTS )
integer( i4b ) :: iters( 0:NTS )
real( dfp ) :: Angle( 0:NTS ) = 0.0_DFP
real( dfp ) :: times( 0:NTS )
real( dfp ), allocatable :: vn( : ), v(:)
integer( i4b ), allocatable :: local_nptrs(:)

  !! maximum number of iteration for pressure convergence
integer( i4b ), allocatable :: nptrs( : )
type( string ), allocatable :: facetmesh( :, : )
logical(lgt) :: isconverge
logical(lgt) :: isRestart

! Material #1
call display( "DAM MATERIAL PROPERTIES" )
allocate( damprops( 4, 2 ) )
rval( 1:4 ) = [1.0E-5, 1.0E3, 1.E-3, 1.0E-10] !! Sw, rhow, muw, ks
damprops( :, 1 ) = rval( 1:4 )
rval( 1:4 ) = [1.0E-5, 1.0E3, 1.E-3, 1.0E-10] !! Sw, rhow, muw, ks
damprops( :, 2 ) = rval( 1:4 )
call display( damprops, 'DAMPROPS' )

!---------------------------------------------------------------------------
!! Material #1
call display( "MESH MATERIAL PROPERTIES" )
rval( 1:3 ) = [1.0, 0.3, 1.0] !! E Nu Xi
rval( 4 ) = &
  & rval( 1 )*rval( 2 ) / ( 1.0 + rval( 2 ) ) / ( 1.0 - 2.0 * rval( 2 ) )
rval( 5 ) = rval( 1 ) / 2.0 / ( 1.0 + rval( 2 ) )
allocate( meshprops( 3, 2 ) )
meshprops( 1, 1 ) = rval( 4 ) ! lam
meshprops( 2, 1 ) = rval( 5 ) ! mu
meshprops( 3, 1 ) = rval( 3 ) ! xi
! domain#2
rval( 1:3 ) = [1.0, 0.3, 1.0] !! E Nu Xi
rval( 4 ) = &
  & rval( 1 )*rval( 2 ) / ( 1.0 + rval( 2 ) ) / ( 1.0 - 2.0 * rval( 2 ) )
rval( 5 ) = rval( 1 ) / 2.0 / ( 1.0 + rval( 2 ) )
meshprops( 1, 2 ) = rval( 4 ) ! lam
meshprops( 2, 2 ) = rval( 5 ) ! mu
meshprops( 3, 2 ) = rval( 3 ) ! xi

call display( meshprops, 'MATPROPS' )