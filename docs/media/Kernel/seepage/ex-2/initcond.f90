program main
use easifem
use seepage_class
use h5fortran
implicit none

integer(i4b), parameter :: nsd=2
integer( i4b ), parameter :: BOTTOM=1, RIGHT1=2, RIGHT2=3, TOP_R=4, &
  & TOP_L=5, LEFT=6, MID=7
real( dfp ), parameter :: rho_w=1000.0_dfp
integer(i4b) :: ii, tnodes
real(dfp), allocatable :: damprops(:,:)
type(domain_) :: dom
type(gmsh_) :: gmsh
type( seepage_ ) :: seep
type(file_) :: afile
type(hdf5_file) :: h5f

call display( "DAM MATERIAL PROPERTIES" )
allocate( damprops( 2, 2 ) )
damprops( :, 1 ) = [1.E-3, 1.0E-10] !! muw, ks
damprops( :, 2 ) = [1.E-3, 1.0E-10] !! muw, ks
call display( damprops, 'DAMPROPS' )

ii = gmsh % initialize( NSD )
ii = gmsh % open( "./", "mesh", ".msh" )
tnodes = gmsh%model%mesh%totalnodes( )

call display('INITIATING DOMAIN')
call gmsh % model % mesh % getElements( dom )

call display( "INITIATING SEEPAGE KERNEL" )
call seep%initiate( NSD=NSD )
call seep%setdomain(dom=dom, omegano=[1,2])
call seep%setMaterialProperties(matprops=damprops)
call seep%setAlgorithm(mainOption=[seep%static])
call seep%setBoundary(DB = [RIGHT1, RIGHT2, TOP_L, TOP_R, LEFT])

call display( "APPLYING BOUNDARY CONDITION FOR SEEPAGE" )
seep%PressureFunc=>upstreamBC
call seep%applyDBC(tag=LEFT)
seep%PressureFunc=>downstreamBC
call seep%applyDBC(tag=RIGHT1)
call seep%applyDBC(tag=RIGHT2)

seep%PressureFunc=>freeSurfaceBC
call seep%applyDBC(tag=TOP_L)
call seep%applyDBC(tag=TOP_R)
call seep % assemble( )
call seep % solve( )
call seep%writedata(path="./InitCondition/", filename="seepage",&
  & extension=".vtu", indx=[0] )

call h5f%initialize( "./InitCondition/seepage.hdf5", status='replace', &
  & action='w')
call h5f%write( "/P0", seep%nodalVar(seep%p)%val )
call h5f%writeattr(dname='/P0', attr='variable type', attrval='nodal')
call h5f%writeattr(dname='/P0', attr='rank', attrval='scalar')
call h5f%writeattr(dname='/P0', attr='tdof', attrval=1)
call h5f%writeattr(dname='/P0', attr='tNodes', attrval=seep%tNodes)
call h5f%writeattr(dname='/P0', attr='nsd', attrval=seep%nsd)
call h5f%writeattr(dname='/P0', attr='nnt', attrval=seep%nnt)
call h5f%writeattr(dname='/P0', attr='tdof', attrval=seep%tdof)
call h5f%writeattr(dname='/P0', attr='tn', attrval=seep%tn)
call h5f%writeattr(dname='/P0', attr='dt', attrval=seep%dt)
call h5f%writeattr(dname='/P0', attr='its0', attrval=0)
call h5f%finalize()

contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION upstreamBC( x ) RESULT( Ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: x( : )
  REAL( DFP ) :: Ans
  Ans = rho_w*9.81*10.0_dfp
END FUNCTION upstreamBC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION downstreamBC( x ) RESULT( Ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: x( : )
  REAL( DFP ) :: Ans
  Ans = rho_w*9.81*10.0_dfp
END FUNCTION downstreamBC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION freeSurfaceBC( x ) RESULT( Ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: x( : )
  REAL( DFP ) :: Ans
  Ans = rho_w*9.81*x(2)
END FUNCTION freeSurfaceBC

end program main