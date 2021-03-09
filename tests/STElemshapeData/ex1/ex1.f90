program main
  !! This program shows how to set local data in
  !! space-time element shape data

use easifem
implicit none

  !line order 1
  type( stelemshapedata_ ), allocatable :: obj( : )
  type( elemshapedata_ ) :: elemsd
  type( quadraturepoint_ ) :: quad
  class( referenceElement_ ), pointer :: refelem
  integer( i4b ) :: ii, NNT, nsd, nns
  real( dfp ) :: tiJ( 1, 2 )
  real( dfp ), allocatable :: xiJ(:,:), xiJa(:,:,:)

  ALLOCATE( ReferenceLine_ :: refelem )
  refelem = ReferenceLine( nsd = 1 )
    !! Note nsd should be 1 when making reference element for time domain

  !> higher order lagrange element can be obtained, however it works on
  ! static type only, therefore, we need to use select type statment.
  ! NNT denotes number of nodes in time
  NNT  = 2
  SELECT TYPE( refelem )
  TYPE IS ( ReferenceLine_  )
    refelem = LagrangeElement( refelem = refelem, order = NNT-1 )
  END SELECT

  quad = GaussLegendreQuadrature(refelem=refelem,order=1 )
  call display(quad, 'quadrature points :: ')

  !> Setting shape function for time
  ! This sets up T, dTdTheta, Jt, Wt, Theta
  ! tiJ denotes the nodal values of time
  tiJ(1,:) = [-1.0,1.0]
  call initiate( &
    & obj = elemsd, quad = quad, refelem = refelem, &
    & ContinuityType= typeH1, InterpolType = TypeLagrangeInterpolation, &
    & XiJ = tiJ )

  !> allocating stelemshape data
  ! This will extract time shape data info from elemsd and put it inside
  ! obj(i)%T, obj(i)%dTdTheta, obj(i)%Jt, obj(i)%Wt, obj(i)%Theta
  call initiate( obj = obj, elemsd = elemsd )

  !> Generating shape functions on space element
  ! Here we are selecting a triangular element
  nsd = 2; nns = 3
  ALLOCATE( ReferenceTriangle_ :: refelem )
  refelem = ReferenceTriangle( nsd = nsd )
  quad = GaussLegendreQuadrature( refelem=refelem, order = 1 )

  !> Setting local data of shape function in space
  ! This will set N, dNdXi, Ws, Quad
  do ii = 1, size( obj )
    call initiate( &
      & obj = obj( ii ), quad = quad, refelem = refelem, &
      & ContinuityType= typeH1, InterpolType = TypeLagrangeInterpolation )
  end do

  !> Setting the remaining data in obj, before data we need to construct
  ! space time element.
  allocate( xiJ( nsd, nns ), xiJa( nsd, nns, nnt ) )
  xiJ = reshape( [0,0,1,0,0,1], [nsd,nns] )
  do ii=1,nnt; xiJa(:,:,ii)=xiJ; end do
  do ii = 1, size(obj)
    call setValue(obj(ii), xiJa )
    call display( obj( ii ), "ii :: "// str( ii ) )
  end do

end program
