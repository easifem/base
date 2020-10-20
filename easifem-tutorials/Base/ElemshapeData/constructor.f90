program main
use easifem
implicit none

! !line order 1
! block
!   type( elemshapedata_ ) :: obj
!   type( quadraturepoint_ ) :: quad
!   type( referenceline_ ) :: refelem
!   integer( i4b ) :: nsd, order
  
!   nsd = 1; order = 1
!   refelem = ReferenceLine( nsd = nsd )
!   quad = GaussLegendreQuadrature( refelem = refelem, order = order )
!   call AllocateData( obj = obj, nsd = refelem % nsd, &
!     & xidim = refelem % xidimension, nns = 2, nips = 1 )
!   call initiate( obj = obj, quad = quad, refelem = refelem, &
!     & ContinuityType= typeH1, InterpolType = TypeLagrangeInterpolation )
!   call display( obj, "obj" )
! end block

! !line order 2
! block
!   type( elemshapedata_ ) :: obj
!   type( quadraturepoint_ ) :: quad
!   type( referenceline_ ) :: refelem
!   integer( i4b ) :: nsd, order
  
!   nsd = 1; order = 2
!   refelem = ReferenceLine( nsd = nsd )
!   quad = GaussLegendreQuadrature( refelem = refelem, order = order )
!   call AllocateData( obj = obj, nsd = refelem % nsd, &
!     & xidim = refelem % xidimension, nns = 2, nips = 1 )
!   call initiate( obj = obj, quad = quad, refelem = refelem, &
!     & ContinuityType= typeH1, InterpolType = TypeLagrangeInterpolation )
!   call display( obj, "obj" )
! end block

!triangle order 1
block
  type( elemshapedata_ ) :: obj
  type( quadraturepoint_ ) :: quad
  type( referencetriangle_ ) :: refelem
  integer( i4b ) :: nsd, order
  
  nsd = 2; order = 1
  refelem = ReferenceTriangle( nsd = nsd )
  quad = GaussLegendreQuadrature( refelem = refelem, order = order )
  call AllocateData( obj = obj, nsd = refelem % nsd, &
    & xidim = refelem % xidimension, nns = 2, nips = 1 )
  call initiate( obj = obj, quad = quad, refelem = refelem, &
    & ContinuityType= typeH1, InterpolType = TypeLagrangeInterpolation )
  call display( obj, "obj" )
end block


end program
