! testing setmethod

program main
use easifem
implicit none

type( elemshapedata_ ) :: obj
type( quadraturepoint_ ) :: quad
type( referencetriangle_ ) :: refelem
integer( i4b ) :: nsd, order

nsd = 2; order = 1; refelem = ReferenceTriangle( nsd = nsd )
quad = GaussLegendreQuadrature( refelem = refelem, order = order )

call initiate( obj = obj, quad = quad, refelem = refelem, &
  & ContinuityType= typeH1, InterpolType = TypeLagrangeInterpolation )

! ! set thickness
! block
!   real( dfp ) :: thickness( 3 )
!   thickness = 2; call display( thickness, 'thickness' )
!   call setThickness( obj, thickness ) 
!   call display( obj, 'obj' )
! end block

! ! test
! block
!   real( dfp ) :: xiJ( nsd, 3 )
!   xiJ = reshape( [1, 0, 0, 1, 1, 1], [nsd,3] )
!   call display( 'setBarycentricCoord()')
!   call setBarycentricCoord( obj, xiJ )
!   call display( obj, 'obj' )
! end block

! ! test
! block
!   real( dfp ) :: xiJ( nsd, 3 )
!   xiJ = reshape( [1, 0, 0, 1, 1, 1], [nsd,3] )
!   call display( 'setJacobian()')
!   call setJacobian( obj, xiJ )
!   call setJs( obj )
!   call display( obj, 'obj' )
! end block
end program main