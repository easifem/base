program main
  use easifem
  use elastoplasticdynamics_class
  implicit none

  type(elastoplasticdynamics_) :: obj
  integer( i4b ) :: nsd, i
  real( dfp ) :: dt

  nsd = 2; dt = 0.01_dfp

  call obj % initiate( nsd, dt )
  call obj % setmaterialproperty( value = [1.0_dfp], key = "rho" )
  
  call obj % matprops( 1 ) % display( )

end program main
