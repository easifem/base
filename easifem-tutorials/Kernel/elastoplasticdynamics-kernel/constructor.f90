program main
  use elastoplasticdynamics_class
  implicit none

  type(elastoplasticdynamics_) :: obj
  integer( i4b ) :: nsd
  real( dfp ) :: dt

  nsd = 2; dt = 0.01_dfp

  call obj % initiate( nsd, dt )

end program main