program main
  use easifem
  implicit none

  real(dfp), allocatable :: xieta(:,:)

  call reallocate( xieta, 2, 3 )
  xieta = 0.0_DFP

end program main