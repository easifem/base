program main
use easifem
implicit none

block
type(realvector_) :: obj
  call random_number( obj, 10 )
  call display( obj, msg="random_var", filename='obj', path='./', &
    & extension='.hdf5' )
end block

block
type(realvector_), allocatable :: obj( : )
  call random_number( obj, [10,20] )
  call display( obj, msg="random_var", filename='obj', path='./', &
    & extension='.hdf5' )
end block

end program main