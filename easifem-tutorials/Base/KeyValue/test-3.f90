program main
use easifem
implicit none

type( keyvalue_ ), allocatable :: obj( : )

call append( obj, keyvalue( 'e', 1.d0 ) )
call append( obj, keyvalue( 'p', [1.d0, 1.d0] ) )
call append( obj, keyvalue( 'eye', eye3 ) )

call display( obj, 'obj' )

end program main
