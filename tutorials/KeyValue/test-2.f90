program main
use easifem
implicit none

type( keyvalue_ ) :: obj

obj = keyvalue( 'H  ', 1.d0)
call display( obj, 'obj' )

call display( obj .eq. 'h', "obj .eq. 'h' = " )
call display( obj .eq. 'H', "obj .eq. 'H' = " )
end program main
