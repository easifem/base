program main
use basetype
use basemethod

type( file_ ) :: afile

call initiate( afile, "./", "Hello", ".txt", "REPLACE", "WRITE" )

end program main