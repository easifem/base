program main
implicit none

CHARACTER( LEN = 100 ) :: dummy
integer :: a, b, iostate

dummy = "1 2 3 4"

read( dummy, *, iostat=iostate ) a, b

write( *, * ) "iostate :: ", iostate
write( *, * ) a, b

end program main