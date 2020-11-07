program main
use stringifor
type(string) :: astring

astring = 'Hello World'
print "(A)", astring%chars() ! "chars" method returns a standard character variable
print "(DT)", astring        ! defined IO is not enabled with GNU gfortran
print "(A)", astring//''
end program main
