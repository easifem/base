module mod1
implicit none
private
interface
module subroutine foo(a,b)
  real, intent(in)::a
  real, intent(in)::b
end subroutine
end interface

public:: foo
end module mod1
