module mod1
implicit none
contains
subroutine foo1()
  write(*,*) "from mod1 and foo1"
end subroutine
end module mod1