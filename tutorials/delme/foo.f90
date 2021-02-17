module foo
implicit none
contains
subroutine hello()
  write(*,*) __FILE__
end subroutine
end module foo