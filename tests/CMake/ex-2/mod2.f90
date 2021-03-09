module mod2
  use mod1
  implicit none
  contains
  subroutine foo2()
    write( *, * ) "calling foo1() from foo2()"
    call foo1()
  end subroutine
end module mod2