submodule(mod1) mod2
implicit none
contains
  module procedure foo
  write(*,*) "a= ",a, "b= ", b
  end procedure
end submodule mod2
