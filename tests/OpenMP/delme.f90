program main
  implicit none
  integer :: i, j, k
  j = 5; k = 3
  i = j/k
  print*, i
  i = int(j/k)
  print*, i
end program main