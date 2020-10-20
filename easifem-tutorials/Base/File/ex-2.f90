program main
  use easifem
  implicit none

  type( file_ ) :: afile

  call OpenFileToWrite(afile, "./", "hello", ".txt")
  call write( afile, "hello world")
  call closefile(afile)


  call openFiletoAppend( afile, "./", "hello", ".txt")
  call write( afile, "second line")
  call closefile(afile)

end program main