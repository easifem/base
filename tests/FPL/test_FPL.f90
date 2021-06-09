module test_FPL
use easifemBase
implicit none
contains
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
subroutine test1
  type(parameterList_) :: mylst
  integer( i4b ), allocatable :: nptrs( : ), s(:)
  integer( i4b ) :: ierr

  CALL FPL_INIT()
  CALL mylst%init()
  nptrs = [1,2,3]
  ierr = mylst%set(key="nptrs", value=nptrs)
  ierr = mylst%getShape(key="nptrs", Shape=s )
  call display( s, "shape: ")
  CALL mylst%free()
  CALL FPL_FINALIZE()
end subroutine
end module test_FPL


program main
use test_FPL
implicit none
call test1
end program main