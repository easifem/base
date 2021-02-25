! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

module test_ErrorHandling
use easifemBase
implicit none

contains

!----------------------------------------------------------------------------
!                                                                 test1
!----------------------------------------------------------------------------

subroutine test1

  call ErrorMSG( &
    & Msg="Some Error Message", &
    & File= "test_ErrorHandling.f90", &
    & Routine =  "test1", &
    & Line = 29 &
  )
end subroutine test1

!----------------------------------------------------------------------------
!                                                                 test2
!----------------------------------------------------------------------------

subroutine test2
  call WarningMSG( &
    & Msg="Warning message", &
    & File="test_ErrorHandling.f90", &
    & Routine="test2()", &
    & Line= 42 &
  )
end subroutine test2

!----------------------------------------------------------------------------
!                                                                 test3
!----------------------------------------------------------------------------

subroutine test3
  call FileError( &
    & istat = 1, &
    & filename = "some-filename", &
    & flg=OPT_Open, &
    & File="test_ErrorHandling.f90", &
    & Routine="test3()", &
    & Line= 55 &
  )
end subroutine test3

end module test_ErrorHandling

program main
use test_ErrorHandling
implicit none

call test1
call test2
call test3

end program main