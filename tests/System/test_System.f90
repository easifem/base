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

MODULE test_System
USE easifemBase
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
integer :: ierr
ierr=system_mkdir("_scratch", IANY([R_USR,W_USR,X_USR]))
ierr=system_mkdir("/Users/vikassharma/Downloads/HelloWorld/", IANY([R_USR,W_USR,X_USR]))
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE test_System

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_System
implicit none
call test1
end program main