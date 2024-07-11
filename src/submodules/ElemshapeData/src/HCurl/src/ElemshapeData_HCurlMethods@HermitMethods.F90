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

SUBMODULE(ElemShapeData_HCurlMethods) HermitMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE HCurl_Hermit1
CALL Errormsg( &
  & msg="[WORK IN PROGRESS] This method is currently not available",  &
  & line=__LINE__,  &
  & routine="HCurl_Hermit1()",  &
  & unitno=stderr,  &
  & file=__FILE__)
END PROCEDURE HCurl_Hermit1

END SUBMODULE HermitMethods
