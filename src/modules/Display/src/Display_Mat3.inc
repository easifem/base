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

CHARACTER( LEN = * ), INTENT( IN ) :: msg
INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: full
CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: advance
!   Define internal variables
INTEGER( I4B ) :: J
DO J = 1, SIZE( Val, 3 )
  CALL Display( val=Val( :, :, J ), &
    & msg=TRIM( msg ) //"( :, :, "//TRIM( Int2Str( J ) ) // " ) = ", &
    & unitNo=unitNo, full=full, advance=advance )
END DO
