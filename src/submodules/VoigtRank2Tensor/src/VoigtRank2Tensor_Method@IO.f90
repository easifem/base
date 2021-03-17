! This program is a part of EASIFEM library,
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D,
!
! This program is free software: you can redistribute it and/or modify,
! it under the terms of the GNU General Public License as published by,
! the Free Software Foundation, either version 3 of the License, or,
! (at your option) any later version.,
!
! This program is distributed in the hope that it will be useful,,
! but WITHOUT ANY WARRANTY; without even the implied warranty of,
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the,
! GNU General Public License for more details.,
!
! You should have received a copy of the GNU General Public License,
! along with this program.  If not, see <https: //www.gnu.org/licenses/>.
!

SUBMODULE( VoigtRank2Tensor_Method ) IO
Use BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE display_obj
  INTEGER( I4B ) :: I, j
  ASSOCIATE( V => Obj%V )
    I = INPUT( option=UnitNo, Default=StdOut )
    WRITE( I, "(A)" ) "# " // TRIM( Msg )
    SELECT CASE( Obj%VoigtType )
    CASE( StressTypeVoigt )
      WRITE( I, "(A)" ) "Stress Like Voigt Type"
    CASE( StrainTypeVoigt )
      WRITE( I, "(A)" ) "Strain Like Voigt Type"
    END SELECT
    CALL Display( Val = V, UnitNo=I, Msg="", orient = "row")
  END ASSOCIATE
END PROCEDURE display_obj

END SUBMODULE IO
