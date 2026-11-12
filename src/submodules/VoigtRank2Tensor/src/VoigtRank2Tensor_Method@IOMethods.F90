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
! along with this program.  If not, see <https://www.gnu.org/licenses/>

SUBMODULE(VoigtRank2Tensor_Method) IOMethods
USE GlobalData, ONLY: stdout
USE GlobalData, ONLY: StressTypeVoigt
USE GlobalData, ONLY: StrainTypeVoigt
USE InputUtility, ONLY: Input
USE Display_Method, ONLY: Util_Display => Display
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE VoigtRank2Tensor_Display
INTEGER(I4B) :: unitNo0

unitNo0 = Input(option=unitNo, default=stdout)

WRITE (unitNo0, "(A)") "# "//TRIM(msg)

SELECT CASE (obj%voigtType)
CASE (StressTypeVoigt)
  WRITE (unitNo0, "(A)") "Stress Like Voigt Type"
CASE (StrainTypeVoigt)
  WRITE (unitNo0, "(A)") "Strain Like Voigt Type"
CASE DEFAULT
  WRITE (unitNo0, "(A)") "Unknown Voigt Type (treated as stress like)"
END SELECT

CALL Util_Display(obj%V, msg="", unitNo=unitNo0, orient="row")
END PROCEDURE VoigtRank2Tensor_Display

END SUBMODULE IOMethods
