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

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Input and output methods of [[VoigtRank2Tensor_]]
!
! Changes:
!   - 2026-11-17: Display rewritten after IntVector_IOMethod, added the
!     array version

SUBMODULE(VoigtRank2Tensor_Method) IOMethods
USE Display_Method, ONLY: Display0 => Display
USE Display_Method, ONLY: ToString
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressTypeVoigt
USE InputUtility, ONLY: Input
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display1
CALL Display0(msg=msg, unitNo=unitNo)
CALL Display0(msg="voigtType: "//VoigtTypeName(obj%voigtType), &
              unitNo=unitNo)
CALL Display0(val=obj%scale, msg="scale: ", unitNo=unitNo)
CALL Display0(val=obj%V, msg="V: ", unitNo=unitNo, &
              orient=Input(option=orient, default="row"))
END PROCEDURE obj_Display1

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display2
INTEGER(I4B) :: ii

CALL Display0(msg=msg, unitNo=unitNo)
CALL Display0(val=SIZE(obj), msg="size: ", unitNo=unitNo)
DO ii = 1, SIZE(obj)
  CALL Display(obj(ii), msg="("//ToString(ii)//")", unitNo=unitNo, &
               orient=orient)
END DO
END PROCEDURE obj_Display2

!----------------------------------------------------------------------------
!                                                              VoigtTypeName
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Name of a Voigt type for Display

PURE FUNCTION VoigtTypeName(voigtType) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: voigtType
  !! StressTypeVoigt, StrainTypeVoigt or other
  CHARACTER(:), ALLOCATABLE :: ans
  !! name of the Voigt type

  SELECT CASE (voigtType)
  CASE (StressTypeVoigt)
    ans = "stress-like"
  CASE (StrainTypeVoigt)
    ans = "strain-like"
  CASE DEFAULT
    ans = "unknown (treated as stress-like)"
  END SELECT
END FUNCTION VoigtTypeName

END SUBMODULE IOMethods
