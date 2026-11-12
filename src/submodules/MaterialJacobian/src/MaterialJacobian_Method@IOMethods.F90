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
! date: 2026-10-21
! summary: Display of [[MaterialJacobian_]]
!
! Description:
!   Ported from the old `MaterialJacobian/Display.part`.

SUBMODULE(MaterialJacobian_Method) IOMethods
USE GlobalData, ONLY: stdout
USE GlobalData, ONLY: StressMeasureCauchy
USE GlobalData, ONLY: StressMeasureKirchhoff
USE GlobalData, ONLY: StressMeasurePK1
USE GlobalData, ONLY: StressMeasurePK2
USE GlobalData, ONLY: StressMeasureMandel
USE GlobalData, ONLY: StrainMeasureSmall
USE GlobalData, ONLY: StrainMeasureGreen
USE GlobalData, ONLY: StrainMeasureAlmansi
USE GlobalData, ONLY: StrainMeasureRateOfDeformation
USE GlobalData, ONLY: StressRateNone
USE GlobalData, ONLY: StressRateJaumann
USE GlobalData, ONLY: StressRateTruesdell
USE GlobalData, ONLY: StressRateGreenNaghdi
USE InputUtility, ONLY: Input
USE Display_Method, ONLY: Util_Display => Display
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_Display
INTEGER(I4B) :: unitNo0

unitNo0 = Input(option=unitNo, default=stdout)
CALL Util_Display(obj%C, msg=msg, unitNo=unitNo0)
WRITE (unitNo0, "(A)") "stress measure: "//StressName(obj%stressType)
WRITE (unitNo0, "(A)") "strain measure: "//StrainName(obj%strainType)
WRITE (unitNo0, "(A)") "stress rate: "//RateName(obj%rateType)
END PROCEDURE MaterialJacobian_Display

!----------------------------------------------------------------------------
!                                                                 StressName
!----------------------------------------------------------------------------

PURE FUNCTION StressName(stressType) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: stressType
  CHARACTER(:), ALLOCATABLE :: ans

  SELECT CASE (stressType)
  CASE (StressMeasureCauchy)
    ans = "Cauchy"
  CASE (StressMeasureKirchhoff)
    ans = "Kirchhoff"
  CASE (StressMeasurePK1)
    ans = "first Piola-Kirchhoff"
  CASE (StressMeasurePK2)
    ans = "second Piola-Kirchhoff"
  CASE (StressMeasureMandel)
    ans = "Mandel"
  CASE DEFAULT
    ans = "unknown"
  END SELECT
END FUNCTION StressName

!----------------------------------------------------------------------------
!                                                                 StrainName
!----------------------------------------------------------------------------

PURE FUNCTION StrainName(strainType) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: strainType
  CHARACTER(:), ALLOCATABLE :: ans

  SELECT CASE (strainType)
  CASE (StrainMeasureSmall)
    ans = "small strain"
  CASE (StrainMeasureGreen)
    ans = "Green-Lagrange"
  CASE (StrainMeasureAlmansi)
    ans = "Euler-Almansi"
  CASE (StrainMeasureRateOfDeformation)
    ans = "rate of deformation"
  CASE DEFAULT
    ans = "unknown"
  END SELECT
END FUNCTION StrainName

!----------------------------------------------------------------------------
!                                                                   RateName
!----------------------------------------------------------------------------

PURE FUNCTION RateName(rateType) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: rateType
  CHARACTER(:), ALLOCATABLE :: ans

  SELECT CASE (rateType)
  CASE (StressRateNone)
    ans = "none"
  CASE (StressRateJaumann)
    ans = "Jaumann"
  CASE (StressRateTruesdell)
    ans = "Truesdell"
  CASE (StressRateGreenNaghdi)
    ans = "Green-Naghdi"
  CASE DEFAULT
    ans = "unknown"
  END SELECT
END FUNCTION RateName

END SUBMODULE IOMethods
