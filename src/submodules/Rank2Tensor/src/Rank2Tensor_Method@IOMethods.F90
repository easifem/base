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
! date: 2021-03-17
! summary: Display of [[Rank2Tensor_]]

SUBMODULE(Rank2Tensor_Method) IOMethods
USE Display_Method, ONLY: Util_Display => Display
USE GlobalData, ONLY: stdout
USE GlobalData, ONLY: StressMeasureCauchy
USE GlobalData, ONLY: StressMeasureKirchhoff
USE GlobalData, ONLY: StressMeasureMandel
USE GlobalData, ONLY: StressMeasurePK1
USE GlobalData, ONLY: StressMeasurePK2
USE InputUtility, ONLY: Input
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Display
INTEGER(I4B) :: unitNo0

unitNo0 = Input(option=unitNo, default=stdout)
CALL Util_Display(obj%T, msg=msg, unitNo=unitNo0)

SELECT TYPE (obj)
CLASS IS (Stress_)
  WRITE (unitNo0, "(A)") "stress measure: "// &
    StressMeasureName(obj%stressType)
END SELECT
END PROCEDURE Rank2Tensor_Display

!----------------------------------------------------------------------------
!                                                          StressMeasureName
!----------------------------------------------------------------------------

! Name of a stress measure

PURE FUNCTION StressMeasureName(stressType) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: stressType
  !! one of the StressMeasure* constants
  CHARACTER(:), ALLOCATABLE :: ans
  !! name of the stress measure

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
END FUNCTION StressMeasureName

END SUBMODULE IOMethods
