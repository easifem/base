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

SUBMODULE(GE_Lapack_Method) LUMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     getLU
!----------------------------------------------------------------------------

MODULE PROCEDURE getLU_1
LU = A
CALL GETRF(A=LU, IPIV=IPIV, RCOND=RCOND, NORM=NORM, info=info)
END PROCEDURE getLU_1

!----------------------------------------------------------------------------
!                                                                     getLU
!----------------------------------------------------------------------------

MODULE PROCEDURE getLU_2
CALL GETRF(A=A, IPIV=IPIV, RCOND=RCOND, NORM=NORM, info=info)
END PROCEDURE getLU_2

!----------------------------------------------------------------------------
!                                                                 LUSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE LUSolve_1
CHARACTER(1) :: TRANS
!!
IF (PRESENT(isTranspose)) THEN
  IF (isTranspose) THEN
    TRANS = "T"
  ELSE
    TRANS = "N"
  END IF
ELSE
  TRANS = "N"
END IF
!!
CALL GETRS(A=A, IPIV=IPIV, B=B, TRANS=TRANS, info=info)
!!
END PROCEDURE LUSolve_1

!----------------------------------------------------------------------------
!                                                                 LUSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE LUSolve_2
CHARACTER(1) :: TRANS
!!
IF (PRESENT(isTranspose)) THEN
  IF (isTranspose) THEN
    TRANS = "T"
  ELSE
    TRANS = "N"
  END IF
ELSE
  TRANS = "N"
END IF
!!
CALL GETRS(A=A, IPIV=IPIV, B=B, TRANS=TRANS, info=info)
!!
END PROCEDURE LUSolve_2

!----------------------------------------------------------------------------
!                                                                 LUSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE LUSolve_3
CHARACTER(1) :: TRANS
!!
IF (PRESENT(isTranspose)) THEN
  IF (isTranspose) THEN
    TRANS = "T"
  ELSE
    TRANS = "N"
  END IF
ELSE
  TRANS = "N"
END IF
!!
X = B
!!
CALL GETRS(A=A, IPIV=IPIV, B=X, TRANS=TRANS, info=info)
!!
END PROCEDURE LUSolve_3

!----------------------------------------------------------------------------
!                                                                 LUSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE LUSolve_4
CHARACTER(1) :: TRANS
!!
IF (PRESENT(isTranspose)) THEN
  IF (isTranspose) THEN
    TRANS = "T"
  ELSE
    TRANS = "N"
  END IF
ELSE
  TRANS = "N"
END IF
!!
X = B
CALL GETRS(A=A, IPIV=IPIV, B=X, TRANS=TRANS, info=info)
!!
END PROCEDURE LUSolve_4

!----------------------------------------------------------------------------
!                                                                     Inv
!----------------------------------------------------------------------------

MODULE PROCEDURE Inv_1
invA = A
CALL GETRI(A=invA, IPIV=IPIV, info=info)
END PROCEDURE Inv_1

!----------------------------------------------------------------------------
!                                                                     Inv
!----------------------------------------------------------------------------

MODULE PROCEDURE Inv_2
CALL GETRI(A=A, IPIV=IPIV, info=info)
END PROCEDURE Inv_2

END SUBMODULE LUMethods
