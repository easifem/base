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

SUBMODULE(ElemshapeData_Method) GradientMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE getLocalGradient_1
dPhidXi = MATMUL(Val, obj%dNdXi)
END PROCEDURE getLocalGradient_1

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE getLocalGradient_2
dVdXi = MATMUL(Val, obj%dNdXi)
END PROCEDURE getLocalGradient_2

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE getLocalGradient_3
dPhidXi = MATMUL(MATMUL(Val, obj%T), obj%dNdXi)
END PROCEDURE getLocalGradient_3

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE getLocalGradient_4
dVdXi = MATMUL(MATMUL(Val, obj%T), obj%dNdXi)
END PROCEDURE getLocalGradient_4

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE getLocalGradient_5
SELECT CASE (val%varType)
CASE (constant)
  CALL reallocate(dPhidXi, SIZE(obj%dNdXi, 2), SIZE(obj%dNdXi, 3))
CASE (space)
  CALL getLocalGradient(obj=obj, dPhidXi=dPhidXi, &
       & Val=Get(val, TypeFEVariableScalar, TypeFEVariableSpace))
CASE (spacetime)
  SELECT TYPE (obj)
  TYPE is (STElemShapeData_)
    CALL getLocalGradient(obj=obj, dPhidXi=dPhidXi, &
         & Val=Get(val, TypeFEVariableScalar, TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE getLocalGradient_5

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE getLocalGradient_6
SELECT CASE (val%varType)
CASE (constant)
  CALL reallocate(dVdXi, obj%refelem%NSD, SIZE(obj%dNdXi, 2), &
       & SIZE(obj%dNdXi, 3))
CASE (space)
  CALL getLocalGradient(obj=obj, dVdXi=dVdXi, &
       & Val=Get(val, TypeFEVariableVector, TypeFEVariableSpace))
CASE (spacetime)
  SELECT TYPE (obj)
  TYPE is (STElemShapeData_)
    CALL getLocalGradient(obj=obj, dVdXi=dVdXi, &
         & Val=Get(val, TypeFEVariableVector, TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE getLocalGradient_6

!----------------------------------------------------------------------------
!                                                         getSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE getSpatialGradient_1
IF (obj%refelem%nsd .EQ. obj%refelem%xidimension) THEN
  dPhidXt = MATMUL(Val, obj%dNdXt)
ELSE
  CALL Reallocate(dPhidXt, obj%refelem%xidimension, SIZE(obj%N, 2))
END IF
END PROCEDURE getSpatialGradient_1

!----------------------------------------------------------------------------
!                                                         getSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE getSpatialGradient_2
IF (obj%refelem%nsd .EQ. obj%refelem%xidimension) THEN
  dVdXt = MATMUL(Val, obj%dNdXt)
ELSE
  CALL Reallocate(dVdXt, obj%refelem%nsd, obj%refelem%xidimension, &
    & SIZE(obj%N, 2))
END IF
END PROCEDURE getSpatialGradient_2

!----------------------------------------------------------------------------
!                                                         getSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE getSpatialGradient_3
IF (obj%refelem%nsd .EQ. obj%refelem%xidimension) THEN
  dPhidXt = MATMUL(MATMUL(Val, obj%T), obj%dNdXt)
ELSE
  CALL Reallocate(dPhidXt, obj%refelem%xidimension, SIZE(obj%N, 2))
END IF
END PROCEDURE getSpatialGradient_3

!----------------------------------------------------------------------------
!                                                         getSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE getSpatialGradient_4
IF (obj%refelem%nsd .EQ. obj%refelem%xidimension) THEN
  dVdXt = MATMUL(MATMUL(Val, obj%T), obj%dNdXt)
ELSE
  CALL Reallocate(dVdXt, obj%refelem%nsd, obj%refelem%xidimension, &
    & SIZE(obj%N, 2))
END IF
END PROCEDURE getSpatialGradient_4

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE getSpatialGradient_5
SELECT CASE (val%varType)
CASE (constant)
  CALL reallocate(dPhidXt, SIZE(obj%dNdXt, 2), SIZE(obj%dNdXt, 3))
CASE (space)
  CALL getSpatialGradient(obj=obj, dPhidXt=dPhidXt, &
       & Val=Get(val, TypeFEVariableScalar, TypeFEVariableSpace))
CASE (spacetime)
  SELECT TYPE (obj)
  TYPE is (STElemShapeData_)
    CALL getSpatialGradient(obj=obj, dPhidXt=dPhidXt, &
         & Val=Get(val, TypeFEVariableScalar, TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE getSpatialGradient_5

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE getSpatialGradient_6
SELECT CASE (val%varType)
CASE (constant)
  CALL reallocate(dVdXt, obj%refelem%NSD, SIZE(obj%dNdXt, 2), &
       & SIZE(obj%dNdXt, 3))
CASE (space)
  CALL getSpatialGradient(obj=obj, dVdXt=dVdXt, &
       & Val=Get(val, TypeFEVariableVector, TypeFEVariableSpace))
CASE (spacetime)
  SELECT TYPE (obj)
  TYPE is (STElemShapeData_)
    CALL getSpatialGradient(obj=obj, dVdXt=dVdXt, &
         & Val=Get(val, TypeFEVariableVector, TypeFEVariableSpaceTime))
  END SELECT
END SELECT
END PROCEDURE getSpatialGradient_6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GradientMethods
