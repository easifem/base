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

SUBMODULE(ElemshapeData_Method) getMethod
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                          getInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE get_interpol_scalar
Interpol = MATMUL(Val, obj%N)
END PROCEDURE get_interpol_scalar

!----------------------------------------------------------------------------
!                                                           getInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE get_interpol_fevar_scalar
SELECT CASE (Val%VarType)
CASE (Constant)
  Interpol = Get(Val, TypeFEVariableScalar, TypeFEVariableConstant)
CASE (Space)
  IF (Val%DefineOn .Eq. Nodal) THEN
    Interpol = Interpolation(obj, &
      & Get(Val, TypeFEVariableScalar, TypeFEVariableSpace))
  ELSE
    Interpol = Get(Val, TypeFEVariableScalar, TypeFEVariableSpace)
  END IF
CASE (SpaceTime)
  SELECT TYPE (obj)
  TYPE IS (STElemShapeData_)
    IF (Val%DefineOn .EQ. Nodal) THEN
      Interpol = STInterpolation(obj, &
      & Get(Val, TypeFEVariableScalar, TypeFEVariableSpaceTime))
    END IF
  END SELECT
END SELECT
END PROCEDURE get_interpol_fevar_scalar

!---------------------------------------------------------------------------
!                                                          getInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE get_interpol_vector
Interpol = MATMUL(Val, obj%N)
END PROCEDURE get_interpol_vector

!----------------------------------------------------------------------------
!                                                           getInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE get_interpol_matrix
Interpol = MATMUL(Val, obj%N)
END PROCEDURE get_interpol_matrix

!----------------------------------------------------------------------------
!                                                           getInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE get_interpol_fevar_matrix
INTEGER(I4B) :: i
SELECT CASE (Val%VarType)
CASE (Constant)

  Interpol(:, :, 1) = Get(Val, TypeFEVariableMatrix, &
    & TypeFEVariableConstant)
  DO i = 2, SIZE(obj%N, 2)
    Interpol(:, :, i) = Interpol(:, :, 1)
  END DO

CASE (Space)

  IF (Val%DefineOn .EQ. Nodal) THEN
    Interpol = Interpolation(obj, &
      & Get(Val, TypeFEVariableMatrix, TypeFEVariableSpace))
  ELSE
    Interpol = Get(Val, TypeFEVariableMatrix, TypeFEVariableSpace)
  END IF

CASE (SpaceTime)

  SELECT TYPE (obj)
  TYPE IS (STElemShapeData_)
    IF (Val%DefineOn .EQ. Nodal) THEN
      Interpol = STInterpolation(obj, &
        & Get(Val, TypeFEVariableMatrix, TypeFEVariableSpaceTime))
    END IF
  END SELECT
END SELECT

END PROCEDURE get_interpol_fevar_matrix

!----------------------------------------------------------------------------
!                                                              Interpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE interpol_scalar
Interpol = MATMUL(Val, obj%N)
END PROCEDURE interpol_scalar

!----------------------------------------------------------------------------
!                                                      InterpolationOfVector
!----------------------------------------------------------------------------

MODULE PROCEDURE interpol_vector
Interpol = MATMUL(Val, obj%N)
END PROCEDURE interpol_vector

!----------------------------------------------------------------------------
!                                                      InterpolationOfVector
!----------------------------------------------------------------------------

MODULE PROCEDURE interpol_matrix
Interpol = MATMUL(Val, obj%N)
END PROCEDURE interpol_matrix

!----------------------------------------------------------------------------
!                                                         getSTInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_get_interpol_scalar
Interpol = MATMUL(MATMUL(Val, obj%T), obj%N)
END PROCEDURE stsd_get_interpol_scalar

!----------------------------------------------------------------------------
!                                                           getInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_get_interpol_fevar_scalar
INTEGER(I4B) :: ii
!! main
SELECT CASE (Val%VarType)
CASE (Constant)
  Interpol = Get(Val, TypeFEVariableScalar, TypeFEVariableConstant)
CASE (Space)
  IF (Val%DefineOn .Eq. Nodal) THEN
    Interpol(:, 1) = Interpolation(obj(1), &
        & Get(Val, TypeFEVariableScalar, TypeFEVariableSpace))
    DO ii = 2, SIZE(obj)
      Interpol(:, ii) = Interpol(:, 1)
    END DO
  ELSE
    Interpol(:, 1) = Get(Val, TypeFEVariableScalar,&
      & TypeFEVariableSpace)
    DO ii = 2, SIZE(obj)
      Interpol(:, ii) = Interpol(:, 1)
    END DO
  END IF
CASE (SpaceTime)
  IF (Val%DefineOn .Eq. Nodal) THEN
    DO ii = 1, SIZE(obj)
      Interpol(:, ii) = STInterpolation(obj(ii), &
        & Get(Val, TypeFEVariableScalar, TypeFEVariableSpaceTime))
    END DO
  ELSE
    Interpol = Get(Val, TypeFEVariableScalar, &
      & TypeFEVariableSpaceTime)
  END IF
END SELECT
END PROCEDURE stsd_get_interpol_fevar_scalar

!----------------------------------------------------------------------------
!                                                         getSTInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_get_interpol_vector
Interpol = MATMUL(MATMUL(Val, obj%T), obj%N)
END PROCEDURE stsd_get_interpol_vector

!----------------------------------------------------------------------------
!                                                         getSTInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_get_interpol_matrix
Interpol = MATMUL(MATMUL(Val, obj%T), obj%N)
END PROCEDURE stsd_get_interpol_matrix

!----------------------------------------------------------------------------
!                                                           getInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_get_interpol_fevar_matrix
INTEGER(I4B) :: ii, jj
!! main
SELECT CASE (Val%VarType)
CASE (Constant)
  Interpol(:, :, 1, 1) = Get(Val, TypeFEVariableMatrix,&
    & TypeFEVariableConstant)
  DO jj = 1, SIZE(Interpol, 4)
    DO ii = 1, SIZE(Interpol, 3)
      IF (ii .EQ. 1 .AND. jj .EQ. 1) CYCLE
      Interpol(:, :, ii, jj) = Interpol(:, :, 1, 1)
    END DO
  END DO
CASE (Space)
  IF (Val%DefineOn .Eq. Nodal) THEN
    Interpol(:, :, :, 1) = Interpolation(obj(1), &
        & Get(Val, TypeFEVariableMatrix, TypeFEVariableSpace))
    DO ii = 2, SIZE(Interpol, 4)
      Interpol(:, :, :, ii) = Interpol(:, :, :, 1)
    END DO
  ELSE
    Interpol(:, :, :, 1) = Get(Val, TypeFEVariableMatrix,&
      & TypeFEVariableSpace)
    DO ii = 2, SIZE(Interpol, 4)
      Interpol(:, :, :, ii) = Interpol(:, :, :, 1)
    END DO
  END IF
CASE (SpaceTime)
  IF (Val%DefineOn .Eq. Nodal) THEN
    DO ii = 1, SIZE(Interpol, 4)
      Interpol(:, :, :, ii) = STInterpolation(obj(ii), &
        & Get(Val, TypeFEVariableMatrix, TypeFEVariableSpaceTime))
    END DO
  ELSE
    Interpol = Get(Val, TypeFEVariableMatrix, &
      & TypeFEVariableSpaceTime)
  END IF
END SELECT
END PROCEDURE stsd_get_interpol_fevar_matrix

!----------------------------------------------------------------------------
!                                                            STInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_interpol_scalar
Interpol = MATMUL(MATMUL(Val, obj%T), obj%N)
END PROCEDURE stsd_interpol_scalar

!----------------------------------------------------------------------------
!                                                            STInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_interpol_vector
Interpol = MATMUL(MATMUL(Val, obj%T), obj%N)
END PROCEDURE stsd_interpol_vector

!----------------------------------------------------------------------------
!                                                            STInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_interpol_matrix
Interpol = MATMUL(MATMUL(Val, obj%T), obj%N)
END PROCEDURE stsd_interpol_matrix

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE getLocalGradient_scalar
dPhidXi = MATMUL(Val, obj%dNdXi)
END PROCEDURE getLocalGradient_scalar

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE getLocalGradient_vector
dVdXi = MATMUL(Val, obj%dNdXi)
END PROCEDURE getLocalGradient_vector

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_getLocalGradient_scalar
dPhidXi = MATMUL(MATMUL(Val, obj%T), obj%dNdXi)
END PROCEDURE stsd_getLocalGradient_scalar

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_getLocalGradient_vector
dVdXi = MATMUL(MATMUL(Val, obj%T), obj%dNdXi)
END PROCEDURE stsd_getLocalGradient_vector

!----------------------------------------------------------------------------
!                                                         getSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE getSpatialGradient_scalar
IF (obj%refelem%nsd .EQ. obj%refelem%xidimension) THEN
  dPhidXt = MATMUL(Val, obj%dNdXt)
ELSE
  CALL Reallocate(dPhidXt, obj%refelem%xidimension, size(obj%N, 2))
END IF
END PROCEDURE getSpatialGradient_scalar

!----------------------------------------------------------------------------
!                                                         getSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE getSpatialGradient_vector
IF (obj%refelem%nsd .EQ. obj%refelem%xidimension) THEN
  dVdXt = MATMUL(Val, obj%dNdXt)
ELSE
  CALL Reallocate(dVdXt, obj%refelem%nsd, obj%refelem%xidimension, &
    & size(obj%N, 2))
END IF
END PROCEDURE getSpatialGradient_vector

!----------------------------------------------------------------------------
!                                                         getSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_getSpatialGradient_scalar
IF (obj%refelem%nsd .EQ. obj%refelem%xidimension) THEN
  dPhidXt = MATMUL(MATMUL(Val, obj%T), obj%dNdXt)
ELSE
  CALL Reallocate(dPhidXt, obj%refelem%xidimension, size(obj%N, 2))
END IF
END PROCEDURE stsd_getSpatialGradient_scalar

!----------------------------------------------------------------------------
!                                                         getSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_getSpatialGradient_vector
IF (obj%refelem%nsd .EQ. obj%refelem%xidimension) THEN
  dVdXt = MATMUL(MATMUL(Val, obj%T), obj%dNdXt)
ELSE
  CALL Reallocate(dVdXt, obj%refelem%nsd, obj%refelem%xidimension, &
    & size(obj%N, 2))
END IF
END PROCEDURE stsd_getSpatialGradient_vector

!----------------------------------------------------------------------------
!                                                       getProjectionOfdNdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetProjectionOfdNdXt_1
! Define internal variables
INTEGER(I4B) :: i
CALL Reallocate(cdNdXt, SIZE(obj%N, 1), SIZE(obj%N, 2))
DO i = 1, SIZE(obj%N, 2)
  cdNdXt(:, i) = MATMUL(obj%dNdXt(:, :, i), Val)
END DO
END PROCEDURE elemsd_GetProjectionOfdNdXt_1

!----------------------------------------------------------------------------
!                                                       getProjectionOfdNdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetProjectionOfdNdXt_2
INTEGER(I4B) :: ii
REAL(DFP), ALLOCATABLE :: CBar(:, :)
!! main
!! Compute CBar from Val
SELECT CASE (Val%VarType)
CASE (Constant)
  CALL Reallocate(CBar, obj%refelem%nsd, SIZE(obj%N, 2))
  CBar(:, 1) = Get(Val, TypeVariableVector, TypeVariableConstant)
  DO ii = 2, SIZE(CBar, 2)
    CBar(:, ii) = CBar(:, 1)
  END DO
CASE (Space)
  IF (Val%DefineOn .EQ. Nodal) THEN
    CALL getInterpolation(obj=obj, Interpol=CBar, &
      & Val=Get(Val, TypeVariableVector, TypeVariableSpace))
  ELSE
    CBar = Get(Val, TypeVariableVector, TypeVariableSpace)
  END IF
CASE (SpaceTime)
  SELECT TYPE (obj)
  TYPE IS (STElemShapeData_)
    CALL getInterpolation(obj=obj, Interpol=CBar, &
      & Val=Get(Val, TypeVariableVector, TypeVariableSpaceTime))
  END SELECT
END SELECT
CALL Reallocate(cdNdXt, SIZE(obj%N, 1), SIZE(obj%N, 2))
DO ii = 1, SIZE(obj%N, 2)
  cdNdXt(:, ii) = MATMUL(obj%dNdXt(:, :, ii), CBar(:, ii))
END DO
IF (ALLOCATED(CBar)) DEALLOCATE (CBar)
END PROCEDURE elemsd_GetProjectionOfdNdXt_2

!----------------------------------------------------------------------------
!                                                      getProjectionOfdNTdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE getProjectionOfdNTdXt_constvector
INTEGER(I4B) :: ii
CALL Reallocate(cdNTdXt, SIZE(obj%N, 1), SIZE(obj%T), SIZE(obj%N, 2))
DO ii = 1, SIZE(obj%N, 2)
  cdNTdXt(:, :, ii) = MATMUL(obj%dNTdXt(:, :, :, ii), Val)
END DO
END PROCEDURE getProjectionOfdNTdXt_constvector

!----------------------------------------------------------------------------
!                                                      getProjectionOfdNTdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE getProjectionOfdNTdXt_fevar
INTEGER(I4B) :: ii
REAL(DFP), ALLOCATABLE :: CBar(:, :)

SELECT CASE (Val%VarType)
CASE (Constant)
  CALL Reallocate(CBar, obj%refelem%nsd, SIZE(obj%N, 2))
  CBar(:, 1) = Get(Val, TypeVariableVector, TypeVariableConstant)
  DO ii = 2, SIZE(CBar, 2)
    CBar(ii, :) = CBar(:, 1)
  END DO

CASE (Space)

  IF (Val%DefineOn .EQ. Nodal) THEN
    CALL getInterpolation(obj=obj, Interpol=CBar, &
      & Val=Get(Val, TypeVariableVector, TypeVariableSpace))
  ELSE
    CBar = Get(Val, TypeVariableVector, TypeVariableSpace)
  END IF

CASE (SpaceTime)

  CALL getInterpolation(obj=obj, Interpol=CBar, &
    & Val=Get(Val, TypeVariableVector, TypeVariableSpaceTime))

END SELECT

CALL Reallocate(cdNTdXt, SIZE(obj%N, 1), SIZE(obj%T), SIZE(obj%N, 2))
DO ii = 1, SIZE(obj%N, 2)
  cdNTdXt(:, :, ii) = MATMUL(obj%dNTdXt(:, :, :, ii), CBar(:, ii))
END DO

DEALLOCATE (CBar)
END PROCEDURE getProjectionOfdNTdXt_fevar

!----------------------------------------------------------------------------
!                                                             getUnitNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE getUnitNormal_scalar
! Define internal variables
REAL(DFP), ALLOCATABLE :: dPhidX(:, :), PhiBar(:), Norm_L2_Phi(:)
INTEGER(I4B) :: NSD, n, i

CALL getInterpolation(obj=obj, Val=Val, Interpol=PhiBar)
CALL getSpatialGradient(obj=obj, dPhidXt=dPhidX, Val=Val)
n = SIZE(obj%N, 2)
NSD = obj%RefElem%NSD
IF (ALLOCATED(R)) THEN
  IF (ANY(SHAPE(R) .NE. [NSD, n])) THEN
    DEALLOCATE (R)
    ALLOCATE (R(NSD, n))
  END IF
ELSE
  ALLOCATE (R(NSD, n))
END IF
R = 0.0_DFP

Norm_L2_Phi = SQRT(SUM(dPhidX**2, Dim=2))

DO i = 1, n
  IF (Norm_L2_Phi(i) .GT. zero) THEN
    IF (PhiBar(i) .GE. 0.0_DFP) THEN
      R(:, i) = dPhidX(:, i) / Norm_L2_Phi(i)
    ELSE
      R(:, i) = -dPhidX(:, i) / Norm_L2_Phi(i)
    END IF
  END IF
END DO

IF (ALLOCATED(dPhidX)) DEALLOCATE (dPhidX)
IF (ALLOCATED(PhiBar)) DEALLOCATE (PhiBar)
IF (ALLOCATED(Norm_L2_Phi)) DEALLOCATE (Norm_L2_Phi)

END PROCEDURE getUnitNormal_scalar

!----------------------------------------------------------------------------
!                                                             getUnitNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE getUnitNormal_vector
! Define internal variables
REAL(DFP), ALLOCATABLE :: dVdX(:, :, :), VBar(:, :), MatVec(:), &
  & Norm_L2_VBar(:)
REAL(DFP) :: Norm_L2_MatVec
INTEGER(I4B) :: NSD, n, i

CALL getInterpolation(obj=obj, Interpol=VBar, Val=Val)
CALL getSpatialGradient(obj=obj, dVdXt=dVdX, Val=Val)
! Norm_L2_VBar = SQRT( SUM( VBar ** 2, DIM = 2 ) )
Norm_L2_VBar = NORM2(VBar, DIM=2)

n = SIZE(obj%N, 2)
NSD = obj%RefElem%NSD
IF (ALLOCATED(R)) THEN
  IF (ANY(SHAPE(R) .NE. [NSD, n])) THEN
    DEALLOCATE (R)
    ALLOCATE (R(NSD, n))
  END IF
ELSE
  ALLOCATE (R(NSD, n))
END IF
R = 0.0_DFP

DO i = 1, n
  IF (Norm_L2_VBar(i) .GT. Zero) THEN
    VBar(:, i) = VBar(:, i) / Norm_L2_VBar(i)
    MatVec = MATMUL(VBar(:, i), dVdX(:, :, i))
    Norm_L2_MatVec = NORM2(MatVec)
    IF (Norm_L2_MatVec .GT. Zero) THEN
      R(:, i) = MatVec / Norm_L2_MatVec
    END IF
  END IF
END DO

IF (ALLOCATED(dVdX)) DEALLOCATE (dVdX)
IF (ALLOCATED(VBar)) DEALLOCATE (VBar)
IF (ALLOCATED(MatVec)) DEALLOCATE (MatVec)
IF (ALLOCATED(Norm_L2_VBar)) DEALLOCATE (Norm_L2_VBar)

END PROCEDURE getUnitNormal_vector

END SUBMODULE getMethod
