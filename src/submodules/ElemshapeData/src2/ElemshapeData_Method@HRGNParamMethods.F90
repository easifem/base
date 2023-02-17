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

SUBMODULE(ElemshapeData_Method) HRGNParamMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               getHRGNParam
!----------------------------------------------------------------------------

PURE SUBROUTINE elemsd_getHRGNParam_a(obj, h, val, opt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: h(:)
  TYPE(FEVariable_), INTENT(IN) :: val
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  !!
  !! define internal variables
  !!
  INTEGER(I4B) :: ii
  REAL(DFP) :: areal
  REAL(DFP), ALLOCATABLE :: q(:, :), hmin(:), hmax(:)
  !! rdNdXt; (I,ips) => projection of dNdXt on unit normal
  TYPE(FEVariable_) :: rvar
  !! vector variable for keeping r
  !!
  !! Main
  !!
  CALL Reallocate(h, SIZE(obj%N, 2))
  !!
  !! Get unitNormal in q
  !!
  CALL GetUnitNormal(obj=obj, val=val, r=q)
  !!
  !! Convert unit normal to [[FEVariable_]]
  !!
  rvar = QuadratureVariable(q, TypeFEVariableVector, TypeFEVariableSpace)
  !!
  !! Call get projection of dNdXt in q
  !!
  CALL GetProjectionOfdNdXt(obj=obj, cdNdXt=q, val=rvar)
  !!
  !! Calculate hmin and hmax
  !!
  CALL GetHminHmax(obj=obj, hmax=hmax, hmin=hmin)
  !!
  DO ii = 1, SIZE(h)
    areal = SUM(ABS(q(:, ii)))
    IF (areal.APPROXEQ.zero) THEN
      h(ii) = hmin(ii)
    ELSE
      h(ii) = 2.0_DFP / areal
    END IF
  END DO
  !!
  IF (ALLOCATED(q)) DEALLOCATE (q)
  IF (ALLOCATED(hmin)) DEALLOCATE (hmin)
  IF (ALLOCATED(hmax)) DEALLOCATE (hmax)
  CALL DEALLOCATE (rvar)
  !!
END SUBROUTINE elemsd_getHRGNParam_a

!----------------------------------------------------------------------------
!                                                              getHRGNParam
!----------------------------------------------------------------------------

PURE SUBROUTINE elemsd_getHRGNParam_b(obj, h, val, opt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: h(:)
  TYPE(FEVariable_), INTENT(IN) :: val
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  !!
  !! internal variables
  !!
  INTEGER(I4B) :: ii
  REAL(DFP) :: areal
  REAL(DFP), ALLOCATABLE :: r(:, :), hmin(:), hmax(:)
  REAL(DFP), ALLOCATABLE :: q(:, :, :)
  !! rdNTdXt; (I,a,ips)m => projection of dNTdXt on unit normal
  TYPE(FEVariable_) :: rvar
  !!
  !! main
  !!
  CALL Reallocate(h, SIZE(obj%N, 2))
  !!
  !! Get unitNormal in r
  !!
  CALL GetUnitNormal(obj=obj, val=val, r=r)
  !!
  !! Make [[FEVariable_]]
  !!
  rvar = QuadratureVariable(r, TypeFEVariableVector, TypeFEVariableSpace)
  !!
  !! Get Projection of dNTdXt in q
  !!
  CALL GetProjectionOfdNTdXt(obj=obj, cdNTdXt=q, val=rvar)
  !!
  !! Calculate hmin and hmax
  !!
  CALL GetHminHmax(obj=obj, hmax=hmax, hmin=hmin)
  !!
  DO ii = 1, SIZE(h, 1)
    areal = SUM(ABS(q(:, :, ii)))
    IF (areal.APPROXEQ.zero) THEN
      h(ii) = hmin(ii)
    ELSE
      h(ii) = 2.0_DFP / areal
    END IF
  END DO
  !!
  IF (ALLOCATED(r)) DEALLOCATE (r)
  IF (ALLOCATED(q)) DEALLOCATE (q)
  CALL DEALLOCATE (rvar)
  !!
END SUBROUTINE elemsd_getHRGNParam_b

!----------------------------------------------------------------------------
!                                                               getHRGNParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getHRGNParam1
SELECT TYPE (obj)
!!
TYPE IS (ElemshapeData_)
  !!
  CALL elemsd_getHRGNParam_a( &
    & obj=obj, &
    & h=h, &
    & val=val, &
    & opt=opt)
  !!
CLASS IS (STElemshapeData_)
  !!
  CALL elemsd_getHRGNParam_b( &
    & obj=obj, &
    & h=h, &
    & val=val, &
    & opt=opt)
  !!
END SELECT
END PROCEDURE elemsd_getHRGNParam1

!----------------------------------------------------------------------------
!                                                               getHRGNParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHRGNParam2
REAL(DFP), ALLOCATABLE :: ans(:)
!!
CALL GetHRGNParam(obj=obj, h=ans, val=val, opt=opt)
h = QuadratureVariable(ans, TypeFEVariableScalar, TypeFEVariableSpace)
IF (ALLOCATED(ans)) DEALLOCATE (ans)
!!
END PROCEDURE elemsd_GetHRGNParam2

!----------------------------------------------------------------------------
!                                                               getHRGNParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHRGNParam3
INTEGER(I4B) :: ii
REAL(DFP), ALLOCATABLE :: avec(:)
!!
!! main
!!
CALL Reallocate(h, SIZE(obj(1)%N, 2), SIZE(obj))
!!
DO ii = 1, SIZE(obj)
  CALL GetHRGNParam( &
    & obj=obj(ii), &
    & h=avec, &
    & val=val, &
    & opt=opt)
  !!
  h(:, ii) = avec(:)
END DO
!!
IF (ALLOCATED(avec)) DEALLOCATE (avec)
END PROCEDURE elemsd_GetHRGNParam3

!----------------------------------------------------------------------------
!                                                               getHRGNParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHRGNParam4
REAL(DFP), ALLOCATABLE :: ans(:, :)
!!
CALL GetHRGNParam(obj=obj, h=ans, val=val, opt=opt)
!!
h = QuadratureVariable( &
  & ans, &
  & TypeFEVariableScalar, &
  & TypeFEVariableSpaceTime)
!!
IF (ALLOCATED(ans)) DEALLOCATE (ans)
END PROCEDURE elemsd_GetHRGNParam4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE HRGNParamMethods
