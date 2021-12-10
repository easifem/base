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
!                                                             getUnitNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE getUnitNormal_1
#include "./getUnitNormal_1.inc"
END PROCEDURE getUnitNormal_1

!----------------------------------------------------------------------------
!                                                             getUnitNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE getUnitNormal_2
#include "./getUnitNormal_2.inc"
END PROCEDURE getUnitNormal_2

!----------------------------------------------------------------------------
!                                                             getUnitNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE getUnitNormal_3
IF (val%rank .EQ. scalar) THEN
  CALL scalar_getUnitNormal_3(obj=obj, r=r, val=val)
ELSEIF (val%rank .EQ. vector) THEN
  CALL vector_getUnitNormal_3(obj=obj, r=r, val=val)
END IF
END PROCEDURE getUnitNormal_3

!----------------------------------------------------------------------------
!                                                             getUnitNormal
!----------------------------------------------------------------------------

PURE SUBROUTINE scalar_getUnitNormal_3(obj, r, val)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: r(:, :)
  TYPE(FEVariable_), INTENT(IN) :: val
#include "./getUnitNormal_1.inc"
END SUBROUTINE scalar_getUnitNormal_3

!----------------------------------------------------------------------------
!                                                             getUnitNormal
!----------------------------------------------------------------------------

PURE SUBROUTINE vector_getUnitNormal_3(obj, r, val)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: r(:, :)
  TYPE(FEVariable_), INTENT(IN) :: val
#include "./getUnitNormal_2.inc"
END SUBROUTINE vector_getUnitNormal_3

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getSUPGParam
SELECT TYPE (obj)
TYPE is (ElemshapeData_)
  CALL elemsd_getSUPGParam_a(obj=obj, tau=tau, c=c, val=val, nu=nu, &
       & dt=dt, opt=opt)
CLASS is (STElemshapeData_)
  CALL elemsd_getSUPGParam_b(obj=obj, tau=tau, c=c, val=val, nu=nu, &
       & dt=dt, opt=opt)
END SELECT
END PROCEDURE elemsd_getSUPGParam

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE elemsd_getSUPGParam_a(obj, tau, c, val, nu, dt, opt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  !! element shape data
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  !! stabilizing parameters
  TYPE(FEVariable_), INTENT(IN) :: c
  !! convective velocity
  !! vector variable
  TYPE(FEVariable_), INTENT(IN) :: val
  !! solution
  TYPE(FEVariable_), INTENT(IN) :: nu
  !! diffusivity coefficient
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !! time step size
  !! default value is zero
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  !! define internal variables
  !! futuristic option to implement other definitions
  INTEGER(I4B) :: ii
  REAL(DFP) :: t1, t2, t3
  REAL(DFP), ALLOCATABLE :: p(:, :)
  !! cdNdXt
  REAL(DFP), ALLOCATABLE :: r(:, :)
  !! unit normal
  REAL(DFP), ALLOCATABLE :: q(:, :)
  !! rdNdXt
  REAL(DFP), ALLOCATABLE :: ans(:)
  !! result
  REAL(DFP), ALLOCATABLE :: nubar(:)
  !! value of nu at space quadrature points
  TYPE(FEVariable_) :: rvar
  !! vector variable for keeping r
  !!
  !! MAIN
  !!
  CALL GetProjectionOfdNdXt(obj=obj, cdNdXt=p, val=c)
  CALL GetUnitNormal(obj=obj, val=val, r=r)
  rvar = QuadratureVariable(r, TypeFEVariableVector, TypeFEVariableSpace)
  CALL GetProjectionOfdNdXt(obj=obj, cdNdXt=q, val=rvar)
  CALL GetInterpolation(obj=obj, val=nu, interpol=nubar)
  !!
  IF (PRESENT(dt)) THEN
    IF (dt .GT. zero) THEN
      t2 = 2.0_DFP / dt
    ELSE
      t2 = 0.0_DFP
    END IF
  ELSE
    t2 = 0.0_DFP
  END IF
  !!
  CALL Reallocate(ans, SIZE(obj%N, 2))
  DO ii = 1, SIZE(ans)
    t1 = SUM(ABS(p(:, ii)))
    t3 = nubar(ii) * (SUM(ABS(q(:, ii))))**2
    ans(ii) = SQRT(1.0_DFP / (t1**2 + t2**2 + t3**2))
  END DO
  tau = QuadratureVariable(ans, TypeFEVariableScalar, TypeFEVariableSpace)
  !! cleanup
  IF (ALLOCATED(p)) DEALLOCATE (p)
  IF (ALLOCATED(r)) DEALLOCATE (r)
  IF (ALLOCATED(q)) DEALLOCATE (q)
  IF (ALLOCATED(ans)) DEALLOCATE (ans)
  IF (ALLOCATED(nubar)) DEALLOCATE (nubar)
  CALL DEALLOCATE (rvar)
END SUBROUTINE elemsd_getSUPGParam_a

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE elemsd_getSUPGParam_b(obj, tau, c, val, nu, dt, opt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  !! space-time element shape data
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  !! stabilization parameter
  TYPE(FEVariable_), INTENT(IN) :: c
  !! convective velocity
  !! vector variable
  TYPE(FEVariable_), INTENT(IN) :: val
  !! solution
  !! scalar/vector variable
  TYPE(FEVariable_), INTENT(IN) :: nu
  !! diffusivity
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !! time-step size
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  !! futuristic
  !!
  !! INTERNAL VARIABLES
  !!
  INTEGER(I4B) :: ii, jj
  REAL(DFP) :: t12, t3
  REAL(DFP), ALLOCATABLE :: p(:, :, :)
  !! cdNTdxt
  REAL(DFP), ALLOCATABLE :: r(:, :)
  !! vector at space quad points
  REAL(DFP), ALLOCATABLE :: q(:, :, :)
  !!
  REAL(DFP), ALLOCATABLE :: ans(:)
  REAL(DFP), ALLOCATABLE :: nubar(:)
  TYPE(FEVariable_) :: rvar
  !!
  !! MAIN
  !!
  CALL GetProjectionOfdNTdXt(obj=obj, cdNTdXt=p, val=c)
  !! make cdNTdxt + dNTdt
  p = p + obj%dNTdt
  CALL GetUnitNormal(obj=obj, val=val, r=r)
  rvar = QuadratureVariable(r, TypeFEVariableVector, TypeFEVariableSpace)
  CALL GetProjectionOfdNTdXt(obj=obj, cdNTdXt=q, val=rvar)
  CALL GetInterpolation(obj=obj, val=nu, interpol=nubar)
  !!
  CALL reallocate(ans, SIZE(obj%N, 2))
  DO ii = 1, SIZE(ans, 1)
    t12 = SUM(ABS(p(:, :, ii)))
    t3 = nubar(ii) * (SUM(ABS(q(:, :, ii))))**2
    ans(ii) = SQRT(1.0_DFP / (t12**2 + t3**2))
  END DO
  tau = QuadratureVariable(ans, TypeFEVariableScalar, TypeFEVariableSpace)
  !! cleanup
  IF (ALLOCATED(p)) DEALLOCATE (p)
  IF (ALLOCATED(r)) DEALLOCATE (r)
  IF (ALLOCATED(q)) DEALLOCATE (q)
  IF (ALLOCATED(ans)) DEALLOCATE (ans)
  IF (ALLOCATED(nubar)) DEALLOCATE (nubar)
  CALL DEALLOCATE (rvar)
END SUBROUTINE elemsd_getSUPGParam_b

!----------------------------------------------------------------------------
!                                                             GetSUPGParam
!----------------------------------------------------------------------------

MODULE PROCEDURE stelemsd_GetSUPGParam
INTEGER(I4B) :: ii
REAL(DFP), ALLOCATABLE :: ans(:, :)
TYPE(FEVariable_) :: a
!! main
CALL Reallocate(ans, SIZE(obj(1)%N, 2), SIZE(obj))
DO ii = 1, SIZE(obj)
  CALL GetSUPGParam(obj=obj(ii), tau=a, c=c, val=val, nu=nu, dt=dt, opt=opt)
  ans(:, ii) = Get(a, TypeFEVariableScalar, TypeFEVariableSpace)
END DO
!!
tau = QuadratureVariable(ans, TypeFEVariableScalar, TypeFEVariableSpaceTime)
!!
CALL DEALLOCATE (a)
DEALLOCATE (ans)
END PROCEDURE stelemsd_GetSUPGParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE getMethod
