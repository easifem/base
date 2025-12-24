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

SUBMODULE(ElemshapeData_DivergenceMethods) Methods
USE ContractionUtility, ONLY: Contraction

USE SwapUtility, ONLY: Swap

USE ReallocateUtility, ONLY: Reallocate

USE FEVariable_Method, ONLY: QuadratureVariable, NodalVariable, shape, Get

USE Basetype, ONLY: TypeFEVariableOpt, TypeFEVariableScalar, &
         TypeFEVariableVector, TypeFEVariableMatrix, TypeFEVariableConstant, &
              TypeFEVariableSpace, TypeFEVariableTime, TypeFEVariableSpaceTime

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             GetDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetDivergence_1
INTEGER(I4B) :: ii, jj, ips

tsize = obj%nips

DO ips = 1, tsize
  ans(ips) = 0.0_DFP

  DO jj = 1, obj%nns
    DO ii = 1, obj%nsd
      ans(ips) = ans(ips) + val(ii, jj) * obj%dNdXt(jj, ii, ips)
    END DO
  END DO
END DO

END PROCEDURE elemsd_GetDivergence_1

!----------------------------------------------------------------------------
!                                                             GetDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetDivergence_2
INTEGER(I4B) :: ips, I, ii, a, ips

tsize = obj%nips

SELECT TYPE (obj); TYPE is (STElemShapeData_)

  DO ips = 1, tsize
    ans(ips) = 0.0_DFP

    DO a = 1, obj%nnt
      DO I = 1, obj%nns
        DO ii = 1, obj%nsd
          ans(ips) = ans(ips) + val(ii, I, a) * obj%dNTdXt(I, a, ii, ips)
        END DO
      END DO
    END DO

  END DO

END SELECT

END PROCEDURE elemsd_GetDivergence_2

!----------------------------------------------------------------------------
!                                                              GetDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetDivergence_3
tsize = obj%nips

SELECT CASE (val%varType)
CASE (TypeFEVariableOpt%constant)
  ! CALL Reallocate(lg, SIZE(obj%N, 2))
  ans(1:tsize) = 0.0

CASE (TypeFEVariableOpt%space)
  CALL GetDivergence(obj=obj, ans=ans, tsize=tsize, &
                     Val=Get(val, TypeFEVariableVector, TypeFEVariableSpace))

CASE (TypeFEVariableOpt%spacetime)

  SELECT TYPE (obj); TYPE is (STElemShapeData_)

    CALL GetDivergence(obj=obj, ans=ans, tsize=tsize, &
                  Val=Get(val, TypeFEVariableVector, TypeFEVariableSpaceTime))
  END SELECT

END SELECT
END PROCEDURE elemsd_GetDivergence_3

!----------------------------------------------------------------------------
!                                                              GetDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetDivergence_4
INTEGER(I4B) :: ii, jj, ips, I

nrow = SIZE(val, 1)
ncol = obj%nips

DO ips = 1, ncol
  DO jj = 1, nrow

    ans(jj, ips) = 0.0_DFP

    DO I = 1, obj%nns
      DO ii = 1, obj%nsd
        ans(jj, ips) = ans(jj, ips) + val(ii, jj, I) * obj%dNdXt(I, ii, ips)
      END DO
    END DO
  END DO
END DO

END PROCEDURE elemsd_GetDivergence_4

!----------------------------------------------------------------------------
!                                                              GetDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetDivergence_5
INTEGER(I4B) :: ii, jj, ips, I, a

nrow = SIZE(val, 1)
ncol = obj%nips

SELECT TYPE (obj); TYPE IS (STElemShapeData_)

  DO ips = 1, ncol
    DO jj = 1, nrow

      ans(jj, ips) = 0.0_DFP

      DO a = 1, obj%nnt
        DO I = 1, obj%nns
          DO ii = 1, obj%nsd
            ans(jj, ips) = ans(jj, ips) + &
                           val(ii, jj, I, a) * obj%dNTdXt(I, a, ii, ips)
          END DO
        END DO
      END DO
    END DO
  END DO

END SELECT

END PROCEDURE elemsd_GetDivergence_5

!----------------------------------------------------------------------------
!                                                              GetDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetDivergence_6
INTEGER(I4B) :: s(2)

SELECT CASE (val%varType)

CASE (TypeFEVariableOpt%constant)
  s = SHAPE(val)
  ! CALL Reallocate(lg, s(1), SIZE(obj%N, 2))
  nrow = s(1)
  ncol = obj%nips
  ans(1:nrow, 1:ncol) = 0.0

CASE (TypeFEVariableOpt%space)
  CALL GetDivergence(obj=obj, ans=ans, nrow=nrow, ncol=ncol, &
                     Val=Get(val, TypeFEVariableMatrix, TypeFEVariableSpace))

CASE (TypeFEVariableOpt%spacetime)
  SELECT TYPE (obj); TYPE IS (STElemShapeData_)

    CALL GetDivergence(obj=obj, ans=ans, nrow=nrow, ncol=ncol, &
                  Val=Get(val, TypeFEVariableMatrix, TypeFEVariableSpaceTime))

  END SELECT

END SELECT
END PROCEDURE elemsd_GetDivergence_6

!----------------------------------------------------------------------------
!                                                              GetDivergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetDivergence_7
REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :)
INTEGER(I4B) :: ii, jj, s(2)

SELECT CASE (val%rank)

CASE (TypeFEVariableOpt%vector)
  ALLOCATE (r1(obj%nips))
  CALL GetDivergence(obj=obj, ans=r1, val=val, tsize=ii)
  ans = QuadratureVariable(r1, typeFEVariableScalar, typeFEVariableSpace)
  DEALLOCATE (r1)

CASE (TypeFEVariableOpt%matrix)
  s = SHAPE(val)
  ALLOCATE (r2(s(1), obj%nips))
  CALL GetDivergence(obj=obj, ans=r2, val=val, nrow=ii, ncol=jj)
  ans = QuadratureVariable(r2, typeFEVariableVector, typeFEVariableSpace)
  DEALLOCATE (r2)

END SELECT
END PROCEDURE elemsd_GetDivergence_7

!----------------------------------------------------------------------------
!                                                                Divergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetDivergence_8
REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :), r3(:, :, :)
INTEGER(I4B) :: ii, nipt, jj, kk, s(2)

nipt = SIZE(obj)

SELECT CASE (val%rank)

CASE (TypeFEVariableOpt%vector)

  ii = 0
  DO jj = 1, nipt
    IF (obj(jj)%nips .GT. ii) ii = obj(jj)%nips
  END DO

  ALLOCATE (r1(ii), r2(ii, nipt))

  DO ii = 1, nipt
    CALL GetDivergence(obj=obj(ii), ans=r1(1:obj(ii)%nips), val=val, tsize=jj)
    r2(1:obj(ii)%nips, ii) = r1(1:obj(ii)%nips)
  END DO

  ans = QuadratureVariable(r2(1:obj(ii)%nips, 1:nipt), typeFEVariableScalar, &
                           typeFEVariableSpaceTime)
  DEALLOCATE (r2, r1)

CASE (TypeFEVariableOpt%matrix)

  nipt = SIZE(obj)

  ii = 0
  DO jj = 1, nipt
    IF (obj(jj)%nips .GT. ii) ii = obj(jj)%nips
  END DO

  s = SHAPE(val)
  kk = s(1)

  ALLOCATE (r2(kk, ii), r3(kk, ii, nipt))

  DO ii = 1, nipt
    CALL GetDivergence(obj=obj(ii), ans=r2, val=val, nrow=jj, ncol=kk)
    r3(1:jj, 1:kk, ii) = r2(1:jj, 1:kk)
  END DO

  ans = QuadratureVariable(r3, typeFEVariableVector, typeFEVariableSpaceTime)
  DEALLOCATE (r2, r3)
END SELECT
END PROCEDURE elemsd_GetDivergence_8

!----------------------------------------------------------------------------
!                                                                Divergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_Divergence_1
CALL GetDivergence(obj=obj, ans=ans, val=val)
END PROCEDURE elemsd_Divergence_1

!----------------------------------------------------------------------------
!                                                                Divergence
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_Divergence_2
CALL GetDivergence(obj=obj, ans=ans, val=val)
END PROCEDURE elemsd_Divergence_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
