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

!----------------------------------------------------------------------------
!                                                                  STCM_17a
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_17a(ans, test, trial, term1, term2, rho, c, &
  & opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_t
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_x_all, del_x, del_y, del_z
  TYPE(FEVariable_), INTENT(IN) :: rho
  !! scalar
  TYPE(FEVariable_), INTENT(IN) :: c
  !! vector varible
  INTEGER(I4B), INTENT(IN) :: opt
  !! opt = 1
  !! projecton test
  !!
  !! Define internal variables
  !!
  INTEGER(I4B) :: ips, ipt, a, b, ii, jj
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: p(:, :, :)
  REAL(DFP), ALLOCATABLE :: rhobar(:, :)
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  !!
  !! main
  !!
  CALL GetInterpolation(obj=trial, interpol=vbar, val=c)
  !!
  CALL GetInterpolation(obj=trial, interpol=rhobar, val=rho)
  !!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & SIZE(vbar, 1), trial(1)%nsd, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
      & trial(ipt)%wt * trial(ipt)%jt * rhobar(:,ipt)
    !!
    CALL GetProjectionOfdNTdXt(obj=test(ipt), cdNTdXt=p, val=c)
    !!
    DO ips = 1, SIZE(realval)
      DO b = 1, SIZE(m6, 6)
        DO a = 1, SIZE(m6, 5)
          DO jj = 1, SIZE(m6, 4)
            DO ii = 1, SIZE(m6, 3)
              m6(:, :, ii, jj, a, b) = m6(:, :, ii, jj, a, b) &
                & + realval(ips) * vbar(ii, ips, ipt) &
                & * outerprod( &
                & p(:, a, ips), &
                & trial(ipt)%dNTdXt(:, b, jj, ips))
            END DO
          END DO
        END DO
      END DO
    END DO
    !!
  END DO
  !!
  CALL Convert(from=m6, to=ans)
  !!
  DEALLOCATE (m6, realval, vbar, rhobar, p)
  !!
END SUBROUTINE STCM_17a

!----------------------------------------------------------------------------
!                                                                  STCM_17b
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_17b(ans, test, trial, term1, term2, rho, c, &
  & opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_x_all, del_x, del_y, del_z
  TYPE(FEVariable_), INTENT(IN) :: rho
  !!
  TYPE(FEVariable_), INTENT(IN) :: c
  !! vector varible
  INTEGER(I4B), INTENT(IN) :: opt
  !! opt = 2
  !! projecton test
  !!
  !! Define internal variables
  !!
  INTEGER(I4B) :: ips, ipt, a, b, ii, jj
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: p(:, :, :)
  REAL(DFP), ALLOCATABLE :: rhobar(:, :)
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  !!
  !! main
  !!
  CALL GetInterpolation(obj=trial, interpol=vbar, val=c)
  !!
  CALL GetInterpolation(obj=trial, interpol=rhobar, val=rho)
  !!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & trial(1)%nsd, SIZE(vbar, 1), &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
      & trial(ipt)%wt * trial(ipt)%jt * rhobar(:,ipt)
    !!
    CALL GetProjectionOfdNTdXt(obj=test(ipt), cdNTdXt=p, val=c)
    !!
    DO ips = 1, SIZE(realval)
      DO b = 1, SIZE(m6, 6)
        DO a = 1, SIZE(m6, 5)
          DO jj = 1, SIZE(m6, 4)
            DO ii = 1, SIZE(m6, 3)
              m6(:, :, ii, jj, a, b) = m6(:, :, ii, jj, a, b) &
                & + realval(ips) * vbar(jj, ips, ipt) &
                & * outerprod( &
                & p(:, a, ips), &
                & trial(ipt)%dNTdXt(:, b, ii, ips))
            END DO
          END DO
        END DO
      END DO
    END DO
    !!
  END DO
  !!
  CALL Convert(from=m6, to=ans)
  !!
  DEALLOCATE (m6, realval, vbar, rhobar, p)
  !!
END SUBROUTINE STCM_17b

!----------------------------------------------------------------------------
!                                                                  STCM_17c
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_17c(ans, test, trial, term1, term2, rho, c, &
  & opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_x_all, del_x, del_y, del_z
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_t
  TYPE(FEVariable_), INTENT(IN) :: rho
  !!
  TYPE(FEVariable_), INTENT(IN) :: c
  !! vector varible
  INTEGER(I4B), INTENT(IN) :: opt
  !! opt = 1
  !! projecton trial
  !!
  !! Define internal variables
  !!
  INTEGER(I4B) :: ips, ipt, a, b, ii, jj
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: p(:, :, :)
  REAL(DFP), ALLOCATABLE :: rhobar(:, :)
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  !!
  !! main
  !!
  CALL GetInterpolation(obj=trial, interpol=vbar, val=c)
  !!
  CALL GetInterpolation(obj=trial, interpol=rhobar, val=rho)
  !!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & SIZE(vbar, 1), trial(1)%nsd, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
      & trial(ipt)%wt * trial(ipt)%jt * rhobar(:,ipt)
    !!
    CALL GetProjectionOfdNTdXt(obj=trial(ipt), cdNTdXt=p, val=c)
    !!
    DO ips = 1, SIZE(realval)
      DO b = 1, SIZE(m6, 6)
        DO a = 1, SIZE(m6, 5)
          DO jj = 1, SIZE(m6, 4)
            DO ii = 1, SIZE(m6, 3)
              m6(:, :, ii, jj, a, b) = m6(:, :, ii, jj, a, b) &
                & + realval(ips) * vbar(ii, ips, ipt) &
                & * outerprod( &
                & test(ipt)%dNTdXt(:, a, jj, ips), &
                & p(:, b, ips))
            END DO
          END DO
        END DO
      END DO
    END DO
    !!
  END DO
  !!
  CALL Convert(from=m6, to=ans)
  !!
  DEALLOCATE (m6, realval, vbar, rhobar, p)
  !!
END SUBROUTINE STCM_17c

!----------------------------------------------------------------------------
!                                                                  STCM_17c
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_17d(ans, test, trial, term1, term2, rho, c, &
  & opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_x_all, del_x, del_y, del_z
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_t
  TYPE(FEVariable_), INTENT(IN) :: rho
  !!
  TYPE(FEVariable_), INTENT(IN) :: c
  !! vector varible
  INTEGER(I4B), INTENT(IN) :: opt
  !! opt = 2
  !! projecton trial
  !!
  !! Define internal variables
  !!
  INTEGER(I4B) :: ips, ipt, a, b, ii, jj
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: p(:, :, :)
  REAL(DFP), ALLOCATABLE :: rhobar(:, :)
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  !!
  !! main
  !!
  CALL GetInterpolation(obj=trial, interpol=vbar, val=c)
  CALL GetInterpolation(obj=trial, interpol=rhobar, val=rho)
  !!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & trial(1)%nsd, SIZE(vbar, 1), &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
      & trial(ipt)%wt * trial(ipt)%jt * rhobar(:,ipt)
    !!
    CALL GetProjectionOfdNTdXt(obj=trial(ipt), cdNTdXt=p, val=c)
    !!
    DO ips = 1, SIZE(realval)
      DO b = 1, SIZE(m6, 6)
        DO a = 1, SIZE(m6, 5)
          DO jj = 1, SIZE(m6, 4)
            DO ii = 1, SIZE(m6, 3)
              m6(:, :, ii, jj, a, b) = m6(:, :, ii, jj, a, b) &
                & + realval(ips) * vbar(jj, ips, ipt) &
                & * outerprod( &
                & test(ipt)%dNTdXt(:, a, ii, ips), &
                & p(:, b, ips))
            END DO
          END DO
        END DO
      END DO
    END DO
    !!
  END DO
  !!
  CALL Convert(from=m6, to=ans)
  !!
  DEALLOCATE (m6, realval, vbar, rhobar, p)
  !!
END SUBROUTINE STCM_17d

