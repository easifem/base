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
!                                                                  STCM_13a
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_13a(ans, test, trial, term1, term2, c, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_x_all, del_x, del_y, del_z
  TYPE(FEVariable_), INTENT(IN) :: c
  !! vector varible
  INTEGER(I4B), INTENT(IN) :: opt
  !! opt = 1
  !!
  !! Define internal variables
  !!
  INTEGER(I4B) :: ips, ipt, a, b, ii, jj
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  !!
  !! main
  !!
  CALL GetInterpolation(obj=trial, interpol=vbar, val=c)
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
      & trial(ipt)%wt * trial(ipt)%jt
    !!
    DO ips = 1, SIZE(realval)
      DO b = 1, SIZE(m6, 6)
        DO a = 1, SIZE(m6, 5)
          DO jj = 1, SIZE(m6, 4)
            DO ii = 1, SIZE(m6, 3)
              m6(:, :, ii, jj, a, b) = m6(:, :, ii, jj, a, b) &
                & + realval(ips) * test(ipt)%T(a) * vbar(ii, ips, ipt) &
                & * outerprod( &
                & test(ipt)%N(:, ips), &
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
  DEALLOCATE (m6, realval, vbar)
  !!
END SUBROUTINE STCM_13a

!----------------------------------------------------------------------------
!                                                                  STCM_13b
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_13b(ans, test, trial, term1, term2, c, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_x_all, del_x, del_y, del_z
  TYPE(FEVariable_), INTENT(IN) :: c
  !! vector varible
  INTEGER(I4B), INTENT(IN) :: opt
  !! opt = 2
  !!
  !! Define internal variables
  !!
  INTEGER(I4B) :: ips, ipt, a, b, ii, jj
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  !!
  !! main
  !!
  CALL GetInterpolation(obj=trial, interpol=vbar, val=c)
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
      & trial(ipt)%wt * trial(ipt)%jt
    !!
    DO ips = 1, SIZE(realval)
      DO b = 1, SIZE(m6, 6)
        DO a = 1, SIZE(m6, 5)
          DO jj = 1, SIZE(m6, 4)
            DO ii = 1, SIZE(m6, 3)
              m6(:, :, ii, jj, a, b) = m6(:, :, ii, jj, a, b) &
                & + realval(ips) * test(ipt)%T(a) * vbar(jj, ips, ipt) &
                & * outerprod( &
                & test(ipt)%N(:, ips), &
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
  DEALLOCATE (m6, realval, vbar)
  !!
END SUBROUTINE STCM_13b

!----------------------------------------------------------------------------
!                                                                  STCM_13c
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_13c(ans, test, trial, term1, term2, c, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_x_all, del_x, del_y, del_z
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_none
  TYPE(FEVariable_), INTENT(IN) :: c
  !! vector varible
  INTEGER(I4B), INTENT(IN) :: opt
  !! opt = 1
  !!
  !! Define internal variables
  !!
  INTEGER(I4B) :: ips, ipt, a, b, ii, jj
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  !!
  !! main
  !!
  CALL GetInterpolation(obj=trial, interpol=vbar, val=c)
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
      & trial(ipt)%wt * trial(ipt)%jt
    !!
    DO ips = 1, SIZE(realval)
      DO b = 1, SIZE(m6, 6)
        DO a = 1, SIZE(m6, 5)
          DO jj = 1, SIZE(m6, 4)
            DO ii = 1, SIZE(m6, 3)
              m6(:, :, ii, jj, a, b) = m6(:, :, ii, jj, a, b) &
                & + realval(ips) * trial(ipt)%T(b) * vbar(ii, ips, ipt) &
                & * outerprod( &
                & test(ipt)%dNTdXt(:, a, jj, ips), &
                & trial(ipt)%N(:, ips))
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
  DEALLOCATE (m6, realval, vbar)
  !!
END SUBROUTINE STCM_13c

!----------------------------------------------------------------------------
!                                                                  STCM_13c
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_13d(ans, test, trial, term1, term2, c, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_x_all, del_x, del_y, del_z
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_none
  TYPE(FEVariable_), INTENT(IN) :: c
  !! vector varible
  INTEGER(I4B), INTENT(IN) :: opt
  !! opt = 1
  !!
  !! Define internal variables
  !!
  INTEGER(I4B) :: ips, ipt, a, b, ii, jj
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  !!
  !! main
  !!
  CALL GetInterpolation(obj=trial, interpol=vbar, val=c)
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
      & trial(ipt)%wt * trial(ipt)%jt
    !!
    DO ips = 1, SIZE(realval)
      DO b = 1, SIZE(m6, 6)
        DO a = 1, SIZE(m6, 5)
          DO jj = 1, SIZE(m6, 4)
            DO ii = 1, SIZE(m6, 3)
              m6(:, :, ii, jj, a, b) = m6(:, :, ii, jj, a, b) &
                & + realval(ips) * trial(ipt)%T(b) * vbar(jj, ips, ipt) &
                & * outerprod( &
                & test(ipt)%dNTdXt(:, a, ii, ips), &
                & trial(ipt)%N(:, ips))
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
  DEALLOCATE (m6, realval, vbar)
  !!
END SUBROUTINE STCM_13d

