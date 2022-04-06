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
!                                                                  STCM_7a
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_7a(ans, test, trial, term1, term2, c, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_x_all
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_x_all, del_x, del_y, del_z
  TYPE(FEVariable_), INTENT(IN) :: c
  !! vector varible
  INTEGER( I4B ), INTENT( IN ) :: opt
  !! 1
  !! 2
  !!
  !! Define internal variables
  !!
  INTEGER(I4B) :: ips, ipt, a, b, ii
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: p(:, :, :)
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  !!
  !! main
  !!
  if( opt .eq. 1 ) then
    CALL Reallocate(m6, &
      & SIZE(test(1)%N, 1), &
      & SIZE(trial(1)%N, 1), &
      & trial(1)%refelem%nsd, 1, &
      & SIZE(test(1)%T), &
      & SIZE(trial(1)%T))
    !!
    DO ipt = 1, SIZE(trial)
      !!
      realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
        & trial(ipt)%wt * trial(ipt)%jt
      !!
      CALL GetProjectionOfdNTdXt(obj=trial(ipt), cdNTdXt=p, val=c)
      !!
      DO ips = 1, SIZE(realval)
        DO b = 1, SIZE(m6, 6)
          DO a = 1, SIZE(m6, 5)
            DO ii = 1, SIZE(m6, 3)
              m6(:, :, ii, 1, a, b) = m6(:, :, ii, 1, a, b) &
                & + realval(ips) &
                & * outerprod( &
                & test(ipt)%dNTdXt(:, a, ii, ips), &
                & p(:, b, ips))
            END DO
          END DO
        END DO
      END DO
      !!
    END DO
  else
    CALL Reallocate(m6, &
      & SIZE(test(1)%N, 1), &
      & SIZE(trial(1)%N, 1), &
      & 1, trial(1)%refelem%nsd, &
      & SIZE(test(1)%T), &
      & SIZE(trial(1)%T))
    !!
    DO ipt = 1, SIZE(trial)
      !!
      realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
        & trial(ipt)%wt * trial(ipt)%jt
      !!
      CALL GetProjectionOfdNTdXt(obj=trial(ipt), cdNTdXt=p, val=c)
      !!
      DO ips = 1, SIZE(realval)
        DO b = 1, SIZE(m6, 6)
          DO a = 1, SIZE(m6, 5)
            DO ii = 1, SIZE(m6, 4)
              m6(:, :, 1, ii, a, b) = m6(:, :, 1, ii, a, b) &
                & + realval(ips) &
                & * outerprod( &
                & test(ipt)%dNTdXt(:, a, ii, ips), &
                & p(:, b, ips))
            END DO
          END DO
        END DO
      END DO
      !!
    END DO
  end if
  !!
  CALL Convert(from=m6, to=ans)
  !!
  DEALLOCATE (m6, realval, p)
END SUBROUTINE STCM_7a

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_7b(ans, test, trial, term1, term2, c, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_x, del_y, del_z
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_x_all
  TYPE(FEVariable_), INTENT(IN) :: c
  !! vector varible
  INTEGER( I4B ), INTENT( IN ) :: opt
  !!
  !! Define internal variables
  !!
  INTEGER(I4B) :: ips, ipt, a, b, ii
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: p(:, :, :)
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  !!
  !! main
  !!
  if( opt .eq. 1 ) then
    CALL Reallocate(m6, &
      & SIZE(test(1)%N, 1), &
      & SIZE(trial(1)%N, 1), &
      & trial(1)%refelem%nsd, 1, &
      & SIZE(test(1)%T), &
      & SIZE(trial(1)%T))
    !!
    DO ipt = 1, SIZE(trial)
      !!
      realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
        & trial(ipt)%wt * trial(ipt)%jt
      !!
      CALL GetProjectionOfdNTdXt(obj=test(ipt), cdNTdXt=p, val=c)
      !!
      DO ips = 1, SIZE(realval)
        DO b = 1, SIZE(m6, 6)
          DO a = 1, SIZE(m6, 5)
            DO ii = 1, SIZE(m6, 3)
              m6(:, :, ii, 1, a, b) = m6(:, :, ii, 1, a, b) &
                & + realval(ips) &
                & * outerprod( &
                & p(:, a, ips), &
                & trial(ipt)%dNTdXt(:, b, ii, ips))
            END DO
          END DO
        END DO
      END DO
      !!
    END DO
  else
    CALL Reallocate(m6, &
      & SIZE(test(1)%N, 1), &
      & SIZE(trial(1)%N, 1), &
      & 1, trial(1)%refelem%nsd, &
      & SIZE(test(1)%T), &
      & SIZE(trial(1)%T))
    !!
    DO ipt = 1, SIZE(trial)
      !!
      realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
        & trial(ipt)%wt * trial(ipt)%jt
      !!
      CALL GetProjectionOfdNTdXt(obj=test(ipt), cdNTdXt=p, val=c)
      !!
      DO ips = 1, SIZE(realval)
        DO b = 1, SIZE(m6, 6)
          DO a = 1, SIZE(m6, 5)
            DO ii = 1, SIZE(m6, 4)
              m6(:, :, 1, ii, a, b) = m6(:, :, 1, ii, a, b) &
                & + realval(ips) &
                & * outerprod( &
                & p(:, a, ips), &
                & trial(ipt)%dNTdXt(:, b, ii, ips))
            END DO
          END DO
        END DO
      END DO
      !!
    END DO
  end if
  !!
  CALL Convert(from=m6, to=ans)
  !!
  DEALLOCATE (m6, realval, p)
END SUBROUTINE STCM_7b
