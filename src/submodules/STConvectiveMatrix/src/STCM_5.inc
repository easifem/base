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
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_5a(ans, test, trial, term1, term2, c, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_t
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_x_all
  TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: c
  !! Scalar varible
  INTEGER( I4B ), INTENT( IN ) :: opt
  !! 1
  !! 2
  !!
  !! Define internal variables
  !!
  !!
  INTEGER(I4B) :: ips, ipt, a, b, ii
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: cbar(:, :)
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  !!
  !! main
  !!
  !! make c bar at ips and ipt
  !!
  IF (PRESENT(c)) THEN
    CALL GetInterpolation(obj=trial, val=c, interpol=cbar)
  ELSE
    CALL Reallocate(cbar, SIZE(trial(1)%N, 2), SIZE(trial))
    cbar = 1.0_DFP
  END IF
  !!
  IF( opt .EQ. 1 ) THEN
    !!
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
        & trial(ipt)%wt * trial(ipt)%jt * cbar(:, ipt)
      !!
      DO ips = 1, SIZE(realval)
        DO b = 1, SIZE(m6, 6)
          DO a = 1, SIZE(m6, 5)
            DO ii = 1, SIZE(m6, 3)
              m6(:, :, ii, 1, a, b) = m6(:, :, ii, 1, a, b) &
                & + realval(ips) &
                & * outerprod( &
                & test(ipt)%dNTdT(:, a, ips), &
                & trial(ipt)%dNTdXt(:, b, ii, ips))
            END DO
          END DO
        END DO
      END DO
      !!
    END DO
    !!
  ELSE
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
        & trial(ipt)%wt * trial(ipt)%jt * cbar(:, ipt)
      !!
      DO ips = 1, SIZE(realval)
        DO b = 1, SIZE(m6, 6)
          DO a = 1, SIZE(m6, 5)
            DO ii = 1, SIZE(m6, 4)
              m6(:, :, 1, ii, a, b) = m6(:, :, 1, ii, a, b) &
                & + realval(ips) &
                & * outerprod( &
                & test(ipt)%dNTdT(:, a, ips), &
                & trial(ipt)%dNTdXt(:, b, ii, ips))
            END DO
          END DO
        END DO
      END DO
      !!
    END DO
    !!
  END IF
  !!
  CALL Convert(from=m6, to=ans)
  !!
  DEALLOCATE (m6, realval, cbar)
  !!
END SUBROUTINE STCM_5a

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_5b(ans, test, trial, term1, term2, c, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_x_all
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_t
  TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: c
  !! Scalar variable
  INTEGER( I4B ), INTENT( IN ) :: opt
  !! 1
  !! 2
  !! Define internal variables
  INTEGER(I4B) :: ips, ipt, a, b, ii
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: cbar(:, :)
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  !!
  !! main
  !!
  !! make c bar at ips and ipt
  !!
  IF (PRESENT(c)) THEN
    CALL GetInterpolation(obj=trial, val=c, interpol=cbar)
  ELSE
    CALL Reallocate(cbar, SIZE(trial(1)%N, 2), SIZE(trial))
    cbar = 1.0_DFP
  END IF
  !!
  IF( opt .EQ. 1 ) THEN
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
        & trial(ipt)%wt * trial(ipt)%jt * cbar(:, ipt)
      !!
      DO ips = 1, SIZE(realval)
        DO b = 1, SIZE(m6, 6)
          DO a = 1, SIZE(m6, 5)
            DO ii = 1, SIZE(m6, 3)
              m6(:, :, ii, 1, a, b) = m6(:, :, ii, 1, a, b) &
                & + realval(ips) &
                & * outerprod( &
                & test(ipt)%dNTdXt(:, a, ii, ips), &
                & trial(ipt)%dNTdt(:, b, ips))
            END DO
          END DO
        END DO
      END DO
      !!
    END DO
  ELSE
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
        & trial(ipt)%wt * trial(ipt)%jt * cbar(:, ipt)
      !!
      DO ips = 1, SIZE(realval)
        DO b = 1, SIZE(m6, 6)
          DO a = 1, SIZE(m6, 5)
            DO ii = 1, SIZE(m6, 4)
              m6(:, :, 1, ii, a, b) = m6(:, :, 1, ii, a, b) &
                & + realval(ips) &
                & * outerprod( &
                & test(ipt)%dNTdXt(:, a, ii, ips), &
                & trial(ipt)%dNTdt(:, b, ips))
            END DO
          END DO
        END DO
      END DO
      !!
    END DO
  END IF
  !!
  CALL Convert(from=m6, to=ans)
  !!
  DEALLOCATE (m6, realval, cbar)
  !!
END SUBROUTINE STCM_5b
