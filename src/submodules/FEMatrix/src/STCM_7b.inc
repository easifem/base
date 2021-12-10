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

! $$
! M\left(I,J,a,b\right)=\int_{I_{n}}\int_{\Omega}\rho\frac{\partial N^{I}T_{a}}
! {\partial x_{i}}c_{p}\frac{\partial N^{J}T_{b}}{\partial x_{p}}d\Omega dt\
! $$
!
! $$
! M\left(I,J,a,b\right)=\int_{I_{n}}\int_{\Omega}\rho c_{p}\frac{\partial
! N^{I}T_{a}}{\partial x_{p}}\frac{\partial N^{J}T_{b}}{\partial x_{i}}d\Omega dt
! $$
!
PURE SUBROUTINE STCM_7b(ans, test, trial, term1, term2, rho, c, projectOn)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_x_all, or (del_x, del_y, del_z)
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_x_all, or (del_x, del_y, del_z)
  TYPE(FEVariable_), INTENT(IN) :: rho
  !! scalar varible
  TYPE(FEVariable_), INTENT(IN) :: c
  !! vector varible
  CHARACTER(LEN=*), INTENT(IN) :: projectOn
  !! "test"
  !! "trial"
  !!
  !!
  !! Define internal variables
  INTEGER(I4B) :: nsd, ll, kk
  REAL(DFP), ALLOCATABLE, DIMENSION(:, :, :, :) :: m1, m2, m3
  !!
  !! main
  !!
  nsd = trial(1)%refelem%nsd
  SELECT CASE (nsd)
  !!
  !! one dimensional case
  !!
  CASE (1)
    !!
    !! projection on test
    !!
    IF (TRIM(projectOn) .EQ. "test") THEN
      CALL STCM_6b(ans=ans, test=test, trial=trial, &
           & term1=term1, term2=del_x, rho=rho, c=c, &
           & projectOn='test')
    !!
    !! projection on trial
    !!
    ELSEIF (TRIM(projectOn) .EQ. "trial") THEN
      CALL STCM_6b(ans=ans, test=test, trial=trial, &
           & term1=del_x, term2=term2, rho=rho, c=c, &
           & projectOn='trial')
    END IF
  !!
  !! two dimensional case
  !!
  CASE (2)
    !!
    !! projection on test
    !!
    IF (TRIM(projectOn) .EQ. "test") THEN
      CALL STCM_6b(ans=m1, test=test, trial=trial, &
           & term1=term1, term2=del_x, rho=rho, c=c, &
           & projectOn='test')
      CALL STCM_6b(ans=m2, test=test, trial=trial, &
           & term1=term1, term2=del_y, rho=rho, c=c, &
           & projectOn='test')
      CALL Reallocate(ans, SIZE(m1, 1), nsd * SIZE(m1, 2), &
           & SIZE(m1, 3), SIZE(m1, 4))
      DO ll = 1, SIZE(ans, 4)
        DO kk = 1, SIZE(ans, 3)
         ans(:, :, kk, ll) = ColConcat(a=m1(:, :, kk, ll), b=m2(:, :, kk, ll))
        END DO
      END DO
    !!
    !! projection on trial
    !!
    ELSEIF (TRIM(projectOn) .EQ. "trial") THEN
      CALL STCM_6b(ans=m1, test=test, trial=trial, &
           & term1=del_x, term2=term2, rho=rho, c=c, &
           & projectOn='trial')
      CALL STCM_6b(ans=m2, test=test, trial=trial, &
           & term1=del_y, term2=term2, rho=rho, c=c, &
           & projectOn='trial')
      !!
      CALL Reallocate(ans, nsd * SIZE(m1, 1), SIZE(m1, 2), &
           & SIZE(m1, 3), SIZE(m1, 4))
      DO ll = 1, SIZE(ans, 4)
        DO kk = 1, SIZE(ans, 3)
         ans(:, :, kk, ll) = RowConcat(a=m1(:, :, kk, ll), b=m2(:, :, kk, ll))
        END DO
      END DO
    END IF
    DEALLOCATE (m1, m2)
  !!
  !! tree dimensional case
  !!
  CASE (3)
    !!
    !! projection on test
    !!
    IF (TRIM(projectOn) .EQ. "test") THEN
      CALL STCM_6b(ans=m1, test=test, trial=trial, &
           & term1=term1, term2=del_x, c=c, rho=rho, &
           & projectOn='test')
      CALL STCM_6b(ans=m2, test=test, trial=trial, &
           & term1=term1, term2=del_y, c=c, rho=rho, &
           & projectOn='test')
      CALL STCM_6b(ans=m3, test=test, trial=trial, &
           & term1=term1, term2=del_z, c=c, rho=rho, &
           & projectOn='test')
      CALL Reallocate(ans, SIZE(m1, 1), nsd * SIZE(m1, 2), &
           & SIZE(m1, 3), SIZE(m1, 4))
      DO ll = 1, SIZE(ans, 4)
        DO kk = 1, SIZE(ans, 3)
          ans(:, :, kk, ll) = ColConcat( &
               & a=ColConcat(a=m1(:, :, kk, ll), b=m2(:, :, kk, ll)), &
               & b=m3(:, :, kk, ll))
        END DO
      END DO
    !!
    !! projectin on trial
    !!
    ELSEIF (TRIM(projectOn) .EQ. "trial") THEN
      CALL STCM_6b(ans=m1, test=test, trial=trial, &
           & term1=del_x, term2=term2, c=c, rho=rho,&
           & projectOn='trial')
      CALL STCM_6b(ans=m2, test=test, trial=trial, &
           & term1=del_y, term2=term2, c=c, rho=rho, &
           & projectOn='trial')
      CALL STCM_6b(ans=m3, test=test, trial=trial, &
           & term1=del_z, term2=term2, c=c, rho=rho, &
           & projectOn='trial')
      CALL Reallocate(ans, nsd * SIZE(m1, 1), SIZE(m1, 2), &
           & SIZE(m1, 3), SIZE(m1, 4))
      DO ll = 1, SIZE(ans, 4)
        DO kk = 1, SIZE(ans, 3)
          ans(:, :, kk, ll) = RowConcat( &
               & a=RowConcat(a=m1(:, :, kk, ll), b=m2(:, :, kk, ll)), &
               & b=m3(:, :, kk, ll))
        END DO
      END DO
    END IF
    DEALLOCATE (m1, m2, m3)
  END SELECT
END SUBROUTINE STCM_7b
