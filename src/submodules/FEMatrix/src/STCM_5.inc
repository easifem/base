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
! M(I,J,a,b)=\int_{I_{n}}\int_{\Omega}c\frac{\partial N^{I}T_{a}}{\partial x_{i}}\frac{\partial N^{J}T_{b}}{\partial t}d\Omega dt
! $$
!
! ```fortran
! mat2=ConvectiveMatrix( test, trial, term1=del_x_all, term2=del_t,c)
! ```
!
! $$
! M(I,J,a,b)=\int_{I_{n}}\int_{\Omega}c\frac{\partial N^{I}T_{a}}{\partial t}\frac{\partial N^{J}T_{b}}{\partial x_{i}}d\Omega dt
! $$
!
! ```fortran
! mat2=ConvectiveMatrix( test, trial, term1=del_t, term2=del_x_all,c)
! ```

PURE SUBROUTINE STCM_5(ans, test, trial, term1, term2, c)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_x_all, del_t
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_x_all, del_t
  TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: c
  !! Scalar varible
  !!
  !! Define internal variables
  !!
  INTEGER(I4B) :: nsd, ll, kk
  REAL(DFP), ALLOCATABLE, DIMENSION(:, :, :, :) :: m1, m2, m3
  !! main
  nsd = trial(1)%refelem%nsd
  SELECT CASE (nsd)
  CASE (1)
    CALL STCM_4(ans=ans, test=test, trial=trial, &
         & term1=term1, term2=term2, dim=1, c=c)
  CASE (2)
    CALL STCM_4(ans=m1, test=test, trial=trial, &
         & term1=term1, term2=term2, dim=1, c=c)
    CALL STCM_4(ans=m2, test=test, trial=trial, &
         & term1=term1, term2=term2, dim=2, c=c)
    !!
    !!
    !!
    IF (term1 .EQ. del_t .AND. term2 .NE. del_t) THEN
      CALL Reallocate(ans, SIZE(m1, 1), nsd * SIZE(m1, 2), &
           & SIZE(m1, 3), SIZE(m1, 4))
      DO ll = 1, SIZE(ans, 4)
        DO kk = 1, SIZE(ans, 3)
         ans(:, :, kk, ll) = ColConcat(a=m1(:, :, kk, ll), b=m2(:, :, kk, ll))
        END DO
      END DO
    !!
    !!
    !!
    ELSE IF (term1 .NE. del_t .AND. term2 .EQ. del_t) THEN
      CALL Reallocate(ans, nsd * SIZE(m1, 1), SIZE(m1, 2), &
           & SIZE(m1, 3), SIZE(m1, 4))
      DO ll = 1, SIZE(ans, 4)
        DO kk = 1, SIZE(ans, 3)
         ans(:, :, kk, ll) = RowConcat(a=m1(:, :, kk, ll), b=m2(:, :, kk, ll))
        END DO
      END DO
    END IF
    DEALLOCATE (m1, m2)
  CASE (3)
    CALL STCM_4(ans=m1, test=test, trial=trial, &
         & term1=term1, term2=term2, dim=1, c=c)
    CALL STCM_4(ans=m2, test=test, trial=trial, &
         & term1=term1, term2=term2, dim=2, c=c)
    CALL STCM_4(ans=m3, test=test, trial=trial, &
         & term1=term1, term2=term2, dim=3, c=c)
    !!
    !!
    !!
    IF (term1 .EQ. del_t .AND. term2 .NE. del_t) THEN
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
    !!
    !!
    ELSE IF (term1 .NE. del_t .AND. term2 .EQ. del_t) THEN
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
END SUBROUTINE STCM_5
