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

SUBMODULE(FEMatrix_Method) STConvectiveMatrixMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         STConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Mat4_STConvectiveMatrix_1
#include "./Mat4_STConvectiveMatrix_1.inc"
END PROCEDURE Mat4_STConvectiveMatrix_1

!----------------------------------------------------------------------------
!                                                         STConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Mat4_STConvectiveMatrix_1b
#include "./Mat4_STConvectiveMatrix_1.inc"
END PROCEDURE Mat4_STConvectiveMatrix_1b

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Mat2_STConvectiveMatrix_1
REAL(DFP), ALLOCATABLE :: mat4(:, :, :, :)
mat4 = STConvectiveMatrix(test=test, trial=trial, c=c, term1=term1, &
     & term2=term2)
CALL convert(from=mat4, to=ans)
IF (ALLOCATED(mat4)) DEALLOCATE (mat4)
END PROCEDURE Mat2_STConvectiveMatrix_1

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Mat2_STConvectiveMatrix_1b
REAL(DFP), ALLOCATABLE :: mat4(:, :, :, :)
mat4 = STConvectiveMatrix(test=test, trial=trial, c=c, term1=term1, &
     & term2=term2)
CALL convert(from=mat4, to=ans)
IF (ALLOCATED(mat4)) DEALLOCATE (mat4)
END PROCEDURE Mat2_STConvectiveMatrix_1b

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE case1_STConvectiveMatrix_2(ans, test, trial, term1, &
     & term2, dim, c)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  INTEGER(I4B), INTENT(IN) :: term2
  INTEGER(I4B), INTENT(IN) :: dim
  TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: c
    !! Define internal variables
  INTEGER(I4B) :: ipt, ips
  REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: cbar(:, :)
    !! main
    !! make c bar at ips and ipt
  IF (PRESENT(c)) THEN
    CALL GetInterpolation(obj=trial, val=c, interpol=cbar)
  ELSE
    CALL Reallocate(cbar, SIZE(trial(1)%N, 2), SIZE(trial))
    cbar = 1.0_DFP
  END IF
    !!
  CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
       & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
    !!
  IF (term1 .EQ. 0 .AND. term2 .EQ. 1) THEN
    DO ipt = 1, SIZE(trial)
      realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
           & trial(ipt)%wt * trial(ipt)%jt * cbar(:, ipt)
      DO ips = 1, SIZE(realval)
        iajb = iajb + realval(ips) &
             & * outerprod( &
             & outerprod(test(ipt)%N(:, ips), test(ipt)%T), &
             & trial(ipt)%dNTdXt(:, :, dim, ips))
      END DO
    END DO
  ELSE IF (term1 .EQ. 1 .AND. term2 .EQ. 0) THEN
    DO ipt = 1, SIZE(trial)
      realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
           & trial(ipt)%wt * trial(ipt)%jt * cbar(:, ipt)
      DO ips = 1, SIZE(realval)
        iajb = iajb + realval(ips) &
             & * outerprod( &
             & trial(ipt)%dNTdXt(:, :, dim, ips), &
             & outerprod(trial(ipt)%N(:, ips), trial(ipt)%T))
      END DO
    END DO
  END IF
  CALL swap(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
    !! cleanup
  IF (ALLOCATED(iajb)) DEALLOCATE (iajb)
  IF (ALLOCATED(realval)) DEALLOCATE (realval)
  IF (ALLOCATED(cbar)) DEALLOCATE (cbar)
END SUBROUTINE case1_STConvectiveMatrix_2

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE case2_STConvectiveMatrix_2(ans, test, trial, term1, &
    & term2, dim, c)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  INTEGER(I4B), INTENT(IN) :: term2
  INTEGER(I4B), INTENT(IN) :: dim
  TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: c
    !! Define internal variables
  INTEGER(I4B) :: nsd, ll, kk
  REAL(DFP), ALLOCATABLE, DIMENSION(:, :, :, :) :: m1, m2, m3
  !! main
  nsd = trial(1)%refelem%nsd
  SELECT CASE (nsd)
  CASE (1)
    CALL case1_STConvectiveMatrix_2(ans, test, trial, term1, term2, 1, c)
  CASE (2)
    CALL case1_STConvectiveMatrix_2(m1, test, trial, term1, term2, 1, c)
    CALL case1_STConvectiveMatrix_2(m2, test, trial, term1, term2, 2, c)
    IF (term1 .EQ. 0 .AND. term2 .EQ. 1) THEN
      CALL Reallocate(ans, SIZE(m1, 1), nsd * SIZE(m1, 2), &
           & SIZE(m1, 3), SIZE(m1, 4))
      DO ll = 1, SIZE(ans, 4)
        DO kk = 1, SIZE(ans, 3)
         ans(:, :, kk, ll) = ColConcat(a=m1(:, :, kk, ll), b=m2(:, :, kk, ll))
        END DO
      END DO
    ELSE IF (term1 .EQ. 1 .AND. term2 .EQ. 0) THEN
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
    CALL case1_STConvectiveMatrix_2(m1, test, trial, term1, term2, 1, c)
    CALL case1_STConvectiveMatrix_2(m2, test, trial, term1, term2, 2, c)
    CALL case1_STConvectiveMatrix_2(m3, test, trial, term1, term2, 3, c)
    IF (term1 .EQ. 0 .AND. term2 .EQ. 1) THEN
      CALL Reallocate(ans, SIZE(m1, 1), nsd * SIZE(m1, 2), &
           & SIZE(m1, 3), SIZE(m1, 4))
      DO ll = 1, SIZE(ans, 4)
        DO kk = 1, SIZE(ans, 3)
           ans(:, :, kk, ll) = ColConcat( &
                & a=ColConcat(a=m1(:, :, kk, ll), b=m2(:, :, kk, ll)), &
                & b=m3(:,:,kk,ll))
        END DO
      END DO
    ELSE IF (term1 .EQ. 1 .AND. term2 .EQ. 0) THEN
      CALL Reallocate(ans, nsd * SIZE(m1, 1), SIZE(m1, 2), &
           & SIZE(m1, 3), SIZE(m1, 4))
      DO ll = 1, SIZE(ans, 4)
        DO kk = 1, SIZE(ans, 3)
           ans(:, :, kk, ll) = RowConcat( &
                & a=RowConcat(a=m1(:, :, kk, ll), b=m2(:, :, kk, ll)), &
                & b=m3(:,:,kk,ll))
        END DO
      END DO
    END IF
    DEALLOCATE (m1, m2, m3)
  END SELECT
END SUBROUTINE case2_STConvectiveMatrix_2

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Mat2_STConvectiveMatrix_2
REAL(DFP), ALLOCATABLE :: mat4(:, :, :, :)
mat4 = STConvectiveMatrix(test=test, trial=trial, term1=term1, &
     & term2=term2, dim=dim, c=c)
CALL convert(from=mat4, to=ans)
IF (ALLOCATED(mat4)) DEALLOCATE (mat4)
END PROCEDURE Mat2_STConvectiveMatrix_2

!----------------------------------------------------------------------------
!                                                         STConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Mat4_STConvectiveMatrix_2
IF (dim .GT. 0) THEN
  CALL case1_STConvectiveMatrix_2(ans=ans, test=test, trial=trial, &
       & term1=term1, term2=term2, dim=dim, c=c)
ELSE
  CALL case2_STConvectiveMatrix_2(ans=ans, test=test, trial=trial, &
       & term1=term1, term2=term2, dim=dim, c=c)
END IF
END PROCEDURE Mat4_STConvectiveMatrix_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE STConvectiveMatrixMethods
