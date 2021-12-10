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
! $$
! M\left( {I,J,a,b} \right) =  {\int_{{I_n}}^{} {\int_\Omega ^{}
! \rho {{c_j}\frac{{\partial {N^I}{T_a}}}{{\partial {x_j}}} \cdot
! {N^J}{T_b}d\Omega dt} } }
! $$
!
! $$
! M\left(I,J,a,b\right)=\int_{I_{n}}\int_{\Omega}
! \rho N^{I}T_{a}c_{j}\frac{\partial
! N^{J}T_{b}}{\partial x_{j}}d\Omega dt
! $$
!
!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_1b(ans, test, trial, term1, term2, rho, c, projectOn)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  TYPE(FEVariable_), INTENT(IN) :: rho
  !! scalar variable
  TYPE(FEVariable_), INTENT(IN) :: c
  !! convective velocity
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  !! del_dx
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_none
  !! del_dx
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: projectOn
  ! Define internal variables
  INTEGER(I4B) :: ips, ipt
  REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: p(:, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: rhobar(:,:)
  !! main
  CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
       & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  CALL GetInterpolation(obj=trial, val=rho, interpol=rhobar)
  !!
  IF (term1 .EQ. del_none ) THEN
    DO ipt = 1, SIZE(trial)
      realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
           & trial(ipt)%wt * trial(ipt)%jt * rhobar(:,ipt)
      CALL GetProjectionOfdNTdXt(obj=trial(ipt), cdNTdXt=p, val=c)
      DO ips = 1, SIZE(realval)
        iajb = iajb + realval(ips) &
             & * outerprod( &
             & outerprod(test(ipt)%N(:, ips), test(ipt)%T), &
             & p(:, :, ips))
      END DO
    END DO
  !!
  ELSE IF (term2 .EQ. del_none) THEN
    DO ipt = 1, SIZE(trial)
      realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
           & trial(ipt)%wt * trial(ipt)%jt * rhobar(:,ipt)
      CALL GetProjectionOfdNTdXt(obj=test(ipt), cdNTdXt=p, val=c)
      DO ips = 1, SIZE(realval)
        iajb = iajb + realval(ips) &
             & * outerprod( &
             & p(:, :, ips), &
             & outerprod(trial(ipt)%N(:, ips), trial(ipt)%T))
      END DO
    END DO
  END IF
  !!
  CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
  !!
  IF (ALLOCATED(iajb)) DEALLOCATE (iajb)
  IF (ALLOCATED(p)) DEALLOCATE (p)
  IF (ALLOCATED(realval)) DEALLOCATE (realval)
  IF (ALLOCATED(rhobar)) DEALLOCATE (rhobar)
END SUBROUTINE STCM_1b
