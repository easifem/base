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
! {\frac{{\partial {N^I}{T_a}}}{{\partial x}} c \cdot {N^J}{T_b}d\Omega dt} } }
! $$
!
! $$
! M\left( {I,J,a,b} \right) =  {\int_{{I_n}}^{} {\int_\Omega ^{}
! {\frac{{\partial {N^I}{T_a}}}{{\partial y}} c \cdot {N^J}{T_b}d\Omega dt} } }
! $$
!
! $$
! M\left( {I,J,a,b} \right) =  {\int_{{I_n}}^{} {\int_\Omega ^{}
! {\frac{{\partial {N^I}{T_a}}}{{\partial z}} c \cdot {N^J}{T_b}d\Omega dt} } }
! $$
!
! $$
! M\left( {I,J,a,b} \right) =  {\int_{{I_n}}^{} {\int_\Omega ^{}
! {{N^J}{T_b} c \cdot  \frac{{\partial {N^J}{T_b}}}{{\partial x}}d\Omega dt} } }
! $$
!
! $$
! M\left( {I,J,a,b} \right) =  {\int_{{I_n}}^{} {\int_\Omega ^{}
! {{N^J}{T_b} c \cdot \frac{{\partial {N^J}{T_b}}}{{\partial y}}d\Omega dt} } }
! $$
!
! $$
! M\left( {I,J,a,b} \right) =  {\int_{{I_n}}^{} {\int_\Omega ^{}
! {{N^J}{T_b} c \cdot \frac{{\partial {N^J}{T_b}}}{{\partial z}}d\Omega dt} } }
! $$

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_2(ans, test, trial, term1, term2, dim, c)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_x, del_y, del_z, del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_x, del_y, del_z, del_none
  INTEGER(I4B), INTENT(IN) :: dim
  !! dim = 1/2/3
  TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: c
  !! Scalar variable
  !! Define internal variables
  INTEGER(I4B) :: ipt, ips
  REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: cbar(:, :)
  !!
  !! main
  !!
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
  IF (term1 .EQ. DEL_NONE .AND. term2 .NE. DEL_NONE) THEN
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
  ELSE IF (term1 .NE. DEL_NONE  .AND. term2 .EQ. DEL_NONE) THEN
    DO ipt = 1, SIZE(trial)
      realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
           & trial(ipt)%wt * trial(ipt)%jt * cbar(:, ipt)
      DO ips = 1, SIZE(realval)
        iajb = iajb + realval(ips) &
             & * outerprod( &
             & test(ipt)%dNTdXt(:, :, dim, ips), &
             & outerprod(trial(ipt)%N(:, ips), trial(ipt)%T))
      END DO
    END DO
  END IF
  CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
    !! cleanup
  IF (ALLOCATED(iajb)) DEALLOCATE (iajb)
  IF (ALLOCATED(realval)) DEALLOCATE (realval)
  IF (ALLOCATED(cbar)) DEALLOCATE (cbar)
END SUBROUTINE STCM_2
