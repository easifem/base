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
!
!
! $$
! M(I,J,a,b)=\int_{I_{n}}\int_{\Omega} \rho c_{p}\frac{\partial N^{I}T_{a}}
! {\partial x_{p}}\frac{\partial N^{J}T_{b}}{\partial x}d\Omega dt
! $$
!
! ```fortran
!     mat2=ConvectiveMatrix( test, trial, term1=del_x, term2=del_x, &
!         & c=c, rho=rho, projectionOn='test')
! ```
!
! $$
! M(I,J,a,b)=\int_{I_{n}}\int_{\Omega} \rho c_{p}\frac{\partial N^{I}T_{a}}
! {\partial x_{p}}\frac{\partial N^{J}T_{b}}{\partial y}d\Omega dt
! $$
!
! ```fortran
!     mat2=ConvectiveMatrix( test, trial, term1=del_x, term2=del_y, &
!         & c=c, rho=rho, projectionOn='test')
! ```
!
! $$
! M(I,J,a,b)=\int_{I_{n}}\int_{\Omega} \rho c_{p}\frac{\partial N^{I}T_{a}}
! {\partial x_{p}}\frac{\partial N^{J}T_{b}}{\partial z}d\Omega dt
! $$
!
! ```fortran
!     mat2=ConvectiveMatrix( test, trial, term1=del_x, term2=del_z, &
!         & c=c, rho=rho, projectionOn='test')
! ```
!
! $$
! M(I,J,a,b)=\int_{I_{n}}\int_{\Omega} \rho \frac{\partial N^{I}T_{a}}
! {\partial x}c_{p}\frac{\partial N^{J}T_{b}}{\partial x_{p}}d\Omega dt
! $$
!
! ```fortran
!     mat2=ConvectiveMatrix( test, trial, term1=del_x, term2=del_x, &
!         & c=c, rho=rho, projectionOn='trial')
! ```
!
! $$
! M(I,J,a,b)=\int_{I_{n}}\int_{\Omega} \rho \frac{\partial N^{I}T_{a}}
! {\partial y}c_{p}\frac{\partial N^{J}T_{b}}{\partial x_{p}}d\Omega dt
! $$
!
! ```fortran
!     mat2=ConvectiveMatrix( test, trial, term1=del_x, term2=del_y, &
!         & c=c, rho=rho, projectionOn='trial')
! ```
!
! $$
! M(I,J,a,b)=\int_{I_{n}}\int_{\Omega} \rho \frac{\partial N^{I}T_{a}}
! {\partial z}c_{p}\frac{\partial N^{J}T_{b}}{\partial x_{p}}d\Omega dt
! $$
!
! ```fortran
!     mat2=ConvectiveMatrix( test, trial, term1=del_x, term2=del_z, &
!         & c=c, rho=rho, projectionOn='trial')
! ```
!
!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_6b(ans, test, trial, term1, term2, rho, c, projectOn)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! dx, dy, dz
  INTEGER(I4B), INTENT(IN) :: term2
  !! dx, dy, dz
  TYPE(FEVariable_), INTENT(IN) :: c
  !! vector variable
  TYPE(FEVariable_), INTENT(IN) :: rho
  !! Scalar variable
  CHARACTER(LEN=*), INTENT(IN) :: projectOn
  !! ""test"
  !! "trial"
  !!
  !! Define internal variables
  INTEGER(I4B) :: ipt, ips
  REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: p(:, :, :)
  REAL(DFP), ALLOCATABLE :: rhobar(:, :)
  !!
  !! main
  !!
  CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
       & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  CALL GetInterpolation(obj=trial, val=rho, interpol=rhobar)
  !!
  SELECT CASE (TRIM(projectOn))
  CASE ("test", "TEST")
    DO ipt = 1, SIZE(trial)
      realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
           & trial(ipt)%wt * trial(ipt)%jt * rhobar(:, ipt)
      CALL GetProjectionOfdNTdXt(obj=test(ipt), cdNTdXt=p, val=c)
      DO ips = 1, SIZE(realval)
        iajb = iajb + realval(ips) &
             & * outerprod( &
             & p(:, :, ips), &
             & trial(ipt)%dNTdXt(:, :, term2, ips))
      END DO
    END DO
  !!
  CASE ("trial", "TRIAL")
    DO ipt = 1, SIZE(trial)
      realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
           & trial(ipt)%wt * trial(ipt)%jt * rhobar(:,ipt)
      CALL GetProjectionOfdNTdXt(obj=trial(ipt), cdNTdXt=p, val=c)
      DO ips = 1, SIZE(realval)
        iajb = iajb + realval(ips) &
             & * outerprod( &
             & test(ipt)%dNTdXt(:, :, term1, ips), &
             & p(:, :, ips))
      END DO
    END DO
  END SELECT
  !!
  CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
    !! cleanup
  IF (ALLOCATED(iajb)) DEALLOCATE (iajb)
  IF (ALLOCATED(realval)) DEALLOCATE (realval)
  IF (ALLOCATED(p)) DEALLOCATE (p)
END SUBROUTINE STCM_6b
