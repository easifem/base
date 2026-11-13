!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for QuadrangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: GaussLegendreQP
USE QuadrangleInterpolationUtility, ONLY: QuadratureNumber_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: QuadraturePoint_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: QuadraturePoint_Quadrangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), ALLOCATABLE :: q(:,:),buffer(:,:)
INTEGER(I4B) :: n(2),nr,nc

n = QuadratureNumber_Quadrangle(2_I4B,2_I4B, &
  GaussLegendreQP,GaussLegendreQP)
ALLOCATE(buffer(3,PRODUCT(n)))
q = QuadraturePoint_Quadrangle(order=2_I4B,quadType=GaussLegendreQP, &
  refQuadrangle="BIUNIT")
CALL QuadraturePoint_Quadrangle_(nipsx=[n(1)],nipsy=[n(2)], &
  quadType1=GaussLegendreQP, &
  quadType2=GaussLegendreQP,refQuadrangle="BIUNIT",ans=buffer, &
  nrow=nr,ncol=nc)
CALL OK(nr == 3 .AND. nc == PRODUCT(n), "Active quadrature dimensions")
CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-11_DFP, "Quadrature forms agree")
CALL OK(ABS(SUM(q(3,:))-(4.0_DFP)) < 1.0E-11_DFP, &
  "Weights integrate reference measure")
CALL OK(ABS(SUM(q(3,:)*q(1,:)**2)-(4.0_DFP/3.0_DFP)) < 1.0E-11_DFP, &
  "Weights integrate an analytic polynomial moment")
END PROGRAM main
