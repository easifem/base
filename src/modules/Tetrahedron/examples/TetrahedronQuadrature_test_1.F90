!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TetrahedronInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: GaussLegendreQP
USE TetrahedronInterpolationUtility, ONLY: QuadratureNumber_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: QuadraturePoint_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: QuadraturePoint_Tetrahedron_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), ALLOCATABLE :: q(:,:),buffer(:,:)
INTEGER(I4B) :: n,nr,nc

n = QuadratureNumber_Tetrahedron(2_I4B,GaussLegendreQP)
ALLOCATE(buffer(4,n))
q = QuadraturePoint_Tetrahedron(order=2_I4B,quadType=GaussLegendreQP, &
  refTetrahedron="UNIT")
CALL QuadraturePoint_Tetrahedron_(order=2_I4B, &
  quadType=GaussLegendreQP,refTetrahedron="UNIT",ans=buffer,nrow=nr,ncol=nc)
CALL OK(nr == 4 .AND. nc == n, "Active quadrature dimensions")
CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-11_DFP, "Quadrature forms agree")
CALL OK(ABS(SUM(q(4,:))-(1.0_DFP/6.0_DFP)) < 1.0E-11_DFP, &
  "Weights integrate reference measure")
CALL OK(ABS(SUM(q(4,:)*q(1,:))-(1.0_DFP/24.0_DFP)) < 1.0E-11_DFP, &
  "Weights integrate an analytic polynomial moment")
END PROGRAM main
