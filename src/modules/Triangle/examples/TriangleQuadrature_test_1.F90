!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for TriangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: GaussLegendreQP
USE TriangleInterpolationUtility, ONLY: QuadratureNumber_Triangle
USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle
USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), ALLOCATABLE :: q(:,:), buffer(:,:)
INTEGER(I4B) :: n,nr,nc

n = QuadratureNumber_Triangle(2_I4B,GaussLegendreQP)
q = QuadraturePoint_Triangle(order=2_I4B, quadType=GaussLegendreQP, &
  refTriangle="UNIT")
CALL OK(ABS(SUM(q(3,:))-0.5_DFP) < 1.0E-10_DFP, &
  "Weights integrate reference measure")
ALLOCATE(buffer(3,n))
CALL QuadraturePoint_Triangle_(order=2_I4B, quadType=GaussLegendreQP, &
  refTriangle="UNIT", ans=buffer, nrow=nr, ncol=nc)
CALL OK(nr == 3 .AND. nc == n, "Active quadrature dimensions")
CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-10_DFP, &
  "Function and subroutine quadrature agree")
END PROGRAM main
