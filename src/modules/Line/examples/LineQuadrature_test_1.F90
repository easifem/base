!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for LineInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: GaussLegendreQP
USE LineInterpolationUtility, ONLY: QuadratureNumber_Line
USE LineInterpolationUtility, ONLY: QuadraturePoint_Line
USE LineInterpolationUtility, ONLY: QuadraturePoint_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), ALLOCATABLE :: q(:,:), buffer(:,:)
INTEGER(I4B) :: n,nr,nc

n = QuadratureNumber_Line(2_I4B,GaussLegendreQP)
q = QuadraturePoint_Line(order=2_I4B, quadType=GaussLegendreQP, &
  layout="INCREASING")
CALL OK(ABS(SUM(q(2,:))-2.0_DFP) < 1.0E-10_DFP, &
  "Weights integrate reference measure")
ALLOCATE(buffer(2,n))
CALL QuadraturePoint_Line_(nips=[n], quadType=GaussLegendreQP, &
  layout="INCREASING", ans=buffer, nrow=nr, ncol=nc)
CALL OK(nr == 2 .AND. nc == n, "Active quadrature dimensions")
CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-10_DFP, &
  "Function and subroutine quadrature agree")
END PROGRAM main
