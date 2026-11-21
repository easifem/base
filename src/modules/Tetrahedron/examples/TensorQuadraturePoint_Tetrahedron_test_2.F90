!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing TensorQuadraturePoint_Tetrahedron with point counts

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: GaussLegendreQP
USE Test_Method, ONLY: OK
USE TetrahedronInterpolationUtility, ONLY: &
  TensorQuadraturePoint_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: &
  TensorQuadraturePoint_Tetrahedron_
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), ALLOCATABLE :: qOrder(:, :)

! Order 4 uses 3, 4 and 3 points in the x, y and z directions.
qOrder = TensorQuadraturePoint_Tetrahedron(order=4_I4B, &
                                           quadType=GaussLegendreQP, &
                                           refTetrahedron="UNIT")

! Test 1: point counts (3, 4, 3) give the same rule as order 4
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)
  q = TensorQuadraturePoint_Tetrahedron(nipsx=[3_I4B], nipsy=[4_I4B], &
                                        nipsz=[3_I4B], &
                                        quadType=GaussLegendreQP, &
                                        refTetrahedron="UNIT")
  CALL OK(SIZE(q, 1) .EQ. 4 .AND. SIZE(q, 2) .EQ. 36, "nips: shape")
  CALL OK(MAXVAL(ABS(q - qOrder)) .LT. tol, "nips: same as order 4")
END BLOCK

! Test 2: subroutine form with an order, nrow = 4 and ncol = 36
BLOCK
  REAL(DFP) :: q(4, 40)
  INTEGER(I4B) :: nrow, ncol
  CALL TensorQuadraturePoint_Tetrahedron_(order=4_I4B, &
                                          quadType=GaussLegendreQP, &
                                          refTetrahedron="UNIT", ans=q, &
                                          nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 4 .AND. ncol .EQ. 36, "subroutine, order: sizes")
  CALL OK(MAXVAL(ABS(q(1:nrow, 1:ncol) - qOrder)) .LT. tol, &
          "subroutine, order: values")
END BLOCK

! Test 3: subroutine form with point counts
BLOCK
  REAL(DFP) :: q(4, 36)
  INTEGER(I4B) :: nrow, ncol
  CALL TensorQuadraturePoint_Tetrahedron_(nipsx=[3_I4B], nipsy=[4_I4B], &
                                          nipsz=[3_I4B], &
                                          quadType=GaussLegendreQP, &
                                          refTetrahedron="UNIT", ans=q, &
                                          nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 4 .AND. ncol .EQ. 36, "subroutine, nips: sizes")
  CALL OK(MAXVAL(ABS(q - qOrder)) .LT. tol, "subroutine, nips: values")
END BLOCK

! Test 4: subroutine form on BIUNIT, weights sum to 8/6
BLOCK
  REAL(DFP) :: q(4, 36)
  INTEGER(I4B) :: nrow, ncol
  CALL TensorQuadraturePoint_Tetrahedron_(nipsx=[3_I4B], nipsy=[4_I4B], &
                                          nipsz=[3_I4B], &
                                          quadType=GaussLegendreQP, &
                                          refTetrahedron="BIUNIT", ans=q, &
                                          nrow=nrow, ncol=ncol)
  CALL OK(ABS(SUM(q(4, 1:ncol)) - 8.0_DFP / 6.0_DFP) .LT. tol, &
          "subroutine, BIUNIT: volume")
END BLOCK

END PROGRAM main
