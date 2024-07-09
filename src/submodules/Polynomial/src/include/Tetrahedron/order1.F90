
PURE SUBROUTINE QP_Tetrahedron_Order1(ans, nrow, ncol)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

  nrow = 4
  ncol = 1

  ans(1, 1) = 0.250000000000000
  ans(2, 1) = 0.250000000000000
  ans(3, 1) = 0.250000000000000
  ans(4, 1) = 0.166666666666667

END SUBROUTINE QP_Tetrahedron_Order1
