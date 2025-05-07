
PURE SUBROUTINE QP_Tetrahedron_Order20(ans, nrow, ncol)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

  !! REAL(DFP) :: ans(4, 1001)
  nrow = 4; ncol = 1001

  CALL QP_Tetrahedron_Order21(ans, nrow, ncol)
END SUBROUTINE QP_Tetrahedron_Order20
