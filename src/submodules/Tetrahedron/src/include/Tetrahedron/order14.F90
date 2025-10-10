PURE SUBROUTINE QP_Tetrahedron_Order14(ans, nrow, ncol)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

  !! REAL(DFP) :: ans(4, 330)
  nrow = 4; ncol = 330

  CALL QP_Tetrahedron_Order15(ans, nrow, ncol)

END SUBROUTINE QP_Tetrahedron_Order14
