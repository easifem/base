PURE SUBROUTINE QP_Tetrahedron_Order18(ans, nrow, ncol)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

  !! REAL(DFP) :: ans(4, 715)
  nrow = 4; ncol = 715

  CALL QP_Tetrahedron_Order19(ans, nrow, ncol)

END SUBROUTINE QP_Tetrahedron_Order18
