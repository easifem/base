PURE SUBROUTINE QP_Tetrahedron_Order12(ans, nrow, ncol)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

  !! REAL(DFP) :: ans(4, 210)
  nrow = 4; ncol = 210

  CALL QP_Tetrahedron_Order13(ans, nrow, ncol)

END SUBROUTINE QP_Tetrahedron_Order12
