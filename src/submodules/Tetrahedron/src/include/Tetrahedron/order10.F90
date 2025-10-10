PURE SUBROUTINE QP_Tetrahedron_Order10(ans, nrow, ncol)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

  !! REAL(DFP) :: ans(4, 126)
  nrow = 4; ncol = 126

  CALL QP_Tetrahedron_Order11(ans, nrow, ncol)

END SUBROUTINE QP_Tetrahedron_Order10
