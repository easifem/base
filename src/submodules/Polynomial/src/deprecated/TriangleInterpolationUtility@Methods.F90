
! PURE SUBROUTINE VertexBasis_Triangle2(Lo1, Lo2, ans)
!   REAL(DFP), INTENT(IN) :: Lo1(1:, 0:)
!   REAL(DFP), INTENT(IN) :: Lo2(1:, 0:)
!     !! coordinates on biunit square
!   REAL(DFP), INTENT(INOUT) :: ans(:, :)
!   ! ans(SIZE(Lo1, 1), 3)
!     !! ans(:,v1) basis function of vertex v1 at all points
!
!   INTEGER(I4B) :: ii, tpoints
!
!   tpoints = SIZE(ans, 1)
!
!   DO CONCURRENT(ii=1:tpoints)
!     ans(ii, 1) = Lo1(ii, 0) * Lo2(ii, 0)
!     ans(ii, 2) = Lo1(ii, 1) * Lo2(ii, 0)
!     ans(ii, 3) = Lo1(ii, 1) * Lo2(ii, 1) + Lo1(ii, 0) * Lo2(ii, 1)
!   END DO
!
! END SUBROUTINE VertexBasis_Triangle2

!----------------------------------------------------------------------------
!                                                        EdgeBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on left, right edge of biunit Triangle (internal only)
!
!# Introduction
!
! Evaluate basis functions on left and right edge of biunit Triangle
!
! qe1 and qe2 should be greater than or equal to 2

! PURE SUBROUTINE EdgeBasis_Triangle2(pe1, pe2, pe3, L1, L2, Lo1, &
!     & Lo2, ans)
!   INTEGER(I4B), INTENT(IN) :: pe1
!     !! order on left vertical edge (e1), should be greater than 1
!   INTEGER(I4B), INTENT(IN) :: pe2
!     !! order on right vertical edge(e2), should be greater than 1
!   INTEGER(I4B), INTENT(IN) :: pe3
!     !! order on right vertical edge(e3), should be greater than 1
!   REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:)
!     !! L1 and L2 are jacobian polynomials
!   REAL(DFP), INTENT(IN) :: Lo1(1:, 0:)
!     !! coordinates on biunit square domain
!   REAL(DFP), INTENT(IN) :: Lo2(1:, 0:)
!     !! coordinates on biunit square domain
!   REAL(DFP), INTENT(INOUT) :: ans(:, :)
!   ! REAL(DFP) :: ans(SIZE(L1, 1), pe1 + pe2 + pe3 - 3)
!
!   INTEGER(I4B) :: maxP, k1, k2, a
!   REAL(DFP) :: asign
!
!   maxP = MAX(pe1, pe2, pe3)
!   ! edge(1) = 1 -> 2
!   a = 0
!
!   DO k1 = 2, pe1
!     ans(:, k1 - 1) = Lo1(:, 0) * Lo1(:, 1) * L1(:, k1 - 2) * (Lo2(:, 0)**k1)
!   END DO
!
!   ! edge(2) = 2 -> 3
!   a = pe1 - 1
!   DO k2 = 2, pe2
!     ans(:, a + k2 - 1) = Lo1(:, 1) * Lo2(:, 0) * Lo2(:, 1) * L2(:, k2 - 2)
!   END DO
!
!   ! edge(3) = 3 -> 1
!   a = pe1 - 1 + pe2 - 1
!   DO k2 = 2, pe3
!     asign = (-1.0_DFP)**(k2 - 2)
! ans(:, a + k2 - 1) = asign * Lo1(:, 0) * Lo2(:, 0) * Lo2(:, 1) * L2(:, k2 - 2)
!   END DO
!
! END SUBROUTINE EdgeBasis_Triangle2

!----------------------------------------------------------------------------
!                                                        CellBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis in the cell of biunit Triangle (internal only)
!
!# Introduction
!
! Evaluate basis functions in the cell of biunit Triangle

! PURE SUBROUTINE CellBasis_Triangle2(order, L1, eta_ij, &
!   & Lo1, Lo2, ans)
!   INTEGER(I4B), INTENT(IN) :: order
!     !! order of approximation inside the cell, order>2
!   REAL(DFP), INTENT(IN) :: L1(1:, 0:)
!     !! lobatto polynomials
!   REAL(DFP), INTENT(IN) :: eta_ij(:, :)
!     !! coordinates on biunit square
!   REAL(DFP), INTENT(IN) :: Lo1(1:, 0:)
!     !! coordinates on biunit square domain
!   REAL(DFP), INTENT(IN) :: Lo2(1:, 0:)
!     !! coordinates on biunit square domain
!   REAL(DFP), INTENT(INOUT) :: ans(:, :)
!   ! REAL(DFP) :: ans(SIZE(L1, 1), INT((order - 1) * (order - 2) / 2))
!
!   ! FIXME: Remove these arrays, no allocation is our goal
!   REAL(DFP) :: P2(SIZE(eta_ij, 2), 0:order)
!   REAL(DFP) :: avec(SIZE(eta_ij, 2)), alpha, beta
!   INTEGER(I4B) :: k1, k2, max_k2, cnt
!
!   alpha = 0.0_DFP
!   beta = 1.0_DFP
!   cnt = 0
!
!   ! FIXME: Make this loop parallel
!
!   DO k1 = 2, order - 1
!     avec = (Lo2(:, 0)**k1) * Lo2(:, 1) * Lo1(:, 0) * Lo1(:, 1)
!     alpha = 2.0_DFP * k1 - 1.0_DFP
!     max_k2 = MAX(order - k1 - 1, 0)
!     P2(:, 0:max_k2) = JacobiEvalAll(n=max_k2, x=eta_ij(2, :), &
!       & alpha=alpha, beta=beta)
!     DO k2 = 2, order - k1 + 1
!       cnt = cnt + 1
!       ans(:, cnt) = L1(:, k1 - 2) * avec * P2(:, k2 - 2)
!     END DO
!   END DO
!
! END SUBROUTINE CellBasis_Triangle2
