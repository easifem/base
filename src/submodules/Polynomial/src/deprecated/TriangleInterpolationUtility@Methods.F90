
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

! PURE SUBROUTINE VertexBasisGradient_Triangle2(Lo1, Lo2, dLo1, dLo2, ans)
!   REAL(DFP), INTENT(IN) :: Lo1(1:, 0:)
!     !! Lobatto polynomials evaluated at x1
!   REAL(DFP), INTENT(IN) :: Lo2(1:, 0:)
!     !! Lobatto polynomials evaluated at x2
!   REAL(DFP), INTENT(IN) :: dLo1(1:, 0:)
!     !! Gradient of Lobatto polynomials at x1
!   REAL(DFP), INTENT(IN) :: dLo2(1:, 0:)
!     !! Gradient of Lobatto polynomials at x2
!   REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
!   ! REAL(DFP) :: ans(SIZE(Lo1, 1), 3, 2)
!     !! ans(:,v1) basis function of vertex v1 at all points
!
!   ans(:, 1, 1) = dLo1(:, 0) * Lo2(:, 0)
!   ans(:, 1, 2) = Lo1(:, 0) * dLo2(:, 0)
!   ans(:, 2, 1) = dLo1(:, 1) * Lo2(:, 0)
!   ans(:, 2, 2) = Lo1(:, 1) * dLo2(:, 0)
!   ans(:, 3, 1) = dLo1(:, 1) * Lo2(:, 1) + dLo1(:, 0) * Lo2(:, 1)
!   ans(:, 3, 2) = Lo1(:, 1) * dLo2(:, 1) + Lo1(:, 0) * dLo2(:, 1)
! END SUBROUTINE VertexBasisGradient_Triangle2

! PURE SUBROUTINE EdgeBasisGradient_Triangle2(pe1, pe2, pe3, L1, L2, &
!                                           Lo1, Lo2, dL1, dL2, dLo1, dLo2, ans)
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
!   REAL(DFP), INTENT(IN) :: dL1(1:, 0:), dL2(1:, 0:)
!     !! L1 and L2 are jacobian polynomials
!   REAL(DFP), INTENT(IN) :: dLo1(1:, 0:)
!     !! coordinates on biunit square domain
!   REAL(DFP), INTENT(IN) :: dLo2(1:, 0:)
!     !! coordinates on biunit square domain
!   REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
!   ! REAL(DFP) :: ans(SIZE(L1, 1), pe1 + pe2 + pe3 - 3, 2)
!
!   INTEGER(I4B) :: maxP, k1, k2, a
!   REAL(DFP), DIMENSION(SIZE(Lo1, 1)) :: avec
!
!   maxP = MAX(pe1, pe2, pe3)
!   ! edge(1)
!   a = 0
!
!   DO k1 = 2, pe1
!     avec = dLo1(:, 0) * Lo1(:, 1) * L1(:, k1 - 2) &
!         & + Lo1(:, 0) * dLo1(:, 1) * L1(:, k1 - 2) &
!         & + Lo1(:, 0) * Lo1(:, 1) * dL1(:, k1 - 2)
!
!     ans(:, k1 - 1, 1) = avec * (Lo2(:, 0)**k1)
!
!     ans(:, k1 - 1, 2) = Lo1(:, 0) * Lo1(:, 1)  &
!                        & * L1(:, k1 - 2)  &
!                        & * REAL(k1, DFP)  &
!                        & * (Lo2(:, 0)**(k1 - 1))  &
!                        & * dLo2(:, 0)
!   END DO
!
!   ! edge(2)
!   a = pe1 - 1
!   DO k2 = 2, pe2
!     avec = dLo2(:, 0) * Lo2(:, 1) * L2(:, k2 - 2) &
!         &+ Lo2(:, 0) * dLo2(:, 1) * L2(:, k2 - 2) &
!         &+ Lo2(:, 0) * Lo2(:, 1) * dL2(:, k2 - 2)
!     ans(:, a + k2 - 1, 1) = dLo1(:, 0) * Lo2(:, 0) * Lo2(:, 1) * L2(:, k2 - 2)
!     ans(:, a + k2 - 1, 2) = Lo1(:, 0) * avec
!   END DO
!
!   ! edge(3)
!   a = pe1 - 1 + pe2 - 1
!   DO k2 = 2, pe3
!     avec = dLo2(:, 0) * Lo2(:, 1) * L2(:, k2 - 2)  &
!         & + Lo2(:, 0) * dLo2(:, 1) * L2(:, k2 - 2)  &
!         & + Lo2(:, 0) * Lo2(:, 1) * dL2(:, k2 - 2)
!     ans(:, a + k2 - 1, 1) = dLo1(:, 1) * Lo2(:, 0) * Lo2(:, 1) * L2(:, k2 - 2)
!     ans(:, a + k2 - 1, 2) = Lo1(:, 1) * avec
!   END DO
! END SUBROUTINE EdgeBasisGradient_Triangle2

! PURE SUBROUTINE CellBasisGradient_Triangle2(order, eta_ij, L1, Lo1, &
!                                             Lo2, dL1, dLo1, dLo2, ans)
!   INTEGER(I4B), INTENT(IN) :: order
!     !! order of approximation inside the cell, order>2
!   REAL(DFP), INTENT(IN) :: eta_ij(:, :)
!     !! coordinates on biunit square
!   REAL(DFP), INTENT(IN) :: L1(1:, 0:)
!     !! lobatto polynomials
!   REAL(DFP), INTENT(IN) :: Lo1(1:, 0:)
!     !! coordinates on biunit square domain
!   REAL(DFP), INTENT(IN) :: Lo2(1:, 0:)
!     !!
!   REAL(DFP), INTENT(IN) :: dL1(1:, 0:)
!     !! lobatto polynomials
!   REAL(DFP), INTENT(IN) :: dLo1(1:, 0:)
!     !!
!   REAL(DFP), INTENT(IN) :: dLo2(1:, 0:)
!     !!
!   REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
!   ! REAL(DFP) :: ans(SIZE(L1, 1), INT((order - 1) * (order - 2) / 2), 2)
!
!   REAL(DFP) :: P2(SIZE(eta_ij, 2), 0:order)
!   REAL(DFP) :: dP2(SIZE(eta_ij, 2), 0:order)
!
!   REAL(DFP) :: temp(SIZE(eta_ij, 2), 13)
!
!   REAL(DFP) :: alpha, beta
!   INTEGER(I4B) :: k1, k2, max_k2, cnt
!
!   alpha = 0.0_DFP
!   beta = 1.0_DFP
!   cnt = 0
!   temp(:, 5) = dLo1(:, 0) * Lo1(:, 1)
!   temp(:, 6) = Lo1(:, 0) * dLo1(:, 1)
!   temp(:, 7) = Lo1(:, 0) * Lo1(:, 1)
!   temp(:, 9) = dLo2(:, 0) * Lo2(:, 1)
!   temp(:, 12) = Lo2(:, 0) * Lo2(:, 1)
!   temp(:, 13) = Lo2(:, 0) * dLo2(:, 1)
!
!   DO k1 = 2, order - 1
!     alpha = 2.0_DFP * k1 - 1.0_DFP
!     max_k2 = MAX(order - k1 - 1, 0)
!     P2(:, 0:max_k2) = JacobiEvalAll(n=max_k2, x=eta_ij(2, :), &
!       & alpha=alpha, beta=beta)
!     dP2(:, 0:max_k2) = JacobiGradientEvalAll(n=max_k2, x=eta_ij(2, :), &
!       & alpha=alpha, beta=beta)
!
!     temp(:, 1) = (temp(:, 5) + temp(:, 6)) * L1(:, k1 - 2)  &
!       & + temp(:, 7) * dL1(:, k1 - 2)
!     temp(:, 11) = Lo2(:, 0)**(k1 - 1)
!     temp(:, 2) = temp(:, 11) * temp(:, 12)
!     temp(:, 3) = temp(:, 7) * L1(:, k1 - 2)
!
!     temp(:, 10) = REAL(k1, dfp) * temp(:, 9) + temp(:, 13)
!     temp(:, 8) = temp(:, 11) * temp(:, 10)
!
!     DO k2 = 2, order - k1 + 1
!       cnt = cnt + 1
!       temp(:, 4) = temp(:, 8) * P2(:, k2 - 2) + temp(:, 2) * dP2(:, k2 - 2)
!
!       ans(:, cnt, 1) = temp(:, 1) * temp(:, 2) * P2(:, k2 - 2)
!       ans(:, cnt, 2) = temp(:, 3) * temp(:, 4)
!     END DO
!
!   END DO
!
! END SUBROUTINE CellBasisGradient_Triangle2

! FUNCTION HeirarchicalBasisGradient_Triangle1(order, pe1, pe2, pe3,&
!   & xij, refTriangle) RESULT(ans)
!   INTEGER(I4B), INTENT(IN) :: order
!     !! Order of approximation inside the triangle (i.e., cell)
!     !! it should be greater than 2 for cell bubble to exist
!   INTEGER(I4B), INTENT(IN) :: pe1
!     !! Order of interpolation on edge e1
!     !! It should be greater than 1 for edge bubble to exists
!   INTEGER(I4B), INTENT(IN) :: pe2
!     !! Order of interpolation on edge e2
!     !! It should be greater than 1 for edge bubble to exists
!   INTEGER(I4B), INTENT(IN) :: pe3
!     !! Order of interpolation on edge e3
!     !! It should be greater than 1 for edge bubble to exists
!   REAL(DFP), INTENT(IN) :: xij(:, :)
!     !! Points of evaluation in xij format
!   CHARACTER(*), INTENT(IN) :: refTriangle
!     !! This parameter denotes the type of reference triangle.
!     !! It can take following values:
!     !! UNIT: in this case xij is in unit Triangle.
!     !! BIUNIT: in this case xij is in biunit triangle.
!   REAL(DFP) :: ans( &
!     & SIZE(xij, 2), &
!     & pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2), 2)
!     !!
!
!   CHARACTER(20) :: layout
!   REAL(DFP) :: x(SIZE(xij, 1), SIZE(xij, 2))
!   REAL(DFP) :: L1(SIZE(xij, 2), 0:MAX(pe1, pe2, pe3, order))
!   REAL(DFP) :: L2(SIZE(xij, 2), 0:MAX(pe1, pe2, pe3, order))
!   REAL(DFP) :: dL1(SIZE(xij, 2), 0:MAX(pe1, pe2, pe3, order))
!   REAL(DFP) :: dL2(SIZE(xij, 2), 0:MAX(pe1, pe2, pe3, order))
!   REAL(DFP) :: Lo1(SIZE(xij, 2), 0:1)
!   REAL(DFP) :: Lo2(SIZE(xij, 2), 0:1)
!   REAL(DFP) :: dLo1(SIZE(xij, 2), 0:1)
!   REAL(DFP) :: dLo2(SIZE(xij, 2), 0:1)
!
!   INTEGER(I4B) :: maxP, a, b
!
!   layout = TRIM(UpperCase(refTriangle))
!
!   IF (layout .EQ. "BIUNIT") THEN
!     x = FromBiUnitTriangle2BiUnitSqr(xin=xij)
!   ELSE
!     x = FromUnitTriangle2BiUnitSqr(xin=xij)
!   END IF
!
!   Lo1(:, 0) = 0.5_DFP * (1.0 - x(1, :))
!   Lo1(:, 1) = 0.5_DFP * (1.0 + x(1, :))
!   Lo2(:, 0) = 0.5_DFP * (1.0 - x(2, :))
!   Lo2(:, 1) = 0.5_DFP * (1.0 + x(2, :))
!   dLo1(:, 0) = -0.5_DFP
!   dLo1(:, 1) = 0.5_DFP
!   dLo2(:, 0) = -0.5_DFP
!   dLo2(:, 1) = 0.5_DFP
!
! !! Vertex basis function
! ! ans = 0.0_DFP
!   CALL VertexBasisGradient_Triangle2(Lo1=Lo1, Lo2=Lo2, dLo1=dLo1, dLo2=dLo2, &
!                                      ans=ans(:, 1:3, 1:2))
!
!   maxP = MAX(pe1, pe2, pe3, order)
!   L1 = JacobiEvalAll(n=maxP, x=x(1, :), alpha=1.0_DFP, beta=1.0_DFP)
!   L2 = JacobiEvalAll(n=maxP, x=x(2, :), alpha=1.0_DFP, beta=1.0_DFP)
!   dL1 = JacobiGradientEvalAll(n=maxP, x=x(1, :), alpha=1.0_DFP, beta=1.0_DFP)
!   dL2 = JacobiGradientEvalAll(n=maxP, x=x(2, :), alpha=1.0_DFP, beta=1.0_DFP)
!
! !! Edge basis function
!   b = 3
!   IF (pe1 .GE. 2_I4B .OR. pe2 .GE. 2_I4B .OR. pe3 .GE. 2_I4B) THEN
!     a = b + 1
!     b = a - 1 + pe1 + pe2 + pe3 - 3 !!4+qe1 + qe2 - 2
!    CALL EdgeBasisGradient_Triangle2(pe1=pe1, pe2=pe2, pe3=pe3, L1=L1, L2=L2, &
!                    Lo1=Lo1, Lo2=Lo2, dL1=dL1, dL2=dL2, dLo1=dLo1, dLo2=dLo2, &
!                                      ans=ans(:, a:b, 1:2))
!   END IF
!
! !! Cell basis function
!   IF (order .GT. 2_I4B) THEN
!     a = b + 1
!     b = a - 1 + INT((order - 1) * (order - 2) / 2)
!     CALL CellBasisGradient_Triangle2( &
!       & order=order, &
!       & L1=L1, &
!       & Lo1=Lo1, &
!       & Lo2=Lo2, &
!       & dL1=dL1, &
!       & dLo1=dLo1, &
!       & dLo2=dLo2, &
!       & eta_ij=x, ans=ans(:, a:b, 1:2))
!   END IF
! END FUNCTION HeirarchicalBasisGradient_Triangle1
